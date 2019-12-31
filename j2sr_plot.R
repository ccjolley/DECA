library(ggplot2)
library(tidyverse)
library(ggrepel)
library(reshape2)
library(mice)
source('utils.R')

###############################################################################
# Return the names of columns in a dataframe, sorted so that the first item 
# is the one with the strongest average correlations to everything else and
# highly-correlated items are adjacent to each other.
#
# Used by j2sr_style_plot() to choose ordering of rows.
###############################################################################
corr_sort <- function(d) {
  all_cor <- d %>% 
    as.matrix %>%
    cor(use='pairwise.complete.obs') %>%
    as.data.frame %>%
    rownames_to_column(var = 'var1') %>%
    gather(var2, value, -var1) %>%
    filter(var1 != var2)
  
  avg_cor <- all_cor %>% 
    group_by(var1) %>%
    summarize(value=mean(value)) %>%
    arrange(desc(value))
  
  # initialize list
  result <- avg_cor$var1[1]
  # greedy search
  remaining <- setdiff(names(d),result)
  while(length(remaining) > 0) {
    x <- result[length(result)]
    xcorr <- all_cor %>% 
      filter(var1==x,var2 %in% remaining) %>%
      arrange(desc(value))
    result <- c(result,xcorr$var2[1])
    remaining <- setdiff(names(d),result)
  }
  result
}

# rescale variable so that average value of lowest quintile is 1 and 
# highest quintile is 5
make_norm <- function(x) { 
  #(x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
  q1 <- mean(x[x <= quantile(x,probs=0.2,na.rm=TRUE)],na.rm=TRUE)
  q5 <- mean(x[x >= quantile(x,probs=0.8,na.rm=TRUE)],na.rm=TRUE)
  xnew <- 4*(x - q1)/(q5-q1) + 1
}

###############################################################################
# Calculate an "overall" value (based on PC1 of included variables),
###############################################################################
pc1_summary <- function(d,country_name,verbose=TRUE) {
  # remove rows that can't be imputed because all values (except 'country') are missing
  missing <- is.na(d) %>% rowSums
  d <- d[missing < (ncol(d)-1),]
  # impute and make PCA projection
  pr <- mice(d,m=1,seed=1234) %>%
    mice::complete(1) %>%
    select(-country) %>%
    prcomp(center=TRUE,scale=TRUE)
  if (verbose) {
    message('Weights for PC1:')
    print(pr$rotation[,1])
  }
  # if weights for PC1 are negative (on average), then flip it so it has the same
  # overall direction as the index components
  flip <- sign(mean(pr$rotation[,1]))
  # normalize PC1 to same scale used in plots
  d2 <- pr$x %>%
    as_tibble %>%
    cbind(select(d,country)) %>%
    mutate(PC1=make_norm(flip*PC1))
  d2[d2$country==country_name,'PC1']
}

###############################################################################
# Create a "J2SR-style" plot that highlights the position of one country across
# a set of indicators, relative to all others in the dataset.
#   data - Tibble containing indices to display (along with PCs that could be 
#          used for modeling)
#   rename_tbl - Tibble to rename variables to something more human-
#                intelligible. Should contain columns "variable" (corresponding
#                to column names in "data"), "label" (text description to be 
#                used in the plot), and "flip" (TRUE if the direction of the 
#                variable should be inverted so that low=bad and high=good, 
#                FALSE if it can be left as-is).
###############################################################################
# TODO: un-select variables not included in rename_tbl so that I don't get an "NA" row
j2sr_style_plot <- function(data,rename_tbl,country_name,show_pred=TRUE,
                            shade_fraction=NA,sort_order='value',num_pcs=2,
                            overall_score='PC1') {
  flip_vars <- filter(rename_tbl,flip)$variable
  flip <- function(x,flip_at=3) {
    (flip_at - x) + flip_at
  }
  tmp <- data  %>%
    select(country,one_of(rename_tbl$variable)) %>%
    mutate_at(rename_tbl$variable,make_norm) %>%
    mutate_at(flip_vars, flip) 
  
  plotme <- tmp %>%
    melt %>%
    mutate(highlight=(country==country_name)) %>%
    left_join(rename_tbl,by='variable') %>%
    group_by(variable) %>%
    arrange(!highlight) %>%
    mutate(highlight_val=first(value)) %>%
    ungroup %>%
    filter(!is.na(highlight_val)) 

  if (show_pred) {
    tmp <- tmp %>% 
      left_join(read_csv('pc.csv'),by='country')
    all_pred <- tmp %>%
      mutate_at(rename_tbl$variable,function(x) {
        f <- formula(paste0('x ~ ',paste0('tmp$PC',1:num_pcs,collapse=' + ')))
        predict(lm(f,data=tmp,na.action=na.exclude))
        })
    
    ci <- sapply(rename_tbl$variable, function(x) {
      (tmp[,x] - all_pred[,x]) %>%
        quantile(na.rm=TRUE,probs=c(0.025,0.975)) %>%
        abs %>% mean
    }) %>%
      enframe %>%
      rename(variable=name,ci=value)
    pred <- all_pred %>%
      filter(country==country_name) %>%
      melt %>%
      rename(pred=value) %>%
      left_join(ci,by='variable')
    plotme <- plotme %>%
      left_join(pred,by=c('country','variable')) %>%
      mutate(sig = abs(value-pred) > ci)
  }
  
  if (sort_order=='value') {
    plotme <- mutate(plotme,label=fct_reorder(label,highlight_val))
  } else if (sort_order=='cor') {
    cs <- corr_sort(select(tmp,-country,-starts_with('PC',ignore.case=FALSE)))
    sorted_levels <- rename_tbl$label[match(cs,rename_tbl$variable)] %>% rev
    plotme <- mutate(plotme,label=factor(label,levels=sorted_levels))
  } else if (sort_order=='none') {
    plotme <- mutate(plotme,label=factor(label,levels=rev(rename_tbl$label)))
  }

  ## Set up plot canvas
  p <- ggplot(plotme,aes(x=value,y=label,color=highlight)) 
  ## Put gray box in the back if desired
  if (!is.na(shade_fraction)) {
    low_q <- (1-shade_fraction)/2
    high_q <- 1-low_q
    low_bound <- sapply(rename_tbl$variable, function(x) {
      tmp[,x] %>% as.data.frame %>% quantile(probs=low_q,na.rm=TRUE)
    }) %>%
      enframe %>%
      mutate(name=sub('\\..*\\%','',name)) %>%
      rename(variable=name,low=value)
    high_bound <- sapply(rename_tbl$variable, function(x) {
      tmp[,x] %>% as.data.frame %>% quantile(probs=high_q,na.rm=TRUE)
    }) %>%
      enframe %>%
      mutate(name=sub('\\..*\\%','',name)) %>%
      rename(variable=name,high=value)
    
    add_boxes <- left_join(plotme,low_bound,by='variable') %>%
      left_join(high_bound,by='variable') %>%
      mutate(center=(low+high)/2,
             width=(high-low))
    p <- p + 
      geom_tile(data=add_boxes,aes(x=center,y=label,width=width,height=0.9),fill='#CFCDC9',color=NA)
  }  
  ## Add dots for countries
  x_min <- min(c(0,plotme$value[plotme$highlight],na.rm=TRUE))
  x_max <- max(c(6,plotme$value[plotme$highlight],na.rm=TRUE))
  p <- p +
    #geom_jitter(data=filter(plotme,!highlight),size=2,alpha=1,width=0,height=0.1,shape=1) +
    geom_jitter(data=filter(plotme,!highlight),size=2,alpha=0.1,width=0,height=0.1) +
    geom_point(data=filter(plotme,highlight),size=5) +
    scale_x_continuous(limits=c(x_min,x_max),breaks=1:5) +
    theme_USAID + colors_USAID +
    theme(legend.position = 'none',
          #axis.text.x=element_blank(),
          axis.title=element_blank())
  ## Add prediction lines if desired
  if (show_pred) {
    p <- p + 
      geom_errorbarh(aes(xmin=pred,xmax=pred),size=1) +
      geom_segment(aes(xend=pred,yend=label),size=1) +
      geom_point(data=filter(plotme,highlight,!sig),size=3,color='#CFCDC9') 
  }
  ### Add overall score if desired
  if (!overall_score %in% c('none','mean','PC1')) {
    message(paste0('Invalid value for overall_score: ',overall_score))
    overall_score <- 'none'
  }
  if (overall_score != 'none') {
    if (overall_score == 'mean') {
      overall <- filter(plotme,highlight)$value %>% mean
    } else if (overall_score == 'PC1') {
      overall <- pc1_summary(tmp,country_name)
    }
    p <- p +
      geom_vline(xintercept=overall,color='#BA0C2F') +
      labs(caption=paste0('Overall score: ',round(overall,1))) +
      theme(plot.caption = element_text(color = '#BA0C2F'))
  }
  p
}

