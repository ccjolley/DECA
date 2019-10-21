library(ggplot2)
library(tidyverse)
library(ggrepel)
library(reshape2)
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
# TODO: what happens if I add in more PCs?
###############################################################################
j2sr_style_plot <- function(data,rename_tbl,country_name,show_pred=TRUE,
                            shade_fraction=NA,sort_order='value') {
  flip_vars <- filter(rename_tbl,flip)$variable
  # Need to use this instead of scale(), because scale returns a matrix and screws up dplyr
  make_norm <- function(x) { 
    (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
  }
  
  tmp <- data  %>%
    mutate_at(rename_tbl$variable,make_norm) %>%
    mutate_at(flip_vars, function(x) -x)
  
  all_pred <- tmp %>%
    mutate_at(rename_tbl$variable,function(x) predict(lm(x ~ tmp$pc1 + tmp$pc2,na.action=na.exclude)))
  
  ci <- sapply(names(tmp)[2:26], function(x) {
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
  
  plotme <- tmp %>%
    select(-pc1,-pc2) %>%
    melt %>%
    mutate(highlight=(country==country_name)) %>%
    left_join(rename_tbl,by='variable') %>%
    group_by(variable) %>%
    arrange(!highlight) %>%
    mutate(highlight_val=first(value)) %>%
    ungroup %>%
    filter(!is.na(highlight_val)) %>%
    left_join(pred,by=c('country','variable')) %>%
    mutate(sig = abs(value-pred) > ci)
  
  if (sort_order=='value') {
    plotme <- mutate(plotme,label=fct_reorder(label,highlight_val))
  } else if (sort_order=='cor') {
    cs <- corr_sort(select(tmp,-country,-starts_with('pc')))
    sorted_levels <- rename_tbl$label[match(cs,rename_tbl$variable)] %>% rev
    plotme <- mutate(plotme,label=factor(label,levels=sorted_levels))
  }
  
  ## Set up plot canvas
  p <- ggplot(plotme,aes(x=value,y=label,color=highlight)) 
  ## Put gray box in the back if desired
  if (!is.na(shade_fraction)) {
    low_q <- (1-shade_fraction)/2
    high_q <- 1-low_q
    low_bound <- sapply(names(tmp)[2:26], function(x) {
      tmp[,x] %>% as.data.frame %>% quantile(probs=low_q,na.rm=TRUE)
    }) %>%
      enframe %>%
      mutate(name=sub('\\..*\\%','',name)) %>%
      rename(variable=name,low=value)
    high_bound <- sapply(names(tmp)[2:26], function(x) {
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
  p <- p +
    #geom_jitter(data=filter(plotme,!highlight),size=2,alpha=1,width=0,height=0.1,shape=1) +
    geom_jitter(data=filter(plotme,!highlight),size=2,alpha=0.1,width=0,height=0.1) +
    geom_point(data=filter(plotme,highlight),size=5) +
    theme_USAID + colors_USAID +
    theme(legend.position = 'none',
          axis.title=element_blank(),
          axis.text.x=element_blank())
  ## Add prediction lines if desired
  if (show_pred) {
    p <- p + 
      geom_errorbarh(aes(xmin=pred,xmax=pred),size=1) +
      geom_segment(aes(xend=pred,yend=label),size=1) +
      geom_point(data=filter(plotme,highlight,!sig),size=3,color='#CFCDC9') 
  }
  p
}
