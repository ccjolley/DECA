library(ggplot2)
library(tidyverse)
library(ggrepel)
library(reshape2)
source('utils.R')

### something that looks sort of like the J2SR dashboard plots

source('vdem.R')
source('fotn.R')
source('rsf.R')
# TODO: add other sources, especially FOTN and RSF

censorship_plot <- function(country_name,show_pred=TRUE,shade_fraction=NA) {
  rename_tbl <- tibble(
    variable=c('v2smgovfilcap','v2smgovfilprc','v2smgovshutcap','v2smgovshut',
               'v2smgovsm','v2smgovsmalt','v2smgovsmmon','v2smgovsmcenprc',
               'v2smgovcapsec','v2smpolcap','v2smregcon','v2smprivex','v2smprivcon',
               'v2smregcap','v2smregapp','v2smlawpr','v2smdefabu',
               "v2x_civlib","v2x_clpol","v2x_clpriv",'press_freedom','access_obstacles','content_limits',
               'user_violations','fotn_total'),
    label=c('Gov filtering capacity','Gov filtering in practice','Gov shutdown capacity',
            'Gov shutdown in practice','Social media shutdown in practice',
            'Social media alternatives','Social media monitoring','Social media censorship',
            'Gov cyber capacity','Political parties cyber capacity',
            'Internet legal regulation content','Privacy protection by law exists',
            'Privacy protection by law content','Gov capacity to regulate online content',
            'Gov online content regulation approach','Defamation protection',
            'Abuse of defamation/copyright law by elites',
            'Civil liberties','Political civil liberties','Private civil liberties',
            'Press freedom (RSF)','Obstacles to access (FH)','Limits on content (FH)',
            'Violations of user rights (FH)','Freedom on the net (FH)')
  ) %>%
    mutate(flip=grepl('\\(.*\\)',label))
  flip_vars <- filter(rename_tbl,flip)$variable
  # Need to use this instead of scale(), because scale returns a matrix and screws up dplyr
  make_norm <- function(x) { 
    (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
  }
  tmp <- left_join(vdem,fotn,by='country') %>%
    left_join(rsf,by='country') %>%
    left_join(read_csv('pc.csv'),by='country') %>%
    mutate_at(2:26,make_norm) %>%
    mutate_at(flip_vars, function(x) -x)

  all_pred <- tmp %>%
    mutate_at(2:26,function(x) predict(lm(x ~ tmp$pc1 + tmp$pc2,na.action=na.exclude)))
  
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
    mutate(label=fct_reorder(label,highlight_val)) %>%
    left_join(pred,by=c('country','variable')) %>%
    mutate(sig = abs(value-pred) > ci)
 
  ## Set up plot canvas
  p <- ggplot(plotme,aes(x=value,y=label,color=highlight)) 
  ## Put gray box in the back if desired
  if (!is.na(shade_fraction)) {
    low_q <- (1-shade_fraction)/2
    high_q <- 1-low
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
    geom_jitter(data=filter(plotme,!highlight),size=2,alpha=1,width=0,height=0.1,shape=1) +
    #geom_jitter(data=filter(plotme,!highlight),size=2,alpha=0.1,width=0,height=0.1) +
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

censorship_plot('Colombia') +
  ggtitle('Censorship, information integrity, and digital rights: Colombia')
censorship_plot('Kenya') +
  ggtitle('Censorship, information integrity, and digital rights: Kenya')
censorship_plot('Kenya',show_pred=FALSE,shade_fraction=0.5) +
  ggtitle('Censorship, information integrity, and digital rights: Kenya')
censorship_plot('Nepal')
censorship_plot('United States of America') 
censorship_plot('China') +
  ggtitle('Censorship, information integrity, and digital rights: China')

# TODO: what happens if I add in more PCs?
# TODO: different ordering option -- put highly-correlated indices close together
# TODO: factor out so that I only need to pass in a rename_df to make plots for new variables
# TODO: make some more versions of this...


