library(ggplot2)
library(tidyverse)
library(ggrepel)
source('utils.R')

### something that looks sort of like the J2SR dashboard plots

source('vdem.R')

plotme <- vdem %>%
  select(country,v2x_civlib) %>%
  mutate(v2x_civlib=scale(v2x_civlib),
         highlight=(country=='Kenya')) 

ggplot(plotme,aes(x=v2x_civlib,y=0,color=highlight)) +
  geom_jitter(data=filter(plotme,!highlight),size=2,alpha=0.2,width=0,height=0.1) +
  geom_point(data=filter(plotme,highlight),size=5) +
  theme_USAID + colors_USAID +
  scale_y_continuous(limits=c(-2,2)) +
  theme(legend.position = 'none')

# TODO: instead of showing just one at y=0, show a series of values

rename_tbl <- tibble(
  variable=c('v2smgovfilcap','v2smgovfilprc','v2smgovshutcap','v2smgovshut',
             'v2smgovsm','v2smgovsmalt','v2smgovsmmon','v2smgovsmcenprc',
             'v2smgovcapsec','v2smpolcap','v2smregcon','v2smprivex','v2smprivcon',
             'v2smregcap','v2smregapp','v2smlawpr','v2smdefabu',
             "v2x_civlib","v2x_clpol","v2x_clpriv"),
  label=c('Gov filtering capacity','Gov filtering in practice','Gov shutdown capacity',
          'Gov shutdown in practice','Social media shutdown in practice',
          'Social media alternatives','Social media monitoring','Social media censorship',
          'Gov cyber capacity','Political parties cyber capacity',
          'Internet legal regulation content','Privacy protection by law exists',
          'Privacy protection by law content','Gov capacity to regulate online content',
          'Gov online content regulation approach','Defamation protection',
          'Abuse of defamation/copyright law by elites',
          'Civil liberties','Political civil liberties','Private civil liberties')
)


plotme2 <- vdem %>%
  mutate_at(2:21,scale) %>%
  melt %>%
  mutate(highlight=(country=='Kenya')) %>%
  left_join(rename_tbl,by='variable') %>%
  group_by(variable) %>%
  arrange(!highlight) %>%
  mutate(highlight_val=first(value)) %>%
  ungroup %>%
  mutate(label=fct_reorder(label,highlight_val))

ggplot(plotme2,aes(x=value,y=label,color=highlight)) +
  geom_jitter(data=filter(plotme2,!highlight),size=2,alpha=0.1,width=0,height=0.1) +
  geom_point(data=filter(plotme2,highlight),size=5) +
  theme_USAID + colors_USAID +
  theme(legend.position = 'none',
        axis.title=element_blank(),
        axis.text.x=element_blank())

# regress each variable with pc2
pc <- read_csv('pc.csv')

tmp <- left_join(vdem,pc,by='country')

y <- lm(v2smgovfilcap ~ pc2,data=tmp,na.action=na.exclude) %>% predict()
qplot(tmp$v2smgovfilcap,y)

pred <- tmp %>%
  mutate_at(2:21,function(x) predict(lm(x ~ tmp$pc2,na.action=na.exclude))) %>%
  filter(country=='Kenya') %>%
  melt %>%
  rename(pred=value)

plotme3 <- plotme2 %>%
  left_join(pred,by=c('country','variable'))

ggplot(plotme3,aes(x=value,y=label,color=highlight)) +
  geom_jitter(data=filter(plotme2,!highlight),size=2,alpha=0.1,width=0,height=0.1) +
  geom_point(data=filter(plotme2,highlight),size=5) +
  geom_errorbarh(aes(xmin=pred,xmax=pred),size=1) +
  geom_segment(aes(xend=pred,yend=label),size=1) +
  theme_USAID + colors_USAID +
  theme(legend.position = 'none',
        axis.title=element_blank(),
        axis.text.x=element_blank())

# TODO: figure out how to get statistical significance. I think I can get this out of
# the standard errors returned by lm

