library(tidyverse)
library(readxl)
library(reshape2)
# source('utils.R')

wef_laws <- read_csv('data/laws_relating_to_icts.csv') %>%
  filter(Indicator=='Laws relating to ICTs, 1-7 (best)',
         `Subindicator Type`=='Index (1-7)') %>%
  rename(country=`Country Name`,ict_laws=`2016`) %>%
  select(country,ict_laws) %>%
  na.omit %>%
  fix_adm0

wef_nri <- read_excel('data/WEF_NRI_2012-2016_Historical_Dataset.xlsx',sheet=2,skip=3) %>%
  rename(code=`Code NRI 2016`) %>%
  filter(Attribute=='Value',
         code %in% c('A','B','C','D','NRI'),
         Edition==2016) %>%
  select(code,Albania:Zimbabwe) %>%
  melt(id.var='code') %>%
  dcast(variable ~ code) %>% 
  rename(country=variable,nri_enviro=A,nri_readiness=B,nri_usage=C,nri_impact=D,nri=NRI) %>%
  na.omit %>%
  fix_adm0

# TODO: there are some other NRI components recommended in "Access and use"

wef <- full_join(wef_laws,wef_nri,by='country')
rm(wef_laws,wef_nri)