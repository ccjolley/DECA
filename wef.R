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
         code %in% c('A','B','C','D','NRI','3.03','6.01','6.02','6.05','6.06'),
         Edition==2016) %>%
  select(code,Albania:Zimbabwe) %>%
  melt(id.var='code') %>%
  dcast(variable ~ code) %>% 
  rename(country=variable,nri_enviro=A,nri_readiness=B,nri_usage=C,nri_impact=D,nri=NRI,
         bandwidth_wef=`3.03`,mobile_subs=`6.01`,internet_users=`6.02`,fixed_internet=`6.05`,
         mobile_broadband=`6.06`) %>%
  na.omit %>%
  mutate_at(2:11,as.numeric) %>%
  fix_adm0

wef <- full_join(wef_laws,wef_nri,by='country')
rm(wef_laws,wef_nri)

# setdiff(wef$country,all_countries)
