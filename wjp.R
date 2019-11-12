library(tidyverse)
library(readxl)
library(reshape2)
# source('utils.R')

wjp_opengov <- read_excel('data/WJP-Open-Gov-2015.xlsx',sheet=1) %>%
  select(country,f_3,f_3_1,f_3_2,f_3_3,f_3_4) %>%
  rename(open_gov=f_3,public_laws=f_3_1,right_to_info=f_3_2,civic_part=f_3_3,
         complaint=f_3_4) %>%
  fix_adm0 %>%
  filter(!country %in% non_countries)

setdiff(wjp_opengov$country,all_countries)

rulelaw_rename <- tibble(
  long=c('WJP Rule of Law Index: Overall Score','Factor 4: Fundamental Rights'),
  short=c('rule_law','fundamental_rights')
)

wjp_rulelaw <- read_excel('data/FINAL_2019_wjp_rule_of_law_index_HISTORICAL_DATA_FILE_0.xlsx',
                          sheet='WJP ROL Index 2019 Scores') %>%
  slice(4,23) %>%
  melt(id.vars='Country') %>%
  rename(long=Country) %>%
  left_join(rulelaw_rename,by='long') %>%
  rename(country=variable) %>%
  select(country,value,short) %>%
  dcast(country ~ short) %>%
  mutate(rule_law=as.numeric(rule_law),
         fundamental_rights=as.numeric(fundamental_rights)) %>%
  fix_adm0

setdiff(wjp_rulelaw$country,all_countries)

wjp <- full_join(wjp_opengov,wjp_rulelaw,by='country')

rm(wjp_rulelaw,wjp_opengov)
