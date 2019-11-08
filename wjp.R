library(tidyverse)
library(readxl)
# source('utils.R')

wjp <- read_excel('data/WJP-Open-Gov-2015.xlsx',sheet=1) %>%
  select(country,f_3,f_3_1,f_3_2,f_3_3,f_3_4) %>%
  rename(open_gov=f_3,public_laws=f_3_1,right_to_info=f_3_2,civic_part=f_3_3,
         complaint=f_3_4) %>%
  fix_adm0 %>%
  filter(!country %in% non_countries)

setdiff(wjp$country,all_countries)
