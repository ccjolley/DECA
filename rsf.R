library(tidyverse)

rsf <- read_csv('data/index_2019_-_pour_import_1_1.csv',
                col_types=cols(`Score A`=col_character())) %>%
  rename(country=EN_country) %>%
  mutate(press_freedom=as.numeric(sub(',','.',`Score A`))) %>%
  select(country,press_freedom) %>%
  fix_adm0

setdiff(rsf$country,all_countries)
