library(tidyverse)

open_data <- read_csv('data/open_data_index_places.csv') %>%
  select(name,score) %>%
  rename(country=name,open_data=score) %>%
  fix_adm0
