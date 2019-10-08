library(tidyverse)
library(readxl)
library(reshape2)

# Sticking with high-level indices for now; it will be easy enough to add
# more of the sub-components if we need them

eiu <- read_excel('data/economist_global_microscope_scores.xlsx') %>%
  rename(indicator=1) %>%
  select(-Min,-Max,-Average,-`Above average (% countries)`,
         -`Below average (% countries)`,-`Coverage (% countries)`) %>%
  slice(1,2,23,59,86,111) %>%
  melt() %>%
  rename(country=variable) %>%
  dcast(country ~ indicator) %>% 
  rename(gov_support=2,stability_integrity=3,products_outlets=4,consumer_protection=5,
         infrastructure=6,overall=7) %>%
  mutate(country=as.character(country))


