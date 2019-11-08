library(tidyverse)
library(readxl)
library(reshape2)

# Sticking with high-level indices for now; it will be easy enough to add
# more of the sub-components if we need them

eiu_gm <- read_excel('data/economist_global_microscope_scores.xlsx') %>%
  rename(indicator=1) %>%
  select(-Min,-Max,-Average,-`Above average (% countries)`,
         -`Below average (% countries)`,-`Coverage (% countries)`) %>%
  slice(1,2,23,59,86,111) %>%
  melt() %>%
  rename(country=variable) %>%
  dcast(country ~ indicator) %>% 
  rename(gov_support=2,stability_integrity=3,products_outlets=4,consumer_protection=5,
         infrastructure_eiu=6,overall_eiu=7) %>%
  mutate(country=as.character(country)) %>%
  fix_adm0

eiu_3i <- read_csv('data/3i-index-data.csv') %>%
  select(`Country/Group`,
         `2.1.1) Smartphone cost (handset) / Score of 0-100, 100=most affordable`,
         `2.1.2) Mobile phone cost (prepaid tariff) / % of monthly GNI per capita`, 
         `2.1.3) Mobile phone cost (postpaid tariff) / % of monthly GNI per capita`, 
         `2.1.4) Fixed-line monthly broadband cost / % of monthly GNI per capita`, 
         `2.2.1) Average revenue per user (ARPU, annualized) / USD`, 
         `2.2.2) Wireless operators' market share / HHI score (0-10,000)`, 
         `2.2.3) Broadband operators' market share / HHI score (0-10,000)`) %>%
  rename(country=1,smartphone_cost=2,prepaid_cost=3,postpaid_cost=4,fixed_bb_cost=5,
         arpu=6,wireless_market_share=7,bb_market_share=8) %>%
  mutate(prepaid_cost=as.numeric(prepaid_cost)) %>% # one of these was NA
  fix_adm0 %>%
  filter(!country %in% non_countries)

eiu <- full_join(eiu_gm,eiu_3i,by='country')
