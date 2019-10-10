library(tidyverse)
library(readxl)

a4ai_pricing <- read_csv('data/A4AI-Mobile-Broadband-Pricing-2019Q2.csv') %>%
  rename(country=Country_Name) %>%
  select(country,Cost_1_GB_Share_GNICM)

afford <- read_excel('data/2018-Affordability-Index_Indicators.xlsx',sheet=1) %>%
  rename(country=1,access_a4ai=2,infrastructure_a4ai=3,overall_a4ai=4) %>%
  select(1:4)

primary <- read_excel('data/2018-Affordability-Index_Indicators.xlsx',sheet=3,skip=1) %>%
  select(Country,
         A1, # To what extent are ICT licensing frameworks flexible, simple, and technology and service neutral?
         A2, # To what extent does the government ICT regulator perform its functions according to published and transparent rules, with the ICT regulatory decisions influenced by public consultations?
         A4, # To what extent is ICT regulatory decision-making informed and influenced by adequate evidence?
         A6, # To what extent are national-level policies or rules in place to facilitate efficient access to public rights of way and tower zoning permission?
         A10) %>% # Transparent, competitive and fair process for increasing spectrum availability 
  rename(country=Country,neutral_licensing=A1,regulator_rules=A2,regulator_evidence=A4,zoning_policies=A6,spectrum_avail=A10)

secondary <- read_excel('data/2018-Affordability-Index_Indicators.xlsx',sheet=4) %>%
  rename(broadband_plan=4,intl_bandwidth=6,secure_servers=7,investment=9) %>%
  select(1,4,6,7,9)

a4ai <- full_join(a4ai_pricing,afford,by='country') %>%
  full_join(primary,by='country') %>%
  full_join(secondary,by='country') %>%
  fix_adm0

rm(a4ai_pricing,afford,primary,secondary)
