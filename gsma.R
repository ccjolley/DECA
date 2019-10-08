library(tidyverse)
library(readxl)

mci <- read_excel('data/MCI_Data_2019.xlsx',sheet=3,skip=2) %>%
  select(2,4,6:10) %>%
  rename(country=1,year=2,mci=3,mci_infra=4,mci_afford=5,mci_consumer=6,mci_content=7) %>%
  filter(year==2018) %>%
  select(-year)

mmri <- read_excel('data/Mobile_Money_Regulatory_Index_Database.xlsx',sheet=3,skip=2) %>%
  select(2,4:10) %>%
  rename(country=1,mmri=2,mmri_auth=3,mmri_consumer=4,mmri_transact=5,mmri_kyc=6,
         mmri_agent=7,mmri_infra=8)

gsma <- left_join(mci,mmri,by='country')

rm(mci,mmri)
