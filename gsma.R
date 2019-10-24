library(tidyverse)
library(readxl)

mci <- read_excel('data/MCI_Data_2019.xlsx',sheet=3,skip=2) %>%
  select(2,4,
         6:10, # Four high-level indices
         11:14, # "Dimension" indices related to infrastructure
         `2G Coverage`:`Spectrum in 1-3GHz bands (MHz per operator)`,
         `TLDs per capita`) %>%
  rename(country=1,year=2,mci=3,mci_infra=4,mci_afford=5,mci_consumer=6,mci_content=7,
         mci_infra_coverage=8,mci_infra_performance=9,mci_infra_enabling=10,mci_infra_spectrum=11,
         cov_2G=`2G Coverage`,cov_3G=`3G Coverage`,cov_4G=`4G Coverage`,
         servers=`Servers per population`,tlds=`TLDs per capita`,ixps=`IXPs per population`,
         download=`Mobile download speeds`,upload=`Mobile upload speeds`,latency=`Mobile latencies`,
         elect=`Access to electricity`,bandwidth=`International Internet bandwidth per user`,
         spectrum_dd=`Digital dividend spectrum (MHz per operator)`,
         spectrum_low=`Other spectrum below 1GHz (MHz per operator)`,
         spectrum_high=`Spectrum in 1-3GHz bands (MHz per operator)`) %>%
  filter(year==2018) %>%
  select(-year) %>%
  fix_adm0

mmri <- read_excel('data/Mobile_Money_Regulatory_Index_Database.xlsx',sheet=3,skip=2) %>%
  select(2,4:10) %>%
  rename(country=1,mmri=2,mmri_auth=3,mmri_consumer=4,mmri_transact=5,mmri_kyc=6,
         mmri_agent=7,mmri_infra=8) %>%
  fix_adm0

gsma <- full_join(mci,mmri,by='country')

rm(mci,mmri)

# setdiff(gsma$country,all_countries)


