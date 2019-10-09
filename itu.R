library(pdftools)
library(tidyverse)
library(readxl)
# source('utils.R')

gci_text <- pdf_text('data/draft-18-00706_Global-Cybersecurity-Index-EV5_print_2.pdf')
# pull out tables on pages 64-70
itu_gci <- gci_text[64:70] %>%
  read_lines() %>%
  str_squish() %>%
  enframe(name=NULL) %>%
  filter(!grepl('Global Cybersecurity Index 2018',value),
         !grepl('Annex B',value),
         !grepl('The countries marked',value),
         !grepl('submitted their answers',value),
         !grepl('Member State',value),
         !value %in% c('',62:68)) %>%
  mutate(country=sub(' 0\\..*','',value),
         country=sub('\\*','',country),
         itu_gci=as.numeric(str_extract(value,'0\\..* '))) %>%
  select(country,itu_gci) %>%
  fix_adm0


itu_core <- read_excel('data/CoreHouseholdIndicators_Jun2019.xlsx',skip=2) %>%
  select(2,4,7,10,13,16,19,25,29,32) %>%
  rename(country=1,hh_radio=2,hh_tv=3,hh_fixedtel=4,hh_mobile=5,hh_computer=6,
         hh_internet=7,ind_computer=8,ind_internet=9,ind_mobile=10) %>%
  filter(!is.na(country)) %>%
  mutate_at(vars(2:10),as.numeric) %>%
  fix_adm0

itu_bb <- read_excel('data/Fixed_broadband_2000-2018_Jun2019_revised27082019.xls',skip=1) %>%
  rename(country=1) %>%
  select(country,`2018...40`) %>%
  rename(fixed_bb=2) %>%
  na.omit %>%
  fix_adm0

itu_tel <- read_excel('data/Fixed_tel_2000-2018_Jun2019_revised_27082019.xls',skip=1) %>%
  rename(country=1) %>%
  select(country,`2018...40`) %>%
  rename(fixed_tel=2) %>%
  na.omit %>% fix_adm0

itu_mobile <- read_excel('data/Mobile_cellular_2000-2018_Jun2019.xls',skip=1) %>%
  rename(country=1) %>%
  select(country,`2018...40`) %>%
  rename(mobile_cell=2) %>%
  na.omit %>%
  fix_adm0

# 2017 data are a lot more complete than 2018, so use 2017 if 2018 is missing
itu_ind_internet <- read_excel('data/Individuals_Internet_2000-2018_Jun2019.xls',skip=1) %>%
  rename(country=1) %>%
  select(country,`2017`,`2018`) %>%
  rename(y2017=2,y2018=3) %>%
  mutate(ind_internet=ifelse(!is.na(y2018),y2018,y2017)) %>%
  select(country,ind_internet) %>%
  fix_adm0

# Calculate gender gap using formula from GSMA Mobile Gender Gap report
itu_gender <- read_excel('data/Individuals using the internet by gender_Jun2019.xlsx',skip=2) %>%
  slice(1:115) %>%
  mutate(Male=as.numeric(Male),
         Female=as.numeric(Female),
         internet_gender_gap=(Male-Female)/Male) %>%
  rename(country=1) %>%
  select(country,internet_gender_gap) %>%
  fix_adm0

itu <- full_join(itu_bb,itu_core,by='country') %>%
  full_join(itu_gci,by='country') %>%
  full_join(itu_gender,by='country') %>%
  full_join(itu_ind_internet,by='country') %>%
  full_join(itu_mobile,by='country') %>%
  full_join(itu_tel,by='country')

rm(gci_text,itu_bb,itu_core,itu_gci,itu_gender,itu_ind_internet,itu_mobile,itu_tel)
