library(pdftools)
library(tidyverse)
library(string)
# source('utils.R')

postal <- pdf_text('data/postalDevelopmentReport2019En.pdf')

iipd1 <- postal[16] %>%
  read_lines() %>%
  enframe(name=NULL,value='s') %>%
  mutate(
    rank1 = as.numeric(substr(s,1,8)),
    country1 = str_trim(substr(s,9,33)),
    iipd1 = as.numeric(substr(s,34,40)),
    change1 = as.numeric(substr(s,44,49)),
    rank2 = as.numeric(substr(s,50,53)),
    country2 = str_trim(substr(s,57,78)),
    iipd2 = as.numeric(substr(s,79,85)),
    change2 = as.numeric(substr(s,86,90))) %>%
  filter(!is.na(iipd1)) %>%
  select(-s)

iipd2 <- postal[17] %>%
  read_lines() %>%
  enframe(name=NULL,value='s') %>%
  mutate(
    rank1 = as.numeric(substr(s,1,7)),
    country1 = str_trim(substr(s,8,40)),
    iipd1 = as.numeric(substr(s,40,47)),
    change1 = as.numeric(substr(s,48,55)),
    rank2 = as.numeric(substr(s,56,61)),
    country2 = str_trim(substr(s,62,79)),
    iipd2 = as.numeric(substr(s,95,100)),
    change2 = as.numeric(substr(s,102,110))
  ) %>%
  filter(!is.na(change1)) %>%
  select(-s) 

iipd_wide <- rbind(iipd1,iipd2)
iipd <- rbind(
  iipd_wide %>% select(rank1:change1) %>% rename(rank=1,country=2,iipd=3,change=4),
  iipd_wide %>% select(rank2:change2) %>% rename(rank=1,country=2,iipd=3,change=4)
) %>% 
  arrange(rank) %>%
  fix_adm0

setdiff(iipd$country,all_countries)
# TODO: some funny names in there; update fix_adm0

