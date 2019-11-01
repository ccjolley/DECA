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
    country2 = str_trim(substr(s,62,94)),
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

###############################################################################
# UNCTAD's B2C e-commerce index correlates this postal capacity dataset with 
# three other things -- see which makes for the best scatterplot.
# Internet users (ITU)
# Secure servers (WB)
# Financial accounts (WB Findex)
###############################################################################

source('wef.R')
source('gsma.R')
source('wb.R')

plotme <- full_join(iipd,select(wef,country,internet_users),by='country') %>%
  full_join(select(gsma,country,servers),by='country') %>%
  full_join(select(wb,country,acct),by='country') %>%
  select(country,iipd,internet_users,servers,acct)

qplot(plotme$iipd,plotme$internet_users)
qplot(plotme$iipd,plotme$servers)
qplot(plotme$iipd,plotme$acct)  

cor(plotme$iipd,plotme$internet_users,use='complete.obs') # lowest correlation -- may be my best choice
cor(plotme$iipd,plotme$servers,use='complete.obs')
cor(plotme$iipd,plotme$acct,use='complete.obs')  

highlight_1 <- c('Kenya','Colombia','Nepal')
plotme %>%
  select(country,iipd,internet_users) %>%
  highlight_scatter(highlight_1) +
  xlab('Integrated Index for Postal Development') +
  ylab('Percent of internet users')

cor(plotme$servers,plotme$internet_users,use='complete.obs') 
cor(plotme$internet_users,plotme$acct,use='complete.obs')
cor(plotme$iipd,plotme$acct,use='complete.obs')  

plotme %>%
  select(country,servers,acct) %>%
  highlight_scatter(highlight_1) +
  xlab('Secure servers per population') +
  ylab('Ownership of financial accounts')
