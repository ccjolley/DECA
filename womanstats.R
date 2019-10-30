### Work with the multivariate scales provided by WomanStats: 
### http://www.womanstats.org/new/

library(tidyverse)
library(readxl)
library(mice)
source('utils.R')

ws1 <- read_xls('data/MULTIVAR-SCALE-1.xls',sheet=1) %>% 
  filter(`Bibliography Year` == 2019) %>%
  select(Country,Data) %>%
  mutate(Data=as.numeric(Data)) %>%
  rename(country=1,physical_security=2)

ws2 <- read_xls('data/MULTIVAR-SCALE-2.xls',sheet=1) %>% 
  filter(`Bibliography Year` == 2015) %>%
  select(Country,Data) %>%
  mutate(Data=as.numeric(Data)) %>%
  rename(country=1,law_practice_discrepancy=2)

ws3 <- read_xls('data/MULTIVAR-SCALE-3.xls',sheet=1) %>% 
  filter(`Bibliography Year` == 2016) %>%
  select(Country,Data) %>%
  mutate(Data=as.numeric(Data)) %>%
  rename(country=1,inequity_family=2)

ws4 <- read_xls('data/MULTIVAR-SCALE-4.xls',sheet=1) %>% 
  filter(`Bibliography Year` == 2015) %>%
  select(Country,Data) %>%
  mutate(Data=as.numeric(Data)) %>%
  rename(country=1,clan_govenance=2)

ws5 <- read_xls('data/MULTIVAR-SCALE-5.xls',sheet=1) %>% 
  filter(`Bibliography Year` == 2015) %>%
  select(Country,Data) %>%
  mutate(Data=as.numeric(Data)) %>%
  rename(country=1,gov_framework=2)

ws6 <- read_xls('data/MULTIVAR-SCALE-6.xls',sheet=1) %>% 
  filter(`Bibliography Year` == 2017) %>%
  select(Country,Data) %>%
  mutate(Data=as.numeric(Data)) %>%
  rename(country=1,patrilin_frat=2)

ws <- full_join(ws1,ws2,by='country') %>%
  full_join(ws3,by='country') %>%
  full_join(ws4,by='country') %>%
  full_join(ws5,by='country') %>%
  full_join(ws6,by='country')

ws %>% filter(is.na(law_practice_discrepancy) | is.na(inequity_family))

# fill gaps with MICE

ws_imputed <- ws %>%
  select(-country) %>%
  mice(m=1,seed=1234) %>%
  mice::complete(1) 

# see how first 2 PCs look

pc <- ws_imputed %>%
  prcomp(center=TRUE,scale=TRUE)
summary(pc) # 74% in first variable

pc$x %>%
  as_tibble() %>%
  cbind(select(ws,country)) %>%
  pc_scatter(c('Kenya','Colombia','Nepal'))

sapply(setdiff(names(ws),'country'),
       function(x) cor(ws[,x],pc$x[,1],use='complete.obs')) %>%
  sort(decr=TRUE)

sapply(setdiff(names(ws),'country'),
       function(x) cor(ws[,x],pc$x[,2],use='complete.obs')) %>%
  sort(decr=TRUE)

# so PC1 correlates strongly with everything except the government framework to address inequality,
# while PC2 correlates with the government framework and sort of soaks up everything else. I'll probably
# lean more heavily on PC1 here.

pc
# in PC1 everything gets about equally weighted, except for the government framework, which gets a little less

ws_pcs <- pc$x %>%
  as_tibble() %>%
  cbind(select(ws,country)) %>%
  select(country,PC1) %>%
  rename(ws_PC1=PC1) %>%
  fix_adm0 %>%
  full_join(read_csv('pc.csv'),by='country')

ws_pcs %>% filter(is.na(PC1))  
ws_pcs %>% filter(is.na(ws_PC1))  

cor(ws_pcs$ws_PC1,ws_pcs$PC1,use='complete.obs')
# -0.849 -- pretty strong with digital sophistication

cor(ws_pcs$ws_PC1,ws_pcs$PC2,use='complete.obs')
# -0.18 -- less so with digital openness

ws_pcs %>%
  select(country,ws_PC1) %>%
  filter(!is.na(ws_PC1)) %>%
  write_csv('ws_pc.csv')
