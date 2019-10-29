source('utils.R')
source('vdem.R')
source('fotn.R')
source('rsf.R')
library(mice)
library(ggrepel)

censor <- left_join(vdem,fotn,by='country') %>%
  left_join(rsf,by='country')

## Trim down

censor_trim1 <- trim_columns(censor,0.4,usaid_countries)
c(ncol(censor),ncol(censor_trim1))
# keep all columns; will need to impute FOTN based on V-Dem

censor_trim2 <- trim_rows(censor_trim1,0.75,usaid_countries)
c(nrow(censor_trim1),nrow(censor_trim2),length(usaid_countries))
# goes from 198 to 175 countries

is.na(censor_trim2) %>% colMeans

## Impute

censor_mice <- censor_trim2 %>%
  select(-country) %>%
  mice(m=1,seed=1234)

censor_imputed <- mice::complete(censor_mice,1)

## PCA

pr <- prcomp(censor_imputed,center=TRUE,scale=TRUE)
plot(pr) 
summary(pr) # 61% in first two components

pr$x %>%
  as_tibble() %>%
  cbind(select(censor_trim2,country)) %>%
  pc_scatter(c('Kenya','Colombia','Nepal'))

### Correlations

pc1_cor <- sapply(setdiff(names(censor_trim2),'country'),
                  function(x) cor(censor_trim2[,x],pr$x[,1],use='complete.obs')) %>%
  sort(decr=TRUE)
head(pc1_cor)
tail(pc1_cor)
# Strongest correlation (negative) with government filtering in practice

pc2_cor <- sapply(setdiff(names(censor_trim2),'country'),
                  function(x) cor(censor_trim2[,x],pr$x[,2],use='complete.obs')) %>%
  sort(decr=TRUE)
head(pc2_cor)
tail(pc2_cor)
# Strong negative correlation with government cyber capacity

### Plot closest correlates

# TODO: factor this out into a function
highlight_1 <- c('Kenya','Colombia','Nepal')
highlight_2 <- c('Kenya','Uganda','United Republic of Tanzania','Ethiopia','Somalia','Rwanda','Burundi')
highlight_3 <- c('Colombia','Brazil','Venezuela','Ecuador','Peru','Panama')

censor_imputed %>%
  cbind(select(censor_trim2,country)) %>%
  select(country,v2smgovfilprc,v2smgovcapsec) %>%
  highlight_scatter(highlight_1) +
  xlab('Government filtering in practice') +
  ylab('Government cyber capacity')
