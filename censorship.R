source('utils.R')
source('vdem.R')
source('fotn.R')
source('rsf.R')
library(mice)

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
highlight_countries <- c('Kenya','Colombia','Nepal')
f <- 0.03
plotme <- censor_imputed %>%
  cbind(select(censor_trim2,country)) %>%
  select(country,v2smgovfilprc,v2smgovcapsec) %>%
  rename(pc1=2,pc2=3) %>%
  filter(!is.na(pc1) & !is.na(pc2)) %>%
  mutate(color=ifelse(country %in% highlight_countries,'b','c'),
         color=ifelse(pc1 < quantile(pc1,probs=f,na.rm=TRUE) | pc1 > quantile(pc1,probs=1-f,na.rm=TRUE) |
                        pc2 < quantile(pc2,probs=f,na.rm=TRUE) | pc2 > quantile(pc2,probs=1-f,na.rm=TRUE),'a',color),
         highlight= (color != 'c'),
         label=ifelse(highlight,country,NA)
  )

ggplot(plotme,aes(x=pc1,y=pc2,color=color,label=label)) +
  geom_point(size=1) +
  geom_point(data=filter(plotme,highlight),size=3,shape=1) +
  theme_USAID + colors_USAID +
  theme(legend.position = 'none') +
  geom_text_repel() +
  xlab('Government filtering in practice') +
  ylab('Government cyber capacity')

###############################################################################
# Time-series plots
###############################################################################
source('../Futures-tools/IFs_plots.R')
country_compare('IFs_exports/mob_broadband_colombia.txt',
                ytitle='Subscriptions per 100 people',dots=TRUE) +
   ggtitle('Mobile broadband adoption')

# TODO: update numbers based on GSMA intelligence, see how close IFs tracks and update as needed