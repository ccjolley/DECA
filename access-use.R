source('utils.R')
source('itu.R')
source('gsma.R')
source('wef.R')
source('a4ai.R')
library(mice)

access <- wef %>%
  select(country,mobile_subs,internet_users,fixed_internet,mobile_broadband,
         nri_usage) %>%
  left_join(select(gsma,country,mci_afford,mci_consumer),
            by='country') %>%
  left_join(select(itu,country,fixed_bb,hh_mobile,hh_computer,hh_internet,
                   ind_computer,ind_mobile,internet_gender_gap,ind_internet,
                   mobile_cell,fixed_tel),by='country') %>%
  left_join(select(a4ai,country,Cost_1_GB_Share_GNICM,access_a4ai,overall_a4ai),
            by='country') 

## Trim down

access_trim1 <- trim_columns(access,0.75,usaid_countries)
c(ncol(access),ncol(access_trim1))
# takes us from 21 columns to 13

access_trim2 <- trim_rows(access_trim1,0.75,usaid_countries)
c(nrow(access_trim1),nrow(access_trim2),length(usaid_countries))
# keeps all 139 countries

is.na(access_trim2) %>% colMeans

## Impute

access_mice <- access_trim2 %>%
  select(-country) %>%
  mice(m=1,seed=1234)

access_imputed <- mice::complete(access_mice,1)

## PCA

pr <- prcomp(access_imputed,center=TRUE,scale=TRUE)
plot(pr) 
summary(pr) # 91% in first two components

pr$x %>%
  as_tibble() %>%
  cbind(select(access_trim2,country)) %>%
  pc_scatter(c('Kenya','Colombia','Nepal'))

### Correlations

pc1_cor <- sapply(setdiff(names(access_trim2),'country'),
                  function(x) cor(access_trim2[,x],pr$x[,1],use='complete.obs')) %>%
  sort(decr=TRUE)
head(pc1_cor)
tail(pc1_cor)
# Strongest correlation with fraction of internet users

pc2_cor <- sapply(setdiff(names(access_trim2),'country'),
                  function(x) cor(access_trim2[,x],pr$x[,2],use='complete.obs')) %>%
  sort(decr=TRUE)
head(pc2_cor)
tail(pc2_cor)
# No correlations are super-strong, but strongest is with affordability

### Plot closest correlates

# TODO: factor this out into a function
highlight_countries <- c('Kenya','Colombia','Nepal')
f <- 0.03
plotme <- access_imputed %>%
  cbind(select(access_trim2,country)) %>%
  select(country,internet_users,Cost_1_GB_Share_GNICM) %>%
  rename(pc1=internet_users,pc2=Cost_1_GB_Share_GNICM) %>%
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
  xlab('Percent of internet users') +
  ylab('Relative cost of 1 GB of data')

###############################################################################
# Time-series plots
###############################################################################
source('../Futures-tools/IFs_plots.R')
country_compare('IFs_exports/mob_broadband_colombia.txt',
                ytitle='Subscriptions per 100 people',dots=TRUE) +
   ggtitle('Mobile broadband adoption')

# TODO: update numbers based on GSMA intelligence, see how close IFs tracks and update as needed