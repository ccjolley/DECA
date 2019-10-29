library(ggplot2)
library(ggrepel)
library(mice)
# Combine all data series, impute missing data, perform PCA

scripts <- c('utils.R','a4ai.R','eiu.R','fotn.R','gsma.R','imf.R','itu.R',
             'rsf.R','sdg4.R','vdem.R','wb.R','wef.R')
for (s in scripts) { source(s) }

all <- full_join(a4ai,eiu,by='country') %>% # most country duplicates present after this step
  full_join(fotn,by='country') %>%
  full_join(gsma,by='country') %>%
  full_join(imf,by='country') %>%
  full_join(itu,by='country') %>%
  full_join(rsf,by='country') %>%
  full_join(sdg4,by='country') %>%
  full_join(vdem,by='country') %>%
  full_join(wb,by='country') %>%
  full_join(wef,by='country') 



# How many columns to keep?
x <- 0.05*(1:20)
y <- sapply(x, function(f) {
  trim_columns(all,f,usaid_countries) %>% ncol
})
qplot(x,y) # try 0.75 for now

all_trim1 <- trim_columns(all,0.75,usaid_countries)
c(ncol(all),ncol(all_trim1))
# takes us from 250 columns to 92



all_trim2 <- trim_rows(all_trim1,0.75,usaid_countries)
c(nrow(all_trim1),nrow(all_trim2),length(usaid_countries))
# took us from 255 countries to 156, of which 100 are USAID countries

###############################################################################
# Imputation using MICE
###############################################################################
all_imp <- all_trim2 %>%
  select(-country) %>%
  mice(m=1,seed=1234)

imputed <- mice::complete(all_imp,1) 

###############################################################################
# Principal components
###############################################################################
pr <- prcomp(imputed,center=TRUE,scale=TRUE)
plot(pr) 
summary(pr)
# As we'd hope, lots of variation in the first PC; about 46% in the first two
# need 30 PCs to cover 90% of variance

# TODO: revise this to use pc_scatter instead

f <- 0.03
plotme <- tibble(country=all_trim2$country,
                 pc1=pr$x[,1],
                 pc2=pr$x[,2]) %>%
  mutate(color=ifelse(country %in% c('Kenya','Colombia','Nepal'),'b','c'),
         color=ifelse(pc1 < quantile(pc1,probs=f) | pc1 > quantile(pc1,probs=1-f) |
                      pc2 < quantile(pc2,probs=f) | pc2 > quantile(pc2,probs=1-f),'a',color),
         highlight= (color != 'c'),
         label=ifelse(highlight,country,NA),
         pc1=-pc1, # Need to do this was obvious looking at the plot
         pc2=-pc2
  )

ggplot(plotme,aes(x=pc1,y=pc2,color=color,label=label)) +
  geom_point(size=1) +
  geom_point(data=filter(plotme,highlight),size=3,shape=1) +
  theme_USAID + colors_USAID +
  theme(legend.position = 'none') +
  geom_text_repel() +
  xlab('PC1: Digital sophistication') +
  ylab('PC2: Digital openness')

pc1_cor <- sapply(setdiff(names(all_trim2),'country'),
       function(x) cor(all_trim2[,x],plotme$pc1,use='complete.obs')) %>%
  sort(decr=TRUE)
head(pc1_cor)
tail(pc1_cor)
# Strongest correlations with Networked Readiness Index, Mobile Connectivity Index, 
# infrastructure and adoption measures
# anti-correlations with various findex access gaps

pc2_cor <- sapply(setdiff(names(all_trim2),'country'),
                  function(x) cor(all_trim2[,x],plotme$pc2,use='complete.obs')) %>%
  sort(decr=TRUE)
head(pc2_cor)
tail(pc2_cor)
# Strongest correlations with civil liberties and government filtering in practice

cbind(select(all_trim2,country),
      as_tibble(pr$x[,1:30])) %>%
  write_csv('pc.csv')


plotme %>%
  select(country,pc1,pc2) %>%
  write_csv('pc.csv')
