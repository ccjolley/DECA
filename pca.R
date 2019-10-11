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

################################################################################
# Function that removes columns that are NA for more than a given fraction of 
# USAID countries
################################################################################
trim_columns <- function(df,frac,keep_list=NULL) {
  # if no keep_list provided, include all countries
  if (is.null(keep_list)) {
    keep_list <- df$country
  }
  na_frac <- df %>% 
    filter(country %in% keep_list) %>%
    is.na %>%
    colMeans
  keep_names <- na_frac[na_frac <= (1-frac)] %>% names
  df %>% select(keep_names)
}

# How many columns to keep?
x <- 0.05*(1:20)
y <- sapply(x, function(f) {
  trim_columns(all,f,usaid_countries) %>% ncol
})
qplot(x,y) # try 0.75 for now

all_trim1 <- trim_columns(all,0.75,usaid_countries)
c(ncol(all),ncol(all_trim1))
# takes us from 244 columns to 80

###############################################################################
# Function that removes non-USAID countries for which more than a given 
# fraction of rows are missing. I'm willing to work a little harder to impute
# missing values for USAID countries, but I'm not going to spend that effort
# on Montserrat, for example.
###############################################################################
trim_rows <- function(df,frac,keep_list=c()) {
  tmp <- df %>% filter(!country %in% keep_list)
  missing <- tmp %>% 
    select(-country) %>%
    is.na %>%
    rowMeans
  also_keep <- tmp$country[missing <= (1-frac)]
  df %>% filter(country %in% c(keep_list,also_keep))
}

all_trim2 <- trim_rows(all_trim1,0.75,usaid_countries)
c(nrow(all_trim1),nrow(all_trim2),length(usaid_countries))
# took us from 255 countries to 156, of which 100 are USAID countries

###############################################################################
# Imputation using MICE
###############################################################################
all_imp <- all_trim2 %>%
  select(-country) %>%
  mice(m=1,seed=1234)

imputed <- complete(all_imp,1) 

###############################################################################
# Principal components
###############################################################################
pr <- prcomp(imputed,center=TRUE,scale=TRUE)
plot(pr) 
summary(pr)
# As we'd hope, lots of variation in the first PC; about 50% in the first two

f <- 0.03
plotme <- tibble(country=all_trim2$country,
                 pc1=pr$x[,1],
                 pc2=pr$x[,2]) %>%
  mutate(color=ifelse(country=='Kenya','b','c'),
         color=ifelse(pc1 < quantile(pc1,probs=f) | pc1 > quantile(pc1,probs=1-f) |
                      pc2 < quantile(pc2,probs=f) | pc2 > quantile(pc2,probs=1-f),'a',color),
         highlight= (color != 'c'),
         label=ifelse(highlight,country,NA),
         pc1=-pc1, # Need to do this was obvious looking at the plot
         pc2=-pc2
  )

ggplot(plotme,aes(x=pc1,y=pc2,color=color,label=label)) +
  geom_point(size=1) +
  theme_USAID + colors_USAID +
  theme(legend.position = 'none') +
  geom_text_repel() +
  xlab('PC1: Digital sophistication') +
  ylab('PC2: Digital openness')

pc1_cor <- sapply(setdiff(names(all_trim2),'country'),
       function(x) cor(all_trim2[,x],plotme$pc1,use='complete.obs')) %>%
  sort()
# Strongest correlations with Networked Readiness Index, Mobile Connectivity Index,
# anti-correlations with various findex access gaps

pc2_cor <- sapply(setdiff(names(all_trim2),'country'),
                  function(x) cor(all_trim2[,x],plotme$pc2,use='complete.obs')) %>%
  sort()
# Strongest correlations with press freedom and government filtering capacity

plotme %>%
  select(country,pc1,pc2) %>%
  write_csv('pc.csv')
