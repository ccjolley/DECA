# Is there one gender gap that works best as a proxy for the others?

source('itu.R')
source('sdg4.R')
source('wb.R')
source('imf.R')

gg <- left_join(
  select(itu,country,internet_gender_gap),
  select(sdg4,country,ends_with('gender_gap')),
  by='country') %>%
  left_join(select(wb,country,ends_with('gender_gap')),by='country') %>%
  left_join(select(imf,country,ends_with('gendergap')),by='country')

is.na(gg) %>% colMeans %>% sort
# World Bank does the best, IMF the worst

gg_trim1 <- trim_columns(gg,0.2,usaid_countries)
c(ncol(gg),ncol(gg_trim1))
# takes us from 21 columns to 9

gg_trim2 <- trim_rows(gg_trim1,0.5,usaid_countries)
c(nrow(gg_trim1),nrow(gg_trim2),length(usaid_countries))
# takes us from 230 to 153

pc <- gg_trim2 %>%
  select(-country) %>%
  mice(m=1,seed=1234) %>% 
  complete(1) %>%
  prcomp(scale=TRUE,center=TRUE)

pc$x %>%
  as_tibble() %>%
  cbind(select(gg_trim2,country)) %>%
  pc_scatter(c('Kenya','Colombia','Nepal'))
# no obvious rich-poor axis here

sapply(setdiff(names(gg_trim2),'country'),
       function(x) cor(gg_trim2[,x],pc$x[,1],use='complete.obs')) %>%
  sort(decr=TRUE)
# Digital payments gender gap has the strongest correlation, and also a small number of missing values. Score!

ws_pc <- read_csv('ws_pc.csv')

highlight_1 <- c('Kenya','Colombia','Nepal')
highlight_2 <- c('Kenya','Uganda','United Republic of Tanzania','Ethiopia','Somalia','Rwanda','Burundi')
highlight_3 <- c('Colombia','Brazil','Venezuela','Ecuador','Peru','Panama')

gg_trim2 %>%
  left_join(ws_pc,by='country') %>%
  select(country,ws_PC1,dig_pay_gender_gap) %>%
  highlight_scatter(highlight_countries = highlight_3) +
  xlab('Aggregate gender score') + ylab('Gender gap in digital payments') +
  labs(caption='Gender score derived from WomanStats')
