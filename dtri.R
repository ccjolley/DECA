# Digital trade restrictiveness index

library(pdftools)
library(tidyverse)
# source ('utils.R')

dtri_pdf <- pdf_text('data/DTRI_FINAL.pdf')

dtri_1 <- dtri_pdf[14] %>%
  read_lines() %>%
  enframe(name=NULL,value='s') %>%
  mutate(
    rank = as.numeric(substr(s,1,4)),
    country1 = str_trim(substr(s,5,11)),
    dtri = as.numeric(substr(s,14,20)),
    country2 = str_trim(substr(s,23,27)),
    fiscal_market = as.numeric(substr(s,35,40)),
    country3 = str_trim(substr(s,40,44)),
    establishment = as.numeric(substr(s,54,59)),
    country4 = str_trim(substr(s,59,63)),
    data_restrictions = as.numeric(substr(s,72,77)),
    country5 = str_trim(substr(s,77,81)),
    trading_restrictions = as.numeric(substr(s,93,98))) %>%
  filter(!is.na(rank)) %>%
  select(-s)

dtri_2 <- dtri_pdf[15] %>%
  read_lines() %>%
  enframe(name=NULL,value='s') %>%
  mutate(
    rank = as.numeric(substr(s,1,4)),
    country1 = str_trim(substr(s,5,11)),
    dtri = as.numeric(substr(s,14,19)),
    country2 = str_trim(substr(s,19,23)),
    fiscal_market = as.numeric(substr(s,32,37)),
    country3 = str_trim(substr(s,37,41)),
    establishment = as.numeric(substr(s,51,56)),
    country4 = str_trim(substr(s,56,60)),
    data_restrictions = as.numeric(substr(s,69,74)),
    country5 = str_trim(substr(s,74,78)),
    trading_restrictions = as.numeric(substr(s,90,95))) %>%
  filter(!is.na(rank)) %>%
  select(-s)

#              1         2         3         4         5         6         7         8         9         
#     123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
s <- " 39  COL      0.20 JPN          0.21 AUT           0.23 ECU          0.20 DNK             0.15"
s <- "  1  CHN      0.70     IND         0.63 CHN           0.77 CHN          0.82 CHN             0.63"

dtri_wide <- rbind(dtri_1,dtri_2)
dtri_col1 <- dtri_wide %>% select(country1,dtri) %>% rename(country=country1)
dtri_col2 <- dtri_wide %>% select(country2,fiscal_market) %>% rename(country=country2)
dtri_col3 <- dtri_wide %>% select(country3,establishment) %>% rename(country=country3)
dtri_col4 <- dtri_wide %>% select(country4,data_restrictions) %>% rename(country=country4)
dtri_col5 <- dtri_wide %>% select(country5,trading_restrictions) %>% rename(country=country5)

dtri <- full_join(dtri_col1,dtri_col2,by='country') %>%
  full_join(dtri_col3,by='country') %>%
  full_join(dtri_col4,by='country') %>%
  full_join(dtri_col5,by='country') %>%
  rename(code3=country) %>%
  left_join(read_csv('data/country_codes.csv') %>% 
              rename(country=1,code3=2,code2=3),by='code3') %>%
  select(country,dtri:trading_restrictions) %>%
  fix_adm0 %>%
  na.omit # remove EUR entry for European Union as a whole

setdiff(dtri$country,all_countries)
rm(dtri_pdf,dtri_1,dtri_2,dtri_wide,dtri_col1,dtri_col2,dtri_col3,dtri_col4,dtri_col5)

# source('wb.R')
# 
# full_join(wb,dtri,by='country') %>%
#   select(country,db_score,dtri)  %>%
#   highlight_scatter(c('Kenya','Colombia','Nepal')) +
#   xlab('Doing Business score') +
#   ylab('Digital Trade Restrictiveness Index')
