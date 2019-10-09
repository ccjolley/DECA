library(tidyverse)
library(haven)

imf_all <- read_dta('data/IMF_financial_access_survey.dta')

imf <- imf_all %>%
  group_by(economy) %>%
  arrange(year) %>%
  summarize_all(last) %>%
  select(economy,ends_with('pop'),ends_with('pop_M'),ends_with('pop_F')) %>%
  rename(country=economy)

is.na(imf) %>%
  colMeans %>%
  sort

# Looks like there are only a few population-normalized indices that are 
# available in most countries, and many more that are available for a select
# group.

tmp <- imf
tmp$measured <- rowMeans(!is.na(tmp))
select(tmp,country,measured) %>%
  arrange(desc(measured))

# So there are a couple of things I can do with this dataset -- one would be to use a couple of
# widely-measured access indicators as part of a general assessment of financial inclusion, the
# other would be to go into greater depth for a particular economy by comparing to those where
# similar things have been measured.

tmp %>% filter(country %in% c('Kenya','Nepal','Colombia')) %>%
  select(country,measured)

rm(imf_all,tmp)
