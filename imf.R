library(tidyverse)
library(haven)
# source('utils.R')

imf_all <- read_dta('data/IMF_financial_access_survey.dta')

imf <- imf_all %>%
  group_by(economy) %>%
  arrange(year) %>%
  summarize_all(last) %>%
  select(economy,ends_with('pop'),ends_with('pop_M'),ends_with('pop_F')) %>%
  rename(country=economy) %>%
  fix_adm0

grep('_M',names(imf),value=TRUE)
imf <- imf %>%
  mutate(i_depositors_A1_hhs_gendergap = (i_depositors_A1_hhs_pop_M - i_depositors_A1_hhs_pop_F)/i_depositors_A1_hhs_pop_M,
         i_deposit_acc_A1_hhs_gendergap = (i_deposit_acc_A1_hhs_pop_M - i_deposit_acc_A1_hhs_pop_F)/i_deposit_acc_A1_hhs_pop_M,
         i_borrowers_A1_hhs_gendergap = (i_borrowers_A1_hhs_pop_M - i_borrowers_A1_hhs_pop_F) / i_borrowers_A1_hhs_pop_M,
         i_borrowers_A3B1a_gendergap = (i_borrowers_A3B1a_pop_M - i_borrowers_A3B1a_pop_F) / i_borrowers_A3B1a_pop_M,
         i_loan_acc_A1_hhs_gendergap = (i_loan_acc_A1_hhs_pop_M - i_loan_acc_A1_hhs_pop_F) / i_loan_acc_A1_hhs_pop_M,
         i_acc_loan_A3B1a_gendergap = (i_acc_loan_A3B1a_pop_M - i_acc_loan_A3B1a_pop_F) / i_acc_loan_A3B1a_pop_M) %>%
  select(-ends_with('_M'),-ends_with('_F'))

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
