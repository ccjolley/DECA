library(tidyverse)
library(readxl)
library(reshape2)
# source('utils.R')

pop <- read_csv('data/API_SP.POP.TOTL_DS2_en_csv_v2_247892.csv',skip=4) %>%
  rename(country=`Country Name`,
         pop_2015=`2015`) %>%
  select(country,pop_2015)

wb_gpss_A <- read_excel('data/CopyofGPSSpaymentsdata2015draft.xlsx',sheet=1,skip=1) %>%
  rename(country=Country) %>%
  filter(!country %in% non_countries) %>%
  left_join(pop,by='country') %>%
  mutate(deposit_accts=`2015...2`/pop_2015,
         debit_cards=`2015...8`/pop_2015,
         credit_cards=`2015...14`/pop_2015,
         e_money_accts=`2015...20`/pop_2015,
         e_money_cards=`2015...26`/pop_2015,
         mm_accts=`2015...32`/pop_2015,
         online_accts=`2015...38`/pop_2015) %>%
  select(country,deposit_accts,debit_cards,credit_cards,e_money_accts,e_money_cards,mm_accts,online_accts) %>% 
  fix_adm0

wb_gpss_B <- read_excel('data/CopyofGPSSpaymentsdata2015draft.xlsx',sheet=2,skip=1) %>%
  rename(country=Country) %>%
  filter(!country %in% non_countries) %>%
  left_join(pop,by='country') %>%
  mutate(atms=`2015...2`/pop_2015,
         pos_term=`2015...8`/pop_2015,
         merchants=`2015...14`/pop_2015,
         atm_networks=`2015...20`, # doesn't make sense to normalize this by population
         pos_networks=`2015...26`, # doesn't make sense to normalize this by population
         psp_branches=`2015...32`/pop_2015, # skipped PSP sub-categories
         agents=`2015...56`/pop_2015) %>% # skipped agent sub-categories
  select(country,atms,pos_term,merchants,atm_networks,pos_networks,psp_branches,agents) %>%
  fix_adm0

gdp <- read_csv('data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_247793.csv',skip=4) %>%
  rename(country=`Country Name`,
         gdp_2015=`2015`) %>%
  select(country,gdp_2015)

wb_gpss_C <- read_excel('data/CopyofGPSSpaymentsdata2015draft.xlsx',sheet=3) %>%
  rename(country=`Country name`,
         var=`Variable name (see variable key in C1)`,
         usd=`Value in USD`,
         avg_size=`Average transaction size in USD`) %>%
  filter(Year==2015,!is.na(var),!country %in% non_countries) %>%
  left_join(gdp,by='country') %>%
  mutate(gdp=usd/gdp_2015) %>%
  select(country,var,gdp,avg_size) %>%
  melt %>%
  mutate(varname=paste0(var,'_',variable)) %>%
  select(country,varname,value) %>%
  dcast(country ~ varname) %>%
  fix_adm0

rename_tbl <- tibble(
  full_name=c("Made or received digital payments in the past year (% age 15+)",
              "Made or received digital payments in the past year, male  (% age 15+)",                                                                             
              "Made or received digital payments in the past year, in labor force  (% age 15+)",
              "Made or received digital payments in the past year, out of labor force  (% age 15+)",
              "Made or received digital payments in the past year, female  (% age 15+)",
              "Made or received digital payments in the past year, young adults  (% age 15-24)",
              "Made or received digital payments in the past year, older adults  (% age 25+)",
              "Made or received digital payments in the past year, primary education or less (% age 15+)",
              "Made or received digital payments in the past year, secondary education or more (% age 15+)",
              "Made or received digital payments in the past year, income, poorest 40%  (% age 15+)",
              "Made or received digital payments in the past year, income, richest 60% (% age 15+)",
              "Made or received digital payments in the past year, rural  (% age 15+)",
              "Mobile money account (% age 15+)",
              "Mobile money account, male  (% age 15+)",
              "Mobile money account, in labor force (% age 15+)",
              "Mobile money account, out of labor force (% age 15+)",
              "Mobile money account, female (% age 15+)",
              "Mobile money account, young adults  (% age 15-24)",
              "Mobile money account, older adults (% age 25+)",
              "Mobile money account, primary education or less (% age 15+)",
              "Mobile money account, secondary education or less (% age 15+)", # guessing this was a typo on their part
              "Mobile money account, income, poorest 40% (% age 15+)",
              "Mobile money account, income, richest 60%  (% age 15+)",
              "Mobile money account, rural  (% age 15+)",
              "Borrowed from a financial institution or used a credit card (% age 15+)",
              "Borrowed from a financial institution or used a credit card, male (% age 15+)",
              "Borrowed from a financial institution or used a credit card, in labor force (% age 15+)",
              "Borrowed from a financial institution or used a credit card, out of labor force (% age 15+)",
              "Borrowed from a financial institution or used a credit card, female (% age 15+)",
              "Borrowed from a financial institution or used a credit card, young adults (% age 15-24)",
              "Borrowed from a financial institution or used a credit card, older adults (% age 25+)",
              "Borrowed from a financial institution or used a credit card, primary education or less (% age 15+)",
              "Borrowed from a financial institution or used a credit card, secondary education or more (% age 15+)",
              "Borrowed from a financial institution or used a credit card, income, poorest 40% (% age 15+)",
              "Borrowed from a financial institution or used a credit card, income, richest 60% (% age 15+)",
              "Borrowed from a financial institution or used a credit card, rural (% age 15+)",
              "Financial institution account (% age 15+)",
              "Financial institution account,male(% age 15+)",
              "Financial institution account, in labor force(% age 15+)",
              "Financial institution account, out of labor force (% age 15+)",
              "Financial institution account,female(% age 15+)",
              "Financial institution account,young adults(% age 15-24)",
              "Financial institution account, older adults(% age 25+)",
              "Financial institution account, primary education or less(% age 15+)",
              "Financial institution account, seconday education or more(% age 15+)",
              "Financial institution account,income,poorest 40% (% age 15+)",
              "Financial institution account,income,richest 60% (% age 15+)",
              "Financial institution account, rural(% age 15+)",
              "No account because financial institutions are too far away (% without a financial institution account, age 15+)",
              "No account because financial services are too expensive (% without a financial institution account, age 15+)",
              "No account because of lack of necessary documentation (% without a financial institution account, age 15+)",
              "No account because of lack of trust in financial institutions (% without a financial institution account, age 15+)",
              "No account because of religious reasons (% without a financial institution account, age 15+)",
              "No account because of insufficient funds (% without a financial institution account, age 15+)",
              "No account because someone in the family has an account (% without a financial institution account, age 15+)",
              "No account because of no need for financial services ONLY (% without a financial institution account, age 15+)" 
  )                  
) %>% mutate(
  short_name=sub("(% age 15+)",'',full_name,fixed=TRUE),
  short_name=sub("(% age 15-24)",'',short_name,fixed=TRUE),
  short_name=sub("(% age 25+)",'',short_name,fixed=TRUE),
  short_name=sub(' (% without a financial institution account, age 15+)','',short_name,fixed=TRUE),
  short_name=sub(', ?male *','_m',short_name),
  short_name=sub(', ?female *','_f',short_name),
  short_name=sub(', in labor force *','_labor',short_name),
  short_name=sub(', out of labor force *','_nolabor',short_name),
  short_name=sub(', ?young adults *','_young',short_name),
  short_name=sub(', older adults *','_old',short_name),
  short_name=sub(', primary education or less *','_uned',short_name),
  short_name=sub(', secondar?y education or more *','_ed',short_name),
  short_name=sub(', secondary education or less *','_ed',short_name), 
  short_name=sub(', ?income, ?poorest 40% *','_poor',short_name),
  short_name=sub(', ?income, ?richest 60% *','_rich',short_name),
  short_name=sub(', rural *','_rural',short_name),
  short_name=sub('Made or received digital payments in the past year *','dig_pay',short_name),
  short_name=sub('Mobile money account *','mm',short_name),
  short_name=sub('Borrowed from a financial institution or used a credit card *','borrow',short_name),
  short_name=sub('Financial institution account *','acct',short_name),
  short_name=sub('No account because ','barrier',short_name),
  short_name=sub('financial institutions are too far away','_toofar',short_name),
  short_name=sub('financial services are too expensive','_tooexpens',short_name),
  short_name=sub('of lack of necessary documentation','_nodocs',short_name),
  short_name=sub('of lack of trust in financial institutions','_trust',short_name),
  short_name=sub('of religious reasons','_relig',short_name),
  short_name=sub('of insufficient funds','_nofunds',short_name),
  short_name=sub('someone in the family has an account','_fam',short_name),
  short_name=sub('of no need for financial services ONLY','_noneed',short_name)
)                

wb_findex <- read_excel('data/Global Findex Database.xlsx',sheet=1) %>%
  rename(year=`...1`, country=`...3`) %>%
  filter(!country %in% non_countries,year==2017) %>% 
  select(-year,-2,-4,-5) %>%
  melt(id.var='country') %>%
  rename(full_name=variable) %>%
  left_join(rename_tbl,by='full_name') %>%
  na.omit %>%
  select(country,short_name,value) %>%
  dcast(country ~ short_name) %>%
  fix_adm0 %>%
  mutate(acct_gender_gap=(acct_m-acct_f)/acct_m,              # Gender gaps
         borrow_gender_gap=(borrow_m-borrow_f)/borrow_m,
         dig_pay_gender_gap=(dig_pay_m-dig_pay_f)/dig_pay_m,
         mm_gender_gap=(mm_m-mm_f)/mm_m,
         acct_wealth_gap=(acct_rich-acct_poor)/acct_rich,     # Wealth gaps
         borrow_wealth_gap=(borrow_rich-borrow_poor)/borrow_rich,
         dig_pay_wealth_gap=(dig_pay_rich-dig_pay_poor)/dig_pay_rich,
         mm_wealth_gap=(mm_rich-mm_poor)/mm_rich,
         acct_emp_gap=(acct_labor-acct_nolabor)/acct_labor,   # Employment gaps
         borrow_emp_gap=(borrow_labor-borrow_nolabor)/borrow_labor,
         dig_pay_emp_gap=(dig_pay_labor-dig_pay_nolabor)/dig_pay_labor,
         mm_emp_gap=(mm_labor-mm_nolabor)/mm_labor,
         acct_age_gap=(acct_old-acct_young)/acct_old,         # Age gaps
         borrow_age_gap=(borrow_old-borrow_young)/borrow_old,
         dig_pay_age_gap=(dig_pay_old-dig_pay_young)/dig_pay_old,
         mm_age_gap=(mm_old-mm_young)/mm_old,
         acct_ed_gap=(acct_ed-acct_uned)/acct_ed,             # Education gaps
         borrow_ed_gap=(borrow_ed-borrow_uned)/borrow_ed,
         dig_pay_ed_gap=(dig_pay_ed-dig_pay_uned)/dig_pay_ed,
         mm_ed_gap=(mm_ed-mm_uned)/mm_ed,
         acct_rural_gap=(acct-acct_rural)/acct,               # Urban-rural gaps
         borrow_rural_gap=(borrow-borrow_rural)/borrow,
         dig_pay_rural_gap=(dig_pay-dig_pay_rural)/dig_pay,
         mm_rural_gap=(mm-mm_rural)/mm) %>%
  select(-ends_with('_m'),-ends_with('_f'),-ends_with('_rich'),-ends_with('_poor'),
         -ends_with('_labor'),-ends_with('_nolabor'),-ends_with('_old'),-ends_with('_young'),
         -ends_with('_ed'),-ends_with('_uned'),-ends_with('_rural'))

# Doing Business index
db_codes <- c('IC.BUS.EASE.DFRN.XQ.DB1719','TRD.ACRS.BRDR.DB1619.DFRN','IC.REG.STRT.BUS.DFRN')
# DB contains some specific cities, in addition to country-level scores
cities <- c("Beijing","Chittagong","Delhi","Dhaka","Jakarta","Kano",
            "Karachi","Lagos","Lahore","Los Angeles","Mexico City","Monterrey",
            "Moscow","Mumbai","New York","Osaka","Puerto Rico","Rio de Janeiro",
            "Sao Paulo","Shanghai","Saint Petersburg","Surabaya","Tokyo")
wb_biz <- read_csv('data/DBData.csv') %>%
  rename(country=`Country Name`,indicator=`Indicator Name`,code=`Indicator Code`,value=`2020`) %>%
  filter(code %in% db_codes) %>%
  select(country,indicator,value) %>%
  dcast(country ~ indicator) %>%
  rename(db_score=2,db_trade=3,db_startbiz=4) %>%
  fix_adm0 %>%
  filter(!country %in% cities)


wb <- full_join(wb_gpss_A,wb_gpss_B,by='country') %>%
  full_join(wb_gpss_C,by='country') %>%
  full_join(wb_findex,by='country') %>%
  full_join(wb_biz,by='country')

  
rm(wb_findex,wb_gpss_A,wb_gpss_B,wb_gpss_C,rename_tbl,pop,gdp)                                       

# setdiff(wb$country,all_countries)
