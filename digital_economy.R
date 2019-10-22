source('wb.R')
source('eiu.R')
source('imf.R')
source('gsma.R')

data_econ <- gsma %>%
  select(country,starts_with('mmri')) %>%
  left_join(wb,by='country') %>%
  left_join(eiu,by='country') %>%
  left_join(imf,by='country')

# OK... so this is 152 variables. I need a way to pare this down...

# TODO: some of the IMF variables report male and female separately; these
# could be recast as gender-gap variables, as long as there's a population-avg
# as well.

# What do the variable counts look like for countries I'm interested in?
data_econ %>% filter(country=='Kenya') %>% melt %>% filter(!is.na(value)) %>% nrow
# 65 for Kenya
data_econ %>% filter(country=='Colombia') %>% melt %>% filter(!is.na(value)) %>% nrow
# 96 for Colombia; still a lot
data_econ %>% filter(country=='Nepal') %>% melt %>% filter(!is.na(value)) %>% nrow
# 50 for Nepal
data_econ %>% filter(country=='Republic of Serbia') %>% melt %>% filter(!is.na(value)) %>% nrow
# 62 for Serbia

# So these vary a lot -- best approach might be to customize at a country
# level rather than try to establish a global list of variables to use.
# (Although having a handful of always-use variables could be nice)

broad_coverage <- data_econ %>% filter(country %in% usaid_countries) %>% 
  is.na %>% colMeans %>% sort
broad_coverage <- names(broad_coverage[broad_coverage < 0.1])
# All of these come from WB findex; most deal with financial access gaps

data_econ_gaps <- data_econ[,broad_coverage] %>%
  left_join(read_csv('pc.csv'),by='country')
rename_econ_gaps <- tibble(
  variable=c("acct","borrow","dig_pay","acct_gender_gap",
             "borrow_gender_gap","dig_pay_gender_gap","acct_wealth_gap",
             "borrow_wealth_gap","dig_pay_wealth_gap","acct_age_gap",
             "borrow_age_gap","dig_pay_age_gap","acct_ed_gap","borrow_ed_gap",
             "dig_pay_ed_gap","acct_rural_gap","borrow_rural_gap",
             "dig_pay_rural_gap"),
  label=c('Account ownership','Borrowed','Used digital payments','Account gender gap',
          'Borrowing gender gap','Digital payments gender gap','Account wealth gap',
          'Borrowing wealth gap','Digital payments wealth gap','Account age gap',
          'Borrowing age gap','Digital payments age gap','Account education gap',
          'Borrowing education gap','Digital payments education gap',
          'Account rural/urban gap','Borrowing rural/urban gap',
          'Digital payments urban/rural gap'),
  flip=FALSE
)

econ_gaps_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                           sort_order='none') {
  j2sr_style_plot(data_econ_broad,rename_econ_broad,country_name,show_pred,
                  shade_fraction,sort_order) +
    ggtitle(paste0('Digital economy (broad coverage): ',country_name))
}

econ_gaps_plot('Kenya')
econ_gaps_plot('Colombia')

data_econ %>% filter(country=='Nepal') %>% melt %>% 
  filter(!is.na(value),!variable %in% rename_econ_gaps$variable) %>% nrow
# 47 left for Kenya, 78 for Colombia, 32 for Nepal
# I probably don't want to display more than about 25.

### Remove all columns from a tibble that are NA for a particular country
country_subset <- function(data,cname) {
  cols <- (data %>% filter(country==cname) %>% melt %>% 
             filter(!is.na(value)))$variable %>% as.character
  data[,c('country',cols)]
}

### If two columns are highly correlated, remove the one with more NAs
# TODO: better to parameterize this with a number of variables, rather
# than a correlation threshold.
cor_select <- function(d,thresh) {
  # TODO: code duplicated from cor_sort; factor out if I need it a third time
  all_cor <- d %>% select(-country) %>%
    as.matrix %>%
    cor(use='pairwise.complete.obs') %>%
    as.data.frame %>%
    rownames_to_column(var = 'var1') %>%
    gather(var2, value, -var1) %>%
    filter(var1 != var2)
  remain_cor <- arrange(all_cor,desc(value))
  missing <- data_econ %>% filter(country %in% usaid_countries) %>% 
    select(-country) %>% is.na %>% colSums
  while (max(remain_cor$value,na.rm=TRUE) > thresh) {
    v1 <- remain_cor$var1[1]
    v2 <- remain_cor$var2[1]
    if (missing[v1] >= missing[v2]) {
      drop_var <- v1
    } else { drop_var <- v2 }
    remain_cor <- filter(remain_cor,var1 != drop_var, var2 != drop_var)
  }
  keep_vars <- c(remain_cor$var1,remain_cor$var2) %>% unique
  d[,c('country',keep_vars)]
}

kenya_subset <- data_econ %>% 
  country_subset('Kenya') %>%
  cor_select(0.4)  %>%
  left_join(read_csv('pc.csv'),by='country')

rename_none <- tibble(
  variable=setdiff(names(kenya_subset),'country'),
  label=setdiff(names(kenya_subset),'country'),
  flip=FALSE
)

# Other possible ways of thinning things down:
# - variables with a small number of unique values (like mmri_kyc) don't look really good in this plot
# - maybe do something with regressions, to pull out the most "suprising" numbers
# - I'll want to include a couple of top-line "summary" numbers whenever they're available
# - might make sense to cluster variables (e.g. with k-means) and make a plot for each cluster
# - might make sense to do a DFS-specific PCA 

j2sr_style_plot(kenya_subset,rename_none,'Kenya',show_pred=FALSE,shade_fraction=0.5,
                sort_order='cor')

# New idea: calculate PCA specifically on DFS variables, find variables that both
# correlate strongly with PCs and have high coverage in USAID countries. Aim to get 2-3
# for each of the first 5 of so PCs.