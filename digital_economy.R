source('j2sr_plot.R')
source('wb.R')
source('eiu.R')
source('imf.R')
source('gsma.R')

data_econ <- gsma %>%
  select(country,starts_with('mmri')) %>%
  left_join(wb,by='country') %>%
  left_join(eiu,by='country') %>%
  left_join(imf,by='country')

###############################################################################
# OK... so this is 152 variables. I need a way to pare this down...
###############################################################################
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



data_econ %>% filter(country=='Nepal') %>% melt %>% 
  filter(!is.na(value),!variable %in% rename_econ_gaps$variable) %>% nrow
# 47 left for Kenya, 78 for Colombia, 32 for Nepal
# I probably don't want to display more than about 25.

###############################################################################
# Try country-specific filtering
###############################################################################
### Remove all columns from a tibble that are NA for a particular country
country_subset <- function(data,cname) {
  cols <- (data %>% filter(country==cname) %>% melt %>% 
             filter(!is.na(value)))$variable %>% as.character
  data[,c('country',cols)]
}

### If two columns are highly correlated, remove the one with more NAs
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

j2sr_style_plot(kenya_subset,rename_none,'Kenya',show_pred=FALSE,shade_fraction=0.5,
                sort_order='cor')

###############################################################################
# Other possible ways of thinning things down:
# - variables with a small number of unique values (like mmri_kyc) don't look really good in this plot
# - maybe do something with regressions, to pull out the most "suprising" numbers
# - I'll want to include a couple of top-line "summary" numbers whenever they're available
# - might make sense to cluster variables (e.g. with k-means) and make a plot for each cluster
# - might make sense to do a DFS-specific PCA 
###############################################################################

################################################################################
# New idea: calculate PCA specifically on DFS variables, find variables that both
# correlate strongly with PCs and have high coverage in USAID countries. Aim to get 2-3
# for each of the first 5 of so PCs.
################################################################################
# TODO: copied from pca.R -- maybe move it to utils.R
# remove columns that are NA for too many USAID countries
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

x <- 0.05*(1:20)
y <- sapply(x, function(f) {
  trim_columns(data_econ,f,usaid_countries) %>% ncol
})
qplot(x,y) # high coverage is pretty much all findex

trim1 <- trim_columns(data_econ,0.45,usaid_countries)
# anything lower than 45% doesn't really work
c(ncol(data_econ),ncol(trim1))
trim_rows <- function(df,frac,keep_list=c()) {
  tmp <- df %>% filter(!country %in% keep_list)
  missing <- tmp %>% 
    select(-country) %>%
    is.na %>%
    rowMeans
  also_keep <- tmp$country[missing <= (1-frac)]
  df %>% filter(country %in% c(keep_list,also_keep))
}

trim2 <- trim_rows(trim1,0.75,usaid_countries)
c(nrow(trim1),nrow(trim2),length(usaid_countries))
# 168 countries down to 97
setdiff(trim2$country,usaid_countries)
setdiff(usaid_countries,trim2$country)

imp <- trim2 %>%
  select(-country) %>%
  mice(m=1,seed=1234)

imputed <- mice::complete(imp,1) 
imputed <- select(imputed,-one_of(rename_econ_gaps$variable))
pr <- prcomp(imputed,center=TRUE,scale=TRUE)
plot(pr) 
summary(pr)

econ_pcs <- pr$x %>% 
  as_tibble %>%
  select(PC1:PC5) 

with_pcs <- cbind(trim2,econ_pcs) %>% as_tibble

# now what I want to do is find the variables that correlate strongly with PC1

var_select_plot <- function(pc,cutoff) {
  plotme <- tibble(varname=setdiff(names(trim2),'country')) %>%
    mutate(abscor=sapply(varname, function(x) {
      abs(cor(with_pcs[,x],with_pcs[,paste0('PC',pc)],use='pairwise.complete.obs'))
      }),
      missing=sapply(varname, function(x) {
        sum(!is.na(with_pcs[,x]))
        })
    ) 
  corr_hi <- quantile(plotme$abscor,probs=cutoff)
  miss_hi <- quantile(plotme$missing,probs=cutoff)
  plotme <- plotme %>%
    mutate(label=ifelse(abscor >= corr_hi,varname,NA),
           highlight=!is.na(label)) 
  
  ggplot(plotme,aes(x=abscor,y=missing,label=label,color=highlight)) +
    geom_point() +
    geom_text_repel() +
    xlab(paste0('Absolute correlation with PC',pc)) +
    ylab('Number of non-missing observations') +
    theme_USAID + colors_USAID +
    theme(legend.position = 'none')
}

var_select_plot(1,0.9)  
# has something to do with account ownership, ATMs and familial barriers to access
# acct, i_ATMs_pop, debit_cards, infrastructure_eiu
var_select_plot(2,0.95)
# access barriers of other kinds (starts with barrier_)
var_select_plot(3,0.9)
# MMRI
var_select_plot(4,0.8) 
# mobile money and digital payments (findex)
var_select_plot(5,0.9)
# employment gaps

# TODO: need text descriptions of what all these things are
rename_eiu <- tibble(
  variable=c("gov_support","stability_integrity","products_outlets",
             "consumer_protection","infrastructure_eiu","overall_eiu"),
  label=c('Government support','Stability/integrity','Products and outlets',
          'Consumer protection','Infrastructure','EIU overall'),
  flip=FALSE
)

eiu_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                           sort_order='cor') {
  left_join(eiu,read_csv('pc.csv'),by='country') %>%
    j2sr_style_plot(rename_eiu,country_name,show_pred,
                    shade_fraction,sort_order) +
      ggtitle(paste0('EIU Global Microscope: ',country_name))
}

eiu_plot('Kenya')
eiu_plot('Colombia') 

data_mmri <- select(gsma,country,starts_with('mmri')) %>%
  left_join(read_csv('pc.csv'),by='country')

# TODO: double-check names of these variables, write a text description of each

rename_mmri <- tibble(
  variable=c("mmri","mmri_auth","mmri_consumer","mmri_transact","mmri_kyc",
             "mmri_agent","mmri_infra"),
  label=c('MMRI','Authorization','Consumer...','Transactinons','KYC','Agent network','Infrastructure'),
  flip=FALSE
)

mmri_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                     sort_order='cor') {
  j2sr_style_plot(data_mmri,rename_mmri,country_name,show_pred,
                  shade_fraction,sort_order) +
    ggtitle(paste0('GSMA Mobile Money Readiness Index: ',country_name))
}

mmri_plot('Kenya')
mmri_plot('Colombia')

rename_barrier <- tibble(
  variable=c("barrier_fam","barrier_nodocs","barrier_nofunds","barrier_noneed",
             "barrier_relig","barrier_tooexpens","barrier_toofar","barrier_trust"),
  label=c('Family','No documents','No funds','No need','Religion','Too expensive',
          'Too far away','Lack of trust'),
  flip=FALSE
)

barrier_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                         sort_order='cor') {
  wb %>%
    select(country,starts_with('barrier')) %>%
    left_join(read_csv('pc.csv'),by='country') %>%
    j2sr_style_plot(rename_barrier,country_name,show_pred,
                    shade_fraction,sort_order) +
      ggtitle(paste0('Findex barriers to access: ',country_name))
}

barrier_plot('Colombia')
barrier_plot('Kenya')

###############################################################################
# Indicators with near-universal coverage (all from WB Findex)
###############################################################################
rename_econ_gaps <- tibble(
  variable=c("acct","borrow","dig_pay","mm","acct_gender_gap",
             "borrow_gender_gap","dig_pay_gender_gap",'mm_gender_gap',"acct_wealth_gap",
             "borrow_wealth_gap","dig_pay_wealth_gap",'mm_wealth_gap',"acct_age_gap",
             "borrow_age_gap","dig_pay_age_gap",'mm_age_gap',"acct_ed_gap","borrow_ed_gap",
             "dig_pay_ed_gap",'mm_ed_gap',"acct_rural_gap","borrow_rural_gap",
             "dig_pay_rural_gap",'mm_rural_gap'),
  label=c('Account ownership','Borrowed','Used digital payments','Mobile money',
          'Account gender gap',
          'Borrowing gender gap','Digital payments gender gap','Mobile money gender gap',
          'Account wealth gap',
          'Borrowing wealth gap','Digital payments wealth gap','Mobile money wealth gap','Account age gap',
          'Borrowing age gap','Digital payments age gap','Mobile money age gap','Account education gap',
          'Borrowing education gap','Digital payments education gap','Mobile money education gap',
          'Account rural/urban gap','Borrowing rural/urban gap',
          'Digital payments urban/rural gap','Mobile money urban/rural gap'),
  flip=FALSE
)

econ_gaps_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                           sort_order='none') {
  wb %>%
    select(country,one_of(rename_econ_gaps$variable)) %>%
    left_join(read_csv('pc.csv')) %>%
    j2sr_style_plot(rename_econ_gaps,country_name,show_pred,
                    shade_fraction,sort_order) +
      ggtitle(paste0('Findex access gaps: ',country_name))
}

econ_gaps_plot('Kenya')
econ_gaps_plot('Colombia')

