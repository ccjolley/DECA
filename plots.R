source('j2sr_plot.R')

###############################################################################
# Access & Use
###############################################################################
# WEF, ITU, GSMA
source('itu.R')
source('gsma.R')
source('wef.R')
source('a4ai.R')

data_access <- wef %>%
  select(country,mobile_subs,internet_users,fixed_internet,mobile_broadband,
         nri_usage) %>%
  left_join(select(gsma,country,mci_afford,mci_consumer,cov_2G,cov_3G,cov_4G),
            by='country') %>%
  left_join(select(itu,country,fixed_bb,hh_mobile,hh_computer,hh_internet,
                   ind_computer,ind_mobile,internet_gender_gap,ind_internet,
                   mobile_cell,fixed_tel),by='country') %>%
  left_join(select(a4ai,country,Cost_1_GB_Share_GNICM,access_a4ai,overall_a4ai),
            by='country') %>%
  left_join(read_csv('pc.csv'),by='country')

rename_access <- tibble(
  variable=c("mobile_subs","internet_users","fixed_internet","mobile_broadband",
             "nri_usage","mci_afford","mci_consumer","cov_2G","cov_3G","cov_4G",
             "fixed_bb","hh_mobile","hh_computer","hh_internet","ind_computer",
             "ind_mobile","internet_gender_gap","ind_internet","mobile_cell","fixed_tel",
             "Cost_1_GB_Share_GNICM","access_a4ai","overall_a4ai"),
    label=c('Mobile subscriptions (WEF)','Internet users (WEF)','Fixed internet subscriptions (WEF)',
            'Mobile broadband subscriptions (WEF)','ICT Use (J2SR/WEF)','Affordability (GSMA)',
            'Consumer readiness (GSMA)','2G coverage (GSMA)','3G coverage (GSMA)',
            '4G coverage (GSMA)','Fixed broadband subscriptions (ITU)',
            'HH mobile ownership (ITU)','HH computer ownership (ITU)','HH internet use (ITU)',
            'Individual computer use (ITU)','Individual mobile use (ITU)',
            'Gender gap in internet use (ITU)','Individual internet use (ITU)',
            'Mobile subscriptions (ITU)','Fixed telephone subscriptions (ITU)',
            'Cost of 1GB data (A4AI)','Access index (A4AI)','Overall index (A4AI)')
) %>%
  mutate(flip=(variable %in% c('Cost_1_GB_Share_GNICM','internet_gender_gap')))

access_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                            sort_order='cor') {
  j2sr_style_plot(data_access,rename_access,country_name,show_pred,
                  shade_fraction,sort_order) +
    ggtitle(paste0('Access and use: ',country_name))
}

access_plot('Kenya')
access_plot('Colombia')

###############################################################################
# Under Access & Use -- specific plot for digital literacy
###############################################################################
source('sdg4.R')

# TODO: double-check these names
rename_sdg4 <- tibble(
  variable=setdiff(names(sdg4),'country)',
  label=c('Copy/paste','Create presentation','Install software','Move files to a device',
          'Move files to a folder','Install a new device','Sent email with attachment',
          'Use spreadsheet','Write program','GG: Copy/paste','GG: Create presentation',
          'GG: Install software','GG: Move files to device','GG: Move files to folder',
          'GG: Install new device','GG: Sent email with attachment','GG: Use spreadsheet',
          'GG: Write program')
)

# TODO: finish this

###############################################################################
# Censorship, information integrity, and digital rights
###############################################################################
source('vdem.R')
source('fotn.R')
source('rsf.R')

rename_censor <- tibble(
  variable=c('v2smgovfilcap','v2smgovfilprc','v2smgovshutcap','v2smgovshut',
             'v2smgovsm','v2smgovsmalt','v2smgovsmmon','v2smgovsmcenprc',
             'v2smgovcapsec','v2smpolcap','v2smregcon','v2smprivex','v2smprivcon',
             'v2smregcap','v2smregapp','v2smlawpr','v2smdefabu',
             "v2x_civlib","v2x_clpol","v2x_clpriv",'press_freedom','access_obstacles','content_limits',
             'user_violations','fotn_total'),
  label=c('Gov filtering capacity','Gov filtering in practice','Gov shutdown capacity',
          'Gov shutdown in practice','Social media shutdown in practice',
          'Social media alternatives','Social media monitoring','Social media censorship',
          'Gov cyber capacity','Political parties cyber capacity',
          'Internet legal regulation content','Privacy protection by law exists',
          'Privacy protection by law content','Gov capacity to regulate online content',
          'Gov online content regulation approach','Defamation protection',
          'Abuse of defamation/copyright law by elites',
          'Civil liberties','Political civil liberties','Private civil liberties',
          'Press freedom (RSF)','Obstacles to access (FH)','Limits on content (FH)',
          'Violations of user rights (FH)','Freedom on the net (FH)')
) %>%
  mutate(flip=grepl('\\(.*\\)',label))

data_censor <- left_join(vdem,fotn,by='country') %>%
  left_join(rsf,by='country') %>%
  left_join(read_csv('pc.csv'),by='country')

censorship_plot <- function(country_name,show_pred=TRUE,shade_fraction=NA,
                            sort_order='value') {
  j2sr_style_plot(data_censor,rename_censor,country_name,show_pred,
                  shade_fraction,sort_order) +
    ggtitle(paste0('Censorship, information integrity, and digital rights: ',country_name))
}

censorship_plot('Colombia',shade_fraction=0.5,show_pred=FALSE,sort_order='cor')
censorship_plot('Kenya',shade_fraction=0.5,show_pred=FALSE,sort_order='cor') 
censorship_plot('China',shade_fraction=0.5,show_pred=FALSE,sort_order='cor') 

###############################################################################
# Digital society and governance
###############################################################################
data_society <- vdem %>%
  select(country,starts_with('v2x_')) %>%
  left_join(select(gsma,country,mci_content),by='country') %>%
  left_join(select(wef,country,ict_laws,nri_enviro),by='country') %>%
  left_join(read_csv('pc.csv'),by='country')
  
rename_society <- tibble(
  variable=c('v2x_civlib','v2x_clpol','v2x_clpriv','mci_content','ict_laws','nri_enviro'),
  label=c('Civil liberties (VDem)','Political civil liberties (VDem)',
          'Private civil liberties (VDem)','Content & Services (GSMA)',
          'Laws relating to ICTs (WEF)','Environment subindex (WEF)'),
  flip=FALSE
)

society_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                        sort_order='cor') {
  j2sr_style_plot(data_society,rename_society,country_name,show_pred,
                  shade_fraction,sort_order) +
    ggtitle(paste0('Digital society and governance: ',country_name))
}

society_plot('Kenya')
society_plot('Colombia')

###############################################################################
# Digital economy
###############################################################################
source('wb.R')
source('eiu.R')
source('imf.R')
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

  
###############################################################################
# What hasn't been used yet?
###############################################################################
c(names(a4ai),names(fotn),names(gsma),names(itu),names(rsf),names(vdem),
  names(wef),names(wb),names(eiu),names(imf)) %>%
  setdiff(c(names(data_access),names(data_censor),names(data_society),
            names(data_econ)))
