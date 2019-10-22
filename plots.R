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

