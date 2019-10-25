source('j2sr_plot.R')

###############################################################################
# Access & Use
###############################################################################
# WEF, ITU, GSMA
source('itu.R')
source('gsma.R')
source('wef.R')
source('a4ai.R')

rename_access <- tibble(
  variable=c("mobile_subs","internet_users","fixed_internet","mobile_broadband",
             "nri_usage","mci_afford","mci_consumer",
             "fixed_bb","hh_mobile","hh_computer","hh_internet","ind_computer",
             "ind_mobile","internet_gender_gap","ind_internet","mobile_cell","fixed_tel",
             "Cost_1_GB_Share_GNICM","access_a4ai","overall_a4ai"),
    label=c('Mobile subscriptions (WEF)','Internet users (WEF)','Fixed internet subscriptions (WEF)',
            'Mobile broadband subscriptions (WEF)','ICT Use (J2SR/WEF)','Affordability (GSMA)',
            'Consumer readiness (GSMA)','Fixed broadband subscriptions (ITU)',
            'HH mobile ownership (ITU)','HH computer ownership (ITU)','HH internet use (ITU)',
            'Individual computer use (ITU)','Individual mobile use (ITU)',
            'Gender gap in internet use (ITU)','Individual internet use (ITU)',
            'Mobile subscriptions (ITU)','Fixed telephone subscriptions (ITU)',
            'Cost of 1GB data (A4AI)','Access index (A4AI)','Overall index (A4AI)')
) %>%
  mutate(flip=(variable %in% c('Cost_1_GB_Share_GNICM','internet_gender_gap')))

access_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                            sort_order='cor') {
  wef %>%
    select(country,mobile_subs,internet_users,fixed_internet,mobile_broadband,
           nri_usage) %>%
    left_join(select(gsma,country,mci_afford,mci_consumer),
              by='country') %>%
    left_join(select(itu,country,fixed_bb,hh_mobile,hh_computer,hh_internet,
                     ind_computer,ind_mobile,internet_gender_gap,ind_internet,
                     mobile_cell,fixed_tel),by='country') %>%
    left_join(select(a4ai,country,Cost_1_GB_Share_GNICM,access_a4ai,overall_a4ai),
              by='country') %>%
    j2sr_style_plot(rename_access,country_name,show_pred,
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
  variable=setdiff(names(sdg4),'country'),
  label=c('Copy/paste','Create presentation','Install software','Move files to a device',
          'Move files to a folder','Install a new device','Sent email with attachment',
          'Use spreadsheet','Write program','GG: Copy/paste','GG: Create presentation',
          'GG: Install software','GG: Move files to device','GG: Move files to folder',
          'GG: Install new device','GG: Sent email with attachment','GG: Use spreadsheet',
          'GG: Write program'),
  flip=FALSE
)

# TODO: other digital literacy variables to include?
dig_lit_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                         sort_order='none') {
  j2sr_style_plot(sdg4,rename_sdg4,country_name,show_pred,
                  shade_fraction,sort_order) +
    ggtitle(paste0('Digital literacy: ',country_name)) +
    labs(caption='GG = Gender Gap')
}
  
dig_lit_plot('Colombia')

# TODO: in this case, since many are in comparable units, it might make sense 
# to skip normalization

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

censorship_plot <- function(country_name,show_pred=TRUE,shade_fraction=NA,
                            sort_order='value') {
  left_join(vdem,fotn,by='country') %>%
    left_join(rsf,by='country') %>%
    j2sr_style_plot(rename_censor,country_name,show_pred,
                    shade_fraction,sort_order) +
      ggtitle(paste0('Censorship, information integrity, and digital rights: ',country_name))
}

censorship_plot('Colombia',shade_fraction=0.5,show_pred=FALSE,sort_order='cor')
censorship_plot('Kenya',shade_fraction=0.5,show_pred=FALSE,sort_order='cor') 
censorship_plot('China',shade_fraction=0.5,show_pred=FALSE,sort_order='cor') 

###############################################################################
# Digital society and governance
###############################################################################
rename_society <- tibble(
  variable=c('v2x_civlib','v2x_clpol','v2x_clpriv','mci_content','ict_laws','nri_enviro'),
  label=c('Civil liberties (VDem)','Political civil liberties (VDem)',
          'Private civil liberties (VDem)','Content & Services (GSMA)',
          'Laws relating to ICTs (WEF)','Environment subindex (WEF)'),
  flip=FALSE
)

society_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                        sort_order='cor') {
  vdem %>%
    select(country,starts_with('v2x_')) %>%
    left_join(select(gsma,country,mci_content),by='country') %>%
    left_join(select(wef,country,ict_laws,nri_enviro),by='country')  %>%
    j2sr_style_plot(rename_society,country_name,show_pred,
                    shade_fraction,sort_order) +
    ggtitle(paste0('Digital society and governance: ',country_name))
}

society_plot('Kenya')
society_plot('Colombia')

###############################################################################
# Digital economy -- there are a few of these
###############################################################################
### EIU Global Microscope
source('eiu.R')
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
  j2sr_style_plot(eiu,rename_eiu,country_name,show_pred,
                    shade_fraction,sort_order) +
    ggtitle(paste0('EIU Global Microscope: ',country_name))
}

eiu_plot('Kenya')
eiu_plot('Colombia') 

### GSMA Mobile Money Regulation Index

# TODO: double-check names of these variables, write a text description of each

rename_mmri <- tibble(
  variable=c("mmri","mmri_auth","mmri_consumer","mmri_transact","mmri_kyc",
             "mmri_agent","mmri_infra"),
  label=c('MMRI','Authorization','Consumer...','Transactinons','KYC','Agent network','Infrastructure'),
  flip=FALSE
)

mmri_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                      sort_order='cor') {
  select(gsma,country,starts_with('mmri')) %>%
    j2sr_style_plot(rename_mmri,country_name,show_pred,
                    shade_fraction,sort_order) +
    ggtitle(paste0('GSMA Mobile Money Regulation Index: ',country_name))
}

mmri_plot('Kenya')
mmri_plot('Colombia')

### World Bank Findex
source('wb.R')

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
    j2sr_style_plot(rename_barrier,country_name,show_pred,
                    shade_fraction,sort_order) +
    ggtitle(paste0('Findex barriers to access: ',country_name))
}

barrier_plot('Colombia')
barrier_plot('Kenya')

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
    j2sr_style_plot(rename_econ_gaps,country_name,show_pred,
                    shade_fraction,sort_order) +
    ggtitle(paste0('Findex access gaps: ',country_name))
}

econ_gaps_plot('Kenya')
econ_gaps_plot('Colombia')

### TODO: I don't yet have anything that visualizes the IMF data.

###############################################################################
# Infrastructure
###############################################################################

# EIU: mostly concerned with financial infrastructure -- "connectivity" subcomponent
# deals with things covered under A&U. they do have an indicator for digital ID, 
# but it's only binary -- not very good. Can do better with ID4D. They have numbers
# for 2G-4G connectivity; I should check to see if they line up with what I'm 
# getting from GSMA.
# MMRI: doesn't really deal with infrastructure in the same sense
# MCI: has some sub-indices on network coverage, network performance, other enabling
# infrastructure, and spectrum. I might need some help understand what the spectrum 
# numbers mean.
# TODO: go back to infrastructure notes to see what people had wanted to include there

rename_infra <- tibble(
  variable=c("mci_infra","mci_infra_coverage","cov_2G","cov_3G","cov_4G",
             "mci_infra_performance",'download','upload','latency',"mci_infra_enabling",'elect',
             'bandwidth',"servers","tlds","ixps",
             'mci_infra_spectrum','spectrum_dd','spectrum_low','spectrum_high'),
  label=c('Infrastructure (MCI subindex)','Network coverage (MCI dimension)',
          '2G coverage','3G coverage','4G coverage',
          'Nework performance (MCI dimension)','Mobile download speed','Mobile upload speed','Mobile latency',
          'Enabling infrastructure (MCI dimension)','Electricy access','Interational internet bandwidth',
          'Secure internet servers','Top-level domains per capita','IXPs per capita','Spectrum (MCI dimension)',
          'Digital dividend spectrum','Spectrum below 1 GHz','Spuctrum 1-3 GHz'),
  flip=FALSE
)

infra_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                         sort_order='none') {
  gsma %>%
    select(country,one_of(rename_infra$variable)) %>%
  j2sr_style_plot(rename_infra,country_name,show_pred,
                 shade_fraction,sort_order) +
    ggtitle(paste0('Infrastructure: ',country_name))
}

infra_plot('Kenya')
infra_plot('Colombia')
  
###############################################################################
# What hasn't been used yet?
###############################################################################
c(names(a4ai),names(fotn),names(gsma),names(itu),names(rsf),names(vdem),
  names(wef),names(wb),names(eiu),names(imf)) %>%
  setdiff(c(names(data_access),names(data_censor),names(data_society),
            names(data_econ)))
