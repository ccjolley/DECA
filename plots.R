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
             "ind_mobile","internet_gender_gap","ind_internet","mobile_cell","fixed_tel"),
    label=c('Mobile subscriptions (WEF)','Internet users (WEF)','Fixed internet subscriptions (WEF)',
            'Mobile broadband subscriptions (WEF)','ICT Use (J2SR/WEF)','Affordability (GSMA)',
            'Consumer readiness (GSMA)','Fixed broadband subscriptions (ITU)',
            'HH mobile ownership (ITU)','HH computer ownership (ITU)','HH internet use (ITU)',
            'Individual computer use (ITU)','Individual mobile use (ITU)',
            'Gender gap in internet use (ITU)','Individual internet use (ITU)',
            'Mobile subscriptions (ITU)','Fixed telephone subscriptions (ITU)')
) %>%
  mutate(flip=(variable %in% c('Cost_1_GB_Share_GNICM','internet_gender_gap')))

access_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                            sort_order='cor',num_pcs=5,overall_score='PC1') {
  wef %>%
    select(country,mobile_subs,internet_users,fixed_internet,mobile_broadband,
           nri_usage) %>%
    left_join(select(gsma,country,mci_afford,mci_consumer),
              by='country') %>%
    left_join(select(itu,country,fixed_bb,hh_mobile,hh_computer,hh_internet,
                     ind_computer,ind_mobile,internet_gender_gap,ind_internet,
                     mobile_cell,fixed_tel),by='country') %>%
    # left_join(select(a4ai,country,Cost_1_GB_Share_GNICM,access_a4ai,overall_a4ai),
    #           by='country') %>%
    j2sr_style_plot(rename_access,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
      ggtitle(paste0('Access and use: ',country_name))
}

# access_plot('Kenya',overall_score='mean')
# access_plot('Colombia',overall_score='PC1')

###############################################################################
# Under Access & Use -- specific plot for digital literacy
###############################################################################
source('sdg4.R')

rename_sdg4 <- tibble(
  variable=c("copy_paste","create_pres","install_soft","move_device","move_file",
             "new_device","sent_email","use_spreadsheet","write_program"),
  label=c('Copy/paste','Create presentation','Install software','Move files to a device',
          'Copy/move file or folder','Install a new device','Sent email with attachment',
          'Use spreadsheet','Write program'),
  flip=FALSE
) 

rename_sdg4_gaps <- tibble(
  variable=c("copy_paste_gender_gap","create_pres_gender_gap","install_soft_gender_gap","move_device_gender_gap",
             "move_file_gender_gap","new_device_gender_gap","sent_email_gender_gap","use_spreadsheet_gender_gap",
             "write_program_gender_gap"),
  label=c('Copy/paste','Create presentation',
          'Install software','Move files to device','Copy/move file or folder',
          'Install new device','Sent email with attachment','Use spreadsheet',
          'Write program'),
  flip=FALSE
) 

# TODO: other digital literacy variables to include?
dig_lit_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                         sort_order='none',num_pcs=5,overall_score='PC1') {
  sdg4 %>% select(country,one_of(rename_sdg4$variable)) %>%
  j2sr_style_plot(rename_sdg4,country_name,show_pred,
                  shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Digital literacy: ',country_name)) 
}

dig_lit_gap_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                         sort_order='none',num_pcs=5,overall_score='PC1') {
  sdg4 %>% select(country,one_of(rename_sdg4_gaps$variable)) %>%
    j2sr_style_plot(rename_sdg4_gaps,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Digital literacy gender gaps: ',country_name)) 
}
  
# dig_lit_plot('Colombia')

# TODO: in this case, since many are in comparable units, it might make sense 
# to skip normalization

###############################################################################
# Affordability
###############################################################################
rename_afford <- tibble(
  variable=c("Cost_1_GB_Share_GNICM","access_a4ai","overall_a4ai",
             'mobile_tariffs','handset_prices','taxation','inequality',
             'smartphone_cost','prepaid_cost','postpaid_cost','fixed_bb_cost','arpu',
             'wireless_market_share','bb_market_share'),
  label=c('Cost of 1GB data (A4AI)','Access index (A4AI)','Overall index (A4AI)',
          'Mobile tariffs (GSMA)','Handset prices (GSMA)','Taxation (GSMA)',
          'Inequality (GSMA)',
          'Smartphone cost (3i)','Prepaid mobile cost (3i)','Postpaid mobile cost (3i)',
          'Fixed broadband cost (3i)','Avg. revenue per user (3i)','Wireless market share (3i)',
          'Broadband market share (3i)')
) %>%
  mutate(flip=(variable %in% c('Cost_1_GB_Share_GNICM','wireless_market_share','bb_market_share',
                               'prepaid_cost','postpaid_cost','fixed_bb_cost')))
# Smartphone cost is on a different scale and doesn't need to be flipped

afford_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                        sort_order='cor',num_pcs=5,overall_score='PC1') {
  left_join(a4ai,gsma,by='country') %>%
    left_join(eiu,by='country') %>%
    select(country,
         Cost_1_GB_Share_GNICM,access_a4ai,overall_a4ai,
         mobile_tariffs,handset_prices,taxation,inequality,
         smartphone_cost,prepaid_cost,postpaid_cost,fixed_bb_cost,arpu,
         wireless_market_share,bb_market_share) %>%
    j2sr_style_plot(rename_afford,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Affordability: ',country_name))
}

###############################################################################
# Censorship, information integrity, and digital rights
###############################################################################
source('vdem.R')
source('fotn.R')
source('rsf.R')

# TODO: this is just too big; find a reasonable way to split it up. Also include WJP indices

rename_censor <- tibble(
  variable=c('v2smgovfilprc','v2smgovshut',
             'v2smgovsm','v2smgovsmalt','v2smgovsmmon','v2smgovsmcenprc',
             'v2smregcon','v2smprivex','v2smprivcon',
             'v2smregcap','v2smregapp','v2smlawpr','v2smdefabu',
             'v2smgovdom','v2smgovab','v2smpardom','v2smparab','v2smfordom','v2smforads','v2smarrest',
             "v2x_civlib","v2x_clpol","v2x_clpriv",'press_freedom','access_obstacles','content_limits',
             'user_violations','fotn_total'),
  label=c('Gov filtering in practice',
          'Gov shutdown in practice','Social media shutdown in practice',
          'Social media alternatives','Social media monitoring','Social media censorship',
          'Internet legal regulation content','Privacy protection by law exists',
          'Privacy protection by law content','Gov capacity to regulate online content',
          'Content regulation done by state (not private)','Defamation protection',
          'Abuse of defamation/copyright law by elites',
          "Government dissemination of false information domestic",
          "Government dissemination of false information abroad",
          "Party dissemination of false information domestic",
          "Party dissemination of false information abroad",
          "Foreign governments dissemination of false information",
          "Foreign governments ads",
          "Arrests for political content",
          'Civil liberties','Political civil liberties','Private civil liberties',
          'Press freedom (RSF)','Obstacles to access (FH)','Limits on content (FH)',
          'Violations of user rights (FH)','Freedom on the net (FH)')
) %>%
  mutate(flip=grepl('\\(.*\\)',label))

# Split into logical clusters based on alignment with PCs
tmp <- left_join(vdem,fotn,by='country') %>%
  left_join(rsf,by='country') %>%
  select(one_of(rename_censor$variable)) %>%
  mice(m=1,seed=1234) %>%
  mice::complete(1) %>%
  mutate_at(filter(rename_censor,flip)$variable, function(x) -x)

pr <- prcomp(tmp,scale=TRUE,center=TRUE)
qplot(1:ncol(tmp),sort(pr$rotation[,1]))
pr$rotation[,1] %>% sort(decreasing=TRUE)
group1 <- c('v2smgovfilprc','v2x_civlib','v2smregcon','v2smarrest','v2x_clpol','v2smdefabu',
            'v2smgovshut','v2smgovsm','v2smgovsmalt','v2smgovsmcenprc','v2x_clpriv','press_freedom')
# strongest alignments with civil liberties, shutdowns, arrests for political content
qplot(1:ncol(tmp),sort(pr$rotation[,2]))
pr$rotation[,2] %>% sort(decreasing=TRUE)
group2 <- c('fotn_total','content_limits','access_obstacles','user_violations')
# four FOTN indices are strongly aligned
qplot(1:ncol(tmp),sort(pr$rotation[,3]))
pr$rotation[,3] %>% sort(decreasing=TRUE)
group3 <- c('v2smprivcon','v2smgovsmmon','v2smprivex','v2smregcap','v2smregapp',
            'v2smlawpr')
# aligns with privacy protections, social media monitoring
qplot(1:ncol(tmp),sort(pr$rotation[,4]))
pr$rotation[,4] %>% sort(decreasing=TRUE)
# aligns with disinformation
group4 <- c('v2smfordom','v2smforads','v2smpardom','v2smparab','v2smgovab','v2smgovdom')

censorship_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                            sort_order='cor',num_pcs=5,overall_score='PC1') {
  left_join(vdem,fotn,by='country') %>%
    left_join(rsf,by='country') %>%
    select(country,one_of(group1)) %>%
    j2sr_style_plot(filter(rename_censor,variable %in% group1),country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Censorship and civil liberties: ',country_name))
}

fotn_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                            sort_order='cor',num_pcs=5,overall_score='PC1') {
  left_join(vdem,fotn,by='country') %>%
    left_join(rsf,by='country') %>%
    select(country,one_of(group2)) %>%
    j2sr_style_plot(filter(rename_censor,variable %in% group2),country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Freedom on the Net: ',country_name))
}

privacy_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                      sort_order='cor',num_pcs=5,overall_score='PC1') {
  left_join(vdem,fotn,by='country') %>%
    left_join(rsf,by='country') %>%
    select(country,one_of(group3)) %>%
    j2sr_style_plot(filter(rename_censor,variable %in% group3),country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Privacy and surveillance: ',country_name))
}

info_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                      sort_order='cor',num_pcs=5,overall_score='PC1') {
  left_join(vdem,fotn,by='country') %>%
    left_join(rsf,by='country') %>%
    select(country,one_of(group4)) %>%
    j2sr_style_plot(filter(rename_censor,variable %in% group4),country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Information integrity: ',country_name))
}

censorship_plot('Colombia')
fotn_plot('Colombia')
privacy_plot('Colombia') # TODO: not all seem to point in the right direction
info_plot('Colombia')

###############################################################################
# Cybersecurity
###############################################################################
source('ncsi.R')
rename_cyber <- tibble(
  label=c('Gov cyber capacity (VDem)','Political parties cyber capacity (VDem)',
          'Global Cybersecurity Index (ITU)','National Cyber Security Index (Estonia)',
          'Gov filtering capacity (VDem)','Gov shutdown capacity (VDem)'),
  variable=c('v2smgovcapsec','v2smpolcap','itu_gci','ncsi','v2smgovfilcap',
             'v2smgovshutcap'),
  flip=FALSE
)

cyber_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                            sort_order='cor',num_pcs=5,overall_score='PC1') {
  left_join(vdem,itu,by='country') %>%
    left_join(ncsi,by='country') %>%
    select(country,one_of(rename_cyber$variable)) %>%
    j2sr_style_plot(rename_cyber,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Cybersecurity: ',country_name))
}

###############################################################################
# Digital society and governance
###############################################################################
# TODO: there are actually a lot more V-Dem indices that could be important here, related to the online media environment.
# TODO: WEF NRI series 1 also has some good content on the high-level policy enviornment around icts, besides tho few I've got here
source('open_data.R')
source('wjp.R')
rename_society <- tibble(
  variable=c('v2x_civlib','v2x_clpol','v2x_clpriv','mci_content','ict_laws','nri_enviro',
             'v2smonex','v2smonper','v2smmefra','v2smorgviol','v2smorgavgact','v2smorgelitact',
             'v2smcamp','v2smpolsoc','v2smpolhate','open_data',
             'open_gov','public_laws','right_to_info','civic_part','complaint'),
  label=c('Civil liberties (VDem)','Political civil liberties (VDem)',
          'Private civil liberties (VDem)','Content & Services (GSMA)',
          'Laws relating to ICTs (WEF)','Environment subindex (WEF)',
          "Online media existence (VDem)",
          "Online media perspectives (VDem)",
          "Online media fractionalization (VDem)",
          "Use of social media to organize offline violence (VDem)",
          "Average people’s use of social media to organize offline action (VDem)",
          "Elites’ use of social media to organize offline action (VDem)",
          "Party/candidate use of social media in campaigns (VDem)",
          "Polarization of society (VDem)",
          "Political parties hate speech (VDem)",'Open Data Index (OKF)',
          'Open government (WJP/J2SR)','Publicized laws and gov data (WJP)',
          'Right to information (WJP)','Civic participation (WJP)','Complaint mechanisms (WJP)'),
  flip=FALSE
)

society_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                        sort_order='cor',num_pcs=5,overall_score='PC1') {
  full_join(vdem,gsma,by='country') %>%
    full_join(wef,by='country') %>%
    full_join(open_data,by='country') %>%
    full_join(wjp,by='country') %>%
    select(country,one_of(rename_society$variable)) %>%
    j2sr_style_plot(rename_society,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Digital society: ',country_name))
}

# society_plot('Kenya')
# society_plot('Colombia')

###############################################################################
# Digital economy -- there are a few of these
###############################################################################
### EIU Global Microscope
source('eiu.R')
rename_eiu <- tibble(
  variable=c("gov_support","stability_integrity","products_outlets",
             "consumer_protection","infrastructure_eiu","overall_eiu"),
  label=c('Government and policy support','Stability and integrity','Products and outlets',
          'Consumer protection','Infrastructure','EIU overall'),
  flip=FALSE
)

# TODO: move these descriptions into a Google doc
# Government and policy support -- includes financial inclusion strategies, promotion of financial literacy, incentives for digitization and emerging technologies
# Stability and integrity -- includes market entry restrictions and ongoing requirements for various financial institutions, customer due diligence, supervisory capacity of regulators, commitment to cybersecurity
# Products and outlets -- features of accounts (opening requirements, deposit insurance, etc.), credit for low-income customers, fintech regulations, inclusive insurance, services offered by financial outlets
# Consumer protection -- consumer rights for financial and insurance customers, cybercrime legal protection
# Infrastructure -- payments (ATM, POS, etc.), Digital ID system, connectivity, credit information

eiu_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                     sort_order='cor',num_pcs=5) {
  j2sr_style_plot(eiu,rename_eiu,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs) +
    ggtitle(paste0('EIU Global Microscope: ',country_name))
}

# eiu_plot('Kenya',show_pred=TRUE)
# eiu_plot('Colombia') 

### GSMA Mobile Money Regulation Index

rename_mmri <- tibble(
  variable=c("mmri","mmri_auth","mmri_consumer","mmri_transact","mmri_kyc",
             "mmri_agent","mmri_infra"),
  label=c('MMRI','Authorization','Consumer Protection','Transaction Limits',
          'KYC','Agent network','Infrastructure and investment environment'),
  flip=FALSE
)

# TODO: move these descriptions into a Google doc
# Authorization -- measures the ability of non-banks to become authorized to offer MM services, incuding eligibility, initial capital requirements, and ability to offer international remittances
# Consumer protection -- measures requirements on MM providers to safeguard funds, provide deposit insurance, and adhere to consumer protection rules
# Transaction limits -- measures limits on single transactions, monthly transactions, and account balances that are prescribed by the country's Central Bank -- higher scores correspond to higher limits
# KYC -- measures how much flexibility providers enjoy in establishing customer identity, whether MM providers are required to report on AFL/CFT, and whether regulations provide requirements on identification
# Agent network -- measures how much flexibility operators have in registering agents, and what those agents are allowed to do
# Infrastructure/investment -- measures infrastructure like automated KYC verification, payments/settlements. Also discriminatory taxation on mobile money services, interest payments on MM accounts, financial inclusion policies

mmri_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                      sort_order='cor',num_pcs=5,overall_score='PC1') {
  select(gsma,country,starts_with('mmri')) %>%
    j2sr_style_plot(rename_mmri,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('GSMA Mobile Money Regulatory Index: ',country_name))
}

# mmri_plot('Kenya')
# mmri_plot('Colombia')

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
                         sort_order='cor',num_pcs=5,overall_score='PC1') {
  wb %>%
    select(country,starts_with('barrier')) %>%
    j2sr_style_plot(rename_barrier,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Findex barriers to access: ',country_name))
}

# barrier_plot('Colombia')
# barrier_plot('Kenya')

rename_findex <- tibble(
  variable=c("acct","borrow","dig_pay","mm"),
  label=c('Account ownership','Borrowed','Used digital payments','Mobile money'),
  flip=FALSE
)

rename_findex_gaps <- tibble(
  variable=c('acct_gender_gap',"borrow_gender_gap","dig_pay_gender_gap",'mm_gender_gap',"acct_wealth_gap",
             "borrow_wealth_gap","dig_pay_wealth_gap",'mm_wealth_gap',"acct_age_gap",
             "borrow_age_gap","dig_pay_age_gap",'mm_age_gap',"acct_ed_gap","borrow_ed_gap",
             "dig_pay_ed_gap",'mm_ed_gap',"acct_rural_gap","borrow_rural_gap",
             "dig_pay_rural_gap",'mm_rural_gap'),
  label=c('Account (GENDER)',
          'Borrowing (GENDER)','Digital payments (GENDER)','Mobile money (GENDER)',
          'Account (WEALTH)',
          'Borrowing (WEALTH)','Digital payments (WEALTH)','Mobile money (WEALTH)','Account (AGE)',
          'Borrowing (AGE)','Digital payments (AGE)','Mobile money (AGE)','Account (EDUCATION)',
          'Borrowing (EDUCATION)','Digital payments (EDUCATION)','Mobile money (EDUCATION)',
          'Account (URBAN/RURAL)','Borrowing (URBAN/RURAL)',
          'Digital payments (URBAN/RURAL)','Mobile money (URBAN/RURAL)'),
  flip=FALSE
)

findex_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                        sort_order='none',num_pcs=5,overall_score='PC1') {
  wb %>%
    select(country,one_of(rename_findex$variable)) %>%
    j2sr_style_plot(rename_findex,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Findex: ',country_name))
}

findex_gaps_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                           sort_order='none',num_pcs=5,overall_score='PC1') {
  wb %>%
    select(country,one_of(rename_findex_gaps$variable)) %>%
    j2sr_style_plot(rename_findex_gaps,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Findex access gaps: ',country_name))
}
# 
# econ_gaps_plot('Kenya')
# econ_gaps_plot('Colombia')

### TODO: I don't yet have anything that visualizes the IMF data.
source('imf.R')
imf_all <- read_dta('data/IMF_financial_access_survey.dta')

get_imf_label <- function(x) {
  attr(getElement(imf_all,x),'label')
}

rename_imf <- tibble(
  variable=c("i_branches_A1_pop","i_branches_A2_pop","i_branches_A3B1a_pop",
             "i_ATMs_pop","i_mob_agent_active_pop","i_mob_agent_registered_pop",
             "i_nonbranch_A1_pop","i_depositors_A1_pop","i_depositors_A1_hhs_pop",
             "i_depositors_A2_pop","i_deposit_acc_A1_pop","i_deposit_acc_A1_hhs_pop",
             "i_deposit_acc_A2_pop","i_borrowers_A1_pop","i_borrowers_A1_hhs_pop",
             "i_borrowers_A2_pop","i_borrowers_A3B1a_pop","i_loan_acc_A1_pop",
             "i_loan_acc_A1_hhs_pop","i_loan_acc_A2_pop","i_acc_loan_A3B1a_pop",
             "i_mob_acc_registered_pop","i_mob_acc_active_pop","i_mob_transactions_number_pop",
             "i_cards_credit_pop","i_cards_debit_pop","i_mob_internet_number_A1_pop",
             "i_policies_B2_life_pop","i_depositors_A1_hhs_gendergap","i_deposit_acc_A1_hhs_gendergap",
             "i_borrowers_A1_hhs_gendergap","i_borrowers_A3B1a_gendergap","i_loan_acc_A1_hhs_gendergap",
             "i_acc_loan_A3B1a_gendergap"),
  flip=FALSE
)
rename_imf$part_label <- sapply(rename_imf$variable,get_imf_label)  
rename_imf$label <- c(rename_imf$part_label[1:28],'Gender gap in depositors of hh sector with commercial banks',
                      'Gender gap in deposit accounts of hh sector with commercial banks',
                      'Gender gap in borrowers of hh sector with commercial banks',
                      'Gender gap in microfinance borrowers','Gender gap in commercial loan account ownership',
                      'Gender gap in microfinance loan account ownership')

imf_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                           sort_order='none',num_pcs=5) {
  imf %>%
    j2sr_style_plot(rename_imf,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs) +
    ggtitle(paste0('IMF Financial Access Survey: ',country_name))
}

# imf_plot('Kenya')
# imf_plot('Colombia')
# TODO: use some regex to shorten titles a little



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
          'Digital dividend spectrum','Spectrum below 1 GHz','Spectrum 1-3 GHz'),
  flip=FALSE
)

infra_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                         sort_order='none',num_pcs=5,overall_score='PC1') {
  gsma %>%
    select(country,one_of(rename_infra$variable)) %>%
  j2sr_style_plot(rename_infra,country_name,show_pred,
                 shade_fraction,sort_order,num_pcs) +
    ggtitle(paste0('Infrastructure: ',country_name))
}

# infra_plot('Kenya')
# infra_plot('Colombia')
  
###############################################################################
# Digital trade and e-commerce
###############################################################################
source('postal.R')
source('dtri.R')

rename_trade <- tibble(
  variable=c('db_score','db_trade','db_startbiz','iipd',
             'dtri','fiscal_market','establishment','data_restrictions','trading_restrictions'),
  label=c('Doing business score (WB)','Trade across borders (WB)',
          'Starting a business (WB)','Postal development index',
          'Digital Trade Restrictiveness (DTRI)','Fiscal restrictions & market access (DTRI)',
          'Establishment restrictions (DTRI)','Restrictions on data (DTRI)','Trading restrictions (DTRI)')
) %>%
  mutate(flip=grepl('DTRI',label))


# Using the mean for the overall score, because DTRI and Doing Business aren't
# very well-correlated (see China, for example)
trade_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                       sort_order='none',num_pcs=5,overall_score='mean') {
  full_join(wb,iipd,by='country') %>%
    full_join(dtri,by='country') %>%
    select(country,one_of(rename_trade$variable)) %>%
    j2sr_style_plot(rename_trade,country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('Digital trade and e-commerce: ',country_name))
}

# trade_plot('Colombia')
# trade_plot('Kenya')


