source('plots.R')
source('../Futures-tools/IFs_plots.R')

###############################################################################
# This is a place to collect all of the plots I need for the desk review 
# template.
###############################################################################

cname <- 'Kenya'
highlight_list <- c('Kenya','Uganda','United Republic of Tanzania','Ethiopia',
                    'Somalia','Rwanda','Burundi','Tanzania')
cname <- 'Colombia'
highlight_list <- c('Colombia','Brazil','Venezuela','Ecuador','Peru','Panama')

cname <- 'North Macedonia'
highlight_list <- c('Albania','Bosnia','Croatia','Montenegro','North Macedonia','Republic of Serbia','Kosovo')

###############################################################################
# Infrastructure
###############################################################################

infra_plot(cname)
infra_plot(cname,show_pred=TRUE,num_pcs=5,overall_score=NULL)

###############################################################################
# Access and Use
###############################################################################

full_join(wef,a4ai,by='country') %>%
  select(country,internet_users,Cost_1_GB_Share_GNICM) %>%
  highlight_scatter(highlight_list) +
  xlab('Percent of internet users') +
  ylab('Relative cost of 1 GB data')

full_join(itu,gsma,by='country') %>%
  select(country,ind_internet,mobile_tariffs) %>%
  mutate(country=ifelse(country=='United Republic of Tanzania','Tanzania',country)) %>%
  highlight_scatter(highlight_list) +
  xlab('Percent of internet users (ITU)') +
  ylab('Affordability of mobile tariffs (GSMA)')

# access indicator plot

access_plot(cname)
access_plot(cname,show_pred=TRUE,num_pcs=5,overall_score=NULL)

# adoption time series plot

country_compare('IFs_exports/mob_broadband_colombia.txt',
                ytitle='Subscriptions per 100 people',dots=TRUE) +
  ggtitle('Mobile broadband adoption') +
  scale_x_continuous(breaks=c(2010,2015,2020,2025,2030))

# affordability indicators

afford_plot(cname,overall_score='PC1')
afford_plot(cname,show_pred=TRUE,num_pcs=5,overall_score=NULL)
# TODO: PC1 summary really doesn't look right for Colombia

# digital literacy indicators

dig_lit_plot(cname,overall_score='PC1')
dig_lit_gap_plot(cname,overall_score='PC1')

source('wef-plots.R')
wef_literacy_plot(cname)
wef_literacy_plot(cname,show_pred=TRUE,num_pcs=5,overall_score=NULL)

# WEF private sector

# TODO: not all of this needs to be source'd here
wef_private_plot(cname,overall_score='PC1')
wef_private_plot(cname,show_pred=TRUE,num_pcs=5,overall_score=NULL)

# WEF public sector

wef_public_plot(cname,overall_score='PC1')
wef_public_plot(cname,show_pred=TRUE,num_pcs=5,overall_score=NULL)

###############################################################################
# Digital society and governance
###############################################################################

society_plot(cname)
society_plot(cname,show_pred=TRUE,num_pcs=5,overall_score=NULL)

# Gender scatterplot

ws <- read_csv('ws_pc.csv')

left_join(ws,wb,by='country') %>%
  select(country,ws_PC1,dig_pay_gender_gap) %>%
  highlight_scatter(highlight_countries = highlight_list) +
  xlab('Aggregate gender score') + ylab('Gender gap in digital payments') +
  labs(caption='Gender score derived from WomanStats')

# Plot capturing most important dimensions from society_plot
society_all <- full_join(vdem,gsma,by='country') %>%
  full_join(wef,by='country') %>%
  full_join(open_data,by='country') %>%
  full_join(wjp,by='country') %>%
  select(country,one_of(rename_society$variable)) %>%
  mice(m=1) %>%
  mice::complete(1) %>%
  select(-country)
society_pr <- prcomp(society_all,center=TRUE,scale=TRUE)$x %>%
  as.tibble

apply(society_all,2,function(x) cor(x,society_pr$PC1)) %>% 
  sort(decreasing=TRUE)
# V-Dem civil liberties come out on top
apply(society_all,2,function(x) cor(x,society_pr$PC2)) %>% 
  sort(decreasing=TRUE)
# V-Dem using social media to organize offline action

vdem %>%
  select(country,v2x_civlib,v2smorgavgact) %>%
  highlight_scatter(highlight_countries=highlight_list) +
  xlab('Civil liberties') + 
  ylab("Average peoples' use of social media to organize offline action") +
  labs(caption='Source: V-Dem')

###############################################################################
# Cennsorship, information integrity and digital rights
###############################################################################

# freedom scatterplot
vdem %>%
  select(country,v2smgovfilprc,v2smgovcapsec) %>%
  highlight_scatter(highlight_list) +
  xlab('Government filtering in practice') +
  ylab('Government cyber capacity')

# freedom indicators

censorship_plot(cname,sort_order='cor',overall_score='PC1')
censorship_plot(cname,show_pred=TRUE,num_pcs=5,overall_score='PC1')

# cybersecurity indicators

cyber_plot(cname)
cyber_plot(cname,show_pred=TRUE,num_pcs=5)

privacy_plot(cname,show_pred=TRUE,num_pcs=5)
info_plot(cname,show_pred=TRUE,num_pcs=5)
fotn_plot(cname,show_pred=TRUE,num_pcs=5)

###############################################################################
# Digital finance
###############################################################################

# findex indicators

findex_plot(cname)
findex_plot(cname,show_pred=TRUE,num_pcs=5)
findex_gaps_plot(cname)
findex_gaps_plot(cname,show_pred=TRUE,num_pcs=5)
# TODO: this plot could use some more visual cues to separate categories

# findex barriers

barrier_plot(cname,overall_score='mean')
# sticking with the mean here, because PC1 doesn't make a lot of sense in this context

# EIU Global Microscope

eiu_plot(cname)
eiu_plot(cname,show_pred=TRUE,num_pcs=5)

# GSMA MMRI

mmri_plot(cname,overall_score='PC1')
mmri_plot(cname,overall_score='PC1',show_pred=TRUE,num_pcs=5)
# TODO: actually needs a new option for overall score when one of the components *is* the overall score

# Plot capturing most important digital finance features
finance_all <- eiu %>%
  full_join(select(gsma,country,starts_with('mmri')),by='country') %>%
  full_join(select(wb,country,one_of(rename_findex$variable)),by='country') %>%
  mice(m=1) %>%
  mice::complete(1) %>%
  select(-country)
finance_pr <- prcomp(finance_all,center=TRUE,scale=TRUE)$x %>%
  as.tibble

apply(finance_all,2,function(x) cor(x,finance_pr$PC1)) %>% 
  sort(decreasing=TRUE)
# Findex account ownership, digital payments
apply(finance_all,2,function(x) cor(x,finance_pr$PC2)) %>% 
  sort(decreasing=TRUE)
# EIU overall and EIU stability/integrity

full_join(wb,eiu,by='country') %>%
  select(country,dig_pay,stability_integrity) %>%
  highlight_scatter(highlight_countries=highlight_list) +
  xlab('Use of digital payments (WB)') + 
  ylab("Stability and integrity (EIU)") 


###############################################################################
# Digital trade and e-commerce
###############################################################################

# Postal scatterplot

source('postal.R')
# TODO: no need to source all of this
left_join(iipd,itu,by='country') %>%
  select(country,iipd,ind_internet) %>%
  highlight_scatter(highlight_list) +
  xlab('Integrated Index for Postal Development') +
  ylab('Percent of internet users (ITU)')

trade_plot(cname)
trade_plot(cname,show_pred=TRUE,num_pcs=5,overall_score=NULL)






