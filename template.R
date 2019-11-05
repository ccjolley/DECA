source('plots.R')
source('../Futures-tools/IFs_plots.R')

###############################################################################
# This is a place to collect all of the plots I need for the desk review 
# template.
###############################################################################

# cname <- 'Kenya'
# highlight_list <- c('Kenya','Uganda','United Republic of Tanzania','Ethiopia',
#                     'Somalia','Rwanda','Burundi')
cname <- 'Colombia'
highlight_list <- c('Colombia','Brazil','Venezuela','Ecuador','Peru','Panama')

# access scatterplot: internet users & cost

full_join(wef,a4ai,by='country') %>%
  select(country,internet_users,Cost_1_GB_Share_GNICM) %>%
  highlight_scatter(highlight_list) +
  xlab('Percent of internet users') +
  ylab('Relative cost of 1 GB data')

# access indicator plot

access_plot(cname)

# adoption time series plot

country_compare('IFs_exports/mob_broadband_colombia.txt',
                ytitle='Subscriptions per 100 people',dots=TRUE) +
  ggtitle('Mobile broadband adoption') +
  scale_x_continuous(breaks=c(2010,2015,2020,2025,2030))

# affordability indicators

afford_plot(cname,overall_score='mean')
# TODO: PC1 summary really doesn't look right for Colombia

# digital literacy indicators

dig_lit_plot(cname,overall_score='mean')

# WEF private sector

# TODO: not all of this needs to be source'd here
source('private-sector.R')
wef_private_plot(cname,overall_score='mean')

# WEF public sector

wef_public_plot(cname,overall_score='mean')
# TODO: something weird about PC1 here

# freedom scatterplot
vdem %>%
  select(country,v2smgovfilprc,v2smgovcapsec) %>%
  highlight_scatter(highlight_list) +
  xlab('Government filtering in practice') +
  ylab('Government cyber capacity')

# freedom indicators

censorship_plot(cname,sort_order='cor',overall_score='mean')

# cybersecurity indicators

cyber_plot(cname)

# findex indicators

econ_gaps_plot(cname,overall_score='mean')
# TODO: this plot could use some more visual cues to separate categories

# findex barriers

barrier_plot(cname,overall_score='mean')
# TODO: somehow, for Kenya and Colombia, the PC1 overall score is much lower in value that all of the points -- doesn't look right

# EIU Global Microscope

eiu_plot(cname)

# GSMA MMRI

mmri_plot(cname,overall_score='mean')
# TODO: actually needs a new option for overall score when one of the components *is* the overall score

# Postal scatterplot

source('postal.R')
# TODO: no need to source all of this
left_join(iipd,wef,by='country') %>%
  select(country,iipd,internet_users) %>%
  highlight_scatter(highlight_list) +
  xlab('Integrated Index for Postal Development') +
  ylab('Percent of internet users')

# Governance indicators

society_plot(cname)

# Gender scatterplot

ws <- read_csv('ws_pc.csv')

left_join(ws,wb,by='country') %>%
  select(country,ws_PC1,dig_pay_gender_gap) %>%
  highlight_scatter(highlight_countries = highlight_list) +
  xlab('Aggregate gender score') + ylab('Gender gap in digital payments') +
  labs(caption='Gender score derived from WomanStats')

# Infrastructure

infra_plot(cname)