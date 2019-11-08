# Extract raw numbers needed to populate template

# TODO: once I have everything here, make it nice with knitr

library(tidyverse)
source('utils.R')
source('itu.R')
source('gsma.R')
source('fotn.R')
source('vdem.R')
source('wb.R')
source('wef-plots.R')

cname <- 'Colombia'

# Mobile penetration (ITU)
filter(itu,country==cname)$ind_mobile %>%
  round(1) %>%
  paste0('Mobile penetration: ',.,'%') %>%
  message
# Internet penetration (ITU)
filter(itu,country==cname)$ind_internet %>%
  round(1) %>%
  paste0('Internet penetration: ',.,'%') %>%
  message
# Affordability score (GSMA)
filter(gsma,country==cname)$mci_afford %>%
  round(1) %>%
  paste0('Affordability score: ',.,' out of 100') %>%
  message
# 
# Freedom on the Net
filter(fotn,country==cname)$fotn_total %>%
  paste0('Freedom on the Net: ',.,' out of 100, higher is less free') %>%
  message
# Privacy protection by law (VDem, with interpretation)
priv_interpret <- tibble(
  value=0:4,
  label=c('The legal framework explicitly allows the government to access any type of personal data on the Internet.',
          'The legal framework explicitly allows the government to access most types of personal data on the Internet.',
          'The legal framework explicitly allows the government to access many types of personal data on the Internet.',
          'The legal framework explicitly allows the government to access only a few types of personal information on the Internet.',
          'The legal framework explicitly allows the government to access personal information on the Internet only in extraordinary circumstances.')
)
val <- filter(vdem,country==cname)$v2smprivcon
paste0('Privacy law content: ',val,' (0-4 scale)') %>% message
# TODO: this isn't going to work unless I pull v2smprivcon_ord from VDem API

# Gov filtering capacity (VDem)
filcap_interpret <- tibble(
  value=0:3,
  label=c('The government lacks any capacity to block access to any sites on the Internet.',
          'The government has limited capacity to block access to a few sites on the Internet.',
          'The government has adequate capacity to block access to most, but not all, specific sites on the Internet if it wanted to.',
          'The government has the capacity to block access to any sites on the Internet if it wanted to.')
)
val <- filter(vdem,country==cname)$v2smgovfilcap
# Gov filtering in practice (VDem)
filprac_interpret <- tibble(
  value=0:4,
  label=c('Extremely often. It is a regular practice for the government to remove political content, except to sites that are pro-government.',
           'Often. The government commonly removes online political content, except sites that are pro-government.',
           'Sometimes. The government successfully removes about half of the critical online political content.',
           'Rarely. There have been only a few occasions on which the government removed political content.',
           'Never, or almost never. The government allows Internet access that is unrestricted.')
)
val <- filter(vdem,country==cname)$v2smgovfilprac

# 
# Mobile money acct (Findex) 
# TODO: Clarify whether this is about active accountsn
(filter(wb,country==cname)$mm*100) %>%
  round(1) %>%
  paste0('Mobile money adoption: ',.,'%') %>%
  message
# Mobile money gender gap
(filter(wb,country==cname)$mm_gender_gap*100) %>%
  round(1) %>%
  paste0('Mobile money gender gap: Women are ',.,'% less likely to have an account than men.') %>%
  message
# 
# Starting a business 
# Trading across borders
filter(wb,country==cname)$db_trade %>%
  round(1) %>%
  paste0('Trading across borders score: ',.,' out of 100') %>%
  message
# Firm-level tech absorption (WEF Global Competitiveness Index)
# 
# Gov capacity to regulate (VDem, with interpretation)
regcap_interpret <- tibble(
  value=0:4,
  label=c('No, almost all online activity happens outside of reach of the state, where it lacks the capacity to remove illegal content.',
          'Not really. The state has extremely limited resources to regulate online content.',
          'Somewhat. The state has the capacity to regulate only some online content or some portions of the law.',
          'Mostly. The state has robust capacity to regulate online content, though not enough to regulate all content and all portions of the law.',
          'Yes, the government has sufficient capacity to regulate all online content.')
)
val <- filter(vdem,country==cname)$v2smregcap
# Avg people's use of social media (VDem)
orgavgact_interpret <- tibble(
  value=0:4,
  label=c('Never or almost never. Average people have almost never used social media to organize offline political action.',
          'Rarely. Average people do not typically use social media to organize offline political action.',
          'Sometimes. There are a few cases in which average people have used social media to organize offline political action.',
          'Often. There have been several cases in which average people have used social media toborganize offline political action.',
          'Regularly. There are numerous cases in which average people have used social media to organize offline political action.')
)
val <- filter(vdem,country==cname)$v2smorgavgact
# Individual rights (WJP rule of law)
# TODO: look at WJP Rule of Law Index
# 
# Electricity and Telephone infrastructure (WEF GCI)
# TODO: need to import WEF GCI
