library(vdem)
library(tidyverse)

# Pull the following into a tibble, and filter for the most recent year
# v2smgovfilcap (gov filtering capacity), 
# v2smgovfilprc (gov filtering in practice), 
# v2smgovshutcap (gov shutdown capacity), 
# v2smgovshut (gov shutdown in practice), 
# v2smgovsm (social media shutdown in practice),
# v2smgovsmalt (social media alternatives), 
# v2smgovsmmon (social media monitoring), 
# v2smgovsmcenprc (social media censorship), 
# v2smgovcapsec (gov cyber capacity), 
# v2smpolcap (political parties cyber capacity)
# v2smregcon (internet legal regulation content)
# v2smprivex (privacy protection by law exists)
# v2smprivcon (privacy protection by law content)
# v2smregcap (gov capacity to regulate online content)
# v2smregapp (gov online content regulation approach)
# v2smlawpr (defamation protection)
# v2smdefabu (abuse of defamation/copyright law by elites)
# v2smgovdom     Government dissemination of false information domestic         
# v2smgovab      Government dissemination of false information abroad           
# v2smpardom     Party dissemination of false information domestic              
# v2smparab      Party dissemination of false information abroad                
# v2smfordom     Foreign governments dissemination of false information         
# v2smforads     Foreign governments ads                                        
# v2smonex       Online media existence                                         
# v2smonper      Online media perspectives                                      
# v2smmefra      Online media fractionalization                                 
# v2smorgviol    Use of social media to organize offline violence               
# v2smorgavgact  Average people’s use of social media to organize offline action
# v2smorgelitact Elites’ use of social media to organize offline action         
# v2smcamp       Party/candidate use of social media in campaigns               
# v2smarrest     Arrests for political content                                  
# v2smpolsoc     Polarization of society                                        
# v2smpolhate    Political parties hate speech 


vdem_digital <- c('v2smgovfilcap','v2smgovfilprc','v2smgovshutcap','v2smgovshut','v2smgovsm','v2smgovsmalt','v2smgovsmmon',
                  'v2smgovsmcenprc','v2smgovcapsec','v2smpolcap','v2smregcon','v2smprivex','v2smprivcon','v2smregcap',
                  'v2smregapp','v2smlawpr','v2smdefabu','v2smgovdom','v2smgovab','v2smpardom','v2smparab','v2smfordom',
                  'v2smforads','v2smonex','v2smonper','v2smmefra','v2smorgviol','v2smorgavgact','v2smorgelitact','v2smcamp',
                  'v2smarrest','v2smpolsoc','v2smpolhate')
vdem1 <- extract_vdem(name_pattern='^v2sm.*') %>%
  select(vdem_country_name,year,vdem_digital) %>%
  rename(country=vdem_country_name) %>%
  na.omit %>%
  group_by(country) %>%
  arrange(year) %>%
  summarize_all(last) %>%
  select(-year) %>%
  fix_adm0 %>%
  group_by(country) %>% # Need to do this because V-Dem treats West Bank & Gaza separately, but fix_adm0 maps both together
  summarize_all(mean)

vdem2 <- extract_vdem(name_pattern='v2x_c.*') %>%
  select(vdem_country_name,year,v2x_civlib,v2x_clpol,v2x_clpriv) %>%
  rename(country=vdem_country_name) %>%
  na.omit %>%
  group_by(country) %>%
  arrange(year) %>%
  summarize_all(last) %>% 
  select(-year) %>%
  fix_adm0 %>%
  group_by(country) %>% # Need to do this because V-Dem treats West Bank & Gaza separately, but fix_adm0 maps both together
  summarize_all(mean)

vdem <- full_join(vdem1,vdem2,by='country')
rm(vdem1,vdem2,vdem_digital)