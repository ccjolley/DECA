source('j2sr_plot.R')

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


