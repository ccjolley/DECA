source('j2sr_plot.R')

###############################################################################
# Tech adoption in private sector
###############################################################################
rename_wef_private <- tibble(
  series = c("Firm-level technology absorption, 1-7 (best)","Capacity for innovation, 1-7 (best)",
             "PCT patents, applications/million pop.","ICT use for business-to-business transactions, 1-7 (best)",
             "Business-to-consumer Internet use, 1-7 (best)","Extent of staff training, 1-7 (best)",
             "Availability of latest technologies, 1-7 (best)"),
  label = c("Firm-level tech absorption","Innovation capacity",
            "PCT patent applications","ICT use for B2B",
            "B2C Internet use","Extent of staff training",'Latest tech available'),
  variable = c('tech_abs','inno_capacity','pct_patents','b2b_use','b2c_use','staff_training',
               'tech_avail'),
  flip=FALSE
)

wef_private <- read_excel('data/WEF_NRI_2012-2016_Historical_Dataset.xlsx',sheet=2,skip=3) %>%
  rename(code=`Code NRI 2016`, series=`Series unindented`) %>%
  filter(series %in% rename_wef_private$series, 
         Attribute=='Value', 
         Edition==2016) %>%
  left_join(rename_wef_private,by='series') %>%
  select(variable,Albania:Zimbabwe) %>%
  melt(id.var='variable',variable.name='country') %>%
  dcast(country ~ variable) %>%
  mutate_at(2:8,as.numeric) 


wef_private_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                        sort_order='cor',num_pcs=5,overall_score='PC1') {
  wef_private %>%
    j2sr_style_plot(rename_wef_private,
                    country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('WEF private sector: ',country_name))
}

wef_private_plot('Colombia')

###############################################################################
# Tech adoption in public sector
###############################################################################
rename_wef_public <- tibble(
  series = c("Importance of ICTs to gov’t vision, 1-7 (best)", "Government Online Service Index, 0–1 (best)",
             "Gov’t success in ICT promotion, 1-7 (best)","ICT use & gov’t efficiency, 1-7 (best)",
             "E-Participation Index, 0–1 (best)","Gov’t procurement of advanced tech, 1-7 (best)",
             "Availability of latest technologies, 1-7 (best)"),
  variable = c('ict_vision','online_services','ict_promotion','ict_gov_efficiency','e_participaton','gov_procure',
               'tech_avail'),
  label = c("Importance of ICTs to gov’t vision", "Government Online Service Index",'Latest tech available',
               "Gov’t success in ICT promotion","ICT improves gov't services",
               "Value of gov't websites","Gov’t procurement of advanced tech"),
  flip=FALSE
)

wef_public <- read_excel('data/WEF_NRI_2012-2016_Historical_Dataset.xlsx',sheet=2,skip=3) %>%
  rename(code=`Code NRI 2016`, series=`Series unindented`) %>%
  filter(series %in% rename_wef_public$series, 
         Attribute=='Value', 
         Edition==2016) %>%
  left_join(rename_wef_public,by='series') %>%
  select(variable,Albania:Zimbabwe) %>%
  melt(id.var='variable',variable.name='country') %>%
  dcast(country ~ variable) %>%
  mutate_at(2:8,as.numeric) 

wef_public_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                            sort_order='cor',num_pcs=5,overall_score='PC1') {
  wef_public %>%
    j2sr_style_plot(rename_wef_public,
                    country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('WEF public sector: ',country_name))
}

wef_public_plot('Colombia')

###############################################################################
# Digital literacy
###############################################################################
rename_wef_literacy <- tibble(
  series = c("Tertiary education gross enrollment rate, %",
             "Quality of management schools, 1-7 (best)",
             "Quality of educational system, 1-7 (best)",
             "Quality of math & science education, 1-7 (best)",
             "Secondary education gross enrollment rate, %",
             "Adult literacy rate, %",
             "Knowledge-intensive jobs, % workforce",
             "Internet access in schools, 1-7 (best)"),
  label = c('Tertiary enrollment (gross)','Mgmt. school quality','Ed system quality',
               'Math & science ed quality','Secondary enrollment (gross)','Adult literacy',
               'Knowledge-intensive jobs','Internet in schools'),
  variable = c('tert_enroll','mgmt_school','ed_quality','stem_quality','sec_enroll',
            'adult_lit','knowledge_jobs','internet_schools'),
  flip=FALSE
)

wef_literacy <- read_excel('data/WEF_NRI_2012-2016_Historical_Dataset.xlsx',sheet=2,skip=3) %>%
  rename(code=`Code NRI 2016`, series=`Series unindented`) %>%
  filter(series %in% rename_wef_literacy$series, 
         Attribute=='Value', 
         Edition==2016) %>%
  left_join(rename_wef_literacy,by='series') %>%
  select(variable,Albania:Zimbabwe) %>%
  melt(id.var='variable',variable.name='country') %>%
  dcast(country ~ variable) %>%
  mutate_at(2:9,as.numeric) 

wef_literacy_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                              sort_order='cor',num_pcs=5,overall_score='PC1') {
  wef_literacy %>%
    j2sr_style_plot(rename_wef_literacy,
                    country_name,show_pred,
                    shade_fraction,sort_order,num_pcs,overall_score) +
    ggtitle(paste0('WEF digital literacy: ',country_name))
}

wef_literacy_plot('Colombia')

