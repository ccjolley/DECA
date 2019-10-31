source('j2sr_plot.R')

###############################################################################
# Tech adoption in private sector
###############################################################################
rename_wef_private <- tibble(
  series = c("Firm-level technology absorption, 1-7 (best)","Capacity for innovation, 1-7 (best)",
             "PCT patents, applications/million pop.","ICT use for business-to-business transactions, 1-7 (best)",
             "Business-to-consumer Internet use, 1-7 (best)","Extent of staff training, 1-7 (best)"),
  label = c("Firm-level tech absorption","Innovation capacity",
            "PCT patent applications","ICT use for B2B",
            "B2C Internet use","Extent of staff training"),
  variable = c('tech_abs','inno_capacity','pct_patents','b2b_use','b2c_use','staff_training'),
  flip=FALSE
)

wef_private <- wef_nri <- read_excel('data/WEF_NRI_2012-2016_Historical_Dataset.xlsx',sheet=2,skip=3) %>%
  rename(code=`Code NRI 2016`, series=`Series unindented`) %>%
  filter(series %in% rename_wef_private$series, 
         Attribute=='Value', 
         Edition==2016) %>%
  left_join(rename_wef_private,by='series') %>%
  select(variable,Albania:Zimbabwe) %>%
  melt(id.var='variable',variable.name='country') %>%
  dcast(country ~ variable) %>%
  mutate_at(2:7,as.numeric) 


wef_private_plot <- function(country_name,show_pred=FALSE,shade_fraction=0.5,
                        sort_order='cor',num_pcs=5) {
  wef_private %>%
    j2sr_style_plot(rename_wef_private,
                    country_name,show_pred,
                    shade_fraction,sort_order,num_pcs) +
    ggtitle(paste0('WEF private sector: ',country_name))
}

wef_private_plot('Colombia')
