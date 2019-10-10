# Combine all data series, impute missing data, perform PCA

scripts <- c('utils.R','a4ai.R','eiu.R','fotn.R','gsma.R','imf.R','itu.R',
             'rsf.R','sdg4.R','vdem.R','wb.R','wef.R')
for (s in scripts) { source(s) }

all <- full_join(a4ai,eiu,by='country') %>%
  full_join(fotn,by='country') %>%
  full_join(gsma,by='country') %>%
  full_join(imf,by='country') %>%
  full_join(itu,by='country') %>%
  full_join(rsf,by='country') %>%
  full_join(sdg4,by='country') %>%
  full_join(vdem,by='country') %>%
  full_join(wb,by='country') %>%
  full_join(wef,by='country') 

# setdiff(all$country,all_countries)
# duplicate names: overall, infrastructure, ind_internet

################################################################################
# Function that removes columns that are NA for more than a given fraction of 
# USAID countries
################################################################################
trim_columns <- function(df,frac,keep_list=NULL {
  # if no keep_list provided, include all countries
  if (is.null(keep_list)) {
    keep_list <- df$country
  }
  na_frac <- df %>% 
    filter(country %in% keep_list) %>%
    is.na %>%
    colMeans
  keep_names <- na_frac[na_frac <= (1-frac)] %>% names
  df %>% select(keep_names)
}

# TODO: function that removes non-USAID countries for which more than a given fraction
# of rows are missing

# TODO: use these two functions to pare down data frame

# TODO: use mice to impute (m=1 for now)

# TODO: run prcomp

# TODO: visualize first two dimensions; mark out Kenya as well as extreme cases

# TODO: regressions to help interpret first PCs

# TODO: save PC1-2 to a text file