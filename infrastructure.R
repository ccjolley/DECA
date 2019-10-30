source('gsma.R')

infra <- gsma %>%
  select(country,mci_infra,mci_infra_coverage,cov_2G,cov_3G,cov_4G,
         mci_infra_performance,download,upload,latency,mci_infra_enabling,elect,
         bandwidth,servers,tlds,ixps,mci_infra_spectrum,spectrum_dd,
         spectrum_low,spectrum_high)

is.na(infra) %>% colMeans %>% sort # all the same
infra %>% filter(is.na(mci_infra)) # can't really impute on these

infra <- na.omit(infra)

pc <- infra %>%
  select(-country) %>%
  prcomp(scale=TRUE,center=TRUE)

pc$x %>%
  as_tibble() %>%
  cbind(select(infra,country)) %>%
  pc_scatter(c('Kenya','Colombia','Nepal'))

sapply(setdiff(names(infra),'country'),
       function(x) cor(infra[,x],pc$x[,1],use='complete.obs')) %>%
  sort(decr=TRUE)
# dominated by MCI infrastructure index

sapply(setdiff(names(infra),'country'),
       function(x) cor(infra[,x],pc$x[,2],use='complete.obs')) %>%
  sort(decr=TRUE)
# strongest correlation is with the spectrum allocation component, but that
# actually correlates more strongly with PC1 -- PC2 isn't really giving me anything.

highlight_1 <- c('Kenya','Colombia','Nepal')
highlight_2 <- c('Kenya','Uganda','United Republic of Tanzania','Ethiopia','Somalia','Rwanda','Burundi')
highlight_3 <- c('Colombia','Brazil','Venezuela','Ecuador','Peru','Panama')

# TODO: not super-happy with this -- need to find something useful to plot against MCI index
infra %>%
  select(country,mci_infra,mci_infra_spectrum) %>%
  highlight_scatter(highlight_countries = highlight_1) +
  xlab('Mobile Connectivity Index') + ylab('Subindex for spectrum allocation') 

# maybe A4AI index?
