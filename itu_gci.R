library(pdftools)
library(tidyverse)

gci_text <- pdf_text('data/draft-18-00706_Global-Cybersecurity-Index-EV5_print_2.pdf')
# pull out tables on pages 64-70
itu_gci <- gci_text[64:70] %>%
  read_lines() %>%
  str_squish() %>%
  enframe(name=NULL) %>%
  filter(!grepl('Global Cybersecurity Index 2018',value),
         !grepl('Annex B',value),
         !grepl('The countries marked',value),
         !grepl('submitted their answers',value),
         !grepl('Member State',value),
         !value %in% c('',62:68)) %>%
  mutate(country=sub(' 0\\..*','',value),
         country=sub('\\*','',country),
         itu_gci=as.numeric(str_extract(value,'0\\..* '))) %>%
  select(country,itu_gci)



