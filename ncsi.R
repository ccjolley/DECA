# National Cyber Security Index

library(pdftools)
library(tidyverse)
library(stringr)
# source('utils.R')

ncsi_pdf <- pdf_text('data/ncsi_digital_smaller.pdf')

ncsi_1 <- ncsi_pdf[9] %>%
  read_lines() %>%
  enframe(name=NULL,value='s') %>% 
  mutate(country=str_trim(substr(s,10,60)),
         ncsi=as.numeric(substr(s,61,90))) 

ncsi_2 <- ncsi_pdf[10] %>%
  read_lines() %>%
  enframe(name=NULL,value='s') %>% 
  mutate(country=str_trim(substr(s,6,26)),
         ncsi=as.numeric(substr(s,28,50))) 

ncsi_3 <- ncsi_pdf[11] %>%
  read_lines() %>%
  enframe(name=NULL,value='s') %>% 
  mutate(country=str_trim(substr(s,10,60)),
         ncsi=as.numeric(substr(s,61,90))) 

ncsi_4 <- ncsi_pdf[12] %>%
  read_lines() %>%
  enframe(name=NULL,value='s') %>% 
  mutate(country=str_trim(substr(s,5,24)),
         ncsi=as.numeric(substr(s,25,49)))

ncsi <- rbind(ncsi_1,ncsi_2,ncsi_3,ncsi_4) %>%
  select(country,ncsi) %>%
  filter(!is.na(ncsi)) %>%
  fix_adm0

#     0        1         2         3         4         5         6         7         8         9
#     123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
s <- "40.  Moldova (Republic of) 48.05                   60.82                    -12.77"
s <- "76.  Côte d'Ivoire       23.38                   39.99                    -16.61    d'Ivoire       23.38                   39.99  -16.6"
s <- "  51.                 Peru                                    38.96                                   51.39                      -12.43"
s <- "26.  Sweden                61.04                   83.48                    -22.44"
s <- "  1.                  France                                  83.12                                79.06                           4.06"
s <- "  25.                 Luxembourg                              62.34                                83.06                         -20.72"