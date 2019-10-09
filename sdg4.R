library(tidyverse)
library(reshape2)
# source('utils.R')

rename_df <- tibble(
  Indicator = c("Proportion of youth and adults who have copied or moved a file or folder (%)",                                                       
                "Proportion of youth and adults who have created electronic presentations with presentation software (%)",                              
                "Proportion of youth and adults who have found, downloaded, installed and configured software, both sexes (%)",                         
                "Proportion of youth and adults who have transferred files between a computer and other devices, both sexes (%)",                       
                "Proportion of youth and adults who have wrote a computer program using a specialised programming language, both sexes (%)",            
                "Proportion of youth and adults who have sent e-mails with attached files (e.g. document, picture, video), both sexes (%)",             
                "Proportion of youth and adults who have used basic arithmetic formulae in a spreadsheet, both sexes (%)",                              
                "Proportion of youth and adults who have used copy and paste tools to duplicate or move information within a document , both sexes (%)",
                "Proportion of youth and adults who have connected and installed new devices (%)",                                                      
                "Proportion of youth and adults who have connected and installed new devices, female (%)",                                              
                "Proportion of youth and adults who have connected and installed new devices, male (%)",                                                
                "Proportion of youth and adults who have copied or moved a file or folder, female (%)",                                                 
                "Proportion of youth and adults who have copied or moved a file or folder, male (%)",                                                   
                "Proportion of youth and adults who have created electronic presentations with presentation software, female (%)",                      
                "Proportion of youth and adults who have created electronic presentations with presentation software, male (%)",                        
                "Proportion of youth and adults who have found, downloaded, installed and configured software, female (%)",                             
                "Proportion of youth and adults who have found, downloaded, installed and configured software, male (%)",                               
                "Proportion of youth and adults who have transferred files between a computer and other devices, female (%)",                           
                "Proportion of youth and adults who have transferred files between a computer and other devices, male (%)",                             
                "Proportion of youth and adults who have used basic arithmetic formulae in a spreadsheet, female (%)",                                  
                "Proportion of youth and adults who have used basic arithmetic formulae in a spreadsheet, male (%)",                                    
                "Proportion of youth and adults who have used copy and paste tools to duplicate or move information within a document, female (%)",     
                "Proportion of youth and adults who have used copy and paste tools to duplicate or move information within a document, male (%)",       
                "Proportion of youth and adults who have wrote a computer program using a specialised programming language, female (%)",                
                "Proportion of youth and adults who have wrote a computer program using a specialised programming language, male (%)",                  
                "Proportion of youth and adults who have sent e-mails with attached files (e.g. document, picture, video), female (%)",                 
                "Proportion of youth and adults who have sent e-mails with attached files (e.g. document, picture, video), male (%)"),
  short = c('move_file','create_pres','install_soft','move_device','write_program',
            'sent_email','use_spreadsheet','copy_paste','new_device','new_device_f',
            'new_device_m','move_file_f','move_file_m','create_pres_f','create_pres_m',
            'install_soft_f','install_soft_m','move_device_f','move_device_m',
            'use_spreadsheet_f','use_spreadsheet_m','copy_paste_f','copy_paste_m',
            'write_program_f','write_program_m','sent_email_f','sent_email_m')
)

sdg4 <- read_csv('data/SDG4_DS_04102019154356211.csv') %>%
  rename(country=Country) %>%
  group_by(country,Indicator) %>%
  arrange(Time) %>%
  summarize(Value=last(Value)) %>%
  left_join(rename_df,by='Indicator') %>%
  select(country,short,Value) %>%
  dcast(country ~ short) %>%
  mutate(copy_paste_gender_gap=(copy_paste_m-copy_paste_f)/copy_paste_m,
         create_pres_gender_gap=(create_pres_m-create_pres_f)/create_pres_m,
         install_soft_gender_gap=(install_soft_m-install_soft_f)/install_soft_m,
         move_device_gender_gap=(move_device_m-move_device_f)/move_device_m,
         move_file_gender_gap=(move_file_m-move_file_f)/move_file_m,
         new_device_gender_gap=(new_device_m-new_device_f)/new_device_m,
         sent_email_gender_gap=(sent_email_m-sent_email_f)/sent_email_m,
         use_spreadsheet_gender_gap=(use_spreadsheet_m-use_spreadsheet_f)/use_spreadsheet_m,
         write_program_gender_gap=(write_program_m-write_program_f)/write_program_m) %>%
  select(-ends_with('_m'),-ends_with('_f')) %>%
  fix_adm0 

#setdiff(sdg4$country,all_countries)

rm(rename_df)
