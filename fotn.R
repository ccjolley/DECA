library(tidyverse)
library(readxl)

fotn <- read_excel('data/FINAL_DATA_FOTN2018_TOTAL_and_CATEGORY_SCORES.xlsx',sheet=1) %>%
  rename(country=1,access_obstacles=3,content_limits=4,user_violations=5,fotn_total=6) %>%
  select(1,3,4,5,6) %>%
  fix_adm0
