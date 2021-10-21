### Purpose: prepare data for analysis of covid media exposure and psychological distress
### Author: S Bauldry
### Date: October 19, 2020

### set working directory and load packages
setwd("~/desktop")
library(tidyverse)
library(haven)

### read Pew data provided by Kevin
pew1 <- read_stata("PEWCOVID.dta")

### prepare variables for analysis
vars <- c("f_agecat", "f_sex", "f_racethn", "f_marital", "covidfol_w64", 
          "mh_track_a_w64", "mh_track_b_w64", "mh_track_c_w64", 
          "mh_track_d_w64", "mh_track_e_w64", "weight_w64", "f_metro", 
          "f_cregion", "f_educcat")

pew2 <- pew1 %>%
  select(all_of(vars)) %>%
  mutate_if(is.numeric, list(~na_if(., 99))) %>%
  mutate(
    
    ### psychological distress
    dis = mh_track_a_w64 + mh_track_b_w64 + mh_track_c_w64 +
          (5 - mh_track_d_w64) + mh_track_e_w64,
    
    ### media consumption
    nws = ifelse(covidfol_w64 == 1, 1, 0), # 1 = very close
    
    ### sociodemographic covariates
    rce = case_when(
      f_racethn == 2 ~ 1,              # non-Hispanic Black
      f_racethn == 3 ~ 2,              # Hispanic
      f_racethn == 1 ~ 3,              # non-Hispanic White
      f_racethn == 4 ~ 4),             # other race/ethnicity
    fem = ifelse(f_sex == 2, 1, 0),    # 1 = female
    age = f_agecat,                    # 1 = 18-29, 2 = 30-49, 3 = 50-64, 4 = 65+
    met = ifelse(f_metro == 1, 1, 0),  # 1 = urban
    reg = f_cregion,                   # 1 = northeast, 2 = midwest, 3 = south, 4 = west
    mar = ifelse(f_marital < 3, 1, 0), # 1 = married or cohabiting
    edu = 4 - f_educcat) %>%           # 1 = HS or less, 2 = some college, 3 = BA+
  
  select(c(dis, nws, rce, fem, age, met, reg, mar, edu, weight_w64))
    
### select analysis sample for preliminary analysis of media consumption
pew_full <- pew2 %>%
  drop_na()

### select analysis sample for primary analysis
pew_old <- pew2 %>%
  filter(age == 4 & rce != 4) %>%
  select(-age)

# baseline sample
length(pew_old$weight_w64)

# drop missing psychological distress
pew_old <- pew_old %>% drop_na(dis)
length(pew_old$weight_w64)

# drop missing media consumption
pew_old <- pew_old %>% drop_na(nws)
length(pew_old$weight_w64)

# drop missing sociodemographic covariates (marital status and education)
pew_old <- pew_old %>% drop_na(c(mar, edu))
length(pew_old$weight_w64)

### save data for analysis
write_csv(pew_full, "cmed-full-data.csv")
write_csv(pew_old, "cmed-old-data.csv")
