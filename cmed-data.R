### Purpose: Prepare data for analysis of covid media consumption and distress
### Author:  S Bauldry
### Date:    August 14, 2022

setwd("~/desktop")
library(tidyverse)
library(haven)


### Extract analysis variables from ATP Wave 64
vars <- c("F_AGECAT", "F_SEX", "F_RACETHN", "F_MARITAL", "F_METRO", 
          "F_CREGION", "F_EDUCCAT", "COVIDFOL_W64", "COVID_MENTAL_W64",
          "MH_TRACK_a_W64", "MH_TRACK_b_W64", "MH_TRACK_c_W64", 
          "MH_TRACK_d_W64", "MH_TRACK_e_W64", "WEIGHT_W64") 

pew1 <- read_sav("ATP W64.sav") %>%
  dplyr::select( all_of(vars) ) %>%
  mutate_if(is.numeric, ~ na_if(., 99))


### Prepare variables for analysis
pew2 <- pew1 %>%
  mutate(
    
    ### psychological distress
    dis = MH_TRACK_a_W64 + MH_TRACK_b_W64 + MH_TRACK_c_W64 +
          (5 - MH_TRACK_d_W64) + MH_TRACK_e_W64,
    
    ### media consumption
    nws = as.factor( ifelse(COVIDFOL_W64 == 1, 1, 0) ), # 1 = very close
    
    ### past mental health condition
    mhc = as.factor( ifelse(COVID_MENTAL_W64 == 1, 1, 0) ), # 1 = yes
    
    ### sociodemographic covariates
    rce = as.factor( case_when(
      F_RACETHN == 3 ~ 1,                           # Hispanic
      F_RACETHN == 2 ~ 2,                           # non-Hispanic Black
      F_RACETHN == 1 ~ 3,                           # non-Hispanic White
      F_RACETHN == 4 ~ 4) ),                        # other race/ethnicity
    fem = as.factor( ifelse(F_SEX == 2, 1, 0) ),    # 1 = female
    age = F_AGECAT,                                 # 1 = 18-29, 2 = 30-49, 3 = 50-64, 4 = 65+
    met = as.factor( ifelse(F_METRO == 1, 1, 0) ),  # 1 = urban
    reg = as.factor( F_CREGION ),                   # 1 = northeast, 2 = midwest, 3 = south, 4 = west
    mar = as.factor( ifelse(F_MARITAL < 3, 1, 0) ), # 1 = married or cohabiting
    edu = as.factor( 4 - F_EDUCCAT ),               # 1 = HS or less, 2 = some college, 3 = BA+
    
    ### weights
    wgt = WEIGHT_W64) %>%
  
  select(c(dis, nws, rce, fem, age, met, reg, mar, edu, mhc, wgt))
    
### select analysis sample for preliminary analysis of full sample
### rescale weights
pew_full <- pew2 %>%
  drop_na() %>%
  mutate(rwt = wgt/sum(wgt)*n()) %>%
  select(-wgt)
dim(pew_full)

### select analysis sample for primary analysis of older adults
pew_old <- pew2 %>%
  filter(age == 4 & rce != 4) %>%
  select(-age)
dim(pew_old)

# drop missing psychological distress
pew_old <- pew_old %>% drop_na(dis)
dim(pew_old)

# drop missing media consumption
pew_old <- pew_old %>% drop_na(nws)
dim(pew_old)

# drop missing covariates (marital status, education, mental health condition)
pew_old <- pew_old %>% drop_na(c(mar, edu, mhc))
dim(pew_old)

### rescale weights
pew_old <- pew_old %>%
  mutate(rwt = wgt/sum(wgt)*n()) %>%
  select(-wgt)

### save data for analysis
write_csv(pew_full, "cmed-full-data.csv")
write_csv(pew_old, "cmed-old-data.csv")
