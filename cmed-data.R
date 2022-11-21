### Purpose: Prepare PEW data for analysis of covid media consumption and distress
### Author:  S Bauldry
### Date:    November 20, 2022

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
  
  ### keep individual indicators to estimate reliability
  rename(dis1 = MH_TRACK_a_W64, dis2 = MH_TRACK_b_W64, dis3 = MH_TRACK_c_W64, 
         dis5 = MH_TRACK_e_W64) %>%
  
  mutate(
    
    ### psychological distress
    dis4 = 5 - MH_TRACK_d_W64,
    dis  = dis1 + dis2 + dis3 + dis4 + dis5,
    
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
    
    ### creating numeric indicators for some commands
    nws1 = ifelse(nws == 1, 1, 0),
    fem1 = ifelse(fem == 1, 1, 0),
    met1 = ifelse(met == 1, 1, 0),
    mar1 = ifelse(mar == 1, 1, 0),
    rce1 = ifelse(rce == 1, 1, 0),
    rce2 = ifelse(rce == 2, 1, 0),
    rce3 = ifelse(rce == 3, 1, 0), 
    edu1 = ifelse(edu == 1, 1, 0),
    edu2 = ifelse(edu == 2, 1, 0),
    edu3 = ifelse(edu == 3, 1, 0),
    reg1 = ifelse(reg == 1, 1, 0),
    reg2 = ifelse(reg == 2, 1, 0),
    reg3 = ifelse(reg == 3, 1, 0),
    reg4 = ifelse(reg == 4, 1, 0),
    mhc1 = ifelse(mhc == 1, 1, 0),
    
    ### weights
    wgt = WEIGHT_W64) %>%
  
    
  
  select(c(dis, nws, rce, fem, age, met, reg, mar, edu, mhc, nws1, fem1, met1, mar1, 
           rce1, rce2, rce3, edu1, edu2, edu3, reg1, reg2, reg3, reg4, mhc1, 
           wgt, dis1, dis2, dis3, dis4, dis5))

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
write_csv(pew_old, "cmed-old-data.csv")
