### Purpose: prepare data for analysis of covid media exposure and psychological distress
### Author: S Bauldry
### Date: August 12, 2020

### set working directory and load packages
rm(list = ls())
setwd("~/desktop")
library(tidyverse)
library(haven)


### read Pew data provided by Kevin
pew1 <- read_stata("PEWCOVID.dta")


### extract analysis variables and select analysis sample
### analysis sample: age 65+, white/black/latinx
vars <- c("f_agecat", "f_sex", "f_racethn", "f_marital", "covidfol_w64", "mh_track_a_w64", 
          "mh_track_b_w64", "mh_track_c_w64", "mh_track_d_w64", "mh_track_e_w64", "weight_w64",
          "f_metro", "f_cregion", "f_educcat")
pew2 <- pew1 %>%
  select(all_of(vars)) %>%  
  filter(f_agecat == 4) %>% 
  filter(f_racethn < 4) 

### preparing variables for analysis
### Marital status: "married" and "living with partner" (MP)
###                "divorced", "separated", "widowed", "never been married" (nMP)
### Media exposure: How closely have you been following the news about the outbreak
###                 "very closely" (VC)
###                 "fairly closely", "not too closely", "not at all closely" (nVC)
pew3 <- pew2 %>%
  mutate_if(is.numeric, list(~na_if(., 99))) %>%
  mutate(sex = recode(as_factor(f_sex), "Male" = "M", "Female" = "F"),
         rac = recode(as_factor(f_racethn), "White non-Hispanic" = "W", "Black non-Hispanic" = "B", "Hispanic" = "L"),
         met = recode(as_factor(f_metro), "Metropolitan" = "M", "Non-metropolitan" = "nM"),
         reg = recode(as_factor(f_cregion), "Northeast" = "N", "Midwest" = "M", "South" = "S", "West" = "W"),
         edu = recode(as_factor(f_educcat), "College graduate+" = "BA+", "Some College" = "SC", "H.S. graduate or less" = "HS-"),
         mar = ifelse(f_marital < 3, 1, 0),
         mar = recode(as_factor(mar), "1" = "MP", "0" = "nMP"),
         nws_n = ifelse(covidfol_w64 == 1, 1, 0),
         nws = recode(as_factor(nws_n), "1" = "VC", "0" = "nVC"),
         dis = mh_track_a_w64 + mh_track_b_w64 + mh_track_c_w64 + (5 - mh_track_d_w64) + mh_track_e_w64,
         wgt = weight_w64) %>%
  select(c(sex, rac, met, reg, edu, mar, nws_n, nws, dis, wgt))
summary(pew3)

### check missing data
### listwise delete due to minimal missing data (< 6 for covariates and 37 for psychological distress)
summary(pew3)
pew4 <- pew3 %>%
  drop_na()


### save data for analysis
write_csv(pew4, "cmed-data.csv")
