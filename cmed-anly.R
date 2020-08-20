### Purpose: prepare analysis of covid media exposure and psychological distress
### Author: S Bauldry
### Date: August 20, 2020

### set working directory and load packages
rm(list = ls())
setwd("~/desktop")
library(tidyverse)
library(ggpubr)


### read prepared data
pew <- read_csv("cmed-data.csv", col_types = list("sex" = col_factor(), "rac" = col_factor(), 
                                                  "met" = col_factor(), "reg" = col_factor(),
                                                  "edu" = col_factor(), "mar" = col_factor(),
                                                  "nws" = col_factor()))
summary(pew)


### unweighted models
m1 <- lm(dis ~ nws + sex + rac + mar + edu + met + reg, data = pew)

m2_mal <- lm(dis ~ nws + rac + mar + edu + met + reg, data = subset(pew, sex == "M"))
m2_fem <- lm(dis ~ nws + rac + mar + edu + met + reg, data = subset(pew, sex == "F"))

m2_wht <- lm(dis ~ nws + sex + mar + edu + met + reg, data = subset(pew, rac == "W"))
m2_blk <- lm(dis ~ nws + sex + mar + edu + met + reg, data = subset(pew, rac == "B"))
m2_ltx <- lm(dis ~ nws + sex + mar + edu + met + reg, data = subset(pew, rac == "L"))

m2_mpt <- lm(dis ~ nws + sex + rac + edu + met + reg, data = subset(pew, mar == "MP"))
m2_nmp <- lm(dis ~ nws + sex + rac + edu + met + reg, data = subset(pew, mar == "nMP"))

m2_ba <- lm(dis ~ nws + sex + rac + mar + met + reg, data = subset(pew, edu == "BA+"))
m2_sc <- lm(dis ~ nws + sex + rac + mar + met + reg, data = subset(pew, edu == "SC"))
m2_hs <- lm(dis ~ nws + sex + rac + mar + met + reg, data = subset(pew, edu == "HS-"))

