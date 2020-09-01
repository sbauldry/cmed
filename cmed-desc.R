### Purpose: prepare descriptives for analysis of covid media exposure and psychological distress
### Author: S Bauldry
### Date: August 12, 2020

### set working directory and load packages
rm(list = ls())
setwd("~/desktop")
library(tidyverse)
library(ggpubr)
library(Hmisc)
library(weights)


### read prepared data
pew <- read_csv("cmed-data.csv", col_types = list("sex" = col_factor(), "rac" = col_factor(), 
                                                  "met" = col_factor(), "reg" = col_factor(),
                                                  "edu" = col_factor(), "mar" = col_factor(),
                                                  "psy" = col_factor(), "nws" = col_factor()))
summary(pew)


### weighted descriptive statistics
pew_vc <- pew %>%
  filter(nws == "VC")
pew_nvc <- pew %>%
  filter(nws == "nVC")

# overall sample size and distribution of media exposure
length(pew$dis)
table(pew$nws)
wpct(pew$nws, weight = pew$wgt)

# psychological distress
mn_dis <- wtd.mean(pew$dis, weights = pew$wgt)
sd_dis <- sqrt( wtd.var(pew$dis, weights = pew$wgt) )

mn_dis_vc <- wtd.mean(pew_vc$dis, weights = pew_vc$wgt)
sd_dis_vc <- sqrt( wtd.var(pew_vc$dis, weights = pew_vc$wgt) )

mn_dis_nvc <- wtd.mean(pew_nvc$dis, weights = pew_nvc$wgt)
sd_dis_nvc <- sqrt( wtd.var(pew_nvc$dis, weights = pew_nvc$wgt) )

c(mn_dis, sd_dis, mn_dis_vc, sd_dis_vc, mn_dis_nvc, sd_dis_nvc)

# covariates
pr_var <- function(x, y, z) {
  a <- wpct(x, weight = pew$wgt)
  b <- wpct(y, weight = pew_vc$wgt)
  c <- wpct(z, weight = pew_nvc$wgt)
  c(a, b, c)
}

pr_var(pew$sex, pew_vc$sex, pew_nvc$sex)
pr_var(pew$rac, pew_vc$rac, pew_nvc$rac)
pr_var(pew$edu, pew_vc$edu, pew_nvc$edu)

pr_var(pew$mar, pew_vc$mar, pew_nvc$mar)
pr_var(pew$met, pew_vc$met, pew_nvc$met)
pr_var(pew$reg, pew_vc$reg, pew_nvc$reg)
pr_var(pew$psy, pew_vc$psy, pew_nvc$psy)



### additional preliminary descriptives -- not reported in paper ###


### univariate descriptives
uni_plot <- function(x, l) {
  ggplot(pew, aes_string(x = x)) +
    geom_bar(aes(y = ..prop.., group = 1)) + 
    ylim(0, 1) +
    labs(y = "proportion respondents", x = l) +
    theme_light()
}

u1 <- uni_plot(x = "sex", l = "sex")
u2 <- uni_plot(x = "rac", l = "race")
u3 <- uni_plot(x = "mar", l = "married/partner")
u4 <- uni_plot(x = "nws", l = "media exposure")
u5 <- uni_plot(x = "edu", l = "education")
u6 <- uni_plot(x = "met", l = "metro area")
u7 <- uni_plot(x = "reg", l = "region")

u8 <- ggplot(pew, aes(x = dis)) + 
  geom_density(alpha = 0.3) +
  labs(x = "psychological distress") +
  theme_light()

ggarrange(u1, u2, u3, u4, u5, u6, u7, u8)


### distribution of psychological distress by selected factors
bi1_plot <- function(f, t) {
  ggplot(data = pew, aes_string(x = "dis", color = f, fill = f)) + 
    geom_density(alpha = 0.3) +
    labs(x = "psychological distress") +
    ggtitle(t) +
    theme_light()
}

b1 <- bi1_plot(f = "nws", t = "media exposure")
b2 <- bi1_plot(f = "sex", t = "sex")
b3 <- bi1_plot(f = "rac", t = "race")
b4 <- bi1_plot(f = "mar", t = "married/partner")
b5 <- bi1_plot(f = "edu", t = "education")
b6 <- bi1_plot(f = "met", t = "metro area")
b7 <- bi1_plot(f = "reg", t = "region")

ggarrange(b1, b2, b3, b4, b5, b6, b7)


### distribution of media exposure by selected factors
bi2_plot <- function(f, l) {
  pew %>%
    group_by_(f, "nws") %>%
    summarize(N = n()) %>%
    mutate(prp = N/sum(N)) %>%
    ggplot(aes_string(x = f, y = "prp", fill = "nws")) +
    geom_col(position = "dodge2") +
    labs(y = "proportion respondents", x = l, fill = "media") +
    ylim(0, 1) +
    theme_light()
}

b8  <- bi2_plot(f = "sex", l = "sex")
b9  <- bi2_plot(f = "rac", l = "race")
b10 <- bi2_plot(f = "mar", l = "married/partner")
b11 <- bi2_plot(f = "edu", l = "education")
b12 <- bi2_plot(f = "met", l = "metro area")
b13 <- bi2_plot(f = "reg", l = "region")

ggarrange(b8, b9, b10, b11, b12, b13)
