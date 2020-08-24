### Purpose: prepare analysis of covid media exposure and psychological distress
### Author: S Bauldry
### Date: August 20, 2020

### set working directory and load packages
rm(list = ls())
setwd("~/desktop")
library(tidyverse)
library(broom)
library(ggpubr)


### read prepared data
pew <- read_csv("cmed-data.csv", col_types = list("sex" = col_factor(), "rac" = col_factor(), 
                                                  "met" = col_factor(), "reg" = col_factor(),
                                                  "edu" = col_factor(), "mar" = col_factor(),
                                                  "nws" = col_factor()))
summary(pew)


### fit unweighted models and extract estimates
m1  <- tidy( lm(dis ~ nws + sex + rac + mar + edu + met + reg, data = pew) )
m2  <- tidy( lm(dis ~ nws + rac + mar + edu + met + reg, data = subset(pew, sex == "M")) )
m3  <- tidy( lm(dis ~ nws + rac + mar + edu + met + reg, data = subset(pew, sex == "F")) )
m4  <- tidy( lm(dis ~ nws + sex + mar + edu + met + reg, data = subset(pew, rac == "W")) )
m5  <- tidy( lm(dis ~ nws + sex + mar + edu + met + reg, data = subset(pew, rac == "B")) )
m6  <- tidy( lm(dis ~ nws + sex + mar + edu + met + reg, data = subset(pew, rac == "L")) )
m7  <- tidy( lm(dis ~ nws + sex + rac + edu + met + reg, data = subset(pew, mar == "MP")) )
m8  <- tidy( lm(dis ~ nws + sex + rac + edu + met + reg, data = subset(pew, mar == "nMP")) )
m9  <- tidy( lm(dis ~ nws + sex + rac + mar + met + reg, data = subset(pew, edu == "BA+")) )
m10 <- tidy( lm(dis ~ nws + sex + rac + mar + met + reg, data = subset(pew, edu == "SC")) )
m11 <- tidy( lm(dis ~ nws + sex + rac + mar + met + reg, data = subset(pew, edu == "HS-")) )


### combine estimates and standard errors for nws
est <- rbind( c(1, m1$estimate[2], m1$std.error[2]), c(2, m2$estimate[2], m2$std.error[2]), 
              c(3, m3$estimate[2], m3$std.error[2]), c(4, m4$estimate[2], m4$std.error[2]),
              c(5, m5$estimate[2], m5$std.error[2]), c(6, m6$estimate[2], m6$std.error[2]),
              c(7, m7$estimate[2], m7$std.error[2]), c(8, m8$estimate[2], m8$std.error[2]),
              c(9, m9$estimate[2], m9$std.error[2]), c(10, m10$estimate[2], m10$std.error[2]),
              c(11, m11$estimate[2], m11$std.error[2]) )
colnames(est) <- c("id", "est", "se")
est <- as_tibble(est) %>%
  mutate(lb = est - 1.96*se,
         ub = est + 1.96*se)


### graph estimates
g1 <- ggplot(est, aes(x = factor(id), y = est, ymin = lb, ymax = ub)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Distress on media exposure", subtitle = "unweighted estimates", 
       x = "", y = "estimate", caption = "Unstandardized estimates with 95% CIs.") +
  scale_x_discrete(labels = c("all", "men", "women", "white", "black", "latinx", 
                              "married/partner", "not married/partner", "college degree or more", 
                              "some college", "high school or less")) +
  coord_flip() +
  theme_light()


### fit weighted models and extract estimates
wm1  <- tidy( lm(dis ~ nws + sex + rac + mar + edu + met + reg, data = pew, weights = wgt) )
wm2  <- tidy( lm(dis ~ nws + rac + mar + edu + met + reg, data = subset(pew, sex == "M"), weights = wgt) )
wm3  <- tidy( lm(dis ~ nws + rac + mar + edu + met + reg, data = subset(pew, sex == "F"), weights = wgt) )
wm4  <- tidy( lm(dis ~ nws + sex + mar + edu + met + reg, data = subset(pew, rac == "W"), weights = wgt) )
wm5  <- tidy( lm(dis ~ nws + sex + mar + edu + met + reg, data = subset(pew, rac == "B"), weights = wgt) )
wm6  <- tidy( lm(dis ~ nws + sex + mar + edu + met + reg, data = subset(pew, rac == "L"), weights = wgt) )
wm7  <- tidy( lm(dis ~ nws + sex + rac + edu + met + reg, data = subset(pew, mar == "MP"), weights = wgt) )
wm8  <- tidy( lm(dis ~ nws + sex + rac + edu + met + reg, data = subset(pew, mar == "nMP"), weights = wgt) )
wm9  <- tidy( lm(dis ~ nws + sex + rac + mar + met + reg, data = subset(pew, edu == "BA+"), weights = wgt) )
wm10 <- tidy( lm(dis ~ nws + sex + rac + mar + met + reg, data = subset(pew, edu == "SC"), weights = wgt) )
wm11 <- tidy( lm(dis ~ nws + sex + rac + mar + met + reg, data = subset(pew, edu == "HS-"), weights = wgt) )


### combine estimates and standard errors for nws
west <- rbind( c(1, wm1$estimate[2], wm1$std.error[2]), c(2, wm2$estimate[2], wm2$std.error[2]), 
              c(3, wm3$estimate[2], wm3$std.error[2]), c(4, wm4$estimate[2], wm4$std.error[2]),
              c(5, wm5$estimate[2], wm5$std.error[2]), c(6, wm6$estimate[2], wm6$std.error[2]),
              c(7, wm7$estimate[2], wm7$std.error[2]), c(8, wm8$estimate[2], wm8$std.error[2]),
              c(9, wm9$estimate[2], wm9$std.error[2]), c(10, wm10$estimate[2], wm10$std.error[2]),
              c(11, wm11$estimate[2], wm11$std.error[2]) )
colnames(west) <- c("id", "est", "se")
west <- as_tibble(west) %>%
  mutate(lb = est - 1.96*se,
         ub = est + 1.96*se)


### graph estimates
g2 <- ggplot(west, aes(x = factor(id), y = est, ymin = lb, ymax = ub)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Distress on media exposure", subtitle = "weighted estimates", 
       x = "", y = "estimate", caption = "Unstandardized estimates with 95% CIs.") +
  scale_x_discrete(labels = c("all", "men", "women", "white", "black", "latinx", 
                              "married/partner", "not married/partner", "college degree or more", 
                              "some college", "high school or less")) +
  coord_flip() +
  theme_light()

fig1 <- ggarrange(g1, g2)
ggsave("fig1.pdf", plot = fig1)



### test for potential race/ethnicity and education differences
dt <- function(i,j) {
  diff <- abs(west[i,2] - west[j,2])
  se   <- sqrt(west[i,3]^2 + west[j,3]^2)
  out  <- cbind(diff, se, diff/se)
  colnames(out) <- c("diff", "se", "t")
  print(out)
}

dt(4,5)   # white - black
dt(4,6)   # white - latinx
dt(5,6)   # black - latinx
dt(9,10)  # ba+ - sc
dt(9,11)  # ba+ - hs-
dt(10,11) # sc - hs-


### Check weights for race/ethnicity and education
chk_wgt <- function(x) {
  pew %>%
    group_by_at(x) %>%
    summarize(mean_wgt = mean(wgt))
}

chk_wgt("rac") # down-weighting latinx and up-weighting black on average
chk_wgt("edu") # down-weighting ba+ and up-weighting hs- on average

# histogram shows outlier weight, reran models excluding and same pattern of results
ggplot(data = subset(pew, rac == "L"), mapping = aes(x = wgt)) +
  geom_histogram() +
  theme_light()

# histogram shows no obvious outliers
ggplot(data = subset(pew, edu == "HS-"), mapping = aes(x = wgt)) +
  geom_histogram() +
  theme_light()

