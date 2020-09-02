### Purpose: prepare analysis of covid media exposure and psychological distress
### Author: S Bauldry
### Date: August 20, 2020

### set working directory and load packages
rm(list = ls())
setwd("~/desktop")
library(tidyverse)
library(broom)


### read prepared data
pew <- read_csv("cmed-data.csv", col_types = list("sex" = col_factor(), "rac" = col_factor(), 
                                                  "met" = col_factor(), "reg" = col_factor(),
                                                  "edu" = col_factor(), "mar" = col_factor(),
                                                  "nws" = col_factor()))
summary(pew)


### set referent categories
pew$nws <- relevel(pew$nws, ref="nVC")
pew$sex <- relevel(pew$sex, ref="M")
pew$reg <- relevel(pew$reg, ref="N")
pew$met <- relevel(pew$nws, ref="M")
pew$edu <- relevel(pew$edu, ref="HS-")


### model for overall sample
m1 <- tidy( lm(dis ~ nws + sex + rac + mar + edu + met + reg, data = pew, weights = wgt) )

# graph estimates for Figure 1
m1_prep <- m1 %>%
  filter(term %nin% "(Intercept)") %>%
  mutate(lb = estimate - 1.96*std.error,
         ub = estimate + 1.96*std.error,
         id = c(11:1))

fig1 <- ggplot(m1_prep, aes(x = factor(id), y = estimate, ymin = lb, ymax = ub)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  scale_x_discrete(labels = c("Region: South", "Region: West", "Region: Midwest", 
                              "Not metropolitan area", "Education: College degree +", "Education: Some college",
                              "Married/partner", "Race/Ethnicity: Black", "Race/Ethnicity: Latinx",
                              "Woman", "Media exposure: Very close")) +
  labs(x = "", y = "unstandardized estimate") +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 15))
fig1
ggsave("fig1.png", plot = fig1, width = 7.8, height = 4.32)

# list estimates
m1


### models stratifying by various subsamples
m2 <- tidy( lm(dis ~ nws + rac + mar + edu + met + reg, data = subset(pew, sex == "F"), weights = wgt) )
m3 <- tidy( lm(dis ~ nws + rac + mar + edu + met + reg, data = subset(pew, sex == "M"), weights = wgt) )
m4 <- tidy( lm(dis ~ nws + sex + mar + edu + met + reg, data = subset(pew, rac == "W"), weights = wgt) )
m5 <- tidy( lm(dis ~ nws + sex + mar + edu + met + reg, data = subset(pew, rac == "B"), weights = wgt) )
m6 <- tidy( lm(dis ~ nws + sex + mar + edu + met + reg, data = subset(pew, rac == "L"), weights = wgt) )
m7 <- tidy( lm(dis ~ nws + sex + rac + mar + met + reg, data = subset(pew, edu == "BA+"), weights = wgt) )
m8 <- tidy( lm(dis ~ nws + sex + rac + mar + met + reg, data = subset(pew, edu == "SC"), weights = wgt) )
m9 <- tidy( lm(dis ~ nws + sex + rac + mar + met + reg, data = subset(pew, edu == "HS-"), weights = wgt) )

# combine estimates and standard errors for nws
est <- rbind( c(8, m2$estimate[2], m2$std.error[2]), c(7, m3$estimate[2], m3$std.error[2]), 
              c(6, m4$estimate[2], m4$std.error[2]), c(5, m5$estimate[2], m5$std.error[2]),
              c(4, m6$estimate[2], m6$std.error[2]), c(3, m7$estimate[2], m7$std.error[2]),
              c(2, m8$estimate[2], m8$std.error[2]), c(1, m9$estimate[2], m9$std.error[2]) )
colnames(est) <- c("id", "est", "se")
est <- as_tibble(est) %>%
  mutate(lb = est - 1.96*se,
         ub = est + 1.96*se)

# graph estimates for Figure 2
fig2 <- ggplot(est, aes(x = factor(id), y = est, ymin = lb, ymax = ub)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 0.6, linetype = "dashed", color = "red") +
  geom_text(x = 0.7, y = 1.3, label = "average association") +
  labs(x = "", y = "unstandardized estimate") +
  scale_x_discrete(labels = c("Education: High school -", "Education: Some college", "Education: College degree +", 
                              "Race/Ethnicity: Latinx", "Race/Ethnicity: Black", "Race/Ethnicity: White", 
                              "Gender: Men", "Gender: Women")) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 15))
fig2
ggsave("fig2.png", plot = fig2, width = 7.8, height = 4.32)


### statistical tests for differences
dt <- function(i, j) {
  diff <- abs(est[i,2] - est[j,2])
  se   <- sqrt(est[i,3]^2 + est[j,3]^2)
  zs   <- diff/se
  pv   <- 2*(1 - pnorm( zs[[1]] ))
  out  <- cbind(diff, se, zs, pv)
  colnames(out) <- c("diff", "se", "zs", "pv")
  print(out)
}

dt(1, 2) # men - women
dt(3, 4) # white - black
dt(3, 5) # white - latinx
dt(4, 5) # black - latinx
dt(6, 7) # ba+ - sc
dt(6, 8) # ba+ - hs
dt(7, 8) # sc - hs


### Check weights for gender, race/ethnicity, and education

# slight down-weighting for men
pew %>%
  group_by(sex) %>%
  dplyr::summarize(mean_wgt = mean(wgt))

# down-weighting latinx & up-weighting black
pew %>%
  group_by(rac) %>%
  dplyr::summarize(mean_wgt = mean(wgt))

# substantial up-weighting HS- & down-weighting BA+
pew %>%
  group_by(edu) %>%
  dplyr::summarize(mean_wgt = mean(wgt))

# histogram shows outlier weight, reran models excluding and same pattern of results
ggplot(data = subset(pew, rac == "L"), mapping = aes(x = wgt)) +
  geom_histogram() +
  theme_light()

# histogram shows no obvious outliers
ggplot(data = subset(pew, edu == "HS-"), mapping = aes(x = wgt)) +
  geom_histogram() +
  theme_light()

