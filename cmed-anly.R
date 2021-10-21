### Purpose: prepare analysis of covid media consumption and psychological distress
### Author: S Bauldry
### Date: October 20, 2021

### set working directory and load packages
setwd("~/desktop")
library(tidyverse)
library(margins)
library(broom)

### preliminary analysis of predictors of very close media consumption
# read prepared data and rescale weights
pew_full <- read_csv("cmed-full-data.csv") %>%
  mutate(wt = weight_w64/sum(weight_w64)*n())

# weighted bivariate relationship between age and media consumption
age_nws <- pew_full %>%
  group_by(age) %>%
  summarize(pr_nws = sum(wt*nws)/sum(wt))
age_nws

# model adjusting for sociodemographic covariates
fac <- c("nws", "age", "rce", "fem", "met", "reg", "mar", "edu")
pew_full[, fac] <- lapply(pew_full[, fac], factor)
m1_nws <- glm(nws ~ age + rce + fem + met + reg + mar + edu, data = pew_full, 
              family = quasibinomial, weights = wt)
summary(m1_nws)
summary( margins(m1_nws) )

length(pew_full$nws)


### auxiliary analysis of predictors of very close media consumption
# read prepared data and rescale weights
pew_old <- read_csv("cmed-old-data.csv") %>%
  mutate(wt = weight_w64/sum(weight_w64)*n())

# model for predictors
fac <- c("nws", "rce", "fem", "met", "reg", "mar", "edu")
pew_old[, fac] <- lapply(pew_old[, fac], factor)
m2_nws <- glm(nws ~ rce + fem + met + reg + mar + edu, data = pew_old, 
              family = quasibinomial, weights = wt)
summary(m2_nws)
summary( margins(m2_nws) )

length(pew_old$nws)


### analysis of predictors of psychological distress
m1_dis <- lm(dis ~ nws + rce + fem + edu + mar + met + reg, 
                   data = pew_old, weights = wt) 
summary(m1_dis)

# graph estimates for Figure 1
m1_prep <- tidy(m1_dis) %>%
  filter(term != "(Intercept)") %>%
  mutate(lb = estimate - 1.96*std.error,
         ub = estimate + 1.96*std.error,
         id = c(11:1))

fig1 <- ggplot(m1_prep, aes(x = factor(id), y = estimate, ymin = lb, ymax = ub)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  scale_x_discrete(labels = c("South", "West", "Midwest", "Not metro area", 
                              "Married/cohabiting", "College degree or more", 
                              "Some college", "Women", "non-Hispanic White", 
                              "Hispanic","VC media consumption")) +
  labs(x = "", y = "unstandardized estimate") +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 15))
fig1
ggsave("fig1.png", plot = fig1, width = 7.8, height = 4.32)

# models stratified by various subsamples
m2 <- tidy( lm(dis ~ nws + mar + met + reg + rce + edu, data = subset(pew_old, fem == 1), weights = wt) )
m3 <- tidy( lm(dis ~ nws + mar + met + reg + rce + edu, data = subset(pew_old, fem == 0), weights = wt) )

m4 <- tidy( lm(dis ~ nws + mar + met + reg + fem + edu, data = subset(pew_old, rce == 1), weights = wt) )
m5 <- tidy( lm(dis ~ nws + mar + met + reg + fem + edu, data = subset(pew_old, rce == 2), weights = wt) )
m6 <- tidy( lm(dis ~ nws + mar + met + reg + fem + edu, data = subset(pew_old, rce == 3), weights = wt) )

m7 <- tidy( lm(dis ~ nws + mar + met + reg + fem + rce, data = subset(pew_old, edu == 3), weights = wt) )
m8 <- tidy( lm(dis ~ nws + mar + met + reg + fem + rce, data = subset(pew_old, edu == 2), weights = wt) )
m9 <- tidy( lm(dis ~ nws + mar + met + reg + fem + rce, data = subset(pew_old, edu == 1), weights = wt) )

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
  labs(x = "", y = "unstandardized estimate") +
  scale_x_discrete(labels = c("High school or less", "Some college", 
                              "College degree or more", "non-Hispanic White", 
                              "Hispanic", "non-Hispanic Black", "Men", "Women")) +
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

dt(1, 2) # women - men
dt(3, 4) # black - hispanic
dt(3, 5) # black - white
dt(4, 5) # hispanic - white
dt(6, 7) # ba - sc
dt(6, 8) # ba - hs
dt(7, 8) # sc - hs
