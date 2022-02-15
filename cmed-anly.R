### Purpose: Prepare analysis of covid media consumption and distress
### Author:  S Bauldry
### Date:    December 20, 2021

setwd("~/desktop")
library(tidyverse)
library(ggpubr)
library(margins)
library(broom)
library(weights)

### preliminary analysis of predictors of very close media consumption
pew_full <- read_csv("cmed-full-data.csv", 
                     col_types = list(dis = "d", nws = "f", rce = "f",
                                      fem = "f", age = "f", met = "f",
                                      reg = "f", mar = "f", edu = "f",
                                      rwt = "d"))
pew_full <- pew_full %>%
  mutate(nws = fct_relevel(nws, "0", "1"),
         rce = fct_relevel(rce, "1", "2", "3", "4"),
         fem = fct_relevel(fem, "0", "1"),
         age = fct_relevel(age, "1", "2", "3", "4"),
         met = fct_relevel(met, "0", "1"),
         reg = fct_relevel(reg, "1", "2", "3", "4"),
         mar = fct_relevel(mar, "0", "1"),
         edu = fct_relevel(edu, "1", "2", "3"))

# weighted media consumption by age group
wpct(pew_full$nws[pew_full$age == 1], weight = pew_full$rwt[pew_full$age == 1])
wpct(pew_full$nws[pew_full$age == 2], weight = pew_full$rwt[pew_full$age == 2])
wpct(pew_full$nws[pew_full$age == 3], weight = pew_full$rwt[pew_full$age == 3])
wpct(pew_full$nws[pew_full$age == 4], weight = pew_full$rwt[pew_full$age == 4])

# model adjusting for sociodemographic covariates
pm1_nws <- glm(nws ~ age + rce + fem + met + reg + mar + edu, data = pew_full, 
              family = quasibinomial, weights = rwt)
summary(pm1_nws)
summary( margins(pm1_nws) )


### Primary analysis of older adults
pew_old <- read_csv("cmed-old-data.csv", 
                     col_types = list(dis = "d", nws = "f", rce = "f",
                                      fem = "f", met = "f", reg = "f", 
                                      mar = "f", edu = "f", rwt = "d"))
pew_old <- pew_old %>%
  mutate(nws = fct_relevel(nws, "0", "1"),
         rce = fct_relevel(rce, "1", "2", "3"),
         fem = fct_relevel(fem, "0", "1"),
         met = fct_relevel(met, "0", "1"),
         reg = fct_relevel(reg, "1", "2", "3", "4"),
         mar = fct_relevel(mar, "0", "1"),
         edu = fct_relevel(edu, "1", "2", "3"))

# auxiliary analysis of predictors of very close media consumption
am1_nws <- glm(nws ~ rce + fem + met + reg + mar + edu, data = pew_old, 
              family = quasibinomial, weights = rwt)
summary(am1_nws)
summary( margins(am1_nws) )

# average net effect of media consumption
m1 <- lm(dis ~ nws + rce + fem + edu + mar + met + reg, data = pew_old, weights = rwt) 
summary(m1)

# graph estimates for figure 1
m1_prep <- tidy(m1) %>%
  filter(term != "(Intercept)") %>%
  mutate(lb = estimate - 1.96*std.error,
         ub = estimate + 1.96*std.error,
         id = c(11:1))

fig1 <- ggplot(m1_prep, aes(x = factor(id), y = estimate, ymin = lb, ymax = ub)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  scale_x_discrete(labels = c("South", "West", "Midwest", "Metro area", 
                              "Married/cohabiting", "College degree or more", 
                              "Some college", "Women", "non-Hispanic White", 
                              "Hispanic","VC media consumption")) +
  labs(x = "", y = "unstandardized estimate") +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 15))
fig1
ggsave("fig1.png", plot = fig1)

# models stratified by various subsamples
m2 <- lm(dis ~ nws + mar + met + reg + rce + edu, data = subset(pew_old, fem == 1), weights = rwt)
m3 <- lm(dis ~ nws + mar + met + reg + rce + edu, data = subset(pew_old, fem == 0), weights = rwt)

m4 <- lm(dis ~ nws + mar + met + reg + fem + edu, data = subset(pew_old, rce == 2), weights = rwt)
m5 <- lm(dis ~ nws + mar + met + reg + fem + edu, data = subset(pew_old, rce == 1), weights = rwt)
m6 <- lm(dis ~ nws + mar + met + reg + fem + edu, data = subset(pew_old, rce == 3), weights = rwt)

m7 <- lm(dis ~ nws + mar + met + reg + fem + rce, data = subset(pew_old, edu == 3), weights = rwt)
m8 <- lm(dis ~ nws + mar + met + reg + fem + rce, data = subset(pew_old, edu == 2), weights = rwt)
m9 <- lm(dis ~ nws + mar + met + reg + fem + rce, data = subset(pew_old, edu == 1), weights = rwt)

m10 <- lm(dis ~ nws + met + reg + fem + rce + edu, data = subset(pew_old, mar == 1), weights = rwt)
m11 <- lm(dis ~ nws + met + reg + fem + rce + edu, data = subset(pew_old, mar == 0), weights = rwt)

# prepare estimate for figure
ge <- function(mod, i) {
  e <- mod$coefficients
  s <- sqrt( diag(vcov(mod)) )
  u <- e[[2]] + 1.96*s[[2]]
  l <- e[[2]] - 1.96*s[[2]]
  c <- cbind(id = i, est = e[[2]], lb = l, ub = u)
  return(c)
}
est <- data.frame( rbind( ge(m2, 10), ge(m3, 9), ge(m4, 8), ge(m5, 7), 
                          ge(m6, 6), ge(m7, 5), ge(m8, 4), ge(m9, 3),
                          ge(m10, 2), ge(m11, 1) ) )

# graph estimates for figure 2
fig2 <- ggplot(est, aes(x = factor(id), y = est, ymin = lb, ymax = ub)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = "", y = "unstandardized estimate") +
  scale_x_discrete(labels = c("Not married", "Mar/Coh", "High school", "Some college", "College degree", 
                              "White", "Black", "Hispanic", 
                              "Men", "Women")) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 15))
fig2
ggsave("fig2.png", plot = fig2)


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
dt(9, 10) # mar - not mar


### Creating a figure of predicted psychological distress
# constructing indicators in order to calculate predicted values
pew_old <- pew_old %>%
  mutate(nws1 = ifelse(nws == 1, 1, 0),
         fem1 = ifelse(fem == 1, 1, 0),
         met1 = ifelse(met == 1, 1, 0),
         mar1 = ifelse(mar == 1, 1, 0),
         rce2 = ifelse(rce == 2, 1, 0),
         rce3 = ifelse(rce == 3, 1, 0), 
         edu2 = ifelse(edu == 2, 1, 0),
         edu3 = ifelse(edu == 3, 1, 0),
         reg2 = ifelse(reg == 2, 1, 0),
         reg3 = ifelse(reg == 3, 1, 0),
         reg4 = ifelse(reg == 4, 1, 0))
summary(pew_old)
         
# function to calculate predicted values for each subsample
pdis <- function(df, sx, sv) {
  
  # select subsample
  x <- eval(substitute(sx), df)
  d <- df %>% filter(x == sv)
  
  # create vector of weighted means
  wmv <- c(weighted.mean(d$mar1, d$rwt), weighted.mean(d$met1, d$rwt),
           weighted.mean(d$reg2, d$rwt), weighted.mean(d$reg3, d$rwt),
           weighted.mean(d$reg4, d$rwt), weighted.mean(d$fem1, d$rwt),
           weighted.mean(d$rce2, d$rwt), weighted.mean(d$rce3, d$rwt),
           weighted.mean(d$edu2, d$rwt), weighted.mean(d$edu3, d$rwt))

  # fit model (note: keeping in subsample indicators for ease of programming)
  m <- lm(dis ~ nws1 + mar1 + met1 + reg2 + reg3 + reg4 + fem1 + rce2 + rce3 + 
                edu2 + edu3, data = d, weights = rwt)
  
  # calculate predicted values of psychological distress
  nd <- data.frame(nws1 = c(0, 1), mar1 = rep(wmv[1], 2), met1 = rep(wmv[2], 2), 
                   reg2 = rep(wmv[3], 2), reg3 = rep(wmv[4], 2), 
                   reg4 = rep(wmv[5], 2), fem1 = rep(wmv[6], 2), 
                   rce2 = rep(wmv[7], 2), rce3 = rep(wmv[8], 2), 
                   edu2 = rep(wmv[9], 2), edu3 = rep(wmv[10], 2))
  pr  <- predict(m, nd, se.fit = T)
  prp <- rbind( c(0, pr$fit[1], pr$se.fit[1]), c(1, pr$fit[2], pr$se.fit[2]) )
  return(prp)
}

# calculating predicted psychological distress for subsamples by media consumption
pr_fem <- pdis(pew_old, fem, 1)
pr_mal <- pdis(pew_old, fem, 0)
pr_hsp <- pdis(pew_old, rce, 2)
pr_blk <- pdis(pew_old, rce, 1)
pr_wht <- pdis(pew_old, rce, 3)
pr_hs  <- pdis(pew_old, edu, 1)
pr_sc  <- pdis(pew_old, edu, 2)
pr_ba  <- pdis(pew_old, edu, 3)
pr_nm  <- pdis(pew_old, mar, 0)
pr_mr  <- pdis(pew_old, mar, 1)

# gathering estimates for graphing
id <- 1:29
prdis <- data.frame( cbind( id, rbind(pr_nm, c(0, 0, 0), pr_mr, c(0, 0, 0), 
                                      pr_hs, c(0, 0, 0), pr_sc, c(0, 0, 0), 
                                      pr_ba, c(0, 0, 0), pr_wht, c(0, 0, 0), 
                                      pr_blk, c(0, 0, 0), pr_hsp, c(0, 0, 0), 
                                      pr_mal, c(0, 0, 0), pr_fem) ) )
colnames(prdis) <- c("id", "nws", "est", "se")

# creating figure
wmdis <- weighted.mean(pew_old$dis, weight = rwt)
fig3 <- ggplot(prdis, aes(x = factor(id), y = est, ymin = est - 1.96*se, ymax =  est + 1.96*se)) +
  geom_pointrange() +
  geom_hline(yintercept = wmdis, linetype = "dashed", color = "blue") +
  labs(x = "", y = "predicted psychological distress") +
  scale_x_discrete(labels = c("not VC", "Not Married -- VC", "",
                              "not VC", "Mar/Coh -- VC", "",
                              "not VC", "High school -- VC", "", 
                              "not VC", "Some college -- VC", "",
                              "not VC", "College degree -- VC", "", 
                              "not VC", "White -- VC", "",
                              "not VC", "Black -- VC", "", 
                              "not VC", "Hispanic -- VC", "",
                              "not VC", "Men -- VC", "", 
                              "not VC", "Women -- VC")) +
  scale_y_continuous(limits = c(7, 12)) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 15))
fig3
ggsave("fig3.png", plot = fig3)

# combining figures 2 and 3
fig <- ggarrange(fig2, fig3) 
fig
ggsave("fig4.png", plot = fig)

