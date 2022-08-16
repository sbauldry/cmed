### Purpose: Prepare analysis of covid media consumption and distress
### Author:  S Bauldry
### Date:    August 15, 2022

setwd("~/desktop")
library(tidyverse)
library(ggpubr)
library(margins)
library(broom)
library(weights)


### load prepared data for analysis
pew_old <- read_csv("cmed-old-data.csv", 
                    col_types = list(nws = "f", rce = "f", fem = "f", met = "f",
                                     reg = "f", mar = "f", edu = "f", mhc = "f")) %>%
  mutate(nws = fct_relevel(nws, "0", "1"),
         rce = fct_relevel(rce, "1", "2", "3"),
         fem = fct_relevel(fem, "0", "1"),
         met = fct_relevel(met, "0", "1"),
         reg = fct_relevel(reg, "1", "2", "3", "4"),
         mar = fct_relevel(mar, "0", "1"),
         edu = fct_relevel(edu, "1", "2", "3"),
         mhc = fct_relevel(mhc, "0", "1"))


### descriptive statistics (Table 1)



### predictors of very close media consumption (Table 2)
nws_m1 <- glm(nws ~ rce + fem + met + reg + mar + edu + mhc, data = pew_old, 
               family = quasibinomial, weights = rwt)
summary(nws_m1)
summary( margins(nws_m1) )


### average net effect of very close media consumption (Table 3)
dis_m1 <- lm(dis ~ nws + rce + fem + edu + mar + met + reg + mhc, data = pew_old, 
             weights = rwt) 
summary(dis_m1)

# graph estimates for presentations
dis_m1_est <- tidy(dis_m1) %>%
  filter(term != "(Intercept)") %>%
  mutate(lb = estimate - 1.96*std.error,
         ub = estimate + 1.96*std.error,
         id = c(12:1))

fig1 <- ggplot(dis_m1_est, aes(x = factor(id), y = estimate, ymin = lb, ymax = ub)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  scale_x_discrete(labels = c("Mental health condition", "South", "West", "Midwest", 
                              "Metro area", "Married/cohabiting", "College degree or more", 
                              "Some college", "Women", "Non-Hispanic White", 
                              "Hispanic", "VC media consumption")) +
  labs(x = "", y = "unstandardized estimate") +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 15))
fig1
ggsave("cmed-fig1.png", plot = fig1)


### subgroup estimates for very close media consumption
str_mod <- function(v, i) {
  d <- pew_old %>% filter( v == i )
  m <- lm(dis ~ nws + met + reg + mhc + fem1 + rce2 + rce3 + edu2 + edu3 + mar1, data = d, weights = rwt)
  return(m)
}

m_mal <- str_mod(pew_old$fem, 0)
m_fem <- str_mod(pew_old$fem, 1)

m_hsp <- str_mod(pew_old$rce, 1)
m_blk <- str_mod(pew_old$rce, 2)
m_wht <- str_mod(pew_old$rce, 3)

m_hs  <- str_mod(pew_old$edu, 1)
m_sc  <- str_mod(pew_old$edu, 2)
m_ba  <- str_mod(pew_old$edu, 3)

m_nmc <- str_mod(pew_old$mar, 0)
m_mc  <- str_mod(pew_old$mar, 1)

# gathering estimates of very close media consumption
ge <- function(mod, i) {
  e <- mod$coefficients
  s <- sqrt( diag(vcov(mod)) )
  u <- e[[2]] + 1.96*s[[2]]
  l <- e[[2]] - 1.96*s[[2]]
  c <- cbind(id = i, est = e[[2]], se = s[[2]], lb = l, ub = u)
  return(c)
}
est <- data.frame( rbind( ge(m_fem, 10), ge(m_mal, 9), ge(m_hsp, 8), ge(m_blk, 7), 
                          ge(m_wht, 6), ge(m_ba, 5), ge(m_sc, 4), ge(m_hs, 3),
                          ge(m_mc, 2), ge(m_nmc, 1) ) )

# Clogg test for pairwise contrasts
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
dt(3, 4) # hispanic - black
dt(4, 5) # black - white
dt(3, 5) # hispanic - white
dt(6, 7) # ba - sc
dt(6, 8) # ba - hs
dt(7, 8) # sc - hs
dt(9, 10) # mar - not mar

# calculate predicted psychological distress
prd_dis <- function(v, i) {
  
  # select subsample
  d <- pew_old %>% filter( v == i )
  
  # create vector of weighted means
  wmv <- c(weighted.mean(d$mar1, d$rwt), weighted.mean(d$met1, d$rwt),
           weighted.mean(d$reg2, d$rwt), weighted.mean(d$reg3, d$rwt),
           weighted.mean(d$reg4, d$rwt), weighted.mean(d$fem1, d$rwt),
           weighted.mean(d$rce2, d$rwt), weighted.mean(d$rce3, d$rwt),
           weighted.mean(d$edu2, d$rwt), weighted.mean(d$edu3, d$rwt),
           weighted.mean(d$mhc1, d$rwt))
  
  # fit model (note: keeping in subsample indicators for ease of programming)
  m <- lm(dis ~ nws1 + mar1 + met1 + reg2 + reg3 + reg4 + fem1 + rce2 + rce3 + 
            edu2 + edu3 + mhc1, data = d, weights = rwt)
  
  # calculate predicted values of psychological distress
  nd <- data.frame(nws1 = c(0, 1), 
                   mar1 = rep(wmv[1], 2), met1 = rep(wmv[2], 2), 
                   reg2 = rep(wmv[3], 2), reg3 = rep(wmv[4], 2), 
                   reg4 = rep(wmv[5], 2), fem1 = rep(wmv[6], 2), 
                   rce2 = rep(wmv[7], 2), rce3 = rep(wmv[8], 2), 
                   edu2 = rep(wmv[9], 2), edu3 = rep(wmv[10], 2),
                   mhc1 = rep(wmv[11], 2))
  pr  <- predict(m, nd, se.fit = T)
  prp <- rbind( c(0, pr$fit[1], pr$se.fit[1]), c(1, pr$fit[2], pr$se.fit[2]) )
  return(prp)
}

pr_fem <- prd_dis(pew_old$fem, 1)
pr_mal <- prd_dis(pew_old$fem, 0)
pr_hsp <- prd_dis(pew_old$rce, 1)
pr_blk <- prd_dis(pew_old$rce, 2)
pr_wht <- prd_dis(pew_old$rce, 3)
pr_hs  <- prd_dis(pew_old$edu, 1)
pr_sc  <- prd_dis(pew_old$edu, 2)
pr_ba  <- prd_dis(pew_old$edu, 3)
pr_nmc <- prd_dis(pew_old$mar, 0)
pr_mc  <- prd_dis(pew_old$mar, 1)

# gathering estimates of predicted psychological distress
id <- 1:29
prdis <- data.frame( cbind( id, rbind(pr_nmc, c(0, 0, 0), pr_mc, c(0, 0, 0), 
                                      pr_hs, c(0, 0, 0), pr_sc, c(0, 0, 0), 
                                      pr_ba, c(0, 0, 0), pr_wht, c(0, 0, 0), 
                                      pr_blk, c(0, 0, 0), pr_hsp, c(0, 0, 0), 
                                      pr_mal, c(0, 0, 0), pr_fem) ) )
colnames(prdis) <- c("id", "nws", "est", "se")

# graph estimates and predicted values
fig2a <- ggplot(est, aes(x = factor(id), y = est, ymin = lb, ymax = ub)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = "", y = "VC media consumption estimate", title = "Panel A") +
  scale_x_discrete(labels = c("Not mar/coh", "Mar/coh", "High school", "Some college", "College degree", 
                              "White", "Black", "Hispanic", "Men", "Women")) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 15))
fig2a

wmdis <- weighted.mean(pew_old$dis, weight = rwt)
fig2b <- ggplot(prdis, aes(x = factor(id), y = est, ymin = est - 1.96*se, ymax =  est + 1.96*se)) +
  geom_pointrange() +
  geom_hline(yintercept = wmdis, linetype = "dashed", color = "blue") +
  labs(x = "", y = "predicted psychological distress", title = "Panel B") +
  scale_x_discrete(labels = c("not VC", "Not mar/coh -- VC", "",
                              "not VC", "Mar/coh -- VC", "",
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
fig2b

fig2 <- ggarrange(fig2a, fig2b) 
fig2
ggsave("fig2.png", plot = fig2)

