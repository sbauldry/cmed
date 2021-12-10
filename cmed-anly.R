### Purpose: Prepare analysis of covid media consumption and distress
### Author:  S Bauldry
### Date:    December 9, 2020

setwd("~/desktop")
library(tidyverse)
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


### auxiliary analysis of predictors of very close media consumption
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

# model for predictors
am1_nws <- glm(nws ~ rce + fem + met + reg + mar + edu, data = pew_old, 
              family = quasibinomial, weights = rwt)
summary(am1_nws)
summary( margins(am1_nws) )


### analysis of predictors of psychological distress

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

# prepare estimate for figure
ge <- function(mod, i) {
  e <- mod$coefficients
  s <- sqrt( diag(vcov(mod)) )
  u <- e[[2]] + 1.96*s[[2]]
  l <- e[[2]] - 1.96*s[[2]]
  c <- cbind(id = i, est = e[[2]], lb = l, ub = u)
  return(c)
}
est <- data.frame( rbind( ge(m2, 8), ge(m3, 7), ge(m4, 6), ge(m5, 5), 
                          ge(m6, 4), ge(m7, 3), ge(m8, 2), ge(m9, 1)) )

# graph estimates for figure 2
fig2 <- ggplot(est, aes(x = factor(id), y = est, ymin = lb, ymax = ub)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = "", y = "unstandardized estimate") +
  scale_x_discrete(labels = c("High school -", "Some college", "College degree +", 
                              "non-Hispanic White", "non-Hispanic Black", "Hispanic", 
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


### Creating a figure of predicted values
# constructing indicators in order to calculate predicted values



m2a <- lm(dis ~ nws + mar, data = pew_old, weights = rwt)



m2and <- data.frame( nws = c(rep("0", 2795), rep("1", 2795)), mar = c(pew_old$mar, pew_old$mar) )
p2 <- data.frame( yh = predict(m2a, newdata = m2and, se.fit = T), m2and )
head(p2)
tapply(p2$yh, p2$nws, mean)

predict(m2a, newdata = m2and, weights = rwt)


mean(pew_old$mar)


m2_nd <- data.frame( expand.grid( nws = c(0,1), mar = mean(pew_old$mar[fem == 1]),
                                                           )))


m2a <- lm(dis ~ nws + mar + met + reg + rce + edu, data = subset(pew_old, fem == 1), weights = rwt)
m2a_fit <- m2a$fitted.values

new_nws <- data.frame( expand.grid( nws = c(0, 1), mar = pew_old$mar, met = pew_old$met, 
                                    reg = pew_old$reg) )

m2a_prd <- predict(m2a, newdata = new_nws)
cor(m2a_fit, m2a_prd)


