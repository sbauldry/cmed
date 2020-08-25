### Purpose: estimate reliability for psychological distress scale
### Author: S Bauldry
### Date: August 25, 2020

### set working directory and load packages
rm(list = ls())
setwd("~/desktop")
library(tidyverse)
library(lavaan)
library(polycor)
library(pbivnorm)


### read prepared data
pew <- read_csv("cmed-data.csv", col_types = list("sex" = col_factor(), "rac" = col_factor(), 
                                                  "met" = col_factor(), "reg" = col_factor(),
                                                  "edu" = col_factor(), "mar" = col_factor(),
                                                  "psy" = col_factor(), "nws" = col_factor()))
summary(pew)


### set indicators as ordered
pew[, c("pd1", "pd2", "pd3", "pd4", "pd5")] <- lapply(pew[, c("pd1", "pd2", "pd3", "pd4", "pd5")], ordered)


### obtain polychoric correlation matrix.
pc12 <- polychor(pew$pd1, pew$pd2)
pc13 <- polychor(pew$pd1, pew$pd3)
pc14 <- polychor(pew$pd1, pew$pd4)
pc15 <- polychor(pew$pd1, pew$pd5)
pc23 <- polychor(pew$pd2, pew$pd3)
pc24 <- polychor(pew$pd2, pew$pd4)
pc25 <- polychor(pew$pd2, pew$pd5)
pc34 <- polychor(pew$pd3, pew$pd4)
pc35 <- polychor(pew$pd3, pew$pd5)
pc45 <- polychor(pew$pd4, pew$pd5)

pcm <- rbind( c(1, pc12, pc13, pc14, pc15), c(pc12, 1, pc23, pc24, pc25), 
              c(pc13, pc23, 1, pc34, pc35), c(pc14, pc24, pc34, 1, pc45), 
              c(pc15, pc25, pc35, pc45, 1))


### obtain thresholds and factor loadings
mod1 <- '
  PD =~ pd1 + pd2 + pd3 + pd4 + pd5
'
fit1 <- cfa(mod1, data = pew)
sde <- standardizedSolution(fit1)
lam <- cbind( sde[1,4], sde[2,4], sde[3,4], sde[4,4], sde[5,4] )
tau <- cbind( c(sde[6,4], sde[9,4], sde[12,4], sde[15,4], sde[18,4]),
              c(sde[7,4], sde[10,4], sde[13,4], sde[16,4], sde[19,4]),
              c(sde[8,4], sde[11,4], sde[14,4], sde[17,4], sde[20,4]) )


### Function to calculate ordinal omega
ordinal_omega <- function(lambda, tau, pcm) {
  
  nj <- length(lambda)
  nc <- ncol(tau)
  
  outer_sum1 <- 0
  outer_sum2 <- 0
  
  for(j in 1:nj) {
    for(jp in 1:nj) {
      
      inner_sum1 <- 0
      inner_sum2 <- 0
      inner_sum3 <- 0
      inner_sum4 <- 0
      
      for(c in 1:nc) {
        
        inner_sum3 <- inner_sum3 + pnorm(tau[j,c])
        inner_sum4 <- inner_sum4 + pnorm(tau[jp,c])
        
        for(cp in 1:nc) {
          inner_sum1 <- inner_sum1 + pbivnorm(tau[j,c], tau[jp,cp], lambda[j]*lambda[jp])
          inner_sum2 <- inner_sum2 + pbivnorm(tau[j,c], tau[jp,cp], pcm[j,jp])
        }
      }
      
      outer_sum1 <- outer_sum1 + inner_sum1 - inner_sum3*inner_sum4
      outer_sum2 <- outer_sum2 + inner_sum2 - inner_sum3*inner_sum4
      
    }
  }
  
  ordinal_omega <- outer_sum1/outer_sum2
  return(ordinal_omega)
}

ordinal_omega(lam, tau, pcm)
