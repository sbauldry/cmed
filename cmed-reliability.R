### Purpose: Estimate reliability for psychological distress scale
### Author:  S Bauldry
### Date:    November 20, 2022

### set working directory and load packages
setwd("~/desktop")
library(tidyverse)
library(lavaan)
library(polycor)
library(pbivnorm)


### Loading prepared data
pew <- read_csv("cmed-old-data.csv")

### set indicators as ordered
pew[, c("dis1", "dis2", "dis3", "dis4", "dis5")] <- 
  lapply(pew[, c("dis1", "dis2", "dis3", "dis4", "dis5")], ordered)

### obtain polychoric correlation matrix
pc12 <- polychor(pew$dis1, pew$dis2)
pc13 <- polychor(pew$dis1, pew$dis3)
pc14 <- polychor(pew$dis1, pew$dis4)
pc15 <- polychor(pew$dis1, pew$dis5)
pc23 <- polychor(pew$dis2, pew$dis3)
pc24 <- polychor(pew$dis2, pew$dis4)
pc25 <- polychor(pew$dis2, pew$dis5)
pc34 <- polychor(pew$dis3, pew$dis4)
pc35 <- polychor(pew$dis3, pew$dis5)
pc45 <- polychor(pew$dis4, pew$dis5)

pcm <- rbind( c(1, pc12, pc13, pc14, pc15), c(pc12, 1, pc23, pc24, pc25), 
              c(pc13, pc23, 1, pc34, pc35), c(pc14, pc24, pc34, 1, pc45), 
              c(pc15, pc25, pc35, pc45, 1) )


### obtain thresholds and factor loadings
mod1 <- '
  PD =~ dis1 + dis2 + dis3 + dis4 + dis5
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
