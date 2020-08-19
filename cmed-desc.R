### Purpose: prepare descriptives for analysis of covid media exposure and psychological distress
### Author: S Bauldry
### Date: August 12, 2020

### set working directory and load packages
rm(list = ls())
setwd("~/desktop")
library(tidyverse)



### read prepared data


         



  mutate_if(is.numeric, list(~na_if(., 99))) %>% 
  
  drop_na() %>%
  mutate(sex = recode(as_factor(f_sex), Male = "M", Female = "F"),
         rac = recode(as_factor(f_racethn), "White non-Hispanic" = "W", "Black non-Hispanic" = "B", 
                      "Hispanic" = "H"),
         mar = ifelse(f_marital < 3, "M/P", "n M/P"),
         nws = ifelse(covidfol_w64 == 1, "VC", "n VC"),
         dis = mh_track_a_w64 + mh_track_b_w64 + mh_track_c_w64 + (5 - mh_track_d_w64) + mh_track_e_w64) 


### univariate distributions
uni_plot <- function(x, l) {
  ggplot(pew2, aes_string(x = x)) +
    geom_bar(aes(y = ..prop.., group = 1)) + 
    ylim(0, 1) +
    labs(y = "proportion respondents", x = l) +
    theme_light()
}

u1 <- uni_plot(x = "sex", l = "sex")
u2 <- uni_plot(x = "rac", l = "race")
u3 <- uni_plot(x = "mar", l = "marital status")
u4 <- uni_plot(x = "nws", l = "media exposure")
u5 <- ggplot(pew2, aes(x = dis)) + 
  geom_density(alpha = 0.3) +
  labs(x = "psychological distress") +
  theme_light()

ggarrange(u1, u2, u3, u4, u5)


### bivariate distributions with sociodemographics and media exposure
bi1_plot <- function(x, l) {
  pew2 %>%
    group_by_(x, "nws") %>%
    summarize(N = n()) %>%
    mutate(prp = N/sum(N)) %>%
    ggplot(aes_string(x = x, y = prp, fill = nws)) +
    geom_col(position = "dodge2") +
    labs(y = "proportion respondents", x = l, fill = "media") +
    ylim(0, 1) +
    theme_light()
}

bi1_plot(x = "sex", l = "sex")
bi1_plot(x = "rac", l = "race")



nws_by_sex <- pew2 %>%
  group_by(sex, nws) %>%
  summarize(N = n()) %>%
  mutate(prp = N/sum(N))
nws_by_sex 
ggplot(nws_by_sex, aes(x = sex, y = prp, fill = nws)) +
  geom_col(position = "dodge2") +
  labs(y = "proportion respondents", fill = "media") +
  ylim(0, 1) +
  theme_light()

ggplot(pew2, aes(x = sex, fill = nws)) +
  geom_bar(aes(y = ..prop.., group = nws), position = 'dodge') + 
  ylim(0, 1) +
  labs(y = "proportion respondents", fill = "media") +
  theme_light()

  

### plot distribution of distress by media consumption
ggplot(data = pew2, aes(x = dis, color = nws, fill = nws)) + 
  geom_density(alpha = 0.3) +
  labs(x = "psychological distress") +
  ggtitle("Density of psychological distress by media consumption") +
  theme_light()

ggplot(data = pew2, aes(x = dis, color = rac, fill = rac)) + 
  geom_density(alpha = 0.3) +
  labs(x = "psychological distress") +
  ggtitle("Density of psychological distress by race/ethnicity") +
  theme_light()

ggplot(data = pew2, aes(x = dis, color = sex, fill = sex)) + 
  geom_density(alpha = 0.3) +
  labs(x = "psychological distress") +
  ggtitle("Density of psychological distress by sex") +
  theme_light()

ggplot(data = pew2, aes(x = dis, color = mar, fill = mar)) + 
  geom_density(alpha = 0.3) +
  labs(x = "psychological distress") +
  ggtitle("Density of psychological distress by marital status") +
  theme_light()



