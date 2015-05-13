# Load packages
library(pwr)
library(dplyr)
library(broom)
library(tidyr)
library(ggplot2)
library(MBESS)
library(psych)
library(car)
library(lsr)
library(MASS)

# Load data
dat <- read.csv("GORN_study3.csv", header = TRUE)
dat <- tbl_df(dat)
#head(dat)
#View(dat)

# Designate factors:
dat$Groep <- as.factor(dat$Groep)
dat$music <- as.factor(dat$music)
dat$chose_advertised_pen <- as.factor(dat$chose_advertised_pen)

## Filter out 2 excluded participants who accidentily took study twice in the paper:
dat_e <- dat %>%
  filter(excluded == 0)
#dat_e
#View(dat_e)

# Analyses on ENTIRE sample:

# Test whether music condition is independent of pen choice:
# (1 == liked music, 2 == disliked music)
# (0 == not chosen, 1 == chosen)
tbl = table(dat_e$music, dat_e$chose_advertised_pen) 
tbl    

# Chi-Square, no correction, no simulation:
cs1 <- chisq.test(tbl, correct = FALSE)

# Power analysis -- Find the power to detect .35 effect size reported in paper:
pwr.chisq.test(w = .39, N = 91, df = 1, sig.level = 0.05, power = NULL)

# Power analysis -- Find the effect size for a test with .8 sensitivity:
pwr.chisq.test(w = NULL, N = 91, df = 1, sig.level = 0.05, power = .8)

# Chi-Square tests - more conservative, not in the paper: 
## With Yate's correction:
# chisq.test(tbl)
## No correction but a Monte Carlo simulation:
# chisq.test(tbl, simulate.p.value = TRUE)

# Analyses on SUBSETTED sample:

# Filter out additional 19 Ss with too positive attitudes in "disliked music" condition & too negative attitudes in "liked music" condition:

# Find Cronbach's Alpha for music attitudes scale (for comparison with article):
# dat_music <- dat_e %>%
# select(att_muz1, att_muz2, att_muz3) 
# View(dat_music)
# alpha(dat_music, na.rm = TRUE, delete=TRUE)

#Find mean attitudes for each music condition (for comparison with article): 
dat_e %>%
  group_by(music) %>%
  summarise(avg = mean(c(att_muz1, att_muz2, att_muz3)))

# Add mean attitude column to dataframe:
dat_e <- dat_e %>%
  mutate(avg_att_muz = (att_muz1 + att_muz2 + att_muz3) / 3)
View(dat_e)

#Check that means (for comparison with article):
#dat_e %>%
#  group_by(music) %>%
#  summarise(avg = mean(avg_att_muz))

# Exclude when average attitude > 3 in disliked cond. or average attitude <3 for liked cond. (1 == liked music, 2 == disliked music)
dat_egood <- dat_e %>%
  filter(music == 1 & avg_att_muz > 3 | music == 2 & avg_att_muz < 3)
View(dat_egood)

## Redo Chi-Square with subsetted data:

# Test whether music condition is independent of pen choice:
tbl2 = table(dat_egood$music, dat_egood$chose_advertised_pen) 
tbl2    

# Chi-Square, no correction, no simulation:
cs2 <- chisq.test(tbl2, correct = FALSE)
tidy(cs2)

# Power analysis -- Find the power to detect .35 effect size reported in paper:
pwr.chisq.test(w = .35, N = 72, df = 1, sig.level = 0.05, power = NULL)
# Power analysis -- Find the effect size for a test with minimum of .8 sensitivity:
pwr.chisq.test(w = NULL, N = 72, df = 1, sig.level = 0.05, power = .8)

# Confidence Intervals (not replicated in paper):
#upper (95%)
#CI95_up <- qchisq(.95, df = 1)
#lower (5%)
#CI95_low <- qchisq(.05, df = 1)

# Other Chi-Square tests - more conservative, not in the paper: 
## With Yate's correction:
# chisq.test(tbl2)
## No correction but a Monte Carlo simulation:
# chisq.test(tbl2, simulate.p.value = TRUE)

# Logistic Regressions
library(ggplot2)
library(tidyr)
library(dplyr)
library(stargazer)


#Create dummy variables:
Music <- factor(dat_egood$music,
                    levels = c(1,2),
                    labels = c("LikedMusic", "DislikedMusic"))


#Recode dummy variables:
dat_egood$music <- as.numeric(dat_egood$music)
dat_egood <- dat_egood %>%
  mutate(music.recode2 = (music - 1))
View(dat_egood)

#Set factors:
Music <- factor(dat_egood$music.recode2,
                    levels = c(0, 1),
                    labels = c("Liked", "DisLiked"))

ChosenPen <- factor(dat_egood$chose_advertised_pen,
                levels = c(1,0),
                labels = c("ChoseAdPen", "NotChoseAdPen"))

#Compare counts of each gender in paper to data for coding:
count(dat_egood, geslacht1)
## 1 = male
## 2 = female

# need to check which is which:
Gender <- factor(dat_egood$geslacht1,
                    levels = c(1,2),
                    labels = c("Male", "Female"))

# for relationship between advertised pen and music, music 2 (disliked) seems significant:
fit1 <- glm(ChosenPen ~ Music, data = dat_egood, family = binomial)
fit1 <- tidy(fit1)
fit1

#### calculate odds 
fit1$estimate
fit1$estimate[1] + fit1$estimate[2] ## 1.216
## odds ratio
exp(fit1$estimate[2]) ##4.43
## predicted probabilities:
# Disliked Music:
(exp(fit1$estimate[1] + fit1$estimate[2])) / (1 + exp(fit1$estimate[1] + fit1$estimate[2]))
# Liked Music: 
exp(fit1$estimate[1]) / (1 + exp(fit1$estimate[1])) 

# does the relationship change when looking ONLY at males & ONLY females?

male.dat_egood <- dat_egood %>%
  filter(geslacht1 == "1")
#count(male.dat_egood, geslacht1)

female.dat_egood <- dat_egood %>%
  filter(geslacht1 == "2")
#count(female.dat_egood, geslacht1)

# MALES:
fitmale <- glm(ChosenPen ~ Music, data = male.dat_egood, family=binomial)
fitmale <- tidy(fitmale)
fitmale
#### calculate odds 
fitmale$estimate
fitmale$estimate[1] + fitmale$estimate[2] ## 1.216
## odds ratio
exp(fitmale$estimate[2]) ##4.43

# FEMALES:
fitfemale <- glm(ChosenPen ~ Music, data = female.dat_egood, family=binomial)
fitfemale <- tidy(fitfemale)
fitfemale
#### calculate odds 
fitfemale$estimate
fitfemale$estimate[1] + fitfemale$estimate[2] ## 1.216
## odds ratio
exp(fitfemale$estimate[2]) ## 4.43


### predicted probabilities = confirmed, gender does not have a moderating effect on the likelihood 
# of choosing the advertised pen when listening to liked versus disliked music.
# Males - Disliked Music = 0.77
(exp(fitmale$estimate[1] + fitmale$estimate[2])) / 
  (1 + exp(fitmale$estimate[1] + fitmale$estimate[2])) 
# Males - Liked Music = 0.43
exp(fitmale$estimate[1]) / (1 + exp(fitmale$estimate[1])) 

# Females - Disliked Music = 0.77
(exp(fitfemale$estimate[1] + fitfemale$estimate[2])) / 
  (1 + exp(fitfemale$estimate[1] + fitfemale$estimate[2])) 
# Males - Liked Music = 0.43
exp(fitfemale$estimate[1]) / (1 + exp(fitfemale$estimate[1])) 












