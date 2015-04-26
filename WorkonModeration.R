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

#have to create dummy variables:

Music <- factor(dat_egood$music,
                    levels = c(1,2),
                    labels = c("LikedMusic", "DislikedMusic"))

ChosenPen <- factor(dat_egood$chose_advertised_pen,
                levels = c(1,0),
                labels = c("ChoseAdPen", "NotChoseAdPen"))

#figure out which is female and which is male in geslacht1
count(dat_egood, geslacht1)

# need to check which is which:
Gender <- factor(dat_egood$geslacht1,
                    levels = c(1,2),
                    labels = c("Male", "Female"))

#Test for homogeneity of variance:
leveneTest(dat_egood$chose_advertised_pen ~ dat_egood$Male, center=mean)

#simple effect of centered predictor on Y
model.centered<-lm(dat_egood$chose_advertised_pen ~ dat_egood$Male)
summary(model.centered)

#centered models:
m1.cent<-lm(dat_egood$chose_advertised_pen ~ dat_egood$Likedmusic.center 
            + dat_egood$Male)
summary(m1.cent)

m2.cent<-lm(dat$Y ~ dat$X.center 
            + dat$M
            + (dat$X.center*dat$M))
summary(m2.cent)

#compare models
anova(m1.cent, m2.cent)

#More like a logistic regression
install.packages("QuantPsyc")
library(QuantPsyc)

# for relationship between advertised pen and music, music 2 (disliked) seems significant, 
# which is weird. 
glm1 <- glm(ChosenPen ~ Music, data=dat_egood, family=binomial)
summary(glm1)
glm1prob<-predict(glm1, dat_egood, type="response")
mean(glm1prob)

# reversed previsious glm
glm1a <- glm(Music ~ ChosenPen, data=dat_egood, family=binomial)
summary(glm1a)
glm1aprob<-predict(glm1a, dat_egood, type="response")
mean(glm1aprob)

# chosen pen outcome, music, and gender as moderator
glm1b <- glm(ChosenPen ~ Music + Gender, data=dat_egood, family=binomial)
summary(glm1b)
glm1bprob<-predict(glm1b, dat_egood, type="response")
mean(glm1bprob)

# music as outcome, chosen pen, gender as moderator 
glm1c <- glm(Music ~ ChosenPen + Gender, data=dat_egood, family=binomial)
summary(glm1b)
glm1cprob<-predict(glm1c, dat_egood, type="response")
mean(glm1cprob)

# basic gender on chosen pen
glm1d <- glm(ChosenPen ~ Gender, data=dat_egood, family=binomial)
summary(glm1d)
glm1dprob<-predict(glm1d, dat_egood, type="response")
mean(glm1dprob)

#Groups to outcome
glm2 <- glm(ChosenPen ~ dat_egood$Groep + Gender, data=dat_egood, family=binomial)
summary(glm2)
glm2prob<-predict(glm2, dat_egood, type="response")
mean(glm2prob)

#outcome is chosen pen, interaction between music and gender 
glm2a <- glm(ChosenPen ~ Music:Gender, data=dat_egood, family=binomial)
summary(glm2a)
glm2aprob<-predict(glm2a, dat_egood, type="response")
mean(glm2aprob)

#color of pen chosen
glm3<- glm(Music ~ dat_egood$pen_keus:Gender, data=dat_egood, family=binomial)
summary(glm3)







      











