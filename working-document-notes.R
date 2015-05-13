---
  title: "Our Working Document/ideas for extensions & notes on unused code"
author: "Yari & Kyonne" 
date: "N/a"
output: html_document
---
  
  Plan for replication project:
(1) Showing abbreviated version of the table 
(2) Explain what that table displays 
(3) R code to get there
----
  Possibilities for added components for replication project (due later)
(1) power analyses - with and without excluded participants
(2) moderation analysis by gender
(3) graph the results with ggplot2

##In order to not show table, {r, eval = FALSE}
##In order to not show code, {r, echo = FALSE}

3: R code
```{r, eval= FALSE}
# (3) Code for analyses & table:

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

# CODE FOR TABLE HERE:
#install.packages("tables")
library(tables)
#install.packages("mosaic")
library(mosaic)
#install.packages("stargazer")
library(stargazer)
#install.packages("xtable")
library(xtable)
library(plyr)
library(dplyr)

# using tables function, doesnt work as planned
#csvtable<-tbl_df("Experiment3_ReplicationTable_csv.csv")
#view(csvtable)

#tabular(dat_egood$music + 1) ~ (n+1) + Format(digits=2)* 
          (dat_egood$chose_advertised_pen)* (n+ proportions), data=dat_egood)))

#not what expected
#mosaicplot(freqtable)

# better
#attach(dat_egood)
#freqtable<-table(dat_egood$music,dat_egood$chose_advertised_pen)
#freqtable
#margin.table(freqtable, 1)
#margin.table(freqtable, 2)
#prop.table(freqtable)
#proportions <-prop.table(freqtable, 1)
#proportions
#prop.table(freqtable, 2)

# trying stargazer... no good
#stargazer(dat_egood[c("")], type= "text", title= "Frequencies...", digits=2, out="freqtable.txt")

# x table gives problems:
#tbl2<-tbl_df(tbl2)
#print(xtable(tbl2), type="html")

## Possible new contribution #1: Moderation analysis 
#have to create dummy variables:

Music <- factor(dat_egood$music,
                levels = c(1,2),
                labels = c("LikedMusic", "DislikedMusic"))

ChosenPen <- factor(dat_egood$chose_advertised_pen,
                    levels = c(1,0),
                    labels = c("ChoseAdPen", "NotChoseAdPen"))

#figure out which is female and which is male in geslacht1
count(dat_egood, geslacht1)

# Gender dummy
Gender <- factor(dat_egood$geslacht1,
                 levels = c(1,2),
                 labels = c("Male", "Female"))

#a logistic regression
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

# Possible new contribution #2: Graph results of moderation analysis??

## Possible new contribution #3: Graph raw data

# Scatterplots = template
graph.simple = ggplot(dat, aes(x = wmc.center, y = stroop.rt)) 
graph.simple + geom_smooth(method = "lm") +
  geom_point() +
  ggtitle("Simple Effect of Working Memory on Stroop Performance (RT)") +
  ylab("Stroop RT") +
  xlab("Working Memory Deviation Score from Mean")

graph.moderator = ggplot(dat, aes(x = X.center, y = Y, colour = M)) 
graph.moderator + geom_smooth(method = "lm") +
  geom_point() +
  ggtitle("Working Memory on Stroop Performance (RT)") +
  ylab("Stroop RT") +
  xlab("Working Memory Deviation Score from Mean")
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
