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

#center predictor variable (using blanks for variable name until we get the translated documents:
dat$X.center<-scale(dat$X, center=TRUE, scale=FALSE)
#set moderator as a factor
dat$M<-as.factor(dat$M)
#Test for homogeneity of variance:
leveneTest(dat$X ~ dat$M, center = mean)
#simple effect of centered predictor on Y
model.centered<-lm(dat$______ ~ dat$______.center)
summary(model.centered)
#centered models:
m1.cent<-lm(dat$Y ~ dat$X.center 
            + dat$M)
summary(m1.cent)

m2.cent<-lm(dat$Y ~ dat$X.center 
            + dat$M
            + (dat$X.center*dat$M))
summary(m2.cent)
#compare models
anova(m1.cent, m2.cent)


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
