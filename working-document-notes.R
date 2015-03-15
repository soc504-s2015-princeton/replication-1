---
  title: "replication report" <-- Our Working Document/ideas for extensions
author: "Yari & Kyonne" 
date: "February 22, 2015"
output: html_document
---
  
  Plan for replication project
(1) Showing abbreviated version of the table 
(2) Explain what that table displays 
(3) R code to get there
----
  Plan for added components for replication project (due later)
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
#dat_music <- dat_e %>%
#  select(att_muz1, att_muz2, att_muz3)
#View(dat_music)
#alpha(dat_music, na.rm = TRUE, delete=TRUE)

#Find mean attitudes for each music condition (for comparison with article): 
#dat_e %>%
#  group_by(music) %>%
#  summarise(avg = mean(c(att_muz1, att_muz2, att_muz3)))

# Add mean attitude column to dataframe:
dat_e <- dat_e %>%
  mutate(avg_att_muz = (att_muz1 + att_muz2 + att_muz3) / 3)
#View(dat_e)

#Check that means (for comparison with article):
#dat_e %>%
#  group_by(music) %>%
#  summarise(avg = mean(avg_att_muz))

# Exclude when average attitude > 3 in disliked cond. or average attitude <3 for liked cond. (1 == liked music, 2 == disliked music)
dat_egood <- dat_e %>%
  filter(music == 1 & avg_att_muz > 3 | music == 2 & avg_att_muz < 3)
#View(dat_egood)

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

# CODE FOR TABLE HERE:

# Create Frequency tallies for replicating the table:
##(chose_ad_pen_reversed: 1 == chose non-advertised, 0 == chose advertised)
##(chose_advertised_pen: 1 == chose advertised pen, 0 == chose non-advertised pen)

```

```{r}

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



```

Graph results of moderation analysis

```{r, echo=FALSE}
##Scatterplots
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
