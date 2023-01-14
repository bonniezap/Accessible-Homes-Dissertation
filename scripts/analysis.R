###########
#Jan 2, 2023
#using file built from round 1 data prep, run logistic regression in flexplot, then add mixed method

#open issues:
#1/2 
#need numeric var on x-axis and need 6 categories in numeric variable to use flexplot
#should vars start at 1 or at 0? competence (0), burden (?)
#to do: add values for 2 more round, then group by round


#####Following youtube Dustin Fife video on GLM

# install.packages("devtools")
# install the stable version
#devtools::install_github("dustinfife/flexplot")
# install the development version
#devtools::install_github("dustinfife/flexplot", ref="development")
# https://github.com/dustinfife/flexplot
library (lme4)
library (flexplot)
library (tidyverse)
library (cowplot)
library (psych)
library (ggplot2)
#make sure corrected data gets in here later. This is draft analytic dataset.
d<- read.csv("~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/rounds12345678910.csv")
describe(d)

#univariate visuals
flexplot(falls~1, data=d)
flexplot(frailty~1, data=d)
flexplot(press~1, data=d)
flexplot(pfp~1, data=d)
flexplot(accburden~1, data=d)
flexplot(competence~1, data=d)

###############FROM SIMPLE LOGISTIC REGRESSION FIFE VIDEO 1/1
#create initial plots
flexplot (falls~competence | press, data=d, method="logistic", ghost.line="gray")

#It is viewing press and frailty as categorical vars in following command, so use competence, above
flexplot (falls~press | frailty, data=d, method="logistic", ghost.line="gray")

describe(d)

full=      glm(falls~competence*press, data=d, family =binomial)
reduced=   glm(falls~competence          , data=d, family =binomial)

full=      glm(falls~competence*press, recentanymod, data=d, family =binomial)
reduced=   glm(falls~competence          , recentanymod, data=d, family =binomial)


compare.fits(falls~competence | press, data=d, full, reduced)
model.comparison(full, reduced)

estimates (full)
visualize (full, plot='model', jitter=c(0, .1))

###############FROM MIXED MODEL WITH LOGISTIC REGRESSION FIFE VIDEO 1/2
flexplot(falls~1, data=d)
flexplot(frailty~1, data=d)
flexplot(press~1, data=d)
flexplot(pfp~1, data=d, bins=6)
flexplot(accburden~1, data=d, bins=6)
flexplot(competence~1, data=d, bins=6)
flexplot(hc1worryfall~1, data=d, bins=6)

#Fit the models
full=glmer(falls~pfp*press +
             (1 |female), data=d, family =binomial)

reduced=glmer(falls~pfp +
             (1 |female), data=d, family =binomial)

#visually compare models
compare.fits(falls~pfp | press + female, data=d, 
                      full, reduced, jitter=c(0, .1))

model.comparison (full, reduced)

visualize (full, plot="model", formula=falls~pfp+press)

###rerun with grouping on SPID and frailty as time progression
#Fit the models
full=glmer(falls~pfp*press + round +
             (1 |pfp), data=d, family =binomial)


reduced=glmer(falls~pfp +round +
                (1 |pfp), data=d, family =binomial)

#visually compare models
compare.fits(falls~pfp | press, data=d, 
             full, reduced, jitter=c(0, .1))

model.comparison (full, reduced)

visualize (full, plot="model", formula=falls~pfp + press)

##############1/2 fixed versus random effects in MLM
#baseline model
#then add fixed effects to model
#then add random effects to model
#if there is a random effect, it is also a fixed effect.

#baseline model: just fits a mean for overall group and individuals
#baseline=      glmer(y_outcome_var ~
#                      1 {means fit a random intercept}+
#                      (1 {1 means random intercept}list random effects here in paren with pipe then:}|{cluster_var})
#                    ,data={dataset}
#                   , family =binomial) {for logistic regression}

#so for my data, once I have the different rounds of data added and data is in long format...
baseline=   glmer(falls ~ 1 +            (1             |spid), data=d, family =binomial)
summary(baseline)

#add fixed_accessible home var
fixed_home= glmer(falls ~ 1 + press + (1             |spid), data=d, family =binomial)
summary(fixed_home)

#add fixed + random accessible home var
fixed_home= glmer(falls ~ 1 + press + (1 + press |spid), data=d, family =binomial)
summary(fixed_home)
estimates(fixed_home)
visualize (fixed_home)

model= glmer (falls~press+ frailty +
                (press + frailty| spid),
              data=d,
              family=binomial)
summary(model)
visualize (model, plot="model")

##################
#1/7/23 following along with https://www.alexcernat.com/estimating-multilevel-models-for-change-in-r/

ggplot(d, aes(round, frailty, group = spid)) +
  geom_line(alpha = 0.01) + # add individual line with transparency
  stat_summary( # add average line
    aes(group = 1),
    fun = mean,
    geom = "line",
    size = 1.5,
    color = "red"
  ) +
  theme_bw() + # nice theme
  labs(x = "Round", y = "Frailty") # nice labels

ggplot(d, aes(round, press, group = spid)) +
  geom_line(alpha = 0.01) + # add individual line with transparency
  stat_summary( # add average line
    aes(group = 1),
    fun = mean,
    geom = "line",
    size = 1.5,
    color = "red"
  ) +
  theme_bw() + # nice theme
  labs(x = "Round", y = "Press") # nice labels

ggplot(d, aes(round, falls, group = spid)) +
  geom_line(alpha = 0.01) + # add individual line with transparency
  stat_summary( # add average line
    aes(group = 1),
    fun = mean,
    geom = "line",
    size = 1.5,
    color = "red"
  ) +
  theme_bw() + # nice theme
  labs(x = "Round", y = "Falls") # nice labels


# unconditional means model (a.k.a. random effects model)
m0 <- glmer(data = d, falls ~ 1 + (1 | spid), family =binomial)
# check results
summary(m0)

# let's look at prediction base on this model
d$pred_m0 <- predict(m0)
d %>% 
  filter(spid < 10000019) %>% # select just five individuals
  ggplot(aes(round, pred_m0, color = spid)) +
  geom_point(aes(round, falls)) + # points for observer FALLS
  geom_smooth(method = lm, se = FALSE) + # linear line based on prediction
  theme_bw() + # nice theme
  labs(x = "Round", y = "Falls") + # nice labels
  theme(legend.position = "none") # hide legend

# create new variable starting from 0
d <- mutate(d, round0 = round - 1)
# see how it looks like
head(d, n = 10)

# unconditional change model (a.k.a. MLMC)
m1 <- glmer(data = d, falls ~ 1 + round0 + (1 | spid), family =binomial)
summary(m1)

# let's look at prediction base on this model
d$pred_m1 <- predict(m1)
d %>% 
  filter(spid <10000019) %>% # select just five individuals
  ggplot(aes(round, pred_m1, color = spid)) +
  geom_point(aes(round, falls)) + # points for observer
  geom_smooth(method = lm, se = FALSE) + # linear line based on prediction
  theme_bw() + # nice theme
  labs(x = "Round", y = "Falls") + # nice labels
  theme(legend.position = "none") # hide legend

# unconditional change model (a.k.a. MLMC) with re for change
m2 <- glmer(data = d, falls ~ 1 + round0 + (1 + round0 | spid), family =binomial)
summary(m2)

# let's look at prediction base on this model
d$pred_m2 <- predict(m2)
d %>% 
  filter(spid <10000019) %>% # select just two individuals
  ggplot(aes(round, pred_m2, color = spid)) +
  geom_point(aes(round, falls)) + # points for observer logincome
  geom_smooth(method = lm, se = FALSE) + # linear line based on prediction
  theme_bw() + # nice theme
  labs(x = "Round", y = "Falls") + # nice labels
  theme(legend.position = "none") # hide legend

#from https://quantdev.ssri.psu.edu/tutorials/growth-modeling-chapter-13-modeling-change-latent-entities
m1 = glmer(falls ~ 1 + round + (1 + round|spid), 
                 data = d, 
                 family = 'binomial')

summary(m1)
