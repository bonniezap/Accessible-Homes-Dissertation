
#After initial build of all vars from all files merged in long format, begin to prepare analytic datset. 
#Next tasks:
#make sure pfp is set to an identifyable other/misssing for round 10 
#TO DO NEXT 
#delete cases where anything that constructs our working variables is missing, so not set to 0 by accident for PFP, acc, or falls.

#done - set missing ot 0. - deal with deathage because missing is dropping all from listwise deletion. 
#5/22/23
#1. Look at the data across all waves and get a sense of it, do logic and missingness checks.
#2. Check final directionality of frailty and acc 
#3. Consider weighting for demographic reporting.
#4. Consider exit interviews for death data of those no longer in study
#5. Build tidy analytic dataset
#6. Check normality
#7. make transformations
#8. mean-center or do that in analysis steps(does r do this automatically in the analytic step?) Keep original variables for demographics. 
# Decision: 2020 does not have complete frailty var information due to Covid, so only use 2020 as outcome year, or drop completely due to COVID factors
library (tidyverse)
library (frequency)

#useful function to show missing values in table
tabled = function (..., useNA = 'ifany') base::table(..., useNA = useNA)

rawa <- read.csv("clean_data/allrounds.csv") 

options(frequency_open_output = TRUE)
freqrawa<- read.csv("clean_data/allrounds.csv")  %>%
  freq(
    file=NULL,
    maxrow=30)


tabled (rawa$educ, rawa$round)
tabled (rawa$pfp, rawa$round)
tabled (rawa$accburden, rawa$round)
tabled (rawa$mobhelp, rawa$round)
tabled (rawa$falls, rawa$round)
tabled (rawa$inbldgst, rawa$round)
tabled (rawa$cane, rawa$round)
tabled (rawa$balance, rawa$round)
tabled (rawa$ownrent, rawa$round)
tabled (rawa$worryfall, rawa$round)
tabled (rawa$breathinglimits, rawa$round)


#coding out some logic in missing data
#if cane or walker does not equal yes, then it equals no. 
mida<-rawa %>% 
 mutate(
   cane=case_when ( #they need to have a walker to say yes to this, so never if no answer. 
     cane==1~1,
     cane==2~2,
     cane==3~3,
     cane==4~4,
     TRUE~5),
  walker=case_when ( #they need to have a walker to say yes to this, so never if no answer. 
    walker==1~1,
    walker==2~2,
    walker==3~3,
    walker==4~4,
    TRUE~5),
  mobhelp=case_when ( #set default to needs help because they are only asked if they don't have a helper living in the house, so therefore they have help in the home. 
    mobhelp==1~1,
    mobhelp==2~2,
    TRUE~1),
  ownrent=case_when( #set default to no, because person is first asked if have thing (pain, breathing problem, strength) and if there is this, then further question
    ownrent==1~1,
    ownrent==2~2,
    ownrent==3~2,
    TRUE~NA_real_),
  painlimits=case_when ( #set default to no, because person is first asked if have thing (pain, breathing problem, strength) and if there is this, then further question
    painlimits==1~1,
    painlimits==2~2,
    TRUE~2),
  breathinglimits=case_when ( #set default to no, because person is first asked if have thing (pain, breathing problem, strength) and if there is this, then further question
    breathinglimits==1~1,
    breathinglimits==2~2,
    TRUE~2),
  upperstrlimits=case_when( #set default to no, because person is first asked if have thing (pain, breathing problem, strength) and if there is this, then further question
    upperstrlimits==1~1,
    upperstrlimits==2~2,
    TRUE~2),
  lowerstrimpacts=case_when( #set default to no, because person is first asked if have thing (pain, breathing problem, strength) and if there is this, then further question
    lowerstrimpacts==1~1,
    lowerstrimpacts==2~2,
    TRUE~2)
  ) %>% 
  filter(round!=2020)

#Create tidy dataset
str(mida)

tabled (mida$cane, mida$round)
tabled (mida$walker, mida$round)
tabled (mida$mobhelp, mida$round)
tabled (mida$educ, mida$round)
tabled (mida$pfp, mida$round)
tabled (mida$accburden, mida$round)
tabled (mida$falls, mida$round)
tabled (mida$inbldgst, mida$round)
tabled (mida$balance, mida$round)
tabled (mida$ownrent, mida$round)
tabled (mida$worryfall, mida$round)
tabled (mida$breathinglimits, mida$round)


############Listwise deletion, if needed
#Then perform listwise deletion to make tidy dataset for analysis
#first count how many rows have missing values in any column
nrow(mida[!complete.cases(mida), ])
#then do a double check to count how many rows do not have missing values in any column
nrow(mida[complete.cases(mida), ])
#then remove all rows that are incomplete
df3 <- mida[complete.cases(mida), ]
#then save that datset to a .csv
write.csv (mida[complete.cases(mida), ],"clean_data/listwise.csv")
