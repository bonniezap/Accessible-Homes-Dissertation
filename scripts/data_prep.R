#NHATS Building Dissertation analytic file, Fall-Winter 2022-2023
#Bonnie Albright
#Falls with Frailty as predictor and Home accessibility as moderator
###########################################
#This script creates the dataset of dependent, independent, and covariate variables for my dissertation and runs descriptive weighted statistics on them. 
#To complete the project, full dependent, independent, moderator, and covariate variables need to be constructed and transformed before being analyzed in a weighted model.
###########################################
#Next tasks:
#2/4/23 Crosswalking task steps:
#1. Identify all variables needed for INDEPendent, DEPendent, first round of COVariates, and WEIGHTING
#2. Make spreadsheet of them in excel with columns for each round
#3. Document crosswalk of each variable across each wave
#4. Then read data collection notes for each round and make notes about any of my variables that were effected in each round
#5. As new covariates are used, go through this process for each new variable

#2/4/23 Get official permission from NHATS to use birth year and other sensitive data in dissertation. Ask Jeff on Tuesday about it
#Find people with NO RACE And see if I can fill it in from sensitive data.

#do health, wealth, dementia, and home caregivers altogether across rounds.  
#add time invarying characteristics: genderOK raceOK, education (emailed Changmin 1/8, I can't find it.)
#add time varying characteristics: birthyrOK, wealth, health status, dementia status (once =yes, make following dementia status =yes), anyone else living in home? Is this alternate explanation?
#check crosswalk of all my vars across rounds in NHATS to see if changes in definition or other changes
#read all readme files for all rounds of NHATS to make sure there are no weird situations in the data


#Open issues:
###REVIEW THIS CHOICE: 1/2/2023 made all scales start at 1 (rather than 0 for frailty and press) so that there are no zero values in data. May MEAN CENTER these. 
#2020 has no observed values for grip strength and speed, so can't do full frailty measure. However, made 3-category pfp_3_2020, which yields frailty_3_2020 and competence_3_2020
#Can use falls outcome var, as well as accessible housing in 2020, so frailty isn't necessary as causal predictor
#look at missing values in falls variable
#work with NAs in construct of accessibility burden inventory ##1404 inapplicable in fl1onefloor, so this makes it difficult to calculate. Look into this.  - looked at this, use raw vars not fl1floor flag because it isn't in all rounds. 
#check bathroom amenities - does everyone have them? - yes, don't use, no variation in dataset so no analysis possible
#Once all necessary variables are coded up then I can compare my sample against the whole dataset, and then run listwise deletion
#Run 3 crosstabs 2x2. Look at distribution of cases and percentages. - Frailty by acc, acc by falls, falls by frailty
#continuous CFS could be a sensitivity analysis, or could be necessary as "x axis" in multilevel modeling.

#Decisions:
#This analysis looks at people's frailty and home accessibility levels in 2011 and their falls in the last year in 2012. 
#People who are in an institution in 2011 are excluded because they weren't asked some questions that contribute to PFP (exhausted, weak)  
#Due to sample structure, I may need to exclude people who were dead in 2012. However, if I can find out if they fell before they died this would be useful. or maybe I can find out if they were hospitalized due to a fall before dying. 
#People who are in NHs don't get asked frailty qs, so they are automatically excluded from this analysis using listwise deletion
# percent of frailty in Bandeen-Roche against my numbers of frail2011 - reasonably close given slight differences in calculations when weighted

##############
#libraries
library (tidyverse)
library (haven)
library (survey)
library(psych)
library (summarytools)
library (naniar)
library (performance)

#useful function to show missing values in table
tabled = function (..., useNA = 'ifany') base::table(..., useNA = useNA)

#################################################################################
####################ROUND 1######################################################
#################################################################################
##Get permanent demogs from sensitive data file, round 1 and merge to round 1 public data
#Get race of people who chose other and have data for major other vars
a<-read_sas("~/Documents/NHATS/NHATS Sensitive Data/Sensitive Round 1/NHATS_Round_1_SP_Sen_Dem_Files_SAS/NHATS_Round_1_SP_Sen_Dem_File.sas7bdat", NULL) %>% 
filter (spid %in% c('10000516',
'10001716'
,'10003601'
,'10003732'
,'10004210'
,'10004384'
,'10004390'
,'10006535'
,'10006951'
,'10007002'
,'10008411'
,'10008788'
,'10009949'
,'10011381'
,'10011684'
,'10012303'))
##CONFIRMED THAT ALL OF THESE PEOPLE REFUSED RACE QUESTION, but they do have frailty and accessibility data. "16 people were not included because they refused to answer race demographic question"
#NO RACE indicated except indicated that they are “NOT HISPANIC”: 10004384, 10004390, 10008411, 10009949, 10012303

round1 <- 
  merge (read_sas("~/Documents/NHATS/NHATS Sensitive Data/Sensitive Round 1/NHATS_Round_1_SP_Sen_Dem_Files_SAS/NHATS_Round_1_SP_Sen_Dem_File.sas7bdat", NULL) %>% 
  select(spid,r1dbirthyr) %>% mutate(birthyr=r1dbirthyr),
(read_sas("~/Documents/NHATS/NHATS Public Data/Public Round 1/NHATS_R1_Final_Release_SAS_V3/NHATS_Round_1_SP_File.sas7bdat", NULL)) %>% 
    select(spid 
#Do Health, Dementia, in-home care, and wealth altogether for each round to make process more efficient. 
           ,hc1health #overall health
              #1 EXCELLENT
              #2 VERY GOOD
              #3 GOOD
              #4 FAIR
              #5 POOR
           ,hc1disescn9 #Dementia Has dementia or alzheimer's; 
              #1 YES
              #2 NO
              #7 PREVIOUSLY REPORTED
          ,mo1insdhlp #got help inside (CHECK THIS VAR, maybe use other)
              #1 YES
              #2 NO
#wealth goes in here next, or some measure of economic hardship. Or use sensitive data if it is in there 
           ,r1dgender
           ,rl1dracehisp
           ,wa1dwlkadm   #if =1 Include if respondent was eligible for walk test, it was administered, and a score was recorded 
           ,gr1dgripadm   #if =1 Include if respondent was eligible for grip strength test, it was administered, and a score was recorded 
           ,hw1currweigh #current weight in pounds
           ,hw1howtallft #current height in feet
           ,hw1howtallin #current height inches (make total height as feet plus inches
           ,gr1grp1rdng  #grip reading trial 1
           ,gr1grp2rdng  #grip reading trial 2, take higher of 1 or 2
           ,wa1wlkc1secs #seconds to walk trial 1
           ,wa1wlk1hndr  #hundredths of a second to walk trial 1
           ,wa1wlkc2secs #seconds to walk trial 2
           ,wa1wlk2hndr  #hundredths of a second to walk trial 2
           ,ss1lowenergy #low energy in last month 1=yes 2=no
           ,ss1loenlmtat #low energy limited activity 1=yes
           ,pa1evrgowalk #ever walk for exercise 2=no
           ,pa1vigoractv #ever do vigorous activity 2=no
           ,rl1dracehisp #race/ethnicity
           ,hc1faleninyr #falls in last year (not including last year, need hcxfllsinmth to calculate)
           ,hc1fllsinmth #falls in last month
           ,hc1multifall #multiple falls
           ,fl1onefloor  #flag if one floor living
           ,hc1worryfall #in last month did you worry about falling down 1=yes 2=n0
           ,hc1worrylimt #diin this last month did a fall worry limit your activities 1=yes 2=n0
           ,ho1entrstair #(1=has stairs, 2=no stairs NOTE: Stairs bad here, so AMENITY COUNT IS IF THIS IS 2!)
           ,ho1entrnramp #has ramp, 1=yes, 2=no 
           ,ho1bldgamen1 #has elevator, 1=yes, 2=no
           ,ho1bldgamen2 #has STAIR LIFT, 1=yes, 2=no
           ,fl1bathgrbbr
           ,fl1tltgrbbr
           ,fl1bathseat
           ,fl1raisedtlt
           ,ho1homeamen1:ho1homeamen3 #home amenities for accessibility
           ,ho1bathamen1:ho1bathamen7 #home bathroom amenities for accessibility
           ,em1modhere1 #if value is 2, then added at least one environmental modification (ramp, elev, stair lift, bath mod) in last year if 
           ,em1addlstyr1:em1addlstyr7 #recent stair avoidance or bathroom modifications added (in past year)
           ,hc1disescn9 #dementia or AD
           ,hw1lst10pnds #lost 10 pounds in last year 1=yes
           ,hw1trytolose #trying to lose weight 1=yes
#          ,w1varunit
#          ,w1varstrat
#          ,w1anfinwgt0
           ) %>% 
  na_if (-9) %>% 
  na_if (-8) %>% 
  na_if (-7) %>% 
  na_if (-1) %>% 
    mutate(
#prepare permanent demogs from round 1, gender and race/eth
        female=case_when( #recodes 0 to male and 1 to female
        r1dgender==1~0,
        r1dgender==2~1,
        TRUE~NA_real_),
  whitenothisp=case_when(  #recodes race to whitenonH or not
    rl1dracehisp==1~1, #whitenonH
    (rl1dracehisp==2 |rl1dracehisp==3 |rl1dracehisp==4 | rl1dracehisp==5)~0, #not White Non Hispanic
    #Race 1=WNH 2=BNH 3=OtherNH 4=Hispanic 5=multiracial 6=DK/RF
    TRUE~NA_real_),
  blacknothisp=case_when(  #recodes race to blacknonH or not
    rl1dracehisp==2~1, #blacknonH
    (rl1dracehisp==1 |rl1dracehisp==3 |rl1dracehisp==4 | rl1dracehisp==5)~0, #not Black Non Hispanic
    #Race 1=WNH 2=BNH 3=OtherNH 4=Hispanic 5=multiracial 6=DK/RF
    TRUE~NA_real_),
  hispanic=case_when(  #recodes race to Hispanic or not
    rl1dracehisp==4~1, #Hispanic
    (rl1dracehisp==1 |rl1dracehisp==2 |rl1dracehisp==3 | rl1dracehisp==5)~0, #not Hispanic
    #Race 1=WNH 2=BNH 3=OtherNH 4=Hispanic 5=multiracial 6=DK/RF
    TRUE~NA_real_),
  otherrace=case_when(  #recodes race to other/multi
    (rl1dracehisp==3 | rl1dracehisp==5)~1, #other/multi
    (rl1dracehisp==1 |rl1dracehisp==2 |rl1dracehisp==4)~0, #not other/multi
    #Race 1=WNH 2=BNH 3=OtherNH 4=Hispanic 5=multiracial 6=DK/RF
    TRUE~NA_real_),
  #make count variable
  count=1,

  #make round year
  round='2011',
  
    #This next section preps variables needed for one or more of the 5 frailty metrics  
      hw1howtallin = replace_na(hw1howtallin, 0), #this replace the 20 people with a height in feet but no inches to inches=0, so 4'(3), 5'(17)
      height2011=hw1howtallft*12+hw1howtallin, #height is needed to calculate BMI and for walking test 
      bmi2011=703*(hw1currweigh/(height2011)^2), #this builds BMI, which is used to create PFP elements grip and shrinking
      
#FIVE FRAILTY ELEMENTS# ######################################
#exhausted
#activity
#weak
#shrink
#slow
#coded following Methods description in Bandeen-Roche 2015, which also coded frailty from NHATS and Frailty Worksheet from Frailty Science (2022)

#ACTIVITY# ######################################12/9/2022 Quality check
#"They met criteria for “low physical activity” if, recently, they never walked for exercise or engaged in vigorous activities." (Bandeen-Roche, 2015)
      activity2011=case_when(
        is.na(pa1evrgowalk) & is.na(pa1vigoractv)~NA_real_,
        pa1evrgowalk==2 & pa1vigoractv==2 ~ 1,
        pa1evrgowalk==1 & pa1vigoractv==1 ~ 0,
        TRUE~0),

#SHRINK# ######################################12/9/2022 quality check
#"Participants met criteria for “shrinking” if they had body mass index (BMI) less than 18.5 kg/m2, based on self-reported height and weight, or reported unintentionally losing 10 or more pounds in the last year." Bandeen-Roche 2015
#"Lost >5% body weight unintentionally in last year, or BMI <18.5kg/m2" frailty science sheet (2022)
     shrink2011=case_when(
         bmi2011<18.5~1, #if bmi is low (under 18.5)
        (hw1lst10pnds==1 & hw1trytolose==2)~1, #if lost 10 lbs but wasn't trying to lose weight - this represents the 5% bodyweight as close as possible. Subsequent rounds could possibly use prior round weight to calculate 5% loss
        (hw1lst10pnds==2 & bmi2011>=18.5)~0,
        (hw1lst10pnds==1 & hw1trytolose==1)~0,
        is.na(bmi2011) & (is.na(hw1lst10pnds) | is.na(hw1trytolose)) ~ NA_real_,
        TRUE~0),
     
#WEAK# ######################################12/9/2022 Quality Check
#About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
#from Frailty Science Definition sheet:
#"Meets criteria for grip strength weakness if:
#Men: ≤29 kg for BMI ≤24, ≤30 kg for BMI 24.1–26, ≤30 kg for BMI 26.1–28, ≤32 kg for BMI >28
#Women: ≤17 kg for BMI ≤23, ≤17.3 kg for BMI 23.1–26, ≤18 kg for BMI 26.1–29, ≤21 kg for BMI >29
grip2011=case_when
(
  gr1grp1rdng>=0 & gr1grp2rdng>=0 ~pmax(gr1grp1rdng,gr1grp2rdng),
  is.na(gr1grp1rdng) & gr1grp2rdng>=0   ~gr1grp2rdng,
  gr1grp1rdng >=0  & is.na(gr1grp2rdng) ~gr1grp1rdng,
  is.na(gr1grp1rdng) & is.na(gr1grp2rdng)  ~NA_real_
),
      weak2011=case_when( 
          is.na(grip2011) & (gr1dgripadm==2 | gr1dgripadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
          is.na(grip2011) & is.na(gr1dgripadm) ~NA_real_, #test not applicable to SP
          gr1dgripadm==2~1, #if not administered due to hand weakness, set to 1
           (female==1 & #FEMALE SETTINGS #using frailty science definition sheet cut points
              (grip2011<=17 |
                (bmi2011<=23 & grip2011<=17) | #UNDERWEIGHT
                ((bmi2011>23 & bmi2011<=26) & grip2011<=17.3) | #NORMAL
                ((bmi2011>26 & bmi2011<=29) & grip2011<=18) |  #OVERWEIGHT
                (bmi2011>29 & grip2011<=21)))~1,        #OBESE
           (female==0 & #MALE SETTINGS
                 (grip2011<=24 |
                   (bmi2011<=24 & grip2011<=29) | #UNDERWEIGHT
                   ((bmi2011>24 & bmi2011<=26) & grip2011<=30) | #NORMAL
                   ((bmi2011>26 & bmi2011<=28) & grip2011<=30) | #OVERWEIGHT
                   (bmi2011>28 & grip2011<=21)))~1, #OBESE
      TRUE~0),

#EXHAUSTED# ######################################12/9/2022 Quality check
#"Participants met criteria for “exhaustion” who reported recently having low energy or being easily exhausted: enough to limit their activities." Bandeen-Roche 2015
exhausted2011=case_when( 
  ss1lowenergy==2~0, #no low energy - code 0
  ss1lowenergy==1 & ss1loenlmtat==2~0, #low energy but not enough to limit activity- also code 0
  ss1loenlmtat==1~1, #low energy is so bad that it limits activity - code 1
  TRUE~NA_real_),

#SLOW# ######################################12/8/2022 quality checked
#From Frailty Science: Frailty Assessment Definition Sheet, However, ms/ was measured over 3 meters because that was tested in the NHATS. 
#"Men ≤0.65m/s for height ≤173 cm (68 inches) ≤0.76m/s for height >173 cm (68 inches)
#Women ≤0.65m/s for height ≤159cm (63 inches) ≤0.76m/s for height >159cm (63 inches)"
#About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
#First create walktime variables, based on the two tests, considering NAs and data errors (missing seconds)
      walktime1=case_when(
        is.na(wa1wlkc1secs) & is.na(wa1wlk1hndr) ~ 0,
        is.na(wa1wlkc1secs) & wa1wlk1hndr>0      ~ 0,
        wa1wlkc1secs>0 & is.na(wa1wlk1hndr)      ~ wa1wlkc1secs,
        wa1wlkc1secs>0 & wa1wlk1hndr>0           ~ (wa1wlkc1secs+ wa1wlk1hndr/100),
        TRUE~0
      ),
        
      walktime2=case_when(
        is.na(wa1wlkc2secs) & is.na(wa1wlk2hndr) ~ 0,
        is.na(wa1wlkc2secs) & wa1wlk2hndr>0      ~ 0,
        wa1wlkc2secs>0 & is.na(wa1wlk2hndr)      ~ wa1wlkc2secs,
        wa1wlkc2secs>0 & wa1wlk2hndr>0           ~ (wa1wlkc2secs+ wa1wlk2hndr/100),
        TRUE~0
      ),
      
#this is giving problems where valid walk scores are not getting speed scores, so this creates an intermediate variable to separate division from pmin
      fasterwalkscore=case_when
        (
          walktime1 !=0 & walktime2 !=0 ~pmin(walktime1,walktime2),
          walktime1 ==0 & walktime2>0   ~walktime2,
          walktime1 >0  & walktime2 ==0 ~walktime1,
          walktime1==0  & walktime2==0  ~NA_real_
        ),
      
      #Create a speed variable which is the faster walk time score converted to meters per second (test is 3 meters long, so divide walktime into 3)
      speed2011=(3/fasterwalkscore), 
      
      #Construct slowness frailty test based on PFP parameters
      slow2011=case_when
       (
         is.na(speed2011) & (wa1dwlkadm==2 | wa1dwlkadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
         is.na(speed2011) & is.na(wa1dwlkadm) ~NA_real_, #test not applicable to SP
        #FEMALE SETTINGS #using frailty science definition sheet cut points
        (female==1 & 
          ((height2011<=63 & speed2011<=.65) | (height2011>63  & speed2011<=.76)))~1, #Not short
        #MALE SETTINGS #using frailty science definition sheet cut points
        (female==0 & 
          ((height2011<=68 & speed2011<=.65) |(height2011>68  & speed2011<=.76)))~1, #not short
        TRUE~0),

    
    #Calculate the raw PFP score based on the 5 frailty test elements
    pfp2011=1+(shrink2011 + weak2011 + exhausted2011 + activity2011 + slow2011), #sum of all 5 elements

    #Construct the independent variable of frailty to create the 3 PFP categories (robust, frail, prefrail) from the 5 frailty tests
    frailty2011=case_when(
      pfp2011==1~1, #'robust'
      (pfp2011==2 | pfp2011==3) ~2, #'prefrail',
      pfp2011>=4~3, #'frail',
      TRUE~NA_real_),

    #Create the dependent variable of recent falls. falls in last month is missing very few cases in 2011, and nobody who has a frailty score has a missing value for falls in last month (yay!) 
    falls2011=case_when((hc1fllsinmth==1 | hc1faleninyr==1 | hc1multifall==1)~1, TRUE~ 0), #This builds FALLS in 2011, to add to Dependent/Outcome variable

    #Next, the moderating variable of home accessibility is prepped
  #first the four places for accessibility burdens are created separately: exterior to the building, in the building but not yet into the home, in the home, and in the bathroom 
    bldgextst2011=case_when( 
      ho1entrstair==2 | is.na(ho1entrstair) ~0, #no stairs - code 0
      ho1entrstair==1 & (ho1entrnramp==1 | is.na(ho1entrnramp))~1, #entry stair and ramp exists
      ho1entrstair==1 & ho1entrnramp==2~2, #entry stair and no ramp exists
      TRUE~NA_real_),

    inbldgst2011=case_when( 
      (ho1bldgamen1==2 & ho1bldgamen2==2)~2, #stairs and no elevator nor chair lift  exists
      (ho1bldgamen1==1 | ho1bldgamen2==1)~1, #stairs and elevator or chair lift exists
      (is.na(ho1bldgamen1) & is.na(ho1bldgamen2)) ~0, #if both NA, then no stairs inside building to get to SP's. home - code 0
      (!(is.na(ho1bldgamen1)) & !(is.na(ho1bldgamen2))) ~1, #if both NOT NA, then stairs inside building to get to SP's. home - code 0
      TRUE~NA_real_),

    inhomest2011=case_when( 
      ho1homeamen1==1 ~0, #living on one floor flagged yes - code 0
      ho1homeamen1!=1 & (ho1homeamen2==1 | ho1homeamen3==1)~1, #not living on one floor and elev or chair lift exists
      ho1homeamen1!=1 & (ho1homeamen2==2 & ho1homeamen3==2) ~2, #not living on one floor and no elev nor chair lift exists
      TRUE~0), #set NAs to 0 because they live in a facility/room where all on one floor or overly accessible
      
#no variability, everybody has bath features below:
    nobathf2011=case_when( 
      (fl1bathgrbbr==1 | fl1tltgrbbr ==1)~0, #| fl1bathseat==1 | fl1raisedtlt==1) ~0, #any bath amenity exists - code 0
      (fl1bathgrbbr==2 & fl1tltgrbbr ==2)~1, # & fl1bathseat==2 & fl1raisedtlt==2)~1, #no bath amenity exists
      TRUE~0),

#create variable that indicates if modifications were recently added, which could confound causation timeline of accessibility and falls
    recentanymod2011=case_when(
      em1modhere1==2~1, #recent modification indicated by "2" in this question
      TRUE~0 #set all else to 0 because recent mods were either not new (1), or they didn't have the feature to begin with
      ),
#recent stair avoidance modification (stair lift, ramp, or elevator)
    recentstructmod2011=case_when(
      (em1addlstyr1==1 | em1addlstyr2==1 | em1addlstyr3==1)~1,
      TRUE~0
    ),

#recent bathroom special feature modification
     recentbathfeat2011=case_when(
      (em1addlstyr4==1 | em1addlstyr5==1 | em1addlstyr6==1 | em1addlstyr7==1)~1,
      TRUE~0
    ),
#sum raw accessibility burden index
    accburden2011=1+(bldgextst2011 + inbldgst2011 + inhomest2011), #sum of all 3 elements

#Construct the independent variable of home accessibility to create the 3 categories (lo, med, high press) from the 5 accburden tests
press2011=case_when(
      accburden2011==1~1, #'low press',
      (accburden2011==2 | accburden2011==3) ~2, #'med press',
      accburden2011>=4~3, #'high press',
  TRUE~NA_real_),

#reverse PFP to make "competence" to align with Lawton scale direction
    competence2011=case_when(
      pfp2011==1 ~6,
      pfp2011==2 ~5,
      pfp2011==3 ~4,
      pfp2011==4 ~3,
      pfp2011==5 ~2,
      pfp2011==6 ~1
    )
    ) #%>% filter (pfp2011>=1) #must have PFP score
) #ends merge

#Create tidy dataset for Tableau heat map, of falls plotted over frailty and acc
vars = c('spid', 'female', 'count', 'round', 'birthyr','whitenothisp', 'blacknothisp', 'hispanic', 'otherrace', 'falls2011', 'competence2011', 'frailty2011', 'pfp2011', 'press2011', 'accburden2011', 'recentstructmod2011')
r1<-round1[vars]
write.csv (r1,"clean_data/forheatmap.csv")
a<-tabled(r1$frailty2011, r1$press2011, r1$falls2011)
#View (a)
write.csv (a,"output/rawfrailtytable.csv")

#Create file for binding rows, so all vars have same names and can be additive to long format file for analysis.
r1same<-r1 %>% rename(
  falls=falls2011,
  competence=competence2011,
  frailty=frailty2011,
  pfp=pfp2011,
  press=press2011,
  accburden=accburden2011) %>% 
  select(spid
         ,round
         ,count
         ,birthyr
         ,falls
         ,pfp
         ,competence
         ,frailty
         ,accburden
         ,press)



############Listwise deletion, if needed (may not need it)
##First set the minimum set of variables needed for analysis
#vars = c('spid','falls2011', 'frailty2011', 'pfp2011', 'press2011', 'accburden2011' )
##then reduce the dataset to that set of variables
#df2<-df1[vars]
##Then perform listwise deletion to make tidy dataset for analysis
##first count how many rows have missing values in any column
#nrow(df2[!complete.cases(df2), ])
##then do a double check to count how many rows do not have missing values in any column
#nrow(df2[complete.cases(df2), ])
##then remove all rows that are incomplete
#df3 <- df2[complete.cases(df2), ]
##then save that datset to a .csv
#write.csv (df3,"~/Google Drive/foo.csv")

#tabled(r1$accburden2011)
#prop.table (tabled(r1$accburden2011))
#prop.table (tabled(r1$press2011))
#prop.table (tabled(r1$pfp2011))
#prop.table (tabled(r1$frailty2011))
#produce raw and weighted demographics on gender and frailty.

##Unweighted crosstabs
# prop.table(table(round1$pfp2011))
# prop.table(tabled(round1$frail2011))
# prop.table(table(round1$pfp2011, round1$r1dgender),2)
# prop.table(table(round1$frail2011, round1$r1dgender),2)
# tabled(round1$frail2011, round1$r1dgender)
# tabled(round1$pfp2011, round1$r1dgender)
# prop.table(table(round1$pfp2011, round1$falls2011),1)
# prop.table(table(round1$frail2011, round1$falls2011),1)
# table (round1$falls2011)
# prop.table(table (round1$falls2011))
# 
# table(round1$frail2011, round1$falls2011)
# tabled(round1$pfp2011, round1$falls2011)
# tabled(round1$hc1fllsinmth)
# tabled(round1$hc1faleninyr)
# tabled(round1$hc1multifall)

#this assigns weight from round 2 to the sample, as per instructions from the NHATS training Spring 2022  
# nhats.dsgn<-svydesign(id=~w1varunit,
#                       strata=~w1varstrat,
#                       weights=~w1anfinwgt0, data = round1, nest=TRUE)
# 
# ##Weighted crosstabs
# prop.table(svytable(~pfp2011,nhats.dsgn))
# prop.table(svytable(~frail2011,nhats.dsgn))
# prop.table(svytable(~pfp2011+r1dgender,nhats.dsgn),2)
# prop.table(svytable(~frail2011+r1dgender,nhats.dsgn),2)


#################################################################################
####################ROUND 2######################################################
#################################################################################
#Jan 2, code rounds 2 and 3 for addition to round 1 for draft MLM analysis
#1st, join gender to the full dataset fot round 2, then make restricted data. 
#preround2<-merge ((r1 %>% select(spid, female)),read_sas("Documents/NHATS/NHATS Public Data/Public Round 2/NHATS_R2_Final_Release_SAS_V3/NHATS_Round_2_SP_File_v2.sas7bdat", NULL))
  
round2 <- merge ((r1 %>% select(spid, female)),read_sas("~/Documents/NHATS/NHATS Public Data/Public Round 2/NHATS_R2_Final_Release_SAS_V3/NHATS_Round_2_SP_File_v2.sas7bdat", NULL)) %>%  
#  read_sas("Documents/NHATS/NHATS Public Data/Public Round 2/NHATS_R2_Final_Release_SAS_V3/NHATS_Round_2_SP_File_v2.sas7bdat", NULL) %>% 
  select(spid
         ,female
         ,wa2dwlkadm   #if =1 Include if respondent was eligible for walk test, it was administered, and a score was recorded 
         ,gr2dgripadm   #if =1 Include if respondent was eligible for grip strength test, it was administered, and a score was recorded 
         ,hw2currweigh #current weight in pounds
         ,hw2howtallft #current height in feet
         ,hw2howtallin #current height inches (make total height as feet plus inches
         ,gr2grp1rdng  #grip reading trial 1
         ,gr2grp2rdng  #grip reading trial 2, take higher of 1 or 2
         ,wa2wlkc1secs #seconds to walk trial 1
         ,wa2wlk1hndr  #hundredths of a second to walk trial 1
         ,wa2wlkc2secs #seconds to walk trial 2
         ,wa2wlk2hndr  #hundredths of a second to walk trial 2
         ,ss2lowenergy #low energy in last month 1=yes 2=no
         ,ss2loenlmtat #low energy limited activity 1=yes
         ,pa2evrgowalk #ever walk for exercise 2=no
         ,pa2vigoractv #ever do vigorous activity 2=no
         ,hc2faleninyr #falls in last year (not including last year, need hcxfllsinmth to calculate)
         ,hc2fllsinmth #falls in last month
         ,hc2multifall #multiple falls
         ,fl2onefloor  #flag if one floor living
         ,hc2worryfall #in last month did you worry about falling down 1=yes 2=n0
         ,hc2worrylimt #diin this last month did a fall worry limit your activities 1=yes 2=n0
         ,ho2entrstair #(1=has stairs, 2=no stairs NOTE: Stairs bad here, so AMENITY COUNT IS IF THIS IS 2!)
         ,ho2entrnramp #has ramp, 1=yes, 2=no 
         ,ho2bldgamen1 #has elevator, 1=yes, 2=no
         ,ho2bldgamen2 #has STAIR LIFT, 1=yes, 2=no
         ,fl2bathgrbbr
         ,fl2tltgrbbr
         ,fl2bathseat
         ,fl2raisedtlt
         ,ho2homeamen1:ho2homeamen3 #home amenities for accessibility
         ,ho2bathamen1:ho2bathamen7 #home bathroom amenities for accessibility
         ,em2modhere1 #if value is 2, then added at least one environmental modification (ramp, elev, stair lift, bath mod) in last year if 
         ,em2addlstyr1:em2addlstyr7 #recent stair avoidance or bathroom modifications added (in past year)
         ,hc2disescn9 #dementia or AD
         ,hw2lst10pnds #lost 10 pounds in last year 1=yes
         ,hw2trytolose #trying to lose weight 1=yes
         ,w2varunit
         ,w2varstrat
         ,W2ANFINWGT0
         ,hc2disescn9 #Dementia Has dementia or alzheimer's =1
         ,hc2health #overall health
  ) %>% 
  na_if (-9) %>% 
  na_if (-8) %>% 
  na_if (-7) %>% 
  na_if (-1) %>% 
  mutate(

    count=1,
    round='2012',
    #This next section preps variables needed for one or more of the 5 frailty metrics  
    hw2howtallin = replace_na(hw2howtallin, 0), #this replace the 20 people with a height in feet but no inches to inches=0, so 4'(3), 5'(17)
    height2012=hw2howtallft*12+hw2howtallin, #height is needed to calculate BMI and for walking test 
    bmi2012=703*(hw2currweigh/(height2012)^2), #this builds BMI, which is used to create PFP elements grip and shrinking
    
    #FIVE FRAILTY ELEMENTS# ######################################
    #exhausted
    #activity
    #weak
    #shrink
    #slow
    #coded following Methods description in Bandeen-Roche 2015, which also coded frailty from NHATS and Frailty Worksheet from Frailty Science (2022)
    
    #ACTIVITY# ######################################
    #"They met criteria for “low physical activity” if, recently, they never walked for exercise or engaged in vigorous activities." (Bandeen-Roche, 2015)
    activity2012=case_when(
      is.na(pa2evrgowalk) & is.na(pa2vigoractv)~NA_real_,
      pa2evrgowalk==2 & pa2vigoractv==2 ~ 1,
      pa2evrgowalk==1 & pa2vigoractv==1 ~ 0,
      TRUE~0),
    
    #SHRINK# ######################################
    #"Participants met criteria for “shrinking” if they had body mass index (BMI) less than 18.5 kg/m2, based on self-reported height and weight, or reported unintentionally losing 10 or more pounds in the last year." Bandeen-Roche 2015
    #"Lost >5% body weight unintentionally in last year, or BMI <18.5kg/m2" frailty science sheet (2022)
    shrink2012=case_when(
      bmi2012<18.5~1, #if bmi is low (under 18.5)
      (hw2lst10pnds==1 & hw2trytolose==2)~1, #if lost 10 lbs but wasn't trying to lose weight - this represents the 5% bodyweight as close as possible. Subsequent rounds could possibly use prior round weight to calculate 5% loss
      (hw2lst10pnds==2 & bmi2012>=18.5)~0,
      (hw2lst10pnds==1 & hw2trytolose==1)~0,
      is.na(bmi2012) & (is.na(hw2lst10pnds) | is.na(hw2trytolose)) ~ NA_real_,
      TRUE~0),
    
    #WEAK# ######################################
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #from Frailty Science Definition sheet:
    #"Meets criteria for grip strength weakness if:
    #Men: ≤29 kg for BMI ≤24, ≤30 kg for BMI 24.1–26, ≤30 kg for BMI 26.1–28, ≤32 kg for BMI >28
    #Women: ≤17 kg for BMI ≤23, ≤17.3 kg for BMI 23.1–26, ≤18 kg for BMI 26.1–29, ≤21 kg for BMI >29
    grip2012=case_when
    (
      gr2grp1rdng>=0 & gr2grp2rdng>=0 ~pmax(gr2grp1rdng,gr2grp2rdng),
      is.na(gr2grp1rdng) & gr2grp2rdng>=0   ~gr2grp2rdng,
      gr2grp1rdng >=0  & is.na(gr2grp2rdng) ~gr2grp1rdng,
      is.na(gr2grp1rdng) & is.na(gr2grp2rdng)  ~NA_real_
    ),
    weak2012=case_when( 
      is.na(grip2012) & (gr2dgripadm==2 | gr2dgripadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(grip2012) & is.na(gr2dgripadm) ~NA_real_, #test not applicable to SP
      gr2dgripadm==2~1, #if not administered due to hand weakness, set to 1
      (female==1 & #FEMALE SETTINGS #using frailty science definition sheet cut points
         (grip2012<=17 |
            (bmi2012<=23 & grip2012<=17) | #UNDERWEIGHT
            ((bmi2012>23 & bmi2012<=26) & grip2012<=17.3) | #NORMAL
            ((bmi2012>26 & bmi2012<=29) & grip2012<=18) |  #OVERWEIGHT
            (bmi2012>29 & grip2012<=21)))~1,        #OBESE
      (female==0 & #MALE SETTINGS
         (grip2012<=24 |
            (bmi2012<=24 & grip2012<=29) | #UNDERWEIGHT
            ((bmi2012>24 & bmi2012<=26) & grip2012<=30) | #NORMAL
            ((bmi2012>26 & bmi2012<=28) & grip2012<=30) | #OVERWEIGHT
            (bmi2012>28 & grip2012<=21)))~1, #OBESE
      TRUE~0),
    
    #EXHAUSTED# ######################################
    #"Participants met criteria for “exhaustion” who reported recently having low energy or being easily exhausted: enough to limit their activities." Bandeen-Roche 2015
    exhausted2012=case_when( 
      ss2lowenergy==2~0, #no low energy - code 0
      ss2lowenergy==1 & ss2loenlmtat==2~0, #low energy but not enough to limit activity- also code 0
      ss2loenlmtat==1~1, #low energy is so bad that it limits activity - code 1
      TRUE~NA_real_),
    
    #SLOW# ######################################
    #From Frailty Science: Frailty Assessment Definition Sheet, However, ms/ was measured over 3 meters because that was tested in the NHATS. 
    #"Men ≤0.65m/s for height ≤173 cm (68 inches) ≤0.76m/s for height >173 cm (68 inches)
    #Women ≤0.65m/s for height ≤159cm (63 inches) ≤0.76m/s for height >159cm (63 inches)"
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #First create walktime variables, based on the two tests, considering NAs and data errors (missing seconds)
    walktime1=case_when(
      is.na(wa2wlkc1secs) & is.na(wa2wlk1hndr) ~ 0,
      is.na(wa2wlkc1secs) & wa2wlk1hndr>0      ~ 0,
      wa2wlkc1secs>0 & is.na(wa2wlk1hndr)      ~ wa2wlkc1secs,
      wa2wlkc1secs>0 & wa2wlk1hndr>0           ~ (wa2wlkc1secs+ wa2wlk1hndr/100),
      TRUE~0
    ),
    
    walktime2=case_when(
      is.na(wa2wlkc2secs) & is.na(wa2wlk2hndr) ~ 0,
      is.na(wa2wlkc2secs) & wa2wlk2hndr>0      ~ 0,
      wa2wlkc2secs>0 & is.na(wa2wlk2hndr)      ~ wa2wlkc2secs,
      wa2wlkc2secs>0 & wa2wlk2hndr>0           ~ (wa2wlkc2secs+ wa2wlk2hndr/100),
      TRUE~0
    ),
    
    #this is giving problems where valid walk scores are not getting speed scores, so this creates an intermediate variable to separate division from pmin
    fasterwalkscore=case_when
    (
      walktime1 !=0 & walktime2 !=0 ~pmin(walktime1,walktime2),
      walktime1 ==0 & walktime2>0   ~walktime2,
      walktime1 >0  & walktime2 ==0 ~walktime1,
      walktime1==0  & walktime2==0  ~NA_real_
    ),
    
    #Create a speed variable which is the faster walk time score converted to meters per second (test is 3 meters long, so divide walktime into 3)
    speed2012=(3/fasterwalkscore), 
    
    #Construct slowness frailty test based on PFP parameters
    slow2012=case_when
    (
      is.na(speed2012) & (wa2dwlkadm==2 | wa2dwlkadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(speed2012) & is.na(wa2dwlkadm) ~NA_real_, #test not applicable to SP
      #FEMALE SETTINGS #using frailty science definition sheet cut points
      (female==1 & 
         ((height2012<=63 & speed2012<=.65) | (height2012>63  & speed2012<=.76)))~1, #Not short
      #MALE SETTINGS #using frailty science definition sheet cut points
      (female==0 & 
         ((height2012<=68 & speed2012<=.65) |(height2012>68  & speed2012<=.76)))~1, #not short
      TRUE~0),
    
    
    #Calculate the raw PFP score based on the 5 frailty test elements
    pfp2012=1+(shrink2012 + weak2012 + exhausted2012 + activity2012 + slow2012), #sum of all 5 elements
    
    #Construct the independent variable of frailty to create the 3 PFP categories (robust, frail, prefrail) from the 5 frailty tests
    frailty2012=case_when(
      pfp2012==1~1, #'robust'
      (pfp2012==2 | pfp2012==3) ~2, #'prefrail',
      pfp2012>=4~3, #'frail',
      TRUE~NA_real_),
    
    #Create the dependent variable of recent falls. falls in last month is missing very few cases in 2012, and nobody who has a frailty score has a missing value for falls in last month (yay!) 
    falls2012=case_when((hc2fllsinmth==1 | hc2faleninyr==1 | hc2multifall==1)~1, TRUE~ 0), #This builds FALLS in 2012, to add to Dependent/Outcome variable
    
    #Next, the moderating variable of home accessibility is prepped
    #first the four places for accessibility burdens are created separately: exterior to the building, in the building but not yet into the home, in the home, and in the bathroom 
    bldgextst2012=case_when( 
      ho2entrstair==2 | is.na(ho2entrstair) ~0, #no stairs - code 0
      ho2entrstair==1 & (ho2entrnramp==1 | is.na(ho2entrnramp))~1, #entry stair and ramp exists
      ho2entrstair==1 & ho2entrnramp==2~2, #entry stair and no ramp exists
      TRUE~NA_real_),
    
    inbldgst2012=case_when( 
      (ho2bldgamen1==2 & ho2bldgamen2==2)~2, #stairs and no elevator nor chair lift  exists
      (ho2bldgamen1==1 | ho2bldgamen2==1)~1, #stairs and elevator or chair lift exists
      (is.na(ho2bldgamen1) & is.na(ho2bldgamen2)) ~0, #if both NA, then no stairs inside building to get to SP's. home - code 0
      (!(is.na(ho2bldgamen1)) & !(is.na(ho2bldgamen2))) ~1, #if both NOT NA, then stairs inside building to get to SP's. home - code 0
      TRUE~NA_real_),
    
    inhomest2012=case_when( 
      ho2homeamen1==1 ~0, #living on one floor flagged yes - code 0
      ho2homeamen1!=1 & (ho2homeamen2==1 | ho2homeamen3==1)~1, #not living on one floor and elev or chair lift exists
      ho2homeamen1!=1 & (ho2homeamen2==2 & ho2homeamen3==2) ~2, #not living on one floor and no elev nor chair lift exists
      TRUE~0), #set NAs to 0 because they live in a facility/room where all on one floor or overly accessible
    
    #no variability, everybody has bath features below:
    nobathf2012=case_when( 
      (fl2bathgrbbr==1 | fl2tltgrbbr ==1)~0, #| fl1bathseat==1 | fl1raisedtlt==1) ~0, #any bath amenity exists - code 0
      (fl2bathgrbbr==2 & fl2tltgrbbr ==2)~1, # & fl1bathseat==2 & fl1raisedtlt==2)~1, #no bath amenity exists
      TRUE~0),
    
    #create variable that indicates if modifications were recently added, which could confound causation timeline of accessibility and falls
    recentanymod2012=case_when(
      em2modhere1==2~1, #recent modification indicated by "2" in this question
      TRUE~0 #set all else to 0 because recent mods were either not new (1), or they didn't have the feature to begin with
    ),
    #recent stair avoidance modification (stair lift, ramp, or elevator)
    recentstructmod2012=case_when(
      (em2addlstyr1==1 | em2addlstyr2==1 | em2addlstyr3==1)~1,
      TRUE~0
    ),
    
    #recent bathroom special feature modification
    recentbathfeat2012=case_when(
      (em2addlstyr4==1 | em2addlstyr5==1 | em2addlstyr6==1 | em2addlstyr7==1)~1,
      TRUE~0
    ),
    
    #sum raw accessibility burden index
    accburden2012=1+(bldgextst2012 + inbldgst2012 + inhomest2012), #sum of all 3 elements
    
    #Construct the independent variable of home accessibility to create the 3 categories (lo, med, high press) from the 5 accburden tests
    press2012=case_when(
      accburden2012==1~1, #'low press',
      (accburden2012==2 | accburden2012==3) ~2, #'med press',
      accburden2012>=4~3, #'high press',
      TRUE~NA_real_),
    
    #reverse PFP to make "competence" to align with Lawton scale direction
    competence2012=case_when(
      pfp2012==1 ~6,
      pfp2012==2 ~5,
      pfp2012==3 ~4,
      pfp2012==4 ~3,
      pfp2012==5 ~2,
      pfp2012==6 ~1
    ))
    #%>% filter (pfp2012>=0) (return to filtering later)

#Create tidy dataset for Tableau heat map, of falls plotted over frailty and acc
vars = c('spid', 'count', 'round', 'falls2012', 'competence2012', 'frailty2012', 'pfp2012', 'press2012', 'accburden2012','recentstructmod2012')
r2<-round2[vars]
#write.csv (r2,"clean_data/r2.csv")


#Create file for binding rows, so all vars have same names and can be additive to long format file for analysis.
r2same<-r2 %>% rename(
  falls=falls2012,
  competence=competence2012,
  frailty=frailty2012,
  pfp=pfp2012,
  press=press2012,
  accburden=accburden2012) %>% 
  select(spid
         ,count
         ,round
         ,falls
         ,pfp
         ,competence
         ,frailty
         ,accburden
         ,press)

#First stack repeat measures with round name but without demographics. 
rounds12<-merge ((r1 %>% select(spid, female, whitenothisp, blacknothisp, hispanic, otherrace)),bind_rows(r1same, r2same))
#write.csv (rounds12,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/rounds12.csv")

#################################################################################
####################ROUND 3######################################################
#################################################################################
round3 <- merge ((r1 %>% select(spid, female)),read_sas("~/Documents/NHATS/NHATS Public Data/Public Round 3/NHATS_R3_Final_Release_SAS_V2/NHATS_Round_3_SP_File.sas7bdat", NULL)) %>%  
  select(spid
         ,female
         ,wa3dwlkadm   #if =1 Include if respondent was eligible for walk test, it was administered, and a score was recorded 
         ,gr3dgripadm   #if =1 Include if respondent was eligible for grip strength test, it was administered, and a score was recorded 
         ,hw3currweigh #current weight in pounds
         ,hw3howtallft #current height in feet
         ,hw3howtallin #current height inches (make total height as feet plus inches
         ,gr3grp1rdng  #grip reading trial 1
         ,gr3grp2rdng  #grip reading trial 2, take higher of 1 or 2
         ,wa3wlkc1secs #seconds to walk trial 1
         ,wa3wlk1hndr  #hundredths of a second to walk trial 1
         ,wa3wlkc2secs #seconds to walk trial 2
         ,wa3wlk2hndr  #hundredths of a second to walk trial 2
         ,ss3lowenergy #low energy in last month 1=yes 2=no
         ,ss3loenlmtat #low energy limited activity 1=yes
         ,pa3evrgowalk #ever walk for exercise 2=no
         ,pa3vigoractv #ever do vigorous activity 2=no
         ,hc3faleninyr #falls in last year (not including last year, need hcxfllsinmth to calculate)
         ,hc3fllsinmth #falls in last month
         ,hc3multifall #multiple falls
#not in round 3         ,fl3onefloor  #flag if one floor living
         ,hc3worryfall #in last month did you worry about falling down 1=yes 2=n0
         ,hc3worrylimt #diin this last month did a fall worry limit your activities 1=yes 2=n0
         ,ho3entrstair #(1=has stairs, 2=no stairs NOTE: Stairs bad here, so AMENITY COUNT IS IF THIS IS 2!)
         ,ho3entrnramp #has ramp, 1=yes, 2=no 
         ,ho3bldgamen1 #has elevator, 1=yes, 2=no
         ,ho3bldgamen2 #has STAIR LIFT, 1=yes, 2=no
         ,fl3bathgrbbr
         ,fl3tltgrbbr
         ,fl3bathseat
         ,fl3raisedtlt
         ,ho3homeamen1:ho3homeamen3 #home amenities for accessibility
         ,ho3bathamen1:ho3bathamen7 #home bathroom amenities for accessibility
         ,em3modhere1 #if value is 2, then added at least one environmental modification (ramp, elev, stair lift, bath mod) in last year if 
         ,em3addlstyr1:em3addlstyr7 #recent stair avoidance or bathroom modifications added (in past year)
         ,hc3disescn9 #dementia or AD
         ,hw3lst10pnds #lost 10 pounds in last year 1=yes
         ,hw3trytolose #trying to lose weight 1=yes
#         ,w3varunit
#         ,w3varstrat
#         ,W3ANFINWGT0
#         ,h3disescn9 #Dementia Has dementia or alzheimer's =1
#         ,h3health #overall health
  ) %>% 
  na_if (-9) %>% 
  na_if (-8) %>% 
  na_if (-7) %>% 
  na_if (-1) %>% 
  mutate(
    count=1,

    round='2013',
    #This next section preps variables needed for one or more of the 5 frailty metrics  
    hw3howtallin = replace_na(hw3howtallin, 0), #this replace the 20 people with a height in feet but no inches to inches=0, so 4'(3), 5'(17)
    height2013=hw3howtallft*12+hw3howtallin, #height is needed to calculate BMI and for walking test 
    bmi2013=703*(hw3currweigh/(height2013)^2), #this builds BMI, which is used to create PFP elements grip and shrinking
    
    #FIVE FRAILTY ELEMENTS# ######################################
    #exhausted
    #activity
    #weak
    #shrink
    #slow
    #coded following Methods description in Bandeen-Roche 2015, which also coded frailty from NHATS and Frailty Worksheet from Frailty Science (2022)
    
    #ACTIVITY# ######################################
    #"They met criteria for “low physical activity” if, recently, they never walked for exercise or engaged in vigorous activities." (Bandeen-Roche, 2015)
    activity2013=case_when(
      is.na(pa3evrgowalk) & is.na(pa3vigoractv)~NA_real_,
      pa3evrgowalk==2 & pa3vigoractv==2 ~ 1,
      pa3evrgowalk==1 & pa3vigoractv==1 ~ 0,
      TRUE~0),
    
    #SHRINK# ######################################
    #"Participants met criteria for “shrinking” if they had body mass index (BMI) less than 18.5 kg/m2, based on self-reported height and weight, or reported unintentionally losing 10 or more pounds in the last year." Bandeen-Roche 2015
    #"Lost >5% body weight unintentionally in last year, or BMI <18.5kg/m2" frailty science sheet (2022)
    shrink2013=case_when(
      bmi2013<18.5~1, #if bmi is low (under 18.5)
      (hw3lst10pnds==1 & hw3trytolose==2)~1, #if lost 10 lbs but wasn't trying to lose weight - this represents the 5% bodyweight as close as possible. Subsequent rounds could possibly use prior round weight to calculate 5% loss
      (hw3lst10pnds==2 & bmi2013>=18.5)~0,
      (hw3lst10pnds==1 & hw3trytolose==1)~0,
      is.na(bmi2013) & (is.na(hw3lst10pnds) | is.na(hw3trytolose)) ~ NA_real_,
      TRUE~0),
    
    #WEAK# ######################################
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #from Frailty Science Definition sheet:
    #"Meets criteria for grip strength weakness if:
    #Men: ≤29 kg for BMI ≤24, ≤30 kg for BMI 24.1–26, ≤30 kg for BMI 26.1–28, ≤32 kg for BMI >28
    #Women: ≤17 kg for BMI ≤23, ≤17.3 kg for BMI 23.1–26, ≤18 kg for BMI 26.1–29, ≤21 kg for BMI >29
    grip2013=case_when
    (
      gr3grp1rdng>=0 & gr3grp2rdng>=0 ~pmax(gr3grp1rdng,gr3grp2rdng),
      is.na(gr3grp1rdng) & gr3grp2rdng>=0   ~gr3grp2rdng,
      gr3grp1rdng >=0  & is.na(gr3grp2rdng) ~gr3grp1rdng,
      is.na(gr3grp1rdng) & is.na(gr3grp2rdng)  ~NA_real_
    ),
    weak2013=case_when( 
      is.na(grip2013) & (gr3dgripadm==2 | gr3dgripadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(grip2013) & is.na(gr3dgripadm) ~NA_real_, #test not applicable to SP
      gr3dgripadm==2~1, #if not administered due to hand weakness, set to 1
      (female==1 & #FEMALE SETTINGS #using frailty science definition sheet cut points
         (grip2013<=17 |
            (bmi2013<=23 & grip2013<=17) | #UNDERWEIGHT
            ((bmi2013>23 & bmi2013<=26) & grip2013<=17.3) | #NORMAL
            ((bmi2013>26 & bmi2013<=29) & grip2013<=18) |  #OVERWEIGHT
            (bmi2013>29 & grip2013<=21)))~1,        #OBESE
      (female==0 & #MALE SETTINGS
         (grip2013<=24 |
            (bmi2013<=24 & grip2013<=29) | #UNDERWEIGHT
            ((bmi2013>24 & bmi2013<=26) & grip2013<=30) | #NORMAL
            ((bmi2013>26 & bmi2013<=28) & grip2013<=30) | #OVERWEIGHT
            (bmi2013>28 & grip2013<=21)))~1, #OBESE
      TRUE~0),
    
    #EXHAUSTED# ######################################
    #"Participants met criteria for “exhaustion” who reported recently having low energy or being easily exhausted: enough to limit their activities." Bandeen-Roche 2015
    exhausted2013=case_when( 
      ss3lowenergy==2~0, #no low energy - code 0
      ss3lowenergy==1 & ss3loenlmtat==2~0, #low energy but not enough to limit activity- also code 0
      ss3loenlmtat==1~1, #low energy is so bad that it limits activity - code 1
      TRUE~NA_real_),
    
    #SLOW# ######################################
    #From Frailty Science: Frailty Assessment Definition Sheet, However, ms/ was measured over 3 meters because that was tested in the NHATS. 
    #"Men ≤0.65m/s for height ≤173 cm (68 inches) ≤0.76m/s for height >173 cm (68 inches)
    #Women ≤0.65m/s for height ≤159cm (63 inches) ≤0.76m/s for height >159cm (63 inches)"
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #First create walktime variables, based on the two tests, considering NAs and data errors (missing seconds)
    walktime1=case_when(
      is.na(wa3wlkc1secs) & is.na(wa3wlk1hndr) ~ 0,
      is.na(wa3wlkc1secs) & wa3wlk1hndr>0      ~ 0,
      wa3wlkc1secs>0 & is.na(wa3wlk1hndr)      ~ wa3wlkc1secs,
      wa3wlkc1secs>0 & wa3wlk1hndr>0           ~ (wa3wlkc1secs+ wa3wlk1hndr/100),
      TRUE~0
    ),
    
    walktime2=case_when(
      is.na(wa3wlkc2secs) & is.na(wa3wlk2hndr) ~ 0,
      is.na(wa3wlkc2secs) & wa3wlk2hndr>0      ~ 0,
      wa3wlkc2secs>0 & is.na(wa3wlk2hndr)      ~ wa3wlkc2secs,
      wa3wlkc2secs>0 & wa3wlk2hndr>0           ~ (wa3wlkc2secs+ wa3wlk2hndr/100),
      TRUE~0
    ),
    
    #this is giving problems where valid walk scores are not getting speed scores, so this creates an intermediate variable to separate division from pmin
    fasterwalkscore=case_when
    (
      walktime1 !=0 & walktime2 !=0 ~pmin(walktime1,walktime2),
      walktime1 ==0 & walktime2>0   ~walktime2,
      walktime1 >0  & walktime2 ==0 ~walktime1,
      walktime1==0  & walktime2==0  ~NA_real_
    ),
    
    #Create a speed variable which is the faster walk time score converted to meters per second (test is 3 meters long, so divide walktime into 3)
    speed2013=(3/fasterwalkscore), 
    
    #Construct slowness frailty test based on PFP parameters
    slow2013=case_when
    (
      is.na(speed2013) & (wa3dwlkadm==2 | wa3dwlkadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(speed2013) & is.na(wa3dwlkadm) ~NA_real_, #test not applicable to SP
      #FEMALE SETTINGS #using frailty science definition sheet cut points
      (female==1 & 
         ((height2013<=63 & speed2013<=.65) | (height2013>63  & speed2013<=.76)))~1, #Not short
      #MALE SETTINGS #using frailty science definition sheet cut points
      (female==0 & 
         ((height2013<=68 & speed2013<=.65) |(height2013>68  & speed2013<=.76)))~1, #not short
      TRUE~0),
    
    
    #Calculate the raw PFP score based on the 5 frailty test elements
    pfp2013=1+(shrink2013 + weak2013 + exhausted2013 + activity2013 + slow2013), #sum of all 5 elements
    
    #Construct the independent variable of frailty to create the 3 PFP categories (robust, frail, prefrail) from the 5 frailty tests
    frailty2013=case_when(
      pfp2013==1~1, #'robust'
      (pfp2013==2 | pfp2013==3) ~2, #'prefrail',
      pfp2013>=4~3, #'frail',
      TRUE~NA_real_),
    
    #Create the dependent variable of recent falls. falls in last month is missing very few cases in 2013, and nobody who has a frailty score has a missing value for falls in last month (yay!) 
    falls2013=case_when((hc3fllsinmth==1 | hc3faleninyr==1 | hc3multifall==1)~1, TRUE~ 0), #This builds FALLS in 2013, to add to Dependent/Outcome variable
    
    #Next, the moderating variable of home accessibility is prepped
    #first the four places for accessibility burdens are created separately: exterior to the building, in the building but not yet into the home, in the home, and in the bathroom 
    bldgextst2013=case_when( 
      ho3entrstair==2 | is.na(ho3entrstair) ~0, #no stairs - code 0
      ho3entrstair==1 & (ho3entrnramp==1 | is.na(ho3entrnramp))~1, #entry stair and ramp exists
      ho3entrstair==1 & ho3entrnramp==2~2, #entry stair and no ramp exists
      TRUE~NA_real_),
    
    inbldgst2013=case_when( 
      (ho3bldgamen1==2 & ho3bldgamen2==2)~2, #stairs and no elevator nor chair lift  exists
      (ho3bldgamen1==1 | ho3bldgamen2==1)~1, #stairs and elevator or chair lift exists
      (is.na(ho3bldgamen1) & is.na(ho3bldgamen2)) ~0, #if both NA, then no stairs inside building to get to SP's. home - code 0
      (!(is.na(ho3bldgamen1)) & !(is.na(ho3bldgamen2))) ~1, #if both NOT NA, then stairs inside building to get to SP's. home - code 0
      TRUE~NA_real_),
    
##############RECODE THIS WITH NEW VARIABLES FOR ROUND 3
    inhomest2013=case_when( 
      ho3homeamen1==1 ~0, #living on one floor flagged yes - code 0
      ho3homeamen1!=1 & (ho3homeamen2==1 | ho3homeamen3==1)~1, #not living on one floor and elev or chair lift exists
      ho3homeamen1!=1 & (ho3homeamen2==2 & ho3homeamen3==2) ~2, #not living on one floor and no elev nor chair lift exists
      TRUE~0), #set NAs to 0 because they live in a facility/room where all on one floor or overly accessible
    
    #no variability, everybody has bath features below:
    nobathf2013=case_when( 
      (fl3bathgrbbr==1 | fl3tltgrbbr ==1)~0, #| fl1bathseat==1 | fl1raisedtlt==1) ~0, #any bath amenity exists - code 0
      (fl3bathgrbbr==2 & fl3tltgrbbr ==2)~1, # & fl1bathseat==2 & fl1raisedtlt==2)~1, #no bath amenity exists
      TRUE~0),
    
    #create variable that indicates if modifications were recently added, which could confound causation timeline of accessibility and falls
    recentanymod2013=case_when(
      em3modhere1==2~1, #recent modification indicated by "2" in this question
      TRUE~0 #set all else to 0 because recent mods were either not new (1), or they didn't have the feature to begin with
    ),
    #recent stair avoidance modification (stair lift, ramp, or elevator)
    recentstructmod2013=case_when(
      (em3addlstyr1==1 | em3addlstyr2==1 | em3addlstyr3==1)~1,
      TRUE~0
    ),
    
    #recent bathroom special feature modification
    recentbathfeat2013=case_when(
      (em3addlstyr4==1 | em3addlstyr5==1 | em3addlstyr6==1 | em3addlstyr7==1)~1,
      TRUE~0
    ),
    
    #sum raw accessibility burden index
    accburden2013=1+(bldgextst2013 + inbldgst2013 + inhomest2013), #sum of all 3 elements
    
    #Construct the independent variable of home accessibility to create the 3 categories (lo, med, high press) from the 5 accburden tests
    press2013=case_when(
      accburden2013==1~1, #'low press',
      (accburden2013==2 | accburden2013==3) ~2, #'med press',
      accburden2013>=4~3, #'high press',
      TRUE~NA_real_),
    
    #reverse PFP to make "competence" to align with Lawton scale direction
    competence2013=case_when(
      pfp2013==1 ~6,
      pfp2013==2 ~5,
      pfp2013==3 ~4,
      pfp2013==4 ~3,
      pfp2013==5 ~2,
      pfp2013==6 ~1
    )
  )# %>% filter (pfp2013>=0) (return to filtering later)

#Create tidy dataset for Tableau heat map, of falls plotted over frailty and acc
vars = c('spid','count', 'round', 'falls2013', 'competence2013', 'frailty2013', 'pfp2013', 'press2013', 'accburden2013','recentstructmod2013', 'hc3worryfall')
r3<-round3[vars]
#write.csv (r3,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/r3.csv")


#Create file for binding rows, so all vars have same names and can be additive to long format file for analysis.
r3same<-r3 %>% rename(
  falls=falls2013,
  competence=competence2013,
  frailty=frailty2013,
  pfp=pfp2013,
  press=press2013,
  accburden=accburden2013) %>% 
  select(spid
         ,count
         ,round
         ,falls
         ,pfp
         ,competence
         ,frailty
         ,accburden
         ,press)

#Stack repeat measures with round name and then add on demographics. 
rounds123<-merge ((r1 %>% select(spid, female, whitenothisp, blacknothisp, hispanic, otherrace)),bind_rows(r1same, r2same, r3same))
#write.csv (rounds123,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/rounds123.csv")

#################################################################################
####################ROUND 4######################################################
#################################################################################
round4 <- merge ((r1 %>% select(spid, female)),read_sas("~/Documents/NHATS/NHATS Public Data/Public Round 4/NHATS_R4_Final_Release_SAS_V2/NHATS_Round_4_SP_File.sas7bdat", NULL)) %>%  
  select(spid
         ,female
         ,wa4dwlkadm   #if =1 Include if respondent was eligible for walk test, it was administered, and a score was recorded 
         ,gr4dgripadm   #if =1 Include if respondent was eligible for grip strength test, it was administered, and a score was recorded 
         ,hw4currweigh #current weight in pounds
         ,hw4howtallft #current height in feet
         ,hw4howtallin #current height inches (make total height as feet plus inches
         ,gr4grp1rdng  #grip reading trial 1
         ,gr4grp2rdng  #grip reading trial 2, take higher of 1 or 2
         ,wa4wlkc1secs #seconds to walk trial 1
         ,wa4wlk1hndr  #hundredths of a second to walk trial 1
         ,wa4wlkc2secs #seconds to walk trial 2
         ,wa4wlk2hndr  #hundredths of a second to walk trial 2
         ,ss4lowenergy #low energy in last month 1=yes 2=no
         ,ss4loenlmtat #low energy limited activity 1=yes
         ,pa4evrgowalk #ever walk for exercise 2=no
         ,pa4vigoractv #ever do vigorous activity 2=no
         ,hc4faleninyr #falls in last year (not including last year, need hcxfllsinmth to calculate)
         ,hc4fllsinmth #falls in last month
         ,hc4multifall #multiple falls
         ,hc4worryfall #in last month did you worry about falling down 1=yes 2=n0
         ,hc4worrylimt #diin this last month did a fall worry limit your activities 1=yes 2=n0
         ,ho4entrstair #(1=has stairs, 2=no stairs NOTE: Stairs bad here, so AMENITY COUNT IS IF THIS IS 2!)
         ,ho4entrnramp #has ramp, 1=yes, 2=no 
         ,ho4bldgamen1 #has elevator, 1=yes, 2=no
         ,ho4bldgamen2 #has STAIR LIFT, 1=yes, 2=no
         ,fl4bathgrbbr
         ,fl4tltgrbbr
         ,fl4bathseat
         ,fl4raisedtlt
         ,ho4homeamen1:ho4homeamen3 #home amenities for accessibility
         ,ho4bathamen1:ho4bathamen7 #home bathroom amenities for accessibility
         ,em4modhere1 #if value is 2, then added at least one environmental modification (ramp, elev, stair lift, bath mod) in last year if 
         ,em4addlstyr1:em4addlstyr7 #recent stair avoidance or bathroom modifications added (in past year)
         ,hc4disescn9 #dementia or AD
         ,hw4lst10pnds #lost 10 pounds in last year 1=yes
         ,hw4trytolose #trying to lose weight 1=yes
  ) %>% 
  na_if (-9) %>% 
  na_if (-8) %>% 
  na_if (-7) %>% 
  na_if (-1) %>% 
  mutate(
    count=1,
    round='2014',

    #This next section preps variables needed for one or more of the 5 frailty metrics  
    hw4howtallin = replace_na(hw4howtallin, 0), #this replace the 20 people with a height in feet but no inches to inches=0, so 4'(3), 5'(17)
    height2014=hw4howtallft*12+hw4howtallin, #height is needed to calculate BMI and for walking test 
    bmi2014=703*(hw4currweigh/(height2014)^2), #this builds BMI, which is used to create PFP elements grip and shrinking
    
    #FIVE FRAILTY ELEMENTS# ######################################
    #exhausted
    #activity
    #weak
    #shrink
    #slow
    #coded following Methods description in Bandeen-Roche 2015, which also coded frailty from NHATS and Frailty Worksheet from Frailty Science (2022)
    
    #ACTIVITY# ######################################
    #"They met criteria for “low physical activity” if, recently, they never walked for exercise or engaged in vigorous activities." (Bandeen-Roche, 2015)
    activity2014=case_when(
      is.na(pa4evrgowalk) & is.na(pa4vigoractv)~NA_real_,
      pa4evrgowalk==2 & pa4vigoractv==2 ~ 1,
      pa4evrgowalk==1 & pa4vigoractv==1 ~ 0,
      TRUE~0),
    
    #SHRINK# ######################################
    #"Participants met criteria for “shrinking” if they had body mass index (BMI) less than 18.5 kg/m2, based on self-reported height and weight, or reported unintentionally losing 10 or more pounds in the last year." Bandeen-Roche 2015
    #"Lost >5% body weight unintentionally in last year, or BMI <18.5kg/m2" frailty science sheet (2022)
    shrink2014=case_when(
      bmi2014<18.5~1, #if bmi is low (under 18.5)
      (hw4lst10pnds==1 & hw4trytolose==2)~1, #if lost 10 lbs but wasn't trying to lose weight - this represents the 5% bodyweight as close as possible. Subsequent rounds could possibly use prior round weight to calculate 5% loss
      (hw4lst10pnds==2 & bmi2014>=18.5)~0,
      (hw4lst10pnds==1 & hw4trytolose==1)~0,
      is.na(bmi2014) & (is.na(hw4lst10pnds) | is.na(hw4trytolose)) ~ NA_real_,
      TRUE~0),
    
    #WEAK# ######################################
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #from Frailty Science Definition sheet:
    #"Meets criteria for grip strength weakness if:
    #Men: ≤29 kg for BMI ≤24, ≤30 kg for BMI 24.1–26, ≤30 kg for BMI 26.1–28, ≤32 kg for BMI >28
    #Women: ≤17 kg for BMI ≤23, ≤17.3 kg for BMI 23.1–26, ≤18 kg for BMI 26.1–29, ≤21 kg for BMI >29
    grip2014=case_when
    (
      gr4grp1rdng>=0 & gr4grp2rdng>=0 ~pmax(gr4grp1rdng,gr4grp2rdng),
      is.na(gr4grp1rdng) & gr4grp2rdng>=0   ~gr4grp2rdng,
      gr4grp1rdng >=0  & is.na(gr4grp2rdng) ~gr4grp1rdng,
      is.na(gr4grp1rdng) & is.na(gr4grp2rdng)  ~NA_real_
    ),
    weak2014=case_when( 
      is.na(grip2014) & (gr4dgripadm==2 | gr4dgripadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(grip2014) & is.na(gr4dgripadm) ~NA_real_, #test not applicable to SP
      gr4dgripadm==2~1, #if not administered due to hand weakness, set to 1
      (female==1 & #FEMALE SETTINGS #using frailty science definition sheet cut points
         (grip2014<=17 |
            (bmi2014<=23 & grip2014<=17) | #UNDERWEIGHT
            ((bmi2014>23 & bmi2014<=26) & grip2014<=17.3) | #NORMAL
            ((bmi2014>26 & bmi2014<=29) & grip2014<=18) |  #OVERWEIGHT
            (bmi2014>29 & grip2014<=21)))~1,        #OBESE
      (female==0 & #MALE SETTINGS
         (grip2014<=24 |
            (bmi2014<=24 & grip2014<=29) | #UNDERWEIGHT
            ((bmi2014>24 & bmi2014<=26) & grip2014<=30) | #NORMAL
            ((bmi2014>26 & bmi2014<=28) & grip2014<=30) | #OVERWEIGHT
            (bmi2014>28 & grip2014<=21)))~1, #OBESE
      TRUE~0),
    
    #EXHAUSTED# ######################################
    #"Participants met criteria for “exhaustion” who reported recently having low energy or being easily exhausted: enough to limit their activities." Bandeen-Roche 2015
    exhausted2014=case_when( 
      ss4lowenergy==2~0, #no low energy - code 0
      ss4lowenergy==1 & ss4loenlmtat==2~0, #low energy but not enough to limit activity- also code 0
      ss4loenlmtat==1~1, #low energy is so bad that it limits activity - code 1
      TRUE~NA_real_),
    
    #SLOW# ######################################
    #From Frailty Science: Frailty Assessment Definition Sheet, However, ms/ was measured over 3 meters because that was tested in the NHATS. 
    #"Men ≤0.65m/s for height ≤173 cm (68 inches) ≤0.76m/s for height >173 cm (68 inches)
    #Women ≤0.65m/s for height ≤159cm (63 inches) ≤0.76m/s for height >159cm (63 inches)"
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #First create walktime variables, based on the two tests, considering NAs and data errors (missing seconds)
    walktime1=case_when(
      is.na(wa4wlkc1secs) & is.na(wa4wlk1hndr) ~ 0,
      is.na(wa4wlkc1secs) & wa4wlk1hndr>0      ~ 0,
      wa4wlkc1secs>0 & is.na(wa4wlk1hndr)      ~ wa4wlkc1secs,
      wa4wlkc1secs>0 & wa4wlk1hndr>0           ~ (wa4wlkc1secs+ wa4wlk1hndr/100),
      TRUE~0
    ),
    
    walktime2=case_when(
      is.na(wa4wlkc2secs) & is.na(wa4wlk2hndr) ~ 0,
      is.na(wa4wlkc2secs) & wa4wlk2hndr>0      ~ 0,
      wa4wlkc2secs>0 & is.na(wa4wlk2hndr)      ~ wa4wlkc2secs,
      wa4wlkc2secs>0 & wa4wlk2hndr>0           ~ (wa4wlkc2secs+ wa4wlk2hndr/100),
      TRUE~0
    ),
    
    #this is giving problems where valid walk scores are not getting speed scores, so this creates an intermediate variable to separate division from pmin
    fasterwalkscore=case_when
    (
      walktime1 !=0 & walktime2 !=0 ~pmin(walktime1,walktime2),
      walktime1 ==0 & walktime2>0   ~walktime2,
      walktime1 >0  & walktime2 ==0 ~walktime1,
      walktime1==0  & walktime2==0  ~NA_real_
    ),
    
    #Create a speed variable which is the faster walk time score converted to meters per second (test is 3 meters long, so divide walktime into 3)
    speed2014=(3/fasterwalkscore), 
    
    #Construct slowness frailty test based on PFP parameters
    slow2014=case_when
    (
      is.na(speed2014) & (wa4dwlkadm==2 | wa4dwlkadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(speed2014) & is.na(wa4dwlkadm) ~NA_real_, #test not applicable to SP
      #FEMALE SETTINGS #using frailty science definition sheet cut points
      (female==1 & 
         ((height2014<=63 & speed2014<=.65) | (height2014>63  & speed2014<=.76)))~1, #Not short
      #MALE SETTINGS #using frailty science definition sheet cut points
      (female==0 & 
         ((height2014<=68 & speed2014<=.65) |(height2014>68  & speed2014<=.76)))~1, #not short
      TRUE~0),
    
    
    #Calculate the raw PFP score based on the 5 frailty test elements
    pfp2014=1+(shrink2014 + weak2014 + exhausted2014 + activity2014 + slow2014), #sum of all 5 elements
    
    #Construct the independent variable of frailty to create the 3 PFP categories (robust, frail, prefrail) from the 5 frailty tests
    frailty2014=case_when(
      pfp2014==1~1, #'robust'
      (pfp2014==2 | pfp2014==3) ~2, #'prefrail',
      pfp2014>=4~3, #'frail',
      TRUE~NA_real_),
    
    #Create the dependent variable of recent falls. falls in last month is missing very few cases in 2013, and nobody who has a frailty score has a missing value for falls in last month (yay!) 
    falls2014=case_when((hc4fllsinmth==1 | hc4faleninyr==1 | hc4multifall==1)~1, TRUE~ 0), #This builds FALLS in 2013, to add to Dependent/Outcome variable
    
    #Next, the moderating variable of home accessibility is prepped
    #first the four places for accessibility burdens are created separately: exterior to the building, in the building but not yet into the home, in the home, and in the bathroom 
    bldgextst2014=case_when( 
      ho4entrstair==2 | is.na(ho4entrstair) ~0, #no stairs - code 0
      ho4entrstair==1 & (ho4entrnramp==1 | is.na(ho4entrnramp))~1, #entry stair and ramp exists
      ho4entrstair==1 & ho4entrnramp==2~2, #entry stair and no ramp exists
      TRUE~NA_real_),
    
    inbldgst2014=case_when( 
      (ho4bldgamen1==2 & ho4bldgamen2==2)~2, #stairs and no elevator nor chair lift  exists
      (ho4bldgamen1==1 | ho4bldgamen2==1)~1, #stairs and elevator or chair lift exists
      (is.na(ho4bldgamen1) & is.na(ho4bldgamen2)) ~0, #if both NA, then no stairs inside building to get to SP's. home - code 0
      (!(is.na(ho4bldgamen1)) & !(is.na(ho4bldgamen2))) ~1, #if both NOT NA, then stairs inside building to get to SP's. home - code 0
      TRUE~NA_real_),
    
    ##############RECODE THIS WITH NEW VARIABLES FOR ROUND 3
    inhomest2014=case_when( 
      ho4homeamen1==1 ~0, #living on one floor flagged yes - code 0
      ho4homeamen1!=1 & (ho4homeamen2==1 | ho4homeamen3==1)~1, #not living on one floor and elev or chair lift exists
      ho4homeamen1!=1 & (ho4homeamen2==2 & ho4homeamen3==2) ~2, #not living on one floor and no elev nor chair lift exists
      TRUE~0), #set NAs to 0 because they live in a facility/room where all on one floor or overly accessible
    
    #no variability, everybody has bath features below:
    nobathf2014=case_when( 
      (fl4bathgrbbr==1 | fl4tltgrbbr ==1)~0, #| fl1bathseat==1 | fl1raisedtlt==1) ~0, #any bath amenity exists - code 0
      (fl4bathgrbbr==2 & fl4tltgrbbr ==2)~1, # & fl1bathseat==2 & fl1raisedtlt==2)~1, #no bath amenity exists
      TRUE~0),
    
    #create variable that indicates if modifications were recently added, which could confound causation timeline of accessibility and falls
    recentanymod2014=case_when(
      em4modhere1==2~1, #recent modification indicated by "2" in this question
      TRUE~0 #set all else to 0 because recent mods were either not new (1), or they didn't have the feature to begin with
    ),
    #recent stair avoidance modification (stair lift, ramp, or elevator)
    recentstructmod2014=case_when(
      (em4addlstyr1==1 | em4addlstyr2==1 | em4addlstyr3==1)~1,
      TRUE~0
    ),
    
    #recent bathroom special feature modification
    recentbathfeat2014=case_when(
      (em4addlstyr4==1 | em4addlstyr5==1 | em4addlstyr6==1 | em4addlstyr7==1)~1,
      TRUE~0
    ),
    
    #sum raw accessibility burden index
    accburden2014=1+(bldgextst2014 + inbldgst2014 + inhomest2014), #sum of all 3 elements
    
    #Construct the independent variable of home accessibility to create the 3 categories (lo, med, high press) from the 5 accburden tests
    press2014=case_when(
      accburden2014==1~1, #'low press',
      (accburden2014==2 | accburden2014==3) ~2, #'med press',
      accburden2014>=4~3, #'high press',
      TRUE~NA_real_),
    
    #reverse PFP to make "competence" to align with Lawton scale direction
    competence2014=case_when(
      pfp2014==1 ~6,
      pfp2014==2 ~5,
      pfp2014==3 ~4,
      pfp2014==4 ~3,
      pfp2014==5 ~2,
      pfp2014==6 ~1
    )
  )# %>% filter (pfp2014>=0) (return to filtering later)

#Create tidy dataset for Tableau heat map, of falls plotted over frailty and acc
vars = c('spid','count', 'round', 'falls2014', 'competence2014', 'frailty2014', 'pfp2014', 'press2014', 'accburden2014','recentstructmod2014', 'hc4worryfall')
r4<-round4[vars]
#write.csv (r4,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/r4.csv")


#Create file for binding rows, so all vars have same names and can be additive to long format file for analysis.
r4same<-r4 %>% rename(
  falls=falls2014,
  competence=competence2014,
  frailty=frailty2014,
  pfp=pfp2014,
  press=press2014,
  accburden=accburden2014) %>% 
  select(spid
         ,count
         ,round
         ,falls
         ,pfp
         ,competence
         ,frailty
         ,accburden
         ,press)

#Stack repeat measures with round name and then add on demographics. 
rounds1234<-merge ((r1 %>% select(spid, female, whitenothisp, blacknothisp, hispanic, otherrace)),bind_rows(r1same, r2same, r3same, r4same))
#write.csv (rounds1234,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/rounds1234.csv")


#################################################################################
####################ROUND 5######################################################
#################################################################################
round5 <- merge ((r1 %>% select(spid, female)),read_sas("~/Documents/NHATS/NHATS Public Data/Public Round 5/NHATS_R5_Final_Release_SAS_V3/NHATS_Round_5_SP_File_v2.sas7bdat", NULL)) %>%  
  select(spid
         ,female
         ,wa5dwlkadm   #if =1 Include if respondent was eligible for walk test, it was administered, and a score was recorded 
         ,gr5dgripadm   #if =1 Include if respondent was eligible for grip strength test, it was administered, and a score was recorded 
         ,hw5currweigh #current weight in pounds
         ,hw5howtallft #current height in feet
         ,hw5howtallin #current height inches (make total height as feet plus inches
         ,gr5grp1rdng  #grip reading trial 1
         ,gr5grp2rdng  #grip reading trial 2, take higher of 1 or 2
         ,wa5wlkc1secs #seconds to walk trial 1
         ,wa5wlk1hndr  #hundredths of a second to walk trial 1
         ,wa5wlkc2secs #seconds to walk trial 2
         ,wa5wlk2hndr  #hundredths of a second to walk trial 2
         ,ss5lowenergy #low energy in last month 1=yes 2=no
         ,ss5loenlmtat #low energy limited activity 1=yes
         ,pa5evrgowalk #ever walk for exercise 2=no
         ,pa5vigoractv #ever do vigorous activity 2=no
         ,hc5faleninyr #falls in last year (not including last year, need hcxfllsinmth to calculate)
         ,hc5fllsinmth #falls in last month
         ,hc5multifall #multiple falls
         ,hc5worryfall #in last month did you worry about falling down 1=yes 2=n0
         ,hc5worrylimt #diin this last month did a fall worry limit your activities 1=yes 2=n0
         ,ho5entrstair #(1=has stairs, 2=no stairs NOTE: Stairs bad here, so AMENITY COUNT IS IF THIS IS 2!)
         ,ho5entrnramp #has ramp, 1=yes, 2=no 
         ,ho5bldgamen1 #has elevator, 1=yes, 2=no
         ,ho5bldgamen2 #has STAIR LIFT, 1=yes, 2=no
         ,fl5bathgrbbr
         ,fl5tltgrbbr
         ,fl5bathseat
         ,fl5raisedtlt
         ,ho5homeamen1:ho5homeamen3 #home amenities for accessibility
         ,ho5bathamen1:ho5bathamen7 #home bathroom amenities for accessibility
         ,em5modhere1 #if value is 2, then added at least one environmental modification (ramp, elev, stair lift, bath mod) in last year if 
         ,em5addlstyr1:em5addlstyr7 #recent stair avoidance or bathroom modifications added (in past year)
         ,hc5disescn9 #dementia or AD
         ,hw5lst10pnds #lost 10 pounds in last year 1=yes
         ,hw5trytolose #trying to lose weight 1=yes
  ) %>% 
  na_if (-9) %>% 
  na_if (-8) %>% 
  na_if (-7) %>% 
  na_if (-1) %>% 
  mutate(
    count=1,
    round='2015',

    #This next section preps variables needed for one or more of the 5 frailty metrics  
    hw5howtallin = replace_na(hw5howtallin, 0), #this replace the 20 people with a height in feet but no inches to inches=0, so 4'(3), 5'(17)
    height2015=hw5howtallft*12+hw5howtallin, #height is needed to calculate BMI and for walking test 
    bmi2015=703*(hw5currweigh/(height2015)^2), #this builds BMI, which is used to create PFP elements grip and shrinking
    
    #FIVE FRAILTY ELEMENTS# ######################################
    #exhausted
    #activity
    #weak
    #shrink
    #slow
    #coded following Methods description in Bandeen-Roche 2015, which also coded frailty from NHATS and Frailty Worksheet from Frailty Science (2022)
    
    #ACTIVITY# ######################################
    #"They met criteria for “low physical activity” if, recently, they never walked for exercise or engaged in vigorous activities." (Bandeen-Roche, 2015)
    activity2015=case_when(
      is.na(pa5evrgowalk) & is.na(pa5vigoractv)~NA_real_,
      pa5evrgowalk==2 & pa5vigoractv==2 ~ 1,
      pa5evrgowalk==1 & pa5vigoractv==1 ~ 0,
      TRUE~0),
    
    #SHRINK# ######################################
    #"Participants met criteria for “shrinking” if they had body mass index (BMI) less than 18.5 kg/m2, based on self-reported height and weight, or reported unintentionally losing 10 or more pounds in the last year." Bandeen-Roche 2015
    #"Lost >5% body weight unintentionally in last year, or BMI <18.5kg/m2" frailty science sheet (2022)
    shrink2015=case_when(
      bmi2015<18.5~1, #if bmi is low (under 18.5)
      (hw5lst10pnds==1 & hw5trytolose==2)~1, #if lost 10 lbs but wasn't trying to lose weight - this represents the 5% bodyweight as close as possible. Subsequent rounds could possibly use prior round weight to calculate 5% loss
      (hw5lst10pnds==2 & bmi2015>=18.5)~0,
      (hw5lst10pnds==1 & hw5trytolose==1)~0,
      is.na(bmi2015) & (is.na(hw5lst10pnds) | is.na(hw5trytolose)) ~ NA_real_,
      TRUE~0),
    
    #WEAK# ######################################
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #from Frailty Science Definition sheet:
    #"Meets criteria for grip strength weakness if:
    #Men: ≤29 kg for BMI ≤24, ≤30 kg for BMI 24.1–26, ≤30 kg for BMI 26.1–28, ≤32 kg for BMI >28
    #Women: ≤17 kg for BMI ≤23, ≤17.3 kg for BMI 23.1–26, ≤18 kg for BMI 26.1–29, ≤21 kg for BMI >29
    grip2015=case_when
    (
      gr5grp1rdng>=0 & gr5grp2rdng>=0 ~pmax(gr5grp1rdng,gr5grp2rdng),
      is.na(gr5grp1rdng) & gr5grp2rdng>=0   ~gr5grp2rdng,
      gr5grp1rdng >=0  & is.na(gr5grp2rdng) ~gr5grp1rdng,
      is.na(gr5grp1rdng) & is.na(gr5grp2rdng)  ~NA_real_
    ),
    weak2015=case_when( 
      is.na(grip2015) & (gr5dgripadm==2 | gr5dgripadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(grip2015) & is.na(gr5dgripadm) ~NA_real_, #test not applicable to SP
      gr5dgripadm==2~1, #if not administered due to hand weakness, set to 1
      (female==1 & #FEMALE SETTINGS #using frailty science definition sheet cut points
         (grip2015<=17 |
            (bmi2015<=23 & grip2015<=17) | #UNDERWEIGHT
            ((bmi2015>23 & bmi2015<=26) & grip2015<=17.3) | #NORMAL
            ((bmi2015>26 & bmi2015<=29) & grip2015<=18) |  #OVERWEIGHT
            (bmi2015>29 & grip2015<=21)))~1,        #OBESE
      (female==0 & #MALE SETTINGS
         (grip2015<=24 |
            (bmi2015<=24 & grip2015<=29) | #UNDERWEIGHT
            ((bmi2015>24 & bmi2015<=26) & grip2015<=30) | #NORMAL
            ((bmi2015>26 & bmi2015<=28) & grip2015<=30) | #OVERWEIGHT
            (bmi2015>28 & grip2015<=21)))~1, #OBESE
      TRUE~0),
    
    #EXHAUSTED# ######################################
    #"Participants met criteria for “exhaustion” who reported recently having low energy or being easily exhausted: enough to limit their activities." Bandeen-Roche 2015
    exhausted2015=case_when( 
      ss5lowenergy==2~0, #no low energy - code 0
      ss5lowenergy==1 & ss5loenlmtat==2~0, #low energy but not enough to limit activity- also code 0
      ss5loenlmtat==1~1, #low energy is so bad that it limits activity - code 1
      TRUE~NA_real_),
    
    #SLOW# ######################################
    #From Frailty Science: Frailty Assessment Definition Sheet, However, ms/ was measured over 3 meters because that was tested in the NHATS. 
    #"Men ≤0.65m/s for height ≤173 cm (68 inches) ≤0.76m/s for height >173 cm (68 inches)
    #Women ≤0.65m/s for height ≤159cm (63 inches) ≤0.76m/s for height >159cm (63 inches)"
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #First create walktime variables, based on the two tests, considering NAs and data errors (missing seconds)
    walktime1=case_when(
      is.na(wa5wlkc1secs) & is.na(wa5wlk1hndr) ~ 0,
      is.na(wa5wlkc1secs) & wa5wlk1hndr>0      ~ 0,
      wa5wlkc1secs>0 & is.na(wa5wlk1hndr)      ~ wa5wlkc1secs,
      wa5wlkc1secs>0 & wa5wlk1hndr>0           ~ (wa5wlkc1secs+ wa5wlk1hndr/100),
      TRUE~0
    ),
    
    walktime2=case_when(
      is.na(wa5wlkc2secs) & is.na(wa5wlk2hndr) ~ 0,
      is.na(wa5wlkc2secs) & wa5wlk2hndr>0      ~ 0,
      wa5wlkc2secs>0 & is.na(wa5wlk2hndr)      ~ wa5wlkc2secs,
      wa5wlkc2secs>0 & wa5wlk2hndr>0           ~ (wa5wlkc2secs+ wa5wlk2hndr/100),
      TRUE~0
    ),
    
    #this is giving problems where valid walk scores are not getting speed scores, so this creates an intermediate variable to separate division from pmin
    fasterwalkscore=case_when
    (
      walktime1 !=0 & walktime2 !=0 ~pmin(walktime1,walktime2),
      walktime1 ==0 & walktime2>0   ~walktime2,
      walktime1 >0  & walktime2 ==0 ~walktime1,
      walktime1==0  & walktime2==0  ~NA_real_
    ),
    
    #Create a speed variable which is the faster walk time score converted to meters per second (test is 3 meters long, so divide walktime into 3)
    speed2015=(3/fasterwalkscore), 
    
    #Construct slowness frailty test based on PFP parameters
    slow2015=case_when
    (
      is.na(speed2015) & (wa5dwlkadm==2 | wa5dwlkadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(speed2015) & is.na(wa5dwlkadm) ~NA_real_, #test not applicable to SP
      #FEMALE SETTINGS #using frailty science definition sheet cut points
      (female==1 & 
         ((height2015<=63 & speed2015<=.65) | (height2015>63  & speed2015<=.76)))~1, #Not short
      #MALE SETTINGS #using frailty science definition sheet cut points
      (female==0 & 
         ((height2015<=68 & speed2015<=.65) |(height2015>68  & speed2015<=.76)))~1, #not short
      TRUE~0),
    
    
    #Calculate the raw PFP score based on the 5 frailty test elements
    pfp2015=1+(shrink2015 + weak2015 + exhausted2015 + activity2015 + slow2015), #sum of all 5 elements
    
    #Construct the independent variable of frailty to create the 3 PFP categories (robust, frail, prefrail) from the 5 frailty tests
    frailty2015=case_when(
      pfp2015==1~1, #'robust'
      (pfp2015==2 | pfp2015==3) ~2, #'prefrail',
      pfp2015>=4~3, #'frail',
      TRUE~NA_real_),
    
    #Create the dependent variable of recent falls. falls in last month is missing very few cases in 2013, and nobody who has a frailty score has a missing value for falls in last month (yay!) 
    falls2015=case_when((hc5fllsinmth==1 | hc5faleninyr==1 | hc5multifall==1)~1, TRUE~ 0), #This builds FALLS in 2013, to add to Dependent/Outcome variable
    
    #Next, the moderating variable of home accessibility is prepped
    #first the four places for accessibility burdens are created separately: exterior to the building, in the building but not yet into the home, in the home, and in the bathroom 
    bldgextst2015=case_when( 
      ho5entrstair==2 | is.na(ho5entrstair) ~0, #no stairs - code 0
      ho5entrstair==1 & (ho5entrnramp==1 | is.na(ho5entrnramp))~1, #entry stair and ramp exists
      ho5entrstair==1 & ho5entrnramp==2~2, #entry stair and no ramp exists
      TRUE~NA_real_),
    
    inbldgst2015=case_when( 
      (ho5bldgamen1==2 & ho5bldgamen2==2)~2, #stairs and no elevator nor chair lift  exists
      (ho5bldgamen1==1 | ho5bldgamen2==1)~1, #stairs and elevator or chair lift exists
      (is.na(ho5bldgamen1) & is.na(ho5bldgamen2)) ~0, #if both NA, then no stairs inside building to get to SP's. home - code 0
      (!(is.na(ho5bldgamen1)) & !(is.na(ho5bldgamen2))) ~1, #if both NOT NA, then stairs inside building to get to SP's. home - code 0
      TRUE~NA_real_),
    
    ##############RECODE THIS WITH NEW VARIABLES FOR ROUND 3
    inhomest2015=case_when( 
      ho5homeamen1==1 ~0, #living on one floor flagged yes - code 0
      ho5homeamen1!=1 & (ho5homeamen2==1 | ho5homeamen3==1)~1, #not living on one floor and elev or chair lift exists
      ho5homeamen1!=1 & (ho5homeamen2==2 & ho5homeamen3==2) ~2, #not living on one floor and no elev nor chair lift exists
      TRUE~0), #set NAs to 0 because they live in a facility/room where all on one floor or overly accessible
    
    #no variability, everybody has bath features below:
    nobathf2015=case_when( 
      (fl5bathgrbbr==1 | fl5tltgrbbr ==1)~0, #| fl1bathseat==1 | fl1raisedtlt==1) ~0, #any bath amenity exists - code 0
      (fl5bathgrbbr==2 & fl5tltgrbbr ==2)~1, # & fl1bathseat==2 & fl1raisedtlt==2)~1, #no bath amenity exists
      TRUE~0),
    
    #create variable that indicates if modifications were recently added, which could confound causation timeline of accessibility and falls
    recentanymod2015=case_when(
      em5modhere1==2~1, #recent modification indicated by "2" in this question
      TRUE~0 #set all else to 0 because recent mods were either not new (1), or they didn't have the feature to begin with
    ),
    #recent stair avoidance modification (stair lift, ramp, or elevator)
    recentstructmod2015=case_when(
      (em5addlstyr1==1 | em5addlstyr2==1 | em5addlstyr3==1)~1,
      TRUE~0
    ),
    
    #recent bathroom special feature modification
    recentbathfeat2015=case_when(
      (em5addlstyr4==1 | em5addlstyr5==1 | em5addlstyr6==1 | em5addlstyr7==1)~1,
      TRUE~0
    ),
    
    #sum raw accessibility burden index
    accburden2015=1+(bldgextst2015 + inbldgst2015 + inhomest2015), #sum of all 3 elements
    
    #Construct the independent variable of home accessibility to create the 3 categories (lo, med, high press) from the 5 accburden tests
    press2015=case_when(
      accburden2015==1~1, #'low press',
      (accburden2015==2 | accburden2015==3) ~2, #'med press',
      accburden2015>=4~3, #'high press',
      TRUE~NA_real_),
    
    #reverse PFP to make "competence" to align with Lawton scale direction
    competence2015=case_when(
      pfp2015==1 ~6,
      pfp2015==2 ~5,
      pfp2015==3 ~4,
      pfp2015==4 ~3,
      pfp2015==5 ~2,
      pfp2015==6 ~1
    )
  )# %>% filter (pfp2013>=0) (return to filtering later)

#Create tidy dataset for Tableau heat map, of falls plotted over frailty and acc
vars = c('spid','count', 'round', 'falls2015', 'competence2015', 'frailty2015', 'pfp2015', 'press2015', 'accburden2015','recentstructmod2015', 'hc5worryfall')
r5<-round5[vars]
#write.csv (r5,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/r5.csv")


#Create file for binding rows, so all vars have same names and can be additive to long format file for analysis.
r5same<-r5 %>% rename(
  falls=falls2015,
  competence=competence2015,
  frailty=frailty2015,
  pfp=pfp2015,
  press=press2015,
  accburden=accburden2015) %>% 
  select(spid
         ,count
         ,round
         ,falls
         ,pfp
         ,competence
         ,frailty
         ,accburden
         ,press)

#Stack repeat measures with round name and then add on demographics. 
rounds12345<-merge ((r1 %>% select(spid, female, whitenothisp, blacknothisp, hispanic, otherrace)),bind_rows(r1same, r2same, r3same, r4same, r5same))
#write.csv (rounds12345,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/rounds12345.csv")

#################################################################################
####################ROUND 6######################################################
#################################################################################
round6 <- merge ((r1 %>% select(spid, female)),read_sas("~/Documents/NHATS/NHATS Public Data/Public Round 6/NHATS_R6_Final_Release_SAS_V3/NHATS_Round_6_SP_File_V2.sas7bdat", NULL)) %>%  
  select(spid
         ,female
         ,wa6dwlkadm   #if =1 Include if respondent was eligible for walk test, it was administered, and a score was recorded 
         ,gr6dgripadm   #if =1 Include if respondent was eligible for grip strength test, it was administered, and a score was recorded 
         ,hw6currweigh #current weight in pounds
         ,hw6howtallft #current height in feet
         ,hw6howtallin #current height inches (make total height as feet plus inches
         ,gr6grp1rdng  #grip reading trial 1
         ,gr6grp2rdng  #grip reading trial 2, take higher of 1 or 2
         ,wa6wlkc1secs #seconds to walk trial 1
         ,wa6wlk1hndr  #hundredths of a second to walk trial 1
         ,wa6wlkc2secs #seconds to walk trial 2
         ,wa6wlk2hndr  #hundredths of a second to walk trial 2
         ,ss6lowenergy #low energy in last month 1=yes 2=no
         ,ss6loenlmtat #low energy limited activity 1=yes
         ,pa6evrgowalk #ever walk for exercise 2=no
         ,pa6vigoractv #ever do vigorous activity 2=no
         ,hc6faleninyr #falls in last year (not including last year, need hcxfllsinmth to calculate)
         ,hc6fllsinmth #falls in last month
         ,hc6multifall #multiple falls
         ,hc6worryfall #in last month did you worry about falling down 1=yes 2=n0
         ,hc6worrylimt #diin this last month did a fall worry limit your activities 1=yes 2=n0
         ,ho6entrstair #(1=has stairs, 2=no stairs NOTE: Stairs bad here, so AMENITY COUNT IS IF THIS IS 2!)
         ,ho6entrnramp #has ramp, 1=yes, 2=no 
         ,ho6bldgamen1 #has elevator, 1=yes, 2=no
         ,ho6bldgamen2 #has STAIR LIFT, 1=yes, 2=no
         ,fl6bathgrbbr
         ,fl6tltgrbbr
         ,fl6bathseat
         ,fl6raisedtlt
         ,ho6homeamen1:ho6homeamen3 #home amenities for accessibility
         ,ho6bathamen1:ho6bathamen7 #home bathroom amenities for accessibility
         ,em6modhere1 #if value is 2, then added at least one environmental modification (ramp, elev, stair lift, bath mod) in last year if 
         ,em6addlstyr1:em6addlstyr7 #recent stair avoidance or bathroom modifications added (in past year)
         ,hc6disescn9 #dementia or AD
         ,hw6lst10pnds #lost 10 pounds in last year 1=yes
         ,hw6trytolose #trying to lose weight 1=yes
         #         ,w3varunit
         #         ,w3varstrat
         #         ,W3ANFINWGT0
         #         ,h3disescn9 #Dementia Has dementia or alzheimer's =1
         #         ,h3health #overall health
  ) %>% 
  na_if (-9) %>% 
  na_if (-8) %>% 
  na_if (-7) %>% 
  na_if (-1) %>% 
  mutate(
    count=1,
    round='2016',

    #This next section preps variables needed for one or more of the 5 frailty metrics  
    hw6howtallin = replace_na(hw6howtallin, 0), #this replace the 20 people with a height in feet but no inches to inches=0, so 4'(3), 5'(17)
    height2016=hw6howtallft*12+hw6howtallin, #height is needed to calculate BMI and for walking test 
    bmi2016=703*(hw6currweigh/(height2016)^2), #this builds BMI, which is used to create PFP elements grip and shrinking
    
    #FIVE FRAILTY ELEMENTS# ######################################
    #exhausted
    #activity
    #weak
    #shrink
    #slow
    #coded following Methods description in Bandeen-Roche 2015, which also coded frailty from NHATS and Frailty Worksheet from Frailty Science (2022)
    
    #ACTIVITY# ######################################
    #"They met criteria for “low physical activity” if, recently, they never walked for exercise or engaged in vigorous activities." (Bandeen-Roche, 2015)
    activity2016=case_when(
      is.na(pa6evrgowalk) & is.na(pa6vigoractv)~NA_real_,
      pa6evrgowalk==2 & pa6vigoractv==2 ~ 1,
      pa6evrgowalk==1 & pa6vigoractv==1 ~ 0,
      TRUE~0),
    
    #SHRINK# ######################################
    #"Participants met criteria for “shrinking” if they had body mass index (BMI) less than 18.5 kg/m2, based on self-reported height and weight, or reported unintentionally losing 10 or more pounds in the last year." Bandeen-Roche 2015
    #"Lost >5% body weight unintentionally in last year, or BMI <18.5kg/m2" frailty science sheet (2022)
    shrink2016=case_when(
      bmi2016<18.5~1, #if bmi is low (under 18.5)
      (hw6lst10pnds==1 & hw6trytolose==2)~1, #if lost 10 lbs but wasn't trying to lose weight - this represents the 5% bodyweight as close as possible. Subsequent rounds could possibly use prior round weight to calculate 5% loss
      (hw6lst10pnds==2 & bmi2016>=18.5)~0,
      (hw6lst10pnds==1 & hw6trytolose==1)~0,
      is.na(bmi2016) & (is.na(hw6lst10pnds) | is.na(hw6trytolose)) ~ NA_real_,
      TRUE~0),
    
    #WEAK# ######################################
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #from Frailty Science Definition sheet:
    #"Meets criteria for grip strength weakness if:
    #Men: ≤29 kg for BMI ≤24, ≤30 kg for BMI 24.1–26, ≤30 kg for BMI 26.1–28, ≤32 kg for BMI >28
    #Women: ≤17 kg for BMI ≤23, ≤17.3 kg for BMI 23.1–26, ≤18 kg for BMI 26.1–29, ≤21 kg for BMI >29
    grip2016=case_when
    (
      gr6grp1rdng>=0 & gr6grp2rdng>=0 ~pmax(gr6grp1rdng,gr6grp2rdng),
      is.na(gr6grp1rdng) & gr6grp2rdng>=0   ~gr6grp2rdng,
      gr6grp1rdng >=0  & is.na(gr6grp2rdng) ~gr6grp1rdng,
      is.na(gr6grp1rdng) & is.na(gr6grp2rdng)  ~NA_real_
    ),
    weak2016=case_when( 
      is.na(grip2016) & (gr6dgripadm==2 | gr6dgripadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(grip2016) & is.na(gr6dgripadm) ~NA_real_, #test not applicable to SP
      gr6dgripadm==2~1, #if not administered due to hand weakness, set to 1
      (female==1 & #FEMALE SETTINGS #using frailty science definition sheet cut points
         (grip2016<=17 |
            (bmi2016<=23 & grip2016<=17) | #UNDERWEIGHT
            ((bmi2016>23 & bmi2016<=26) & grip2016<=17.3) | #NORMAL
            ((bmi2016>26 & bmi2016<=29) & grip2016<=18) |  #OVERWEIGHT
            (bmi2016>29 & grip2016<=21)))~1,        #OBESE
      (female==0 & #MALE SETTINGS
         (grip2016<=24 |
            (bmi2016<=24 & grip2016<=29) | #UNDERWEIGHT
            ((bmi2016>24 & bmi2016<=26) & grip2016<=30) | #NORMAL
            ((bmi2016>26 & bmi2016<=28) & grip2016<=30) | #OVERWEIGHT
            (bmi2016>28 & grip2016<=21)))~1, #OBESE
      TRUE~0),
    
    #EXHAUSTED# ######################################
    #"Participants met criteria for “exhaustion” who reported recently having low energy or being easily exhausted: enough to limit their activities." Bandeen-Roche 2015
    exhausted2016=case_when( 
      ss6lowenergy==2~0, #no low energy - code 0
      ss6lowenergy==1 & ss6loenlmtat==2~0, #low energy but not enough to limit activity- also code 0
      ss6loenlmtat==1~1, #low energy is so bad that it limits activity - code 1
      TRUE~NA_real_),
    
    #SLOW# ######################################
    #From Frailty Science: Frailty Assessment Definition Sheet, However, ms/ was measured over 3 meters because that was tested in the NHATS. 
    #"Men ≤0.65m/s for height ≤173 cm (68 inches) ≤0.76m/s for height >173 cm (68 inches)
    #Women ≤0.65m/s for height ≤159cm (63 inches) ≤0.76m/s for height >159cm (63 inches)"
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #First create walktime variables, based on the two tests, considering NAs and data errors (missing seconds)
    walktime1=case_when(
      is.na(wa6wlkc1secs) & is.na(wa6wlk1hndr) ~ 0,
      is.na(wa6wlkc1secs) & wa6wlk1hndr>0      ~ 0,
      wa6wlkc1secs>0 & is.na(wa6wlk1hndr)      ~ wa6wlkc1secs,
      wa6wlkc1secs>0 & wa6wlk1hndr>0           ~ (wa6wlkc1secs+ wa6wlk1hndr/100),
      TRUE~0
    ),
    
    walktime2=case_when(
      is.na(wa6wlkc2secs) & is.na(wa6wlk2hndr) ~ 0,
      is.na(wa6wlkc2secs) & wa6wlk2hndr>0      ~ 0,
      wa6wlkc2secs>0 & is.na(wa6wlk2hndr)      ~ wa6wlkc2secs,
      wa6wlkc2secs>0 & wa6wlk2hndr>0           ~ (wa6wlkc2secs+ wa6wlk2hndr/100),
      TRUE~0
    ),
    
    #this is giving problems where valid walk scores are not getting speed scores, so this creates an intermediate variable to separate division from pmin
    fasterwalkscore=case_when
    (
      walktime1 !=0 & walktime2 !=0 ~pmin(walktime1,walktime2),
      walktime1 ==0 & walktime2>0   ~walktime2,
      walktime1 >0  & walktime2 ==0 ~walktime1,
      walktime1==0  & walktime2==0  ~NA_real_
    ),
    
    #Create a speed variable which is the faster walk time score converted to meters per second (test is 3 meters long, so divide walktime into 3)
    speed2016=(3/fasterwalkscore), 
    
    #Construct slowness frailty test based on PFP parameters
    slow2016=case_when
    (
      is.na(speed2016) & (wa6dwlkadm==2 | wa6dwlkadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(speed2016) & is.na(wa6dwlkadm) ~NA_real_, #test not applicable to SP
      #FEMALE SETTINGS #using frailty science definition sheet cut points
      (female==1 & 
         ((height2016<=63 & speed2016<=.65) | (height2016>63  & speed2016<=.76)))~1, #Not short
      #MALE SETTINGS #using frailty science definition sheet cut points
      (female==0 & 
         ((height2016<=68 & speed2016<=.65) |(height2016>68  & speed2016<=.76)))~1, #not short
      TRUE~0),
    
    
    #Calculate the raw PFP score based on the 5 frailty test elements
    pfp2016=1+(shrink2016 + weak2016 + exhausted2016 + activity2016 + slow2016), #sum of all 5 elements
    
    #Construct the independent variable of frailty to create the 3 PFP categories (robust, frail, prefrail) from the 5 frailty tests
    frailty2016=case_when(
      pfp2016==1~1, #'robust'
      (pfp2016==2 | pfp2016==3) ~2, #'prefrail',
      pfp2016>=4~3, #'frail',
      TRUE~NA_real_),
    
    #Create the dependent variable of recent falls. falls in last month is missing very few cases in 2013, and nobody who has a frailty score has a missing value for falls in last month (yay!) 
    falls2016=case_when((hc6fllsinmth==1 | hc6faleninyr==1 | hc6multifall==1)~1, TRUE~ 0), #This builds FALLS in 2013, to add to Dependent/Outcome variable
    
    #Next, the moderating variable of home accessibility is prepped
    #first the four places for accessibility burdens are created separately: exterior to the building, in the building but not yet into the home, in the home, and in the bathroom 
    bldgextst2016=case_when( 
      ho6entrstair==2 | is.na(ho6entrstair) ~0, #no stairs - code 0
      ho6entrstair==1 & (ho6entrnramp==1 | is.na(ho6entrnramp))~1, #entry stair and ramp exists
      ho6entrstair==1 & ho6entrnramp==2~2, #entry stair and no ramp exists
      TRUE~NA_real_),
    
    inbldgst2016=case_when( 
      (ho6bldgamen1==2 & ho6bldgamen2==2)~2, #stairs and no elevator nor chair lift  exists
      (ho6bldgamen1==1 | ho6bldgamen2==1)~1, #stairs and elevator or chair lift exists
      (is.na(ho6bldgamen1) & is.na(ho6bldgamen2)) ~0, #if both NA, then no stairs inside building to get to SP's. home - code 0
      (!(is.na(ho6bldgamen1)) & !(is.na(ho6bldgamen2))) ~1, #if both NOT NA, then stairs inside building to get to SP's. home - code 0
      TRUE~NA_real_),
    
    inhomest2016=case_when( 
      ho6homeamen1==1 ~0, #living on one floor flagged yes - code 0
      ho6homeamen1!=1 & (ho6homeamen2==1 | ho6homeamen3==1)~1, #not living on one floor and elev or chair lift exists
      ho6homeamen1!=1 & (ho6homeamen2==2 & ho6homeamen3==2) ~2, #not living on one floor and no elev nor chair lift exists
      TRUE~0), #set NAs to 0 because they live in a facility/room where all on one floor or overly accessible
    
    #no variability, everybody has bath features below:
    nobathf2016=case_when( 
      (fl6bathgrbbr==1 | fl6tltgrbbr ==1)~0, #| fl1bathseat==1 | fl1raisedtlt==1) ~0, #any bath amenity exists - code 0
      (fl6bathgrbbr==2 & fl6tltgrbbr ==2)~1, # & fl1bathseat==2 & fl1raisedtlt==2)~1, #no bath amenity exists
      TRUE~0),
    
    #create variable that indicates if modifications were recently added, which could confound causation timeline of accessibility and falls
    recentanymod2016=case_when(
      em6modhere1==2~1, #recent modification indicated by "2" in this question
      TRUE~0 #set all else to 0 because recent mods were either not new (1), or they didn't have the feature to begin with
    ),
    #recent stair avoidance modification (stair lift, ramp, or elevator)
    recentstructmod2016=case_when(
      (em6addlstyr1==1 | em6addlstyr2==1 | em6addlstyr3==1)~1,
      TRUE~0
    ),
    
    #recent bathroom special feature modification
    recentbathfeat2016=case_when(
      (em6addlstyr4==1 | em6addlstyr5==1 | em6addlstyr6==1 | em6addlstyr7==1)~1,
      TRUE~0
    ),
    
    #sum raw accessibility burden index
    accburden2016=1+(bldgextst2016 + inbldgst2016 + inhomest2016), #sum of all 3 elements
    
    #Construct the independent variable of home accessibility to create the 3 categories (lo, med, high press) from the 5 accburden tests
    press2016=case_when(
      accburden2016==1~1, #'low press',
      (accburden2016==2 | accburden2016==3) ~2, #'med press',
      accburden2016>=4~3, #'high press',
      TRUE~NA_real_),
    
    #reverse PFP to make "competence" to align with Lawton scale direction
    competence2016=case_when(
      pfp2016==1 ~6,
      pfp2016==2 ~5,
      pfp2016==3 ~4,
      pfp2016==4 ~3,
      pfp2016==5 ~2,
      pfp2016==6 ~1
    )
  )# %>% filter (pfp2013>=0) (return to filtering later)

#Create tidy dataset for Tableau heat map, of falls plotted over frailty and acc
vars = c('spid','count', 'round', 'falls2016', 'competence2016', 'frailty2016', 'pfp2016', 'press2016', 'accburden2016','recentstructmod2016', 'hc6worryfall')
r6<-round6[vars]
#write.csv (r6,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/r6.csv")


#Create file for binding rows, so all vars have same names and can be additive to long format file for analysis.
r6same<-r6 %>% rename(
  falls=falls2016,
  competence=competence2016,
  frailty=frailty2016,
  pfp=pfp2016,
  press=press2016,
  accburden=accburden2016) %>% 
  select(spid
         ,count
         ,round
         ,falls
         ,pfp
         ,competence
         ,frailty
         ,accburden
         ,press)

#Stack repeat measures with round name and then add on demographics. 
rounds123456<-merge ((r1 %>% select(spid, female, whitenothisp, blacknothisp, hispanic, otherrace)),bind_rows(r1same, r2same, r3same, r4same, r5same, r6same))
#write.csv (rounds123456,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/rounds123456.csv")

#################################################################################
####################ROUND 7######################################################
#################################################################################
round7 <- merge ((r1 %>% select(spid, female)),read_sas("~/Documents/NHATS/NHATS Public Data/Public Round 7/NHATS_R7_Final_Release_SAS_V2/NHATS_Round_7_SP_File.sas7bdat", NULL)) %>%  
  select(spid
         ,female
         ,wa7dwlkadm   #if =1 Include if respondent was eligible for walk test, it was administered, and a score was recorded 
         ,gr7dgripadm   #if =1 Include if respondent was eligible for grip strength test, it was administered, and a score was recorded 
         ,hw7currweigh #current weight in pounds
         ,hw7howtallft #current height in feet
         ,hw7howtallin #current height inches (make total height as feet plus inches
         ,gr7grp1rdng  #grip reading trial 1
         ,gr7grp2rdng  #grip reading trial 2, take higher of 1 or 2
         ,wa7wlkc1secs #seconds to walk trial 1
         ,wa7wlk1hndr  #hundredths of a second to walk trial 1
         ,wa7wlkc2secs #seconds to walk trial 2
         ,wa7wlk2hndr  #hundredths of a second to walk trial 2
         ,ss7lowenergy #low energy in last month 1=yes 2=no
         ,ss7loenlmtat #low energy limited activity 1=yes
         ,pa7evrgowalk #ever walk for exercise 2=no
         ,pa7vigoractv #ever do vigorous activity 2=no
         ,hc7faleninyr #falls in last year (not including last year, need hcxfllsinmth to calculate)
         ,hc7fllsinmth #falls in last month
         ,hc7multifall #multiple falls
         ,hc7worryfall #in last month did you worry about falling down 1=yes 2=n0
         ,hc7worrylimt #diin this last month did a fall worry limit your activities 1=yes 2=n0
         ,ho7entrstair #(1=has stairs, 2=no stairs NOTE: Stairs bad here, so AMENITY COUNT IS IF THIS IS 2!)
         ,ho7entrnramp #has ramp, 1=yes, 2=no 
         ,ho7bldgamen1 #has elevator, 1=yes, 2=no
         ,ho7bldgamen2 #has STAIR LIFT, 1=yes, 2=no
         ,fl7bathgrbbr
         ,fl7tltgrbbr
         ,fl7bathseat
         ,fl7raisedtlt
         ,ho7homeamen1:ho7homeamen3 #home amenities for accessibility
         ,ho7bathamen1:ho7bathamen7 #home bathroom amenities for accessibility
         ,em7modhere1 #if value is 2, then added at least one environmental modification (ramp, elev, stair lift, bath mod) in last year if 
         ,em7addlstyr1:em7addlstyr7 #recent stair avoidance or bathroom modifications added (in past year)
         ,hc7disescn9 #dementia or AD
         ,hw7lst10pnds #lost 10 pounds in last year 1=yes
         ,hw7trytolose #trying to lose weight 1=yes
         #         ,w3varunit
         #         ,w3varstrat
         #         ,W3ANFINWGT0
         #         ,h3disescn9 #Dementia Has dementia or alzheimer's =1
         #         ,h3health #overall health
  ) %>% 
  na_if (-9) %>% 
  na_if (-8) %>% 
  na_if (-7) %>% 
  na_if (-1) %>% 
  mutate(
    count=1,
    round='2017',

    #This next section preps variables needed for one or more of the 5 frailty metrics  
    hw7howtallin = replace_na(hw7howtallin, 0), #this replace the 20 people with a height in feet but no inches to inches=0, so 4'(3), 5'(17)
    height2017=hw7howtallft*12+hw7howtallin, #height is needed to calculate BMI and for walking test 
    bmi2017=703*(hw7currweigh/(height2017)^2), #this builds BMI, which is used to create PFP elements grip and shrinking
    
    #FIVE FRAILTY ELEMENTS# ######################################
    #exhausted
    #activity
    #weak
    #shrink
    #slow
    #coded following Methods description in Bandeen-Roche 2015, which also coded frailty from NHATS and Frailty Worksheet from Frailty Science (2022)
    
    #ACTIVITY# ######################################
    #"They met criteria for “low physical activity” if, recently, they never walked for exercise or engaged in vigorous activities." (Bandeen-Roche, 2015)
    activity2017=case_when(
      is.na(pa7evrgowalk) & is.na(pa7vigoractv)~NA_real_,
      pa7evrgowalk==2 & pa7vigoractv==2 ~ 1,
      pa7evrgowalk==1 & pa7vigoractv==1 ~ 0,
      TRUE~0),
    
    #SHRINK# ######################################
    #"Participants met criteria for “shrinking” if they had body mass index (BMI) less than 18.5 kg/m2, based on self-reported height and weight, or reported unintentionally losing 10 or more pounds in the last year." Bandeen-Roche 2015
    #"Lost >5% body weight unintentionally in last year, or BMI <18.5kg/m2" frailty science sheet (2022)
    shrink2017=case_when(
      bmi2017<18.5~1, #if bmi is low (under 18.5)
      (hw7lst10pnds==1 & hw7trytolose==2)~1, #if lost 10 lbs but wasn't trying to lose weight - this represents the 5% bodyweight as close as possible. Subsequent rounds could possibly use prior round weight to calculate 5% loss
      (hw7lst10pnds==2 & bmi2017>=18.5)~0,
      (hw7lst10pnds==1 & hw7trytolose==1)~0,
      is.na(bmi2017) & (is.na(hw7lst10pnds) | is.na(hw7trytolose)) ~ NA_real_,
      TRUE~0),
    
    #WEAK# ######################################
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #from Frailty Science Definition sheet:
    #"Meets criteria for grip strength weakness if:
    #Men: ≤29 kg for BMI ≤24, ≤30 kg for BMI 24.1–26, ≤30 kg for BMI 26.1–28, ≤32 kg for BMI >28
    #Women: ≤17 kg for BMI ≤23, ≤17.3 kg for BMI 23.1–26, ≤18 kg for BMI 26.1–29, ≤21 kg for BMI >29
    grip2017=case_when
    (
      gr7grp1rdng>=0 & gr7grp2rdng>=0 ~pmax(gr7grp1rdng,gr7grp2rdng),
      is.na(gr7grp1rdng) & gr7grp2rdng>=0   ~gr7grp2rdng,
      gr7grp1rdng >=0  & is.na(gr7grp2rdng) ~gr7grp1rdng,
      is.na(gr7grp1rdng) & is.na(gr7grp2rdng)  ~NA_real_
    ),
    weak2017=case_when( 
      is.na(grip2017) & (gr7dgripadm==2 | gr7dgripadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(grip2017) & is.na(gr7dgripadm) ~NA_real_, #test not applicable to SP
      gr7dgripadm==2~1, #if not administered due to hand weakness, set to 1
      (female==1 & #FEMALE SETTINGS #using frailty science definition sheet cut points
         (grip2017<=17 |
            (bmi2017<=23 & grip2017<=17) | #UNDERWEIGHT
            ((bmi2017>23 & bmi2017<=26) & grip2017<=17.3) | #NORMAL
            ((bmi2017>26 & bmi2017<=29) & grip2017<=18) |  #OVERWEIGHT
            (bmi2017>29 & grip2017<=21)))~1,        #OBESE
      (female==0 & #MALE SETTINGS
         (grip2017<=24 |
            (bmi2017<=24 & grip2017<=29) | #UNDERWEIGHT
            ((bmi2017>24 & bmi2017<=26) & grip2017<=30) | #NORMAL
            ((bmi2017>26 & bmi2017<=28) & grip2017<=30) | #OVERWEIGHT
            (bmi2017>28 & grip2017<=21)))~1, #OBESE
      TRUE~0),
    
    #EXHAUSTED# ######################################
    #"Participants met criteria for “exhaustion” who reported recently having low energy or being easily exhausted: enough to limit their activities." Bandeen-Roche 2015
    exhausted2017=case_when( 
      ss7lowenergy==2~0, #no low energy - code 0
      ss7lowenergy==1 & ss7loenlmtat==2~0, #low energy but not enough to limit activity- also code 0
      ss7loenlmtat==1~1, #low energy is so bad that it limits activity - code 1
      TRUE~NA_real_),
    
    #SLOW# ######################################
    #From Frailty Science: Frailty Assessment Definition Sheet, However, ms/ was measured over 3 meters because that was tested in the NHATS. 
    #"Men ≤0.65m/s for height ≤173 cm (68 inches) ≤0.76m/s for height >173 cm (68 inches)
    #Women ≤0.65m/s for height ≤159cm (63 inches) ≤0.76m/s for height >159cm (63 inches)"
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #First create walktime variables, based on the two tests, considering NAs and data errors (missing seconds)
    walktime1=case_when(
      is.na(wa7wlkc1secs) & is.na(wa7wlk1hndr) ~ 0,
      is.na(wa7wlkc1secs) & wa7wlk1hndr>0      ~ 0,
      wa7wlkc1secs>0 & is.na(wa7wlk1hndr)      ~ wa7wlkc1secs,
      wa7wlkc1secs>0 & wa7wlk1hndr>0           ~ (wa7wlkc1secs+ wa7wlk1hndr/100),
      TRUE~0
    ),
    
    walktime2=case_when(
      is.na(wa7wlkc2secs) & is.na(wa7wlk2hndr) ~ 0,
      is.na(wa7wlkc2secs) & wa7wlk2hndr>0      ~ 0,
      wa7wlkc2secs>0 & is.na(wa7wlk2hndr)      ~ wa7wlkc2secs,
      wa7wlkc2secs>0 & wa7wlk2hndr>0           ~ (wa7wlkc2secs+ wa7wlk2hndr/100),
      TRUE~0
    ),
    
    #this is giving problems where valid walk scores are not getting speed scores, so this creates an intermediate variable to separate division from pmin
    fasterwalkscore=case_when
    (
      walktime1 !=0 & walktime2 !=0 ~pmin(walktime1,walktime2),
      walktime1 ==0 & walktime2>0   ~walktime2,
      walktime1 >0  & walktime2 ==0 ~walktime1,
      walktime1==0  & walktime2==0  ~NA_real_
    ),
    
    #Create a speed variable which is the faster walk time score converted to meters per second (test is 3 meters long, so divide walktime into 3)
    speed2017=(3/fasterwalkscore), 
    
    #Construct slowness frailty test based on PFP parameters
    slow2017=case_when
    (
      is.na(speed2017) & (wa7dwlkadm==2 | wa7dwlkadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(speed2017) & is.na(wa7dwlkadm) ~NA_real_, #test not applicable to SP
      #FEMALE SETTINGS #using frailty science definition sheet cut points
      (female==1 & 
         ((height2017<=63 & speed2017<=.65) | (height2017>63  & speed2017<=.76)))~1, #Not short
      #MALE SETTINGS #using frailty science definition sheet cut points
      (female==0 & 
         ((height2017<=68 & speed2017<=.65) |(height2017>68  & speed2017<=.76)))~1, #not short
      TRUE~0),
    
    
    #Calculate the raw PFP score based on the 5 frailty test elements
    pfp2017=1+(shrink2017 + weak2017 + exhausted2017 + activity2017 + slow2017), #sum of all 5 elements
    
    #Construct the independent variable of frailty to create the 3 PFP categories (robust, frail, prefrail) from the 5 frailty tests
    frailty2017=case_when(
      pfp2017==1~1, #'robust'
      (pfp2017==2 | pfp2017==3) ~2, #'prefrail',
      pfp2017>=4~3, #'frail',
      TRUE~NA_real_),
    
    #Create the dependent variable of recent falls. falls in last month is missing very few cases in 2013, and nobody who has a frailty score has a missing value for falls in last month (yay!) 
    falls2017=case_when((hc7fllsinmth==1 | hc7faleninyr==1 | hc7multifall==1)~1, TRUE~ 0), #This builds FALLS in 2013, to add to Dependent/Outcome variable
    
    #Next, the moderating variable of home accessibility is prepped
    #first the four places for accessibility burdens are created separately: exterior to the building, in the building but not yet into the home, in the home, and in the bathroom 
    bldgextst2017=case_when( 
      ho7entrstair==2 | is.na(ho7entrstair) ~0, #no stairs - code 0
      ho7entrstair==1 & (ho7entrnramp==1 | is.na(ho7entrnramp))~1, #entry stair and ramp exists
      ho7entrstair==1 & ho7entrnramp==2~2, #entry stair and no ramp exists
      TRUE~NA_real_),
    
    inbldgst2017=case_when( 
      (ho7bldgamen1==2 & ho7bldgamen2==2)~2, #stairs and no elevator nor chair lift  exists
      (ho7bldgamen1==1 | ho7bldgamen2==1)~1, #stairs and elevator or chair lift exists
      (is.na(ho7bldgamen1) & is.na(ho7bldgamen2)) ~0, #if both NA, then no stairs inside building to get to SP's. home - code 0
      (!(is.na(ho7bldgamen1)) & !(is.na(ho7bldgamen2))) ~1, #if both NOT NA, then stairs inside building to get to SP's. home - code 0
      TRUE~NA_real_),
    
    inhomest2017=case_when( 
      ho7homeamen1==1 ~0, #living on one floor flagged yes - code 0
      ho7homeamen1!=1 & (ho7homeamen2==1 | ho7homeamen3==1)~1, #not living on one floor and elev or chair lift exists
      ho7homeamen1!=1 & (ho7homeamen2==2 & ho7homeamen3==2) ~2, #not living on one floor and no elev nor chair lift exists
      TRUE~0), #set NAs to 0 because they live in a facility/room where all on one floor or overly accessible
    
    #no variability, everybody has bath features below:
    nobathf2017=case_when( 
      (fl7bathgrbbr==1 | fl7tltgrbbr ==1)~0, #| fl1bathseat==1 | fl1raisedtlt==1) ~0, #any bath amenity exists - code 0
      (fl7bathgrbbr==2 & fl7tltgrbbr ==2)~1, # & fl1bathseat==2 & fl1raisedtlt==2)~1, #no bath amenity exists
      TRUE~0),
    
    #create variable that indicates if modifications were recently added, which could confound causation timeline of accessibility and falls
    recentanymod2017=case_when(
      em7modhere1==2~1, #recent modification indicated by "2" in this question
      TRUE~0 #set all else to 0 because recent mods were either not new (1), or they didn't have the feature to begin with
    ),
    #recent stair avoidance modification (stair lift, ramp, or elevator)
    recentstructmod2017=case_when(
      (em7addlstyr1==1 | em7addlstyr2==1 | em7addlstyr3==1)~1,
      TRUE~0
    ),
    
    #recent bathroom special feature modification
    recentbathfeat2017=case_when(
      (em7addlstyr4==1 | em7addlstyr5==1 | em7addlstyr6==1 | em7addlstyr7==1)~1,
      TRUE~0
    ),
    
    #sum raw accessibility burden index
    accburden2017=1+(bldgextst2017 + inbldgst2017 + inhomest2017), #sum of all 3 elements
    
    #Construct the independent variable of home accessibility to create the 3 categories (lo, med, high press) from the 5 accburden tests
    press2017=case_when(
      accburden2017==1~1, #'low press',
      (accburden2017==2 | accburden2017==3) ~2, #'med press',
      accburden2017>=4~3, #'high press',
      TRUE~NA_real_),
    
    #reverse PFP to make "competence" to align with Lawton scale direction
    competence2017=case_when(
      pfp2017==1 ~6,
      pfp2017==2 ~5,
      pfp2017==3 ~4,
      pfp2017==4 ~3,
      pfp2017==5 ~2,
      pfp2017==6 ~1
    )
  )# %>% filter (pfp2013>=0) (return to filtering later)

#Create tidy dataset for Tableau heat map, of falls plotted over frailty and acc
vars = c('spid','count', 'round', 'falls2017', 'competence2017', 'frailty2017', 'pfp2017', 'press2017', 'accburden2017','recentstructmod2017', 'hc7worryfall')
r7<-round7[vars]
#write.csv (r7,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/r7.csv")


#Create file for binding rows, so all vars have same names and can be additive to long format file for analysis.
r7same<-r7 %>% rename(
  falls=falls2017,
  competence=competence2017,
  frailty=frailty2017,
  pfp=pfp2017,
  press=press2017,
  accburden=accburden2017) %>% 
  select(spid
         ,count
         ,round
         ,falls
         ,pfp
         ,competence
         ,frailty
         ,accburden
         ,press)

#Stack repeat measures with round name and then add on demographics. 
rounds1234567<-merge ((r1 %>% select(spid, female, whitenothisp, blacknothisp, hispanic, otherrace)),bind_rows(r1same, r2same, r3same, r4same, r5same, r6same, r7same))
#write.csv (rounds1234567,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/rounds1234567.csv")

#################################################################################
####################ROUND 8######################################################
#################################################################################
round8 <- merge ((r1 %>% select(spid, female)),read_sas("~/Documents/NHATS/NHATS Public Data/Public Round 8/NHATS_R8_Final_Release_SAS_V2/NHATS_Round_8_SP_File.sas7bdat", NULL)) %>%  
  select(spid
         ,female
         ,wa8dwlkadm   #if =1 Include if respondent was eligible for walk test, it was administered, and a score was recorded 
         ,gr8dgripadm   #if =1 Include if respondent was eligible for grip strength test, it was administered, and a score was recorded 
         ,hw8currweigh #current weight in pounds
         ,hw8howtallft #current height in feet
         ,hw8howtallin #current height inches (make total height as feet plus inches
         ,gr8grp1rdng  #grip reading trial 1
         ,gr8grp2rdng  #grip reading trial 2, take higher of 1 or 2
         ,wa8wlkc1secs #seconds to walk trial 1
         ,wa8wlk1hndr  #hundredths of a second to walk trial 1
         ,wa8wlkc2secs #seconds to walk trial 2
         ,wa8wlk2hndr  #hundredths of a second to walk trial 2
         ,ss8lowenergy #low energy in last month 1=yes 2=no
         ,ss8loenlmtat #low energy limited activity 1=yes
         ,pa8evrgowalk #ever walk for exercise 2=no
         ,pa8vigoractv #ever do vigorous activity 2=no
         ,hc8faleninyr #falls in last year (not including last year, need hcxfllsinmth to calculate)
         ,hc8fllsinmth #falls in last month
         ,hc8multifall #multiple falls
         ,hc8worryfall #in last month did you worry about falling down 1=yes 2=n0
         ,hc8worrylimt #diin this last month did a fall worry limit your activities 1=yes 2=n0
         ,ho8entrstair #(1=has stairs, 2=no stairs NOTE: Stairs bad here, so AMENITY COUNT IS IF THIS IS 2!)
         ,ho8entrnramp #has ramp, 1=yes, 2=no 
         ,ho8bldgamen1 #has elevator, 1=yes, 2=no
         ,ho8bldgamen2 #has STAIR LIFT, 1=yes, 2=no
         ,fl8bathgrbbr
         ,fl8tltgrbbr
         ,fl8bathseat
         ,fl8raisedtlt
         ,ho8homeamen1:ho8homeamen3 #home amenities for accessibility
         ,ho8bathamen1:ho8bathamen7 #home bathroom amenities for accessibility
         ,em8modhere1 #if value is 2, then added at least one environmental modification (ramp, elev, stair lift, bath mod) in last year if 
         ,em8addlstyr1:em8addlstyr7 #recent stair avoidance or bathroom modifications added (in past year)
         ,hc8disescn9 #dementia or AD
         ,hw8lst10pnds #lost 10 pounds in last year 1=yes
         ,hw8trytolose #trying to lose weight 1=yes
         #         ,w3varunit
         #         ,w3varstrat
         #         ,W3ANFINWGT0
         #         ,h3disescn9 #Dementia Has dementia or alzheimer's =1
         #         ,h3health #overall health
  ) %>% 
  na_if (-9) %>% 
  na_if (-8) %>% 
  na_if (-7) %>% 
  na_if (-1) %>% 
  mutate(
    count=1,
    round='2018',

    #This next section preps variables needed for one or more of the 5 frailty metrics  
    hw8howtallin = replace_na(hw8howtallin, 0), #this replace the 20 people with a height in feet but no inches to inches=0, so 4'(3), 5'(17)
    height2018=hw8howtallft*12+hw8howtallin, #height is needed to calculate BMI and for walking test 
    bmi2018=703*(hw8currweigh/(height2018)^2), #this builds BMI, which is used to create PFP elements grip and shrinking
    
    #FIVE FRAILTY ELEMENTS# ######################################
    #exhausted
    #activity
    #weak
    #shrink
    #slow
    #coded following Methods description in Bandeen-Roche 2015, which also coded frailty from NHATS and Frailty Worksheet from Frailty Science (2022)
    
    #ACTIVITY# ######################################
    #"They met criteria for “low physical activity” if, recently, they never walked for exercise or engaged in vigorous activities." (Bandeen-Roche, 2015)
    activity2018=case_when(
      is.na(pa8evrgowalk) & is.na(pa8vigoractv)~NA_real_,
      pa8evrgowalk==2 & pa8vigoractv==2 ~ 1,
      pa8evrgowalk==1 & pa8vigoractv==1 ~ 0,
      TRUE~0),
    
    #SHRINK# ######################################
    #"Participants met criteria for “shrinking” if they had body mass index (BMI) less than 18.5 kg/m2, based on self-reported height and weight, or reported unintentionally losing 10 or more pounds in the last year." Bandeen-Roche 2015
    #"Lost >5% body weight unintentionally in last year, or BMI <18.5kg/m2" frailty science sheet (2022)
    shrink2018=case_when(
      bmi2018<18.5~1, #if bmi is low (under 18.5)
      (hw8lst10pnds==1 & hw8trytolose==2)~1, #if lost 10 lbs but wasn't trying to lose weight - this represents the 5% bodyweight as close as possible. Subsequent rounds could possibly use prior round weight to calculate 5% loss
      (hw8lst10pnds==2 & bmi2018>=18.5)~0,
      (hw8lst10pnds==1 & hw8trytolose==1)~0,
      is.na(bmi2018) & (is.na(hw8lst10pnds) | is.na(hw8trytolose)) ~ NA_real_,
      TRUE~0),
    
    #WEAK# ######################################
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #from Frailty Science Definition sheet:
    #"Meets criteria for grip strength weakness if:
    #Men: ≤29 kg for BMI ≤24, ≤30 kg for BMI 24.1–26, ≤30 kg for BMI 26.1–28, ≤32 kg for BMI >28
    #Women: ≤17 kg for BMI ≤23, ≤17.3 kg for BMI 23.1–26, ≤18 kg for BMI 26.1–29, ≤21 kg for BMI >29
    grip2018=case_when
    (
      gr8grp1rdng>=0 & gr8grp2rdng>=0 ~pmax(gr8grp1rdng,gr8grp2rdng),
      is.na(gr8grp1rdng) & gr8grp2rdng>=0   ~gr8grp2rdng,
      gr8grp1rdng >=0  & is.na(gr8grp2rdng) ~gr8grp1rdng,
      is.na(gr8grp1rdng) & is.na(gr8grp2rdng)  ~NA_real_
    ),
    weak2018=case_when( 
      is.na(grip2018) & (gr8dgripadm==2 | gr8dgripadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(grip2018) & is.na(gr8dgripadm) ~NA_real_, #test not applicable to SP
      gr8dgripadm==2~1, #if not administered due to hand weakness, set to 1
      (female==1 & #FEMALE SETTINGS #using frailty science definition sheet cut points
         (grip2018<=17 |
            (bmi2018<=23 & grip2018<=17) | #UNDERWEIGHT
            ((bmi2018>23 & bmi2018<=26) & grip2018<=17.3) | #NORMAL
            ((bmi2018>26 & bmi2018<=29) & grip2018<=18) |  #OVERWEIGHT
            (bmi2018>29 & grip2018<=21)))~1,        #OBESE
      (female==0 & #MALE SETTINGS
         (grip2018<=24 |
            (bmi2018<=24 & grip2018<=29) | #UNDERWEIGHT
            ((bmi2018>24 & bmi2018<=26) & grip2018<=30) | #NORMAL
            ((bmi2018>26 & bmi2018<=28) & grip2018<=30) | #OVERWEIGHT
            (bmi2018>28 & grip2018<=21)))~1, #OBESE
      TRUE~0),
    
    #EXHAUSTED# ######################################
    #"Participants met criteria for “exhaustion” who reported recently having low energy or being easily exhausted: enough to limit their activities." Bandeen-Roche 2015
    exhausted2018=case_when( 
      ss8lowenergy==2~0, #no low energy - code 0
      ss8lowenergy==1 & ss8loenlmtat==2~0, #low energy but not enough to limit activity- also code 0
      ss8loenlmtat==1~1, #low energy is so bad that it limits activity - code 1
      TRUE~NA_real_),
    
    #SLOW# ######################################
    #From Frailty Science: Frailty Assessment Definition Sheet, However, ms/ was measured over 3 meters because that was tested in the NHATS. 
    #"Men ≤0.65m/s for height ≤173 cm (68 inches) ≤0.76m/s for height >173 cm (68 inches)
    #Women ≤0.65m/s for height ≤159cm (63 inches) ≤0.76m/s for height >159cm (63 inches)"
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #First create walktime variables, based on the two tests, considering NAs and data errors (missing seconds)
    walktime1=case_when(
      is.na(wa8wlkc1secs) & is.na(wa8wlk1hndr) ~ 0,
      is.na(wa8wlkc1secs) & wa8wlk1hndr>0      ~ 0,
      wa8wlkc1secs>0 & is.na(wa8wlk1hndr)      ~ wa8wlkc1secs,
      wa8wlkc1secs>0 & wa8wlk1hndr>0           ~ (wa8wlkc1secs+ wa8wlk1hndr/100),
      TRUE~0
    ),
    
    walktime2=case_when(
      is.na(wa8wlkc2secs) & is.na(wa8wlk2hndr) ~ 0,
      is.na(wa8wlkc2secs) & wa8wlk2hndr>0      ~ 0,
      wa8wlkc2secs>0 & is.na(wa8wlk2hndr)      ~ wa8wlkc2secs,
      wa8wlkc2secs>0 & wa8wlk2hndr>0           ~ (wa8wlkc2secs+ wa8wlk2hndr/100),
      TRUE~0
    ),
    
    #this is giving problems where valid walk scores are not getting speed scores, so this creates an intermediate variable to separate division from pmin
    fasterwalkscore=case_when
    (
      walktime1 !=0 & walktime2 !=0 ~pmin(walktime1,walktime2),
      walktime1 ==0 & walktime2>0   ~walktime2,
      walktime1 >0  & walktime2 ==0 ~walktime1,
      walktime1==0  & walktime2==0  ~NA_real_
    ),
    
    #Create a speed variable which is the faster walk time score converted to meters per second (test is 3 meters long, so divide walktime into 3)
    speed2018=(3/fasterwalkscore), 
    
    #Construct slowness frailty test based on PFP parameters
    slow2018=case_when
    (
      is.na(speed2018) & (wa8dwlkadm==2 | wa8dwlkadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(speed2018) & is.na(wa8dwlkadm) ~NA_real_, #test not applicable to SP
      #FEMALE SETTINGS #using frailty science definition sheet cut points
      (female==1 & 
         ((height2018<=63 & speed2018<=.65) | (height2018>63  & speed2018<=.76)))~1, #Not short
      #MALE SETTINGS #using frailty science definition sheet cut points
      (female==0 & 
         ((height2018<=68 & speed2018<=.65) |(height2018>68  & speed2018<=.76)))~1, #not short
      TRUE~0),
    
    
    #Calculate the raw PFP score based on the 5 frailty test elements
    pfp2018=1+(shrink2018 + weak2018 + exhausted2018 + activity2018 + slow2018), #sum of all 5 elements
    
    #Construct the independent variable of frailty to create the 3 PFP categories (robust, frail, prefrail) from the 5 frailty tests
    frailty2018=case_when(
      pfp2018==1~1, #'robust'
      (pfp2018==2 | pfp2018==3) ~2, #'prefrail',
      pfp2018>=4~3, #'frail',
      TRUE~NA_real_),
    
    #Create the dependent variable of recent falls. falls in last month is missing very few cases in 2013, and nobody who has a frailty score has a missing value for falls in last month (yay!) 
    falls2018=case_when((hc8fllsinmth==1 | hc8faleninyr==1 | hc8multifall==1)~1, TRUE~ 0), #This builds FALLS in 2013, to add to Dependent/Outcome variable
    
    #Next, the moderating variable of home accessibility is prepped
    #first the four places for accessibility burdens are created separately: exterior to the building, in the building but not yet into the home, in the home, and in the bathroom 
    bldgextst2018=case_when( 
      ho8entrstair==2 | is.na(ho8entrstair) ~0, #no stairs - code 0
      ho8entrstair==1 & (ho8entrnramp==1 | is.na(ho8entrnramp))~1, #entry stair and ramp exists
      ho8entrstair==1 & ho8entrnramp==2~2, #entry stair and no ramp exists
      TRUE~NA_real_),
    
    inbldgst2018=case_when( 
      (ho8bldgamen1==2 & ho8bldgamen2==2)~2, #stairs and no elevator nor chair lift  exists
      (ho8bldgamen1==1 | ho8bldgamen2==1)~1, #stairs and elevator or chair lift exists
      (is.na(ho8bldgamen1) & is.na(ho8bldgamen2)) ~0, #if both NA, then no stairs inside building to get to SP's. home - code 0
      (!(is.na(ho8bldgamen1)) & !(is.na(ho8bldgamen2))) ~1, #if both NOT NA, then stairs inside building to get to SP's. home - code 0
      TRUE~NA_real_),
    
    inhomest2018=case_when( 
      ho8homeamen1==1 ~0, #living on one floor flagged yes - code 0
      ho8homeamen1!=1 & (ho8homeamen2==1 | ho8homeamen3==1)~1, #not living on one floor and elev or chair lift exists
      ho8homeamen1!=1 & (ho8homeamen2==2 & ho8homeamen3==2) ~2, #not living on one floor and no elev nor chair lift exists
      TRUE~0), #set NAs to 0 because they live in a facility/room where all on one floor or overly accessible
    
    #no variability, everybody has bath features below:
    nobathf2018=case_when( 
      (fl8bathgrbbr==1 | fl8tltgrbbr ==1)~0, #| fl1bathseat==1 | fl1raisedtlt==1) ~0, #any bath amenity exists - code 0
      (fl8bathgrbbr==2 & fl8tltgrbbr ==2)~1, # & fl1bathseat==2 & fl1raisedtlt==2)~1, #no bath amenity exists
      TRUE~0),
    
    #create variable that indicates if modifications were recently added, which could confound causation timeline of accessibility and falls
    recentanymod2018=case_when(
      em8modhere1==2~1, #recent modification indicated by "2" in this question
      TRUE~0 #set all else to 0 because recent mods were either not new (1), or they didn't have the feature to begin with
    ),
    #recent stair avoidance modification (stair lift, ramp, or elevator)
    recentstructmod2018=case_when(
      (em8addlstyr1==1 | em8addlstyr2==1 | em8addlstyr3==1)~1,
      TRUE~0
    ),
    
    #recent bathroom special feature modification
    recentbathfeat2018=case_when(
      (em8addlstyr4==1 | em8addlstyr5==1 | em8addlstyr6==1 | em8addlstyr7==1)~1,
      TRUE~0
    ),
    
    #sum raw accessibility burden index
    accburden2018=1+(bldgextst2018 + inbldgst2018 + inhomest2018), #sum of all 3 elements
    
    #Construct the independent variable of home accessibility to create the 3 categories (lo, med, high press) from the 5 accburden tests
    press2018=case_when(
      accburden2018==1~1, #'low press',
      (accburden2018==2 | accburden2018==3) ~2, #'med press',
      accburden2018>=4~3, #'high press',
      TRUE~NA_real_),
    
    #reverse PFP to make "competence" to align with Lawton scale direction
    competence2018=case_when(
      pfp2018==1 ~6,
      pfp2018==2 ~5,
      pfp2018==3 ~4,
      pfp2018==4 ~3,
      pfp2018==5 ~2,
      pfp2018==6 ~1
    )
  )# %>% filter (pfp2013>=0) (return to filtering later)

#Create tidy dataset for Tableau heat map, of falls plotted over frailty and acc
vars = c('spid','count', 'round', 'falls2018', 'competence2018', 'frailty2018', 'pfp2018', 'press2018', 'accburden2018','recentstructmod2018', 'hc8worryfall')
r8<-round8[vars]
#write.csv (r8,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/r8.csv")


#Create file for binding rows, so all vars have same names and can be additive to long format file for analysis.
r8same<-r8 %>% rename(
  falls=falls2018,
  competence=competence2018,
  frailty=frailty2018,
  pfp=pfp2018,
  press=press2018,
  accburden=accburden2018) %>% 
  select(spid
         ,count
         ,round
         ,falls
         ,pfp
         ,competence
         ,frailty
         ,accburden
         ,press)

#Stack repeat measures with round name and then add on demographics. 
rounds12345678<-merge ((r1 %>% select(spid, female, whitenothisp, blacknothisp, hispanic, otherrace)),bind_rows(r1same, r2same, r3same, r4same, r5same, r6same, r7same, r8same))
#write.csv (rounds12345678,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/rounds12345678.csv")


#################################################################################
####################ROUND 9######################################################
#################################################################################
round9 <- merge ((r1 %>% select(spid, female)),read_sas("~/Documents/NHATS/NHATS Public Data/Public Round 9/NHATS_R9_Final_Release_SAS/NHATS_Round_9_SP_File.sas7bdat", NULL)) %>%  
  select(spid
         ,female
         ,wa9dwlkadm   #if =1 Include if respondent was eligible for walk test, it was administered, and a score was recorded 
         ,gr9dgripadm   #if =1 Include if respondent was eligible for grip strength test, it was administered, and a score was recorded 
         ,hw9currweigh #current weight in pounds
         ,hw9howtallft #current height in feet
         ,hw9howtallin #current height inches (make total height as feet plus inches
         ,gr9grp1rdng  #grip reading trial 1
         ,gr9grp2rdng  #grip reading trial 2, take higher of 1 or 2
         ,wa9wlkc1secs #seconds to walk trial 1
         ,wa9wlk1hndr  #hundredths of a second to walk trial 1
         ,wa9wlkc2secs #seconds to walk trial 2
         ,wa9wlk2hndr  #hundredths of a second to walk trial 2
         ,ss9lowenergy #low energy in last month 1=yes 2=no
         ,ss9loenlmtat #low energy limited activity 1=yes
         ,pa9evrgowalk #ever walk for exercise 2=no
         ,pa9vigoractv #ever do vigorous activity 2=no
         ,hc9faleninyr #falls in last year (not including last year, need hcxfllsinmth to calculate)
         ,hc9fllsinmth #falls in last month
         ,hc9multifall #multiple falls
         ,hc9worryfall #in last month did you worry about falling down 1=yes 2=n0
         ,hc9worrylimt #diin this last month did a fall worry limit your activities 1=yes 2=n0
         ,ho9entrstair #(1=has stairs, 2=no stairs NOTE: Stairs bad here, so AMENITY COUNT IS IF THIS IS 2!)
         ,ho9entrnramp #has ramp, 1=yes, 2=no 
         ,ho9bldgamen1 #has elevator, 1=yes, 2=no
         ,ho9bldgamen2 #has STAIR LIFT, 1=yes, 2=no
         ,fl9bathgrbbr
         ,fl9tltgrbbr
         ,fl9bathseat
         ,fl9raisedtlt
         ,ho9homeamen1:ho9homeamen3 #home amenities for accessibility
         ,ho9bathamen1:ho9bathamen7 #home bathroom amenities for accessibility
         ,em9modhere1 #if value is 2, then added at least one environmental modification (ramp, elev, stair lift, bath mod) in last year if 
         ,em9addlstyr1:em9addlstyr7 #recent stair avoidance or bathroom modifications added (in past year)
         ,hc9disescn9 #dementia or AD
         ,hw9lst10pnds #lost 10 pounds in last year 1=yes
         ,hw9trytolose #trying to lose weight 1=yes
         #         ,w3varunit
         #         ,w3varstrat
         #         ,W3ANFINWGT0
         #         ,h3disescn9 #Dementia Has dementia or alzheimer's =1
         #         ,h3health #overall health
  ) %>% 
  na_if (-9) %>% 
  na_if (-8) %>% 
  na_if (-7) %>% 
  na_if (-1) %>% 
  mutate(
    count=1,
    round='2019',

    #This next section preps variables needed for one or more of the 5 frailty metrics  
    hw9howtallin = replace_na(hw9howtallin, 0), #this replace the 20 people with a height in feet but no inches to inches=0, so 4'(3), 5'(17)
    height2019=hw9howtallft*12+hw9howtallin, #height is needed to calculate BMI and for walking test 
    bmi2019=703*(hw9currweigh/(height2019)^2), #this builds BMI, which is used to create PFP elements grip and shrinking
    
    #FIVE FRAILTY ELEMENTS# ######################################
    #exhausted
    #activity
    #weak
    #shrink
    #slow
    #coded following Methods description in Bandeen-Roche 2015, which also coded frailty from NHATS and Frailty Worksheet from Frailty Science (2022)
    
    #ACTIVITY# ######################################
    #"They met criteria for “low physical activity” if, recently, they never walked for exercise or engaged in vigorous activities." (Bandeen-Roche, 2015)
    activity2019=case_when(
      is.na(pa9evrgowalk) & is.na(pa9vigoractv)~NA_real_,
      pa9evrgowalk==2 & pa9vigoractv==2 ~ 1,
      pa9evrgowalk==1 & pa9vigoractv==1 ~ 0,
      TRUE~0),
    
    #SHRINK# ######################################
    #"Participants met criteria for “shrinking” if they had body mass index (BMI) less than 18.5 kg/m2, based on self-reported height and weight, or reported unintentionally losing 10 or more pounds in the last year." Bandeen-Roche 2015
    #"Lost >5% body weight unintentionally in last year, or BMI <18.5kg/m2" frailty science sheet (2022)
    shrink2019=case_when(
      bmi2019<18.5~1, #if bmi is low (under 18.5)
      (hw9lst10pnds==1 & hw9trytolose==2)~1, #if lost 10 lbs but wasn't trying to lose weight - this represents the 5% bodyweight as close as possible. Subsequent rounds could possibly use prior round weight to calculate 5% loss
      (hw9lst10pnds==2 & bmi2019>=18.5)~0,
      (hw9lst10pnds==1 & hw9trytolose==1)~0,
      is.na(bmi2019) & (is.na(hw9lst10pnds) | is.na(hw9trytolose)) ~ NA_real_,
      TRUE~0),
    
    #WEAK# ######################################
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #from Frailty Science Definition sheet:
    #"Meets criteria for grip strength weakness if:
    #Men: ≤29 kg for BMI ≤24, ≤30 kg for BMI 24.1–26, ≤30 kg for BMI 26.1–28, ≤32 kg for BMI >28
    #Women: ≤17 kg for BMI ≤23, ≤17.3 kg for BMI 23.1–26, ≤18 kg for BMI 26.1–29, ≤21 kg for BMI >29
    grip2019=case_when
    (
      gr9grp1rdng>=0 & gr9grp2rdng>=0 ~pmax(gr9grp1rdng,gr9grp2rdng),
      is.na(gr9grp1rdng) & gr9grp2rdng>=0   ~gr9grp2rdng,
      gr9grp1rdng >=0  & is.na(gr9grp2rdng) ~gr9grp1rdng,
      is.na(gr9grp1rdng) & is.na(gr9grp2rdng)  ~NA_real_
    ),
    weak2019=case_when( 
      is.na(grip2019) & (gr9dgripadm==2 | gr9dgripadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(grip2019) & is.na(gr9dgripadm) ~NA_real_, #test not applicable to SP
      gr9dgripadm==2~1, #if not administered due to hand weakness, set to 1
      (female==1 & #FEMALE SETTINGS #using frailty science definition sheet cut points
         (grip2019<=17 |
            (bmi2019<=23 & grip2019<=17) | #UNDERWEIGHT
            ((bmi2019>23 & bmi2019<=26) & grip2019<=17.3) | #NORMAL
            ((bmi2019>26 & bmi2019<=29) & grip2019<=18) |  #OVERWEIGHT
            (bmi2019>29 & grip2019<=21)))~1,        #OBESE
      (female==0 & #MALE SETTINGS
         (grip2019<=24 |
            (bmi2019<=24 & grip2019<=29) | #UNDERWEIGHT
            ((bmi2019>24 & bmi2019<=26) & grip2019<=30) | #NORMAL
            ((bmi2019>26 & bmi2019<=28) & grip2019<=30) | #OVERWEIGHT
            (bmi2019>28 & grip2019<=21)))~1, #OBESE
      TRUE~0),
    
    #EXHAUSTED# ######################################
    #"Participants met criteria for “exhaustion” who reported recently having low energy or being easily exhausted: enough to limit their activities." Bandeen-Roche 2015
    exhausted2019=case_when( 
      ss9lowenergy==2~0, #no low energy - code 0
      ss9lowenergy==1 & ss9loenlmtat==2~0, #low energy but not enough to limit activity- also code 0
      ss9loenlmtat==1~1, #low energy is so bad that it limits activity - code 1
      TRUE~NA_real_),
    
    #SLOW# ######################################
    #From Frailty Science: Frailty Assessment Definition Sheet, However, ms/ was measured over 3 meters because that was tested in the NHATS. 
    #"Men ≤0.65m/s for height ≤173 cm (68 inches) ≤0.76m/s for height >173 cm (68 inches)
    #Women ≤0.65m/s for height ≤159cm (63 inches) ≤0.76m/s for height >159cm (63 inches)"
    #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    #First create walktime variables, based on the two tests, considering NAs and data errors (missing seconds)
    walktime1=case_when(
      is.na(wa9wlkc1secs) & is.na(wa9wlk1hndr) ~ 0,
      is.na(wa9wlkc1secs) & wa9wlk1hndr>0      ~ 0,
      wa9wlkc1secs>0 & is.na(wa9wlk1hndr)      ~ wa9wlkc1secs,
      wa9wlkc1secs>0 & wa9wlk1hndr>0           ~ (wa9wlkc1secs+ wa9wlk1hndr/100),
      TRUE~0
    ),
    
    walktime2=case_when(
      is.na(wa9wlkc2secs) & is.na(wa9wlk2hndr) ~ 0,
      is.na(wa9wlkc2secs) & wa9wlk2hndr>0      ~ 0,
      wa9wlkc2secs>0 & is.na(wa9wlk2hndr)      ~ wa9wlkc2secs,
      wa9wlkc2secs>0 & wa9wlk2hndr>0           ~ (wa9wlkc2secs+ wa9wlk2hndr/100),
      TRUE~0
    ),
    
    #this is giving problems where valid walk scores are not getting speed scores, so this creates an intermediate variable to separate division from pmin
    fasterwalkscore=case_when
    (
      walktime1 !=0 & walktime2 !=0 ~pmin(walktime1,walktime2),
      walktime1 ==0 & walktime2>0   ~walktime2,
      walktime1 >0  & walktime2 ==0 ~walktime1,
      walktime1==0  & walktime2==0  ~NA_real_
    ),
    
    #Create a speed variable which is the faster walk time score converted to meters per second (test is 3 meters long, so divide walktime into 3)
    speed2019=(3/fasterwalkscore), 
    
    #Construct slowness frailty test based on PFP parameters
    slow2019=case_when
    (
      is.na(speed2019) & (wa9dwlkadm==2 | wa9dwlkadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
      is.na(speed2019) & is.na(wa9dwlkadm) ~NA_real_, #test not applicable to SP
      #FEMALE SETTINGS #using frailty science definition sheet cut points
      (female==1 & 
         ((height2019<=63 & speed2019<=.65) | (height2019>63  & speed2019<=.76)))~1, #Not short
      #MALE SETTINGS #using frailty science definition sheet cut points
      (female==0 & 
         ((height2019<=68 & speed2019<=.65) |(height2019>68  & speed2019<=.76)))~1, #not short
      TRUE~0),
    
    
    #Calculate the raw PFP score based on the 5 frailty test elements
    pfp2019=1+(shrink2019 + weak2019 + exhausted2019 + activity2019 + slow2019), #sum of all 5 elements
    
    #Construct the independent variable of frailty to create the 3 PFP categories (robust, frail, prefrail) from the 5 frailty tests
    frailty2019=case_when(
      pfp2019==1~1, #'robust'
      (pfp2019==2 | pfp2019==3) ~2, #'prefrail',
      pfp2019>=4~3, #'frail',
      TRUE~NA_real_),
    
    #Create the dependent variable of recent falls. falls in last month is missing very few cases in 2013, and nobody who has a frailty score has a missing value for falls in last month (yay!) 
    falls2019=case_when((hc9fllsinmth==1 | hc9faleninyr==1 | hc9multifall==1)~1, TRUE~ 0), #This builds FALLS in 2013, to add to Dependent/Outcome variable
    
    #Next, the moderating variable of home accessibility is prepped
    #first the four places for accessibility burdens are created separately: exterior to the building, in the building but not yet into the home, in the home, and in the bathroom 
    bldgextst2019=case_when( 
      ho9entrstair==2 | is.na(ho9entrstair) ~0, #no stairs - code 0
      ho9entrstair==1 & (ho9entrnramp==1 | is.na(ho9entrnramp))~1, #entry stair and ramp exists
      ho9entrstair==1 & ho9entrnramp==2~2, #entry stair and no ramp exists
      TRUE~NA_real_),
    
    inbldgst2019=case_when( 
      (ho9bldgamen1==2 & ho9bldgamen2==2)~2, #stairs and no elevator nor chair lift  exists
      (ho9bldgamen1==1 | ho9bldgamen2==1)~1, #stairs and elevator or chair lift exists
      (is.na(ho9bldgamen1) & is.na(ho9bldgamen2)) ~0, #if both NA, then no stairs inside building to get to SP's. home - code 0
      (!(is.na(ho9bldgamen1)) & !(is.na(ho9bldgamen2))) ~1, #if both NOT NA, then stairs inside building to get to SP's. home - code 0
      TRUE~NA_real_),
    
    inhomest2019=case_when( 
      ho9homeamen1==1 ~0, #living on one floor flagged yes - code 0
      ho9homeamen1!=1 & (ho9homeamen2==1 | ho9homeamen3==1)~1, #not living on one floor and elev or chair lift exists
      ho9homeamen1!=1 & (ho9homeamen2==2 & ho9homeamen3==2) ~2, #not living on one floor and no elev nor chair lift exists
      TRUE~0), #set NAs to 0 because they live in a facility/room where all on one floor or overly accessible
    
    #no variability, everybody has bath features below:
    nobathf2019=case_when( 
      (fl9bathgrbbr==1 | fl9tltgrbbr ==1)~0, #| fl1bathseat==1 | fl1raisedtlt==1) ~0, #any bath amenity exists - code 0
      (fl9bathgrbbr==2 & fl9tltgrbbr ==2)~1, # & fl1bathseat==2 & fl1raisedtlt==2)~1, #no bath amenity exists
      TRUE~0),
    
    #create variable that indicates if modifications were recently added, which could confound causation timeline of accessibility and falls
    recentanymod2019=case_when(
      em9modhere1==2~1, #recent modification indicated by "2" in this question
      TRUE~0 #set all else to 0 because recent mods were either not new (1), or they didn't have the feature to begin with
    ),
    #recent stair avoidance modification (stair lift, ramp, or elevator)
    recentstructmod2019=case_when(
      (em9addlstyr1==1 | em9addlstyr2==1 | em9addlstyr3==1)~1,
      TRUE~0
    ),
    
    #recent bathroom special feature modification
    recentbathfeat2019=case_when(
      (em9addlstyr4==1 | em9addlstyr5==1 | em9addlstyr6==1 | em9addlstyr7==1)~1,
      TRUE~0
    ),
    
    #sum raw accessibility burden index
    accburden2019=1+(bldgextst2019 + inbldgst2019 + inhomest2019), #sum of all 3 elements
    
    #Construct the independent variable of home accessibility to create the 3 categories (lo, med, high press) from the 5 accburden tests
    press2019=case_when(
      accburden2019==1~1, #'low press',
      (accburden2019==2 | accburden2019==3) ~2, #'med press',
      accburden2019>=4~3, #'high press',
      TRUE~NA_real_),
    
    #reverse PFP to make "competence" to align with Lawton scale direction
    competence2019=case_when(
      pfp2019==1 ~6,
      pfp2019==2 ~5,
      pfp2019==3 ~4,
      pfp2019==4 ~3,
      pfp2019==5 ~2,
      pfp2019==6 ~1
    )
  )# %>% filter (pfp2013>=0) (return to filtering later)

#Create tidy dataset for Tableau heat map, of falls plotted over frailty and acc
vars = c('spid','count', 'round', 'falls2019', 'competence2019', 'frailty2019', 'pfp2019', 'press2019', 'accburden2019','recentstructmod2019', 'hc9worryfall')
r9<-round9[vars]
#write.csv (r9,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/r9.csv")


#Create file for binding rows, so all vars have same names and can be additive to long format file for analysis.
r9same<-r9 %>% rename(
  falls=falls2019,
  competence=competence2019,
  frailty=frailty2019,
  pfp=pfp2019,
  press=press2019,
  accburden=accburden2019) %>% 
  select(spid
         ,count
         ,round
         ,falls
         ,pfp
         ,competence
         ,frailty
         ,accburden
         ,press)

#Stack repeat measures with round name and then add on demographics. 
rounds123456789<-merge ((r1 %>% select(spid, female, whitenothisp, blacknothisp, hispanic, otherrace)),bind_rows(r1same, r2same, r3same, r4same, r5same, r6same, r7same, r8same, r9same))
#write.csv (rounds123456789,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/rounds123456789.csv")

#################################################################################
####################ROUND 10######################################################
#################################################################################
round10 <- merge ((r1 %>% select(spid, female)),read_sas("~/Documents/NHATS/NHATS Public Data/Public Round 10/NHATS_R10_Final_Release_SAS/NHATS_Round_10_SP_File.sas7bdat", NULL)) %>%  
  select(spid
         ,female
#         ,wa10dwlkadm   #if =1 Include if respondent was eligible for walk test, it was administered, and a score was recorded 
#         ,gr10dgripadm   #if =1 Include if respondent was eligible for grip strength test, it was administered, and a score was recorded 
          ,hw10currweigh #current weight in pounds
          ,hw10howtallft #current height in feet
          ,hw10howtallin #current height inches (make total height as feet plus inches
#         ,gr10grp1rdng  #grip reading trial 1
#         ,gr10grp2rdng  #grip reading trial 2, take higher of 1 or 2
#         ,wa10wlkc1secs #seconds to walk trial 1
#         ,wa10wlk1hndr  #hundredths of a second to walk trial 1
#         ,wa10wlkc2secs #seconds to walk trial 2
#         ,wa10wlk2hndr  #hundredths of a second to walk trial 2
          ,ss10lowenergy #low energy in last month 1=yes 2=no
          ,ss10loenlmtat #low energy limited activity 1=yes
          ,pa10evrgowalk #ever walk for exercise 2=no
          ,pa10vigoractv #ever do vigorous activity 2=no
         ,hc10faleninyr #falls in last year (not including last year, need hcxfllsinmth to calculate)
         ,hc10fllsinmth #falls in last month
         ,hc10multifall #multiple falls
         ,hc10worryfall #in last month did you worry about falling down 1=yes 2=n0
         ,hc10worrylimt #diin this last month did a fall worry limit your activities 1=yes 2=n0
         ,ho10entrstair #(1=has stairs, 2=no stairs NOTE: Stairs bad here, so AMENITY COUNT IS IF THIS IS 2!)
         ,ho10entrnramp #has ramp, 1=yes, 2=no 
         ,ho10bldgamen1 #has elevator, 1=yes, 2=no
         ,ho10bldgamen2 #has STAIR LIFT, 1=yes, 2=no
         ,fl10bathgrbbr
         ,fl10tltgrbbr
         ,fl10bathseat
         ,fl10raisedtlt
         ,ho10homeamen1:ho10homeamen3 #home amenities for accessibility
         ,ho10bathamen1:ho10bathamen7 #home bathroom amenities for accessibility
         ,em10modhere1 #if value is 2, then added at least one environmental modification (ramp, elev, stair lift, bath mod) in last year if 
         ,em10addlstyr1:em10addlstyr7 #recent stair avoidance or bathroom modifications added (in past year)
         ,hc10disescn9 #dementia or AD
         ,hw10lst10pnds #lost 10 pounds in last year 1=yes
         ,hw10trytolose #trying to lose weight 1=yes
         #         ,w3varunit
         #         ,w3varstrat
         #         ,W3ANFINWGT0
         #         ,h3disescn9 #Dementia Has dementia or alzheimer's =1
         #         ,h3health #overall health
  ) %>% 
  na_if (-9) %>% 
  na_if (-8) %>% 
  na_if (-7) %>% 
  na_if (-1) %>% 
  mutate(
    count=1,
    round='2020',

    #This next section preps variables needed for one or more of the 5 frailty metrics  
    hw10howtallin = replace_na(hw10howtallin, 0), #this replace the 20 people with a height in feet but no inches to inches=0, so 4'(3), 5'(17)
    height2020=hw10howtallft*12+hw10howtallin, #height is needed to calculate BMI and for walking test 
    bmi2020=703*(hw10currweigh/(height2020)^2), #this builds BMI, which is used to create PFP elements grip and shrinking
    
#FRAILTY COULD NOT BE CODED in 2020 due to COVID-19 PANDEMIC - in ability to measure GRIP and WALK SPEED. OTher 3 elements are included, though.
    #FIVE FRAILTY ELEMENTS# ######################################
    #exhausted
    #activity
    #weak
    #shrink
    #slow
    #coded following Methods description in Bandeen-Roche 2015, which also coded frailty from NHATS and Frailty Worksheet from Frailty Science (2022)
    
    #ACTIVITY# ######################################
    #"They met criteria for “low physical activity” if, recently, they never walked for exercise or engaged in vigorous activities." (Bandeen-Roche, 2015)
    activity2020=case_when(
      is.na(pa10evrgowalk) & is.na(pa10vigoractv)~NA_real_,
      pa10evrgowalk==2 & pa10vigoractv==2 ~ 1,
      pa10evrgowalk==1 & pa10vigoractv==1 ~ 0,
      TRUE~0),
    
    #SHRINK# ######################################
    #"Participants met criteria for “shrinking” if they had body mass index (BMI) less than 18.5 kg/m2, based on self-reported height and weight, or reported unintentionally losing 10 or more pounds in the last year." Bandeen-Roche 2015
    #"Lost >5% body weight unintentionally in last year, or BMI <18.5kg/m2" frailty science sheet (2022)
    shrink2020=case_when(
      bmi2020<18.5~1, #if bmi is low (under 18.5)
      (hw10lst10pnds==1 & hw10trytolose==2)~1, #if lost 10 lbs but wasn't trying to lose weight - this represents the 5% bodyweight as close as possible. Subsequent rounds could possibly use prior round weight to calculate 5% loss
      (hw10lst10pnds==2 & bmi2020>=18.5)~0,
      (hw10lst10pnds==1 & hw10trytolose==1)~0,
      is.na(bmi2020) & (is.na(hw10lst10pnds) | is.na(hw10trytolose)) ~ NA_real_,
      TRUE~0),
    
    # #WEAK# ######################################
    # #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    # #from Frailty Science Definition sheet:
    # #"Meets criteria for grip strength weakness if:
    # #Men: ≤29 kg for BMI ≤24, ≤30 kg for BMI 24.1–26, ≤30 kg for BMI 26.1–28, ≤32 kg for BMI >28
    # #Women: ≤17 kg for BMI ≤23, ≤17.3 kg for BMI 23.1–26, ≤18 kg for BMI 26.1–29, ≤21 kg for BMI >29
    # grip2020=case_when
    # (
    #   gr10grp1rdng>=0 & gr10grp2rdng>=0 ~pmax(gr10grp1rdng,gr10grp2rdng),
    #   is.na(gr10grp1rdng) & gr10grp2rdng>=0   ~gr10grp2rdng,
    #   gr10grp1rdng >=0  & is.na(gr10grp2rdng) ~gr10grp1rdng,
    #   is.na(gr10grp1rdng) & is.na(gr10grp2rdng)  ~NA_real_
    # ),
    # weak2020=case_when( 
    #   is.na(grip2020) & (gr10dgripadm==2 | gr10dgripadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
    #   is.na(grip2020) & is.na(gr10dgripadm) ~NA_real_, #test not applicable to SP
    #   gr10dgripadm==2~1, #if not administered due to hand weakness, set to 1
    #   (female==1 & #FEMALE SETTINGS #using frailty science definition sheet cut points
    #      (grip2020<=17 |
    #         (bmi2020<=23 & grip2020<=17) | #UNDERWEIGHT
    #         ((bmi2020>23 & bmi2020<=26) & grip2020<=17.3) | #NORMAL
    #         ((bmi2020>26 & bmi2020<=29) & grip2020<=18) |  #OVERWEIGHT
    #         (bmi2020>29 & grip2020<=21)))~1,        #OBESE
    #   (female==0 & #MALE SETTINGS
    #      (grip2020<=24 |
    #         (bmi2020<=24 & grip2020<=29) | #UNDERWEIGHT
    #         ((bmi2020>24 & bmi2020<=26) & grip2020<=30) | #NORMAL
    #         ((bmi2020>26 & bmi2020<=28) & grip2020<=30) | #OVERWEIGHT
    #         (bmi2020>28 & grip2020<=21)))~1, #OBESE
    #   TRUE~0),
    # 
    #EXHAUSTED# ######################################
    #"Participants met criteria for “exhaustion” who reported recently having low energy or being easily exhausted: enough to limit their activities." Bandeen-Roche 2015
    exhausted2020=case_when( 
      ss10lowenergy==2~0, #no low energy - code 0
      ss10lowenergy==1 & ss10loenlmtat==2~0, #low energy but not enough to limit activity- also code 0
      ss10loenlmtat==1~1, #low energy is so bad that it limits activity - code 1
      TRUE~NA_real_),
    
    # #SLOW# ######################################
    # #From Frailty Science: Frailty Assessment Definition Sheet, However, ms/ was measured over 3 meters because that was tested in the NHATS. 
    # #"Men ≤0.65m/s for height ≤173 cm (68 inches) ≤0.76m/s for height >173 cm (68 inches)
    # #Women ≤0.65m/s for height ≤159cm (63 inches) ≤0.76m/s for height >159cm (63 inches)"
    # #About nontested: "For each, "participants not tested because of safety concerns, ineligible due to recent surgery or pain, or who attempted but were unable to complete a test, were scored as “0” following recommended practice" (Bandeen-Roche, 2015)
    # #First create walktime variables, based on the two tests, considering NAs and data errors (missing seconds)
    # walktime1=case_when(
    #   is.na(wa10wlkc1secs) & is.na(wa10wlk1hndr) ~ 0,
    #   is.na(wa10wlkc1secs) & wa10wlk1hndr>0      ~ 0,
    #   wa10wlkc1secs>0 & is.na(wa10wlk1hndr)      ~ wa10wlkc1secs,
    #   wa10wlkc1secs>0 & wa10wlk1hndr>0           ~ (wa10wlkc1secs+ wa10wlk1hndr/100),
    #   TRUE~0
    # ),
    # 
    # walktime2=case_when(
    #   is.na(wa10wlkc2secs) & is.na(wa10wlk2hndr) ~ 0,
    #   is.na(wa10wlkc2secs) & wa10wlk2hndr>0      ~ 0,
    #   wa10wlkc2secs>0 & is.na(wa10wlk2hndr)      ~ wa10wlkc2secs,
    #   wa10wlkc2secs>0 & wa10wlk2hndr>0           ~ (wa10wlkc2secs+ wa10wlk2hndr/100),
    #   TRUE~0
    # ),
    # 
    # #this is giving problems where valid walk scores are not getting speed scores, so this creates an intermediate variable to separate division from pmin
    # fasterwalkscore=case_when
    # (
    #   walktime1 !=0 & walktime2 !=0 ~pmin(walktime1,walktime2),
    #   walktime1 ==0 & walktime2>0   ~walktime2,
    #   walktime1 >0  & walktime2 ==0 ~walktime1,
    #   walktime1==0  & walktime2==0  ~NA_real_
    # ),
    # 
    # #Create a speed variable which is the faster walk time score converted to meters per second (test is 3 meters long, so divide walktime into 3)
    # speed2020=(3/fasterwalkscore), 
    # 
    # #Construct slowness frailty test based on PFP parameters
    # slow2020=case_when
    # (
    #   is.na(speed2020) & (wa10dwlkadm==2 | wa10dwlkadm==4)~ 0, #code up conditions based on Bandeen-Roche where eligible and not tested means 0
    #   is.na(speed2020) & is.na(wa10dwlkadm) ~NA_real_, #test not applicable to SP
    #   #FEMALE SETTINGS #using frailty science definition sheet cut points
    #   (female==1 & 
    #      ((height2020<=63 & speed2020<=.65) | (height2020>63  & speed2020<=.76)))~1, #Not short
    #   #MALE SETTINGS #using frailty science definition sheet cut points
    #   (female==0 & 
    #      ((height2020<=68 & speed2020<=.65) |(height2020>68  & speed2020<=.76)))~1, #not short
    #   TRUE~0),
    # 
    
    #Calculate the raw PFP score based on the 5 frailty test elements
#    pfp2020=1+(shrink2020 + weak2020 + exhausted2020 + activity2020 + slow2020), #sum of all 5 elements
    pfp_3_2020=1+(shrink2020 + exhausted2020 + activity2020), #sum of 3 elements

    #Construct the independent variable of frailty to create the 3 PFP categories (robust, frail, prefrail) from the 5 frailty tests
    # frailty2020=case_when(
    #   pfp2020==1~1, #'robust'
    #   (pfp2020==2 | pfp2020==3) ~2, #'prefrail',
    #   pfp2020>=4~3, #'frail',
    #   TRUE~NA_real_),
    frailty_3_2020=case_when(
      pfp_3_2020==1~1, #'robust'
      (pfp_3_2020==2 | pfp_3_2020==3) ~2, #'prefrail',
      pfp_3_2020>=4~3, #'frail',
      TRUE~NA_real_),

    
    #Create the dependent variable of recent falls. falls in last month is missing very few cases in 2013, and nobody who has a frailty score has a missing value for falls in last month (yay!) 
    falls2020=case_when((hc10fllsinmth==1 | hc10faleninyr==1 | hc10multifall==1)~1, TRUE~ 0), #This builds FALLS in 2013, to add to Dependent/Outcome variable
    
    #Next, the moderating variable of home accessibility is prepped
    #first the four places for accessibility burdens are created separately: exterior to the building, in the building but not yet into the home, in the home, and in the bathroom 
    bldgextst2020=case_when( 
      ho10entrstair==2 | is.na(ho10entrstair) ~0, #no stairs - code 0
      ho10entrstair==1 & (ho10entrnramp==1 | is.na(ho10entrnramp))~1, #entry stair and ramp exists
      ho10entrstair==1 & ho10entrnramp==2~2, #entry stair and no ramp exists
      TRUE~NA_real_),
    
    inbldgst2020=case_when( 
      (ho10bldgamen1==2 & ho10bldgamen2==2)~2, #stairs and no elevator nor chair lift  exists
      (ho10bldgamen1==1 | ho10bldgamen2==1)~1, #stairs and elevator or chair lift exists
      (is.na(ho10bldgamen1) & is.na(ho10bldgamen2)) ~0, #if both NA, then no stairs inside building to get to SP's. home - code 0
      (!(is.na(ho10bldgamen1)) & !(is.na(ho10bldgamen2))) ~1, #if both NOT NA, then stairs inside building to get to SP's. home - code 0
      TRUE~NA_real_),
    
    inhomest2020=case_when( 
      ho10homeamen1==1 ~0, #living on one floor flagged yes - code 0
      ho10homeamen1!=1 & (ho10homeamen2==1 | ho10homeamen3==1)~1, #not living on one floor and elev or chair lift exists
      ho10homeamen1!=1 & (ho10homeamen2==2 & ho10homeamen3==2) ~2, #not living on one floor and no elev nor chair lift exists
      TRUE~0), #set NAs to 0 because they live in a facility/room where all on one floor or overly accessible
    
    #no variability, everybody has bath features below:
    nobathf2020=case_when( 
      (fl10bathgrbbr==1 | fl10tltgrbbr ==1)~0, #| fl1bathseat==1 | fl1raisedtlt==1) ~0, #any bath amenity exists - code 0
      (fl10bathgrbbr==2 & fl10tltgrbbr ==2)~1, # & fl1bathseat==2 & fl1raisedtlt==2)~1, #no bath amenity exists
      TRUE~0),
    
    #create variable that indicates if modifications were recently added, which could confound causation timeline of accessibility and falls
    recentanymod2020=case_when(
      em10modhere1==2~1, #recent modification indicated by "2" in this question
      TRUE~0 #set all else to 0 because recent mods were either not new (1), or they didn't have the feature to begin with
    ),
    #recent stair avoidance modification (stair lift, ramp, or elevator)
    recentstructmod2020=case_when(
      (em10addlstyr1==1 | em10addlstyr2==1 | em10addlstyr3==1)~1,
      TRUE~0
    ),
    
    #recent bathroom special feature modification
    recentbathfeat2020=case_when(
      (em10addlstyr4==1 | em10addlstyr5==1 | em10addlstyr6==1 | em10addlstyr7==1)~1,
      TRUE~0
    ),
    
    #sum raw accessibility burden index
    accburden2020=1+(bldgextst2020 + inbldgst2020 + inhomest2020), #sum of all 3 elements
    
    #Construct the independent variable of home accessibility to create the 3 categories (lo, med, high press) from the 5 accburden tests
    press2020=case_when(
      accburden2020==1~1, #'low press',
      (accburden2020==2 | accburden2020==3) ~2, #'med press',
      accburden2020>=4~3, #'high press',
      TRUE~NA_real_),
    
    #reverse PFP to make "competence" to align with Lawton scale direction
    # competence2020=case_when(
    #   pfp2020==1 ~6,
    #   pfp2020==2 ~5,
    #   pfp2020==3 ~4,
    #   pfp2020==4 ~3,
    #   pfp2020==5 ~2,
    #   pfp2020==6 ~1

    competence_3_2020=case_when(
        pfp_3_2020==1 ~6,
        pfp_3_2020==2 ~5,
        pfp_3_2020==3 ~4,
        pfp_3_2020==4 ~3,
        pfp_3_2020==5 ~2,
        pfp_3_2020==6 ~1
        
        )
  )# %>% filter (pfp2013>=0) (return to filtering later)

#Create tidy dataset for Tableau heat map, of falls plotted over frailty and acc
vars = c('spid','count', 'round', 'falls2020', 'competence_3_2020', 'frailty_3_2020', 'pfp_3_2020', 'press2020', 'accburden2020','recentstructmod2020', 'hc10worryfall')
r10<-round10[vars]
#write.csv (r10,"~/Google Drive/000Albright Dissertation Progress/Dissertation Analytic Work/r10.csv")


#Create file for binding rows, so all vars have same names and can be additive to long format file for analysis.
r10same<-r10 %>% rename(
  falls=falls2020,
  competence_3_=competence_3_2020,
  frailty_3_=frailty_3_2020,
  pfp_3_=pfp_3_2020,
  press=press2020,
  accburden=accburden2020) %>% 
  select(spid
         ,count
         ,round
         ,falls
         ,pfp_3_
         ,competence_3_
         ,frailty_3_
         ,accburden
         ,press)

#Stack repeat measures with round name and then add on demographics. 
rounds12345678910<-merge ((r1 %>% select(spid, female, whitenothisp, blacknothisp, hispanic, otherrace)),bind_rows(r1same, r2same, r3same, r4same, r5same, r6same, r7same, r8same, r9same, r10same))
write.csv (rounds12345678910,"clean_data/allcleanrounds.csv")
