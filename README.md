# PS531PreAnalysisPlan
```{r setup, echo=FALSE, results=FALSE, include=FALSE}
#install.packages("knitr")
#install.packages("skimr")
library(knitr)
library(readr)
library(tidyverse)
library(skimr)
library(tidyverse)
library(estimatr)
library(DeclareDesign)
library(MASS)
library(optmatch)
library(RItools)
library(robustbase)
library(designmatch)
library(coin)
library(sensemakr)
library(sensitivitymv)
library(sensitivitymult)
library(sensitivityfull)
library(senstrat)
Fall_2022_Survey_Lewis_November_24_2022_14_45_csv <- read_csv("C:/Users/Alesha/Downloads/Fall 2022 - Survey (Lewis)_November 24, 2022_14.45.csv.zip")
AELSurveyData <- Fall_2022_Survey_Lewis_November_24_2022_14_45_csv
#View(AELSurveyData)
skim(AELSurveyData)
AELSurveyData<-AELSurveyData[-c(1,2),]
AELSurveyData<-AELSurveyData[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13)]
AELSurveyData$ID=1:390
```

```{r}
#voting in last election
AELSurveyData$lastelection <- AELSurveyData$Q9
AELSurveyData$lastelection[AELSurveyData$lastelection=="NA's"] <- NA
AELSurveyData$lastelection[AELSurveyData$lastelection == "Yes"] <- 1
AELSurveyData$lastelection[AELSurveyData$lastelection == "No"] <- 0
AELSurveyData$lastelection <- as.factor(AELSurveyData$lastelection)
levels(AELSurveyData$lastelection)
summary(AELSurveyData$lastelection)

# How many missing values do we have for voting? 
sum(is.na(AELSurveyData$lastelection))
# check distribution of covariate
table(AELSurveyData$lastelection)
# determine most common category
most_common <- names(sort(table(AELSurveyData$lastelection), decreasing = TRUE))[1]
# impute missing values with most common category
AELSurveyData$lastelection[is.na(AELSurveyData$lastelection)] <- most_common

#discrimination, yes or no
AELSurveyData$discrimination <- AELSurveyData$Q32
AELSurveyData$discrimination[AELSurveyData$discrimination=="NA's"] <- NA
AELSurveyData$discrimination[AELSurveyData$discrimination == "Yes"] <- 1
AELSurveyData$discrimination[AELSurveyData$discrimination == "No"] <- 0
AELSurveyData$discrimination <- as.factor(AELSurveyData$discrimination)
levels(AELSurveyData$discrimination)
summary(AELSurveyData$discrimination)

#age
Age <- AELSurveyData$Q50
as.numeric(Age)
# create a vector
Age <- c('18', '19', '20', '21', '22')
# convert to numeric
Age <- as.numeric(Age)
# display the vector
print(Age)
# display vector's type
print(class(Age))

# How many missing values do we have for age? 
sum(is.na(AELSurveyData$Q50))

# check distribution of covariate
# Not sure why, but it says age isn't a numeric variable
hist(AELSurveyData$Q50)

# impute missing values with mean
mean_covariate <- mean(AELSurveyData$Q50, na.rm = TRUE)
AELSurveyData$Q50[is.na(AELSurveyData$Q50)] <- mean_covariate




#gender
AELSurveyData$gender <- AELSurveyData$Q49
AELSurveyData$gender[AELSurveyData$gender == "Male"] <- 0
AELSurveyData$gender[AELSurveyData$gender == "Female"] <- 1
AELSurveyData$gender[AELSurveyData$gender == "Another identification (please specify)"] <- 2
levels(AELSurveyData$gender)
summary(AELSurveyData$gender)
table(AELSurveyData$gender)

# How many missing values do we have for gender? 
sum(is.na(AELSurveyData$gender))
# check distribution of covariate
table(AELSurveyData$gender)
# determine most common category
most_common <- names(sort(table(AELSurveyData$gender), decreasing = TRUE))[1]
# impute missing values with most common category
AELSurveyData$gender[is.na(AELSurveyData$gender)] <- most_common

#race
AELSurveyData$race <- AELSurveyData$Q51
AELSurveyData$race[AELSurveyData$race=="NA's"] <- NA
AELSurveyData$race[AELSurveyData$race == "Caucasian/White"] <- 0
AELSurveyData$race[AELSurveyData$race == "African American/Black"] <- 1
AELSurveyData$race[AELSurveyData$race == "Asian"] <- 2
AELSurveyData$race[AELSurveyData$race == "Native American/Alaska Native"] <- 3
AELSurveyData$race[AELSurveyData$race == "Multiple/Mixed"] <- 4
AELSurveyData$race[AELSurveyData$race == "Other (please specify)"] <- 5
AELSurveyData$race <- as.factor(AELSurveyData$race)
levels(AELSurveyData$race)
summary(AELSurveyData$race)

# How many missing values do we have for race? We'll need to know this for when we stratify later in the code
sum(is.na(AELSurveyData$race))
# check distribution of covariate
table(AELSurveyData$race)
# determine most common category
most_common <- names(sort(table(AELSurveyData$race), decreasing = TRUE))[1]
# impute missing values with most common category
AELSurveyData$race[is.na(AELSurveyData$race)] <- most_common


#ethnicity
AELSurveyData$ethnicity <- AELSurveyData$Q52
AELSurveyData$ethnicity[AELSurveyData$ethnicity=="NA's"] <- NA
AELSurveyData$ethnicity[AELSurveyData$ethnicity == "Yes"] <- 1
AELSurveyData$ethnicity[AELSurveyData$ethnicity == "No"] <- 0
AELSurveyData$ethnicity <- as.factor(AELSurveyData$ethnicity)
levels(AELSurveyData$ethnicity)
summary(AELSurveyData$ethnicity)

# How many missing values do we have for ethnicity? 
sum(is.na(AELSurveyData$ethnicity))
# check distribution of covariate
table(AELSurveyData$ethnicity)
# determine most common category
most_common <- names(sort(table(AELSurveyData$ethnicity), decreasing = TRUE))[1]
# impute missing values with most common category
AELSurveyData$ethnicity[is.na(AELSurveyData$ethnicity)] <- most_common

#citizenship
AELSurveyData$citizenship <- AELSurveyData$Q46
AELSurveyData$citizenship[AELSurveyData$citizenship=="NA's"] <- NA
AELSurveyData$citizenship[AELSurveyData$citizenship == "Yes"] <- 1
AELSurveyData$citizenship[AELSurveyData$citizenship == "No"] <- 0
AELSurveyData$citizenship <- as.factor(AELSurveyData$citizenship)
levels(AELSurveyData$citizenship)
summary(AELSurveyData$citizenship)

# How many missing values do we have for citizenship? 
sum(is.na(AELSurveyData$citizenship))
# check distribution of covariate
table(AELSurveyData$citizenship)
# determine most common category
most_common <- names(sort(table(AELSurveyData$citizenship), decreasing = TRUE))[1]
# impute missing values with most common category
AELSurveyData$citizenship[is.na(AELSurveyData$citizenship)] <- most_common

#voting eligibility
which(colnames(AELSurveyData)=="Q8") #tells you which column number
AELSurveyData$voteeligible <- AELSurveyData$Q8
AELSurveyData$voteeligible[AELSurveyData$voteeligible=="NA's"] <- NA
AELSurveyData$voteeligible[AELSurveyData$voteeligible == "Yes"] <- 1
AELSurveyData$voteeligible[AELSurveyData$voteeligible == "No"] <- 0
AELSurveyData$voteeligible <- as.factor(AELSurveyData$voteeligible)
levels(AELSurveyData$voteeligible)
summary(AELSurveyData$voteeligible)

# I don't think I should impute for this

#discrimination, anxious
AELSurveyData$discanxious <- AELSurveyData$Q33
AELSurveyData$discanxious[AELSurveyData$discanxious=="Never"] <- 0
AELSurveyData$discanxious[AELSurveyData$discanxious == "Rarely"] <- 1
AELSurveyData$discanxious[AELSurveyData$discanxious == "Sometimes"] <- 2
AELSurveyData$discanxious[AELSurveyData$discanxious == "Often"] <- 3
AELSurveyData$discanxious <- as.factor(AELSurveyData$discanxious)
levels(AELSurveyData$discanxious)
summary(AELSurveyData$discanxious)

#discrimination, afraid
AELSurveyData$discafraid <- AELSurveyData$Q34
AELSurveyData$discafraid[AELSurveyData$discafraid=="Never"] <- 0
AELSurveyData$discafraid[AELSurveyData$discafraid == "Rarely"] <- 1
AELSurveyData$discafraid[AELSurveyData$discafraid == "Sometimes"] <- 2
AELSurveyData$discafraid[AELSurveyData$discafraid == "Often"] <- 3
AELSurveyData$discafraid <- as.factor(AELSurveyData$discafraid)
levels(AELSurveyData$discafraid)
summary(AELSurveyData$discafraid)

#discrimination, nightmares
AELSurveyData$discnight <- AELSurveyData$Q35
AELSurveyData$discnight[AELSurveyData$discnight=="Never"] <- 0
AELSurveyData$discnight[AELSurveyData$discnight == "Rarely"] <- 1
AELSurveyData$discnight[AELSurveyData$discnight == "Sometimes"] <- 2
AELSurveyData$discnight[AELSurveyData$discnight == "Often"] <- 3
AELSurveyData$discnight <- as.factor(AELSurveyData$discnight)
levels(AELSurveyData$discnight)
summary(AELSurveyData$discnight)

# discrimination, worry
AELSurveyData$discworry <- AELSurveyData$Q37
AELSurveyData$discworry[AELSurveyData$discworry=="Never"] <- 0
AELSurveyData$discworry[AELSurveyData$discworry == "Rarely"] <- 1
AELSurveyData$discworry[AELSurveyData$discworry == "Sometimes"] <- 2
AELSurveyData$discworry[AELSurveyData$discworry == "Often"] <- 3
AELSurveyData$discworry <- as.factor(AELSurveyData$discworry)
levels(AELSurveyData$discworry)
summary(AELSurveyData$discworry)

#discrimination, embarrassed
AELSurveyData$discembarrass <- AELSurveyData$Q38
AELSurveyData$discembarrass[AELSurveyData$discembarrass=="Never"] <- 0
AELSurveyData$discembarrass[AELSurveyData$discembarrass == "Rarely"] <- 1
AELSurveyData$discembarrass[AELSurveyData$discembarrass == "Sometimes"] <- 2
AELSurveyData$discembarrass[AELSurveyData$discembarrass == "Often"] <- 3
AELSurveyData$discembarrass <- as.factor(AELSurveyData$discembarrass)
levels(AELSurveyData$discembarrass)
summary(AELSurveyData$discembarrass)

#discrimination, annoyed/irritable
AELSurveyData$discannoy <- AELSurveyData$Q39
AELSurveyData$discannoy[AELSurveyData$discannoy=="Never"] <- 0
AELSurveyData$discannoy[AELSurveyData$discannoy == "Rarely"] <- 1
AELSurveyData$discannoy[AELSurveyData$discannoy == "Sometimes"] <- 2
AELSurveyData$discannoy[AELSurveyData$discannoy == "Often"] <- 3
AELSurveyData$discannoy <- as.factor(AELSurveyData$discannoy)
levels(AELSurveyData$discannoy)
summary(AELSurveyData$discannoy)

#discrimination, hypervigilance
AELSurveyData$dishyperv <- AELSurveyData$Q40
AELSurveyData$dishyperv[AELSurveyData$dishyperv=="Never"] <- 0
AELSurveyData$dishyperv[AELSurveyData$dishyperv == "Rarely"] <- 1
AELSurveyData$dishyperv[AELSurveyData$dishyperv == "Sometimes"] <- 2
AELSurveyData$dishyperv[AELSurveyData$dishyperv == "Often"] <- 3
AELSurveyData$dishyperv <- as.factor(AELSurveyData$dishyperv)
levels(AELSurveyData$dishyperv)
summary(AELSurveyData$dishyperv)

#discrimination, unsafe
AELSurveyData$discunsafe <- AELSurveyData$Q41
AELSurveyData$discunsafe[AELSurveyData$discunsafe=="Never"] <- 0
AELSurveyData$discunsafe[AELSurveyData$discunsafe == "Rarely"] <- 1
AELSurveyData$discunsafe[AELSurveyData$discunsafe == "Sometimes"] <- 2
AELSurveyData$discunsafe[AELSurveyData$discunsafe == "Often"] <- 3
AELSurveyData$discunsafe <- as.factor(AELSurveyData$discunsafe)
levels(AELSurveyData$discunsafe)
summary(AELSurveyData$discunsafe)

#discrimination, isolation
AELSurveyData$discisolate <- AELSurveyData$Q42
AELSurveyData$discisolate[AELSurveyData$discisolate=="Never"] <- 0
AELSurveyData$discisolate[AELSurveyData$discisolate == "Rarely"] <- 1
AELSurveyData$discisolate[AELSurveyData$discisolate == "Sometimes"] <- 2
AELSurveyData$discisolate[AELSurveyData$discisolate == "Often"] <- 3
AELSurveyData$discisolate <- as.factor(AELSurveyData$discisolate)
levels(AELSurveyData$discisolate)
summary(AELSurveyData$discisolate)

#discrimination, avoid
AELSurveyData$discavoid <- AELSurveyData$Q43
AELSurveyData$discavoid[AELSurveyData$discavoid=="Never"] <- 0
AELSurveyData$discavoid[AELSurveyData$discavoid == "Rarely"] <- 1
AELSurveyData$discavoid[AELSurveyData$discavoid == "Sometimes"] <- 2
AELSurveyData$discavoid[AELSurveyData$discavoid == "Often"] <- 3
AELSurveyData$discavoid <- as.factor(AELSurveyData$discavoid)
levels(AELSurveyData$discavoid)
summary(AELSurveyData$discavoid)

#discrimination, control emotions
AELSurveyData$discemotion <- AELSurveyData$Q44
AELSurveyData$discemotion[AELSurveyData$discemotion=="Never"] <- 0
AELSurveyData$discemotion[AELSurveyData$discemotion == "Rarely"] <- 1
AELSurveyData$discemotion[AELSurveyData$discemotion == "Sometimes"] <- 2
AELSurveyData$discemotion[AELSurveyData$discemotion == "Often"] <- 3
AELSurveyData$discemotion <- as.factor(AELSurveyData$discemotion)
levels(AELSurveyData$discemotion)
summary(AELSurveyData$discemotion)

#discrimination, daily functioning
AELSurveyData$discfunction <- AELSurveyData$Q45
AELSurveyData$discfunction[AELSurveyData$discfunction=="Never"] <- 0
AELSurveyData$discfunction[AELSurveyData$discfunction == "Rarely"] <- 1
AELSurveyData$discfunction[AELSurveyData$discfunction == "Sometimes"] <- 2
AELSurveyData$discfunction[AELSurveyData$discfunction == "Often"] <- 3
AELSurveyData$discfunction <- as.factor(AELSurveyData$discfunction)
levels(AELSurveyData$discfunction)
summary(AELSurveyData$discfunction)

#participation, Displayed a yard sign
#assign levels to the yard sign item so that the NA's (people not checking the box) are 0 and anything other than NA's (1-6) are a 1
AELSurveyData$yardsign <- AELSurveyData$Q12_1
AELSurveyData$yardsign[AELSurveyData$yardsign=="NA"] <- 0
AELSurveyData$yardsign[AELSurveyData$yardsign =="For a Presidential candidate's campaign"] <- 1
AELSurveyData$yardsign[AELSurveyData$yardsign == "For a candidate for another office"] <- 1
AELSurveyData$yardsign[AELSurveyData$yardsign == "For another political group or cause"] <- 1
AELSurveyData$yardsign[AELSurveyData$yardsign == "For a Presidential candidate's campaign,For a candidate for another office"] <- 1
AELSurveyData$yardsign[AELSurveyData$yardsign == "For a candidate for another office,For another political group or cause"] <- 1
AELSurveyData$yardsign[AELSurveyData$yardsign == "For a Presidential candidate's campaign,For another political group or cause"] <- 1          
AELSurveyData$yardsign[AELSurveyData$yardsign == "For a Presidential candidate's campaign,For a candidate for another office,For another political group or cause"] <- 1

#participation, Displayed a button bumper sticker
AELSurveyData$buttonbumper <- AELSurveyData$Q12_2
AELSurveyData$buttonbumper[AELSurveyData$buttonbumper=="NA"] <- 0
AELSurveyData$buttonbumper[AELSurveyData$buttonbumper=="For a Presidential candidate's campaign"] <- 1
AELSurveyData$buttonbumper[AELSurveyData$buttonbumper == "For a candidate for another office"] <- 1
AELSurveyData$buttonbumper[AELSurveyData$buttonbumper == "For another political group or cause"] <- 1
AELSurveyData$buttonbumper <- as.factor(AELSurveyData$buttonbumper)
levels(AELSurveyData$buttonbumper)
summary(AELSurveyData$buttonbumper)

#participation, Worked for a campaign/volunteered
AELSurveyData$campaignvolunteer <- AELSurveyData$Q12_3
AELSurveyData$campaignvolunteer[AELSurveyData$campaignvolunteer=="NA"] <- 0
AELSurveyData$campaignvolunteer[AELSurveyData$campaignvolunteer=="For a Presidential candidate's campaign"] <- 1
AELSurveyData$campaignvolunteer[AELSurveyData$campaignvolunteer == "For a candidate for another office"] <- 1
AELSurveyData$campaignvolunteer[AELSurveyData$campaignvolunteer == "For another political group or cause"] <- 1
AELSurveyData$campaignvolunteer <- as.factor(AELSurveyData$campaignvolunteer)
levels(AELSurveyData$campaignvolunteer)
summary(AELSurveyData$campaignvolunteer)

#participation, Attended a rally
AELSurveyData$rally <- AELSurveyData$Q12_4
AELSurveyData$rally[AELSurveyData$rally=="NA"] <- 0
AELSurveyData$rally[AELSurveyData$rally=="For a Presidential candidate's campaign"] <- 1
AELSurveyData$rally[AELSurveyData$rally == "For a candidate for another office"] <- 1
AELSurveyData$rally[AELSurveyData$rally == "For another political group or cause"] <- 1
AELSurveyData$rally <- as.factor(AELSurveyData$rally)
levels(AELSurveyData$rally)
summary(AELSurveyData$rally)

#participation, Contributed money
AELSurveyData$money <- AELSurveyData$Q12_5
AELSurveyData$money[AELSurveyData$money=="NA"] <- 0
AELSurveyData$money[AELSurveyData$money=="For a Presidential candidate's campaign"] <- 1
AELSurveyData$money[AELSurveyData$money == "For a candidate for another office"] <- 1
AELSurveyData$money[AELSurveyData$money == "For another political group or cause"] <- 1
AELSurveyData$money <- as.factor(AELSurveyData$money)
levels(AELSurveyData$money)
summary(AELSurveyData$money)

# Test internal reliability for the TSDS
discrimination.index<- data.frame(AELSurveyData$dishyperv, AELSurveyData$discworry, AELSurveyData$discisolate, AELSurveyData$discemotion, AELSurveyData$discnight, AELSurveyData$discunsafe, AELSurveyData$discavoid, AELSurveyData$discanxious, AELSurveyData$discafraid, AELSurveyData$discfunction, AELSurveyData$discembarrass, AELSurveyData$discannoy)

library(dplyr)

discrimination.index.numeric <- discrimination.index %>% mutate_if(is.factor, as.numeric)

discrimination.index.mean <- discrimination.index.numeric%>% 
  mutate(average = rowMeans(across(starts_with("AELSurveyData")) , na.rm=T))

discrimination.index.mean%>%
  mutate(average = rowMeans(discrimination.index.numeric[,-1]), na.rm=T)

#calculate Cronbach's alpha to ensure items are related
#disc alpha = 0.922
install.packages("ltm")
library(ltm)
disc.alpha<-cronbach.alpha(discrimination.index, na.rm = TRUE)

#Yard sign
participation.yardsign<- data.frame(AELSurveyData$ID, AELSurveyData$yardsign)
participation.yardsign.numeric <- participation.yardsign %>% mutate_if(is.factor, as.numeric)

#Button or bumper sticker
participation.buttonbumper<- data.frame(AELSurveyData$ID, AELSurveyData$buttonbumper)
participation.buttonbumper.numeric <- participation.buttonbumper %>% mutate_if(is.factor, as.numeric)

#Volunteered for campaign
participation.campaignvolunteer<- data.frame(AELSurveyData$ID, AELSurveyData$campaignvolunteer)
participation.campaignvolunteer.numeric <- participation.campaignvolunteer %>% mutate_if(is.factor, as.numeric)

#Went to a rally
participation.rally<- data.frame(AELSurveyData$ID, AELSurveyData$rally)
participation.rally.numeric <- participation.rally %>% mutate_if(is.factor, as.numeric)

#Gave money
participation.money<- data.frame(AELSurveyData$ID, AELSurveyData$money)
participation.money.numeric <- participation.money %>% mutate_if(is.factor, as.numeric)

#combine the data frames for discrimination trauma symptoms and displaying a yard sign
total <- merge(discrimination.index.mean, participation.yardsign.numeric,by="AELSurveyData.ID")
#plot trauma symptoms and displaying a yard sign
plot(total$average, total$AELSurveyData.yardsign)
cor(total$average, total$AELSurveyData.yardsign)

#combine the data frames for discrimination trauma symptoms and attending a rally
total <- merge(discrimination.index.mean, participation.rally.numeric,by="AELSurveyData.ID")
#plot trauma symptoms and going to a rally
plot(total$average, total$AELSurveyData.rally)

#combine the data frames for discrimination trauma symptoms and displaying a button or bumper sticker
total <- merge(discrimination.index.mean, participation.buttonbumper.numeric,by="AELSurveyData.ID")
#plot trauma symptoms and displaying a button or bumper sticker
plot(total$average, total$AELSurveyData.buttonbumper)

#combine the data frames for discrimination trauma symptoms and worked on a campaign/volunteered
total <- merge(discrimination.index.mean, participation.campaignvolunteer.numeric,by="AELSurveyData.ID")
#plot trauma symptoms and worked on a campaign/volunteered
plot(total$average, total$AELSurveyData.campaignvolunteer)

#combine the data frames for discrimination trauma symptoms and donating money
total <- merge(discrimination.index.mean, participation.money.numeric,by="AELSurveyData.ID")
#plot trauma symptoms and contributing money
plot(total$average, total$AELSurveyData.money)


```

Here are our covariates of interest

```{r}
# For some reason there's an error message saying that 'age' doesn't exist even though I changed the variable name from Q50 to age earlier in the code. In order to produce a summary, I have to put Q50 instead of age
covs <- c("Q50", "gender", "race", "ethnicity", "voteeligible", "citizenship")
summary(AELSurveyData[, covs])
```

Now, we use 'DeclareDesign'to figure out which estimator to use

```{r}
# Treat the data like a population, then re-sample from the population to assess the estimators
# Stratify based on race. This way, you'll be able to see if there were differences between whites and non-whites in the sample
pop <- declare_model(AELSurveyData)
sampling_plan <- declare_sampling(
  S = strata_rs(strata = race),
  legacy = FALSE
)
estimand1 <- declare_inquiry(diffprop = coef(lm(lastelection ~ discrimination))[[2]], label = "diffprop")
estimand2 <- declare_inquiry(logit = coef(glm(lastelection ~ discrimination, family = binomial), label = "logit")[[2]])
design <- pop + estimand1 + estimand2 + sampling_plan
## Looks like by default it samples half the people within each stratum
set.seed(150)
resampdata <- draw_data(design)
table(resampdata$race)
mean(resampdata$lastelection)
resampdata1 <- draw_data(design)
mean(resampdata1$lastelection)
estimator1 <- declare_estimator(lastelection ~ discrimination, .method = lm_robust, se_type = "classical", label = "est1")
estimator2 <- declare_estimator(lastelection ~ discrimination, .method = lm_robust, clusters = race, se_type = "CR0", label = "est2")
estimator3 <- declare_estimator(lastelection ~ discrimination, .method = glm, family = binomial, label = "logit")
des_plus_est <- design + estimator1 + estimator2 + estimator3
str(des_plus_est)
sims <- simulate_design(des_plus_est, sims = c(1, 1, 1, 10, 1, 1, 1))
head(sims)
sims %>%
  group_by(inquiry, estimator) %>%
  summarize(mean(estimate), unique(estimand))
diagnosis <- diagnose_design(des_plus_est, bootstrap_sims = 0, sims = c(1, 1, 1, 100, 1, 1, 1))
diagnosis
## diag_sims <- diagnosis$simulations_df
```

Now we do full matching based on mahalanobis distance

```{r}

#First, calculate the mahalanobis distance
match_fmla <- reformulate(covs,response="discrimination")

wrkdat <- fill.NAs(discrimination ~ age + gender + race + ethnicity + voteeligible + citizenship, data = AELSurveyData)
mhdist <- match_on(discrimination ~ race + ethnicity,
  within = exactMatch(discrimination ~ AELSurveyData$ID, data = AELSurveyData),
  data = AELSurveyData,
  method = "rank_mahalanobis") #But you can't do a rank-based mahalanobis test if the outcome variable is binary, so does this mean I can't match based on mahalanobis distance as a way of testing the first hypothesis (discrimination predicts voting in the last election)?
summary(mhdist)

# Now let's do full matching based mahalanobis distance

fmatching <- fullmatch(mhdistCal, omit.fraction = omit_frac, min.controls=.5, tol=.0001, solver="LEMON",data = AELSurveyData)
summary(fmatching,min.controls=0,max.controls=Inf)
AELSurveyData$fmatching <- factor(fmatching)
with(AELSurveyData,table(is.na(fmatching), AELSurveyData$ID ,exclude=c()))
res1 <- lm_robust(lastelection ~ discrimination, fixed_effects = ~fmatching, data = AELSurveyData, subset = !is.na(AELSurveyData$fmatching))
res1

AELSurveyData$fmatching <- factor(fmatching)
baltest <- balanceTest(lastelection ~ race + gender + strata(fmatching), data = AELSurveyData)
zapsmall(baltest$results[, , "fmatching"])
zapsmall(baltest$overall[, ])

```
This next part is the sensitivity analysis portion. Matching can be done using mahalanobis distance to calculate how far apart observations are in a multivariate space, but another option is matching based on propensity scores as a way to weight covariates that are predictive of racial discrimination (e.g. race, ethnicity) to give a linear combination of covariates. There is also the option of doing pair matching (i.e. matching respondents who report discrimination to those who do not) or full matching (i.e. matching one treated respondent to one or more non-treated respondents). I intend to do full matching to ensure that fewer observations need to be dropped, but matching based on mahahalanobis distance or propensity scores is more complicated. The data is observational, therefore it could be sensitive to confounding variables. Arguably, the best way to determine how to match respondents lies in sensitivity analysis, which will reveal which matching method is least vulnerable to confounders.
Here, I am essentially using sensitivity analysis as a comparative robustness check to pick a matching method.
```{r}
# And now we do sensitivity analysis for full matching based on mahalanobis distance
res1_lm <- lm(lastelection ~ discrimination+fmatching, data = AELSurveyData, subset = !is.na(AELSurveyData$fmatching))
stopifnot(all.equal(coef(res1)[["discrimination"]],coef(res1_lm)[["discrimination"]]))
sens1 <-  sensemakr(model = res1_lm, treatment = "discrimination")
sens_analysis_1 <- sensemakr(estimate = coef(res1)[["discrimination"]], se = res1$std.error[["discrimination"]], treatment = "discrimination", dof = res1$df)
summary(sens_analysis_1)
plot(sens_analysis_1)
# shows adjusted treatment effect, which includes unobserved confounders
# sensitivity analysis tells you what the treatment effect is both with and without the unobserved confounders
```

This next bit uses pair matching for mahalanobis distance

```{r}
# Match by pairs

pmatching <- pairmatch(mhdist, remove.unmatchables = TRUE, data = AELSurveyData)
summary(pmatching)

AELSurveyData$pmatching <- factor(pmatching)
baltest <- balanceTest(lastelection ~ race + gender + strata(pmatching), data = AELSurveyData)
zapsmall(baltest$results[, , "pmatching"])
zapsmall(baltest$overall[, ])

```

Now let's do sensitivity analysis for the pair matching

```{r}
res2_lm <- lm(lastelection ~ discrimination+pmatching, data = AELSurveyData, subset = !is.na(AELSurveyData$pmatching))
stopifnot(all.equal(coef(res2)[["discrimination"]],coef(res2_lm)[["discrimination"]]))
sens1 <- sensemakr(model = res2_lm, treatment = "discrimination")

sens_analysis_2 <- sensemakr(estimate = coef(res2)[["discrimination"]], se = res2$std.error[["discrimination"]], treatment = "discrimination", dof = res2$df)

summary(sens_analysis_2)
plot(sens_analysis_2)

```

Given that both hypotheses are uni-directional, I intend to use a one-tailed t-test; I will compare rates of political participation between those who report discrimination and those who do not
The alpha level will be set at 0.05 and I will reject the null hypothesis if the p-value is greater than 0.05.
Since the study contains more than one hypothesis and will require more than one test, I plan to adjust for multiple comparisons using Bonferroni’s correction

Here, we can visualize the distribution of non-electoral participation scores with a boxplot and then calculate Bonferroni's correction.

```{r}
boxplot(rally ~ discrimination.index,
        data = AELSurveyData,
        main = "Non-Electoral Participation by Racial Trauma",
        xlab = "Racial Trauma",
        ylab = "Non-Electoral Participation 1",
        col = "steelblue",
        border = "black")

#fit the one-way ANOVA model
model <- aov(discrimination.index.numeric ~ rally, data = AELSurveyData)

#view model output
summary(model)

#perform pairwise t-tests with Bonferroni's correction
pairwise.t.test(AELSurveyData$rally, discrimination.index.numeric, p.adjust.method="bonferroni")

```

This is a randomization-based permutation test

```{r}
# Generating data
set.seed(150)
d <- as.data.frame(cbind(rnorm(1:20, 500, 50), c(rep(0, 10), rep(1, 10))))


#Difference in means
original <- diff(tapply(lastelection, discrimination, mean))
mean(lastelection[treatment==1])-mean(lastelection[discrimination==0])

#Permutation test
permutation.test <- function(discrimination, lastelection, n){
  distribution=c()
  result=0
  for(i in 1:n){
    distribution[i]=diff(by(lastelection, sample(discrimination, length(treatment), FALSE), mean))
  }
  result=sum(abs(distribution) >= abs(original))/(n)
  return(list(result, distribution))
}

test1 <- permutation.test(discrimination, lastelection, 10000)
hist(test1[[2]], breaks=50, col='grey', main="Permutation Distribution", las=1, xlab='')
abline(v=original, lwd=3, col="red")

test1[[1]]


#Compare to t-test
t.test(lastelection~treatment)
