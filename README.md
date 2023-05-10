# PS531PreAnalysisPlan
```{r setup, include=FALSE}
# install.packages("knitr")
# install.packages("skimr")
library(knitr)
library(readr)
library(tidyverse)
library(skimr)
Fall_2022_Survey_Lewis_November_24_2022_14_45_csv <- read_csv("C:/Users/Alesha/Downloads/Fall 2022 - Survey (Lewis)_November 24, 2022_14.45.csv.zip")
AELSurveyData <- Fall_2022_Survey_Lewis_November_24_2022_14_45_csv
View(AELSurveyData)
skim(AELSurveyData)
AELSurveyData<-AELSurveyData[-c(1,2),]
AELSurveyData<-AELSurveyData[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13)]
AELSurveyData$ID=1:390

# Q49 = gender
# Q50 = age
# Q51 = race
# Q52 = ethnicity ("are you Hispanic or Latino?")
# Q46 = citizenship ("are you a US citizen?")
# Q8 = voting eligibility ("were you eligible to vote in the US federal election in 2020?")
# Q9 = voting in the last election ("did you vote in the US federal election in 2020?")


#race
AELSurveyData$race <- AELSurveyData$Q51
AELSurveyData$race[AELSurveyData$race=="NA's"] <- NA
AELSurveyData$race[AELSurveyData$race == "African American/Black"] <- 1
AELSurveyData$race[AELSurveyData$race == "Asian"] <- 2
AELSurveyData$race[AELSurveyData$race == "Native American/Alaska Native"] <- 3
AELSurveyData$race[AELSurveyData$race == "Caucasian/White"] <- 4
AELSurveyData$race[AELSurveyData$race == "Multiple/Mixed"] <- 5
AELSurveyData$race[AELSurveyData$race == "Other (please specify)"] <- 6
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
# impute missing values with mean
mean_covariate <- mean(AELSurveyData$Q50, na.rm = TRUE)
AELSurveyData$Q50[is.na(AELSurveyData$Q50)] <- mean_covariate

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
# Remove the missing values
na.omit(AELSurveyData$lastelection)

# Convert binary outcome variable (voting in the last election) to a numeric variable
elect_numeric <- as.numeric(AELSurveyData$lastelection)
# Print the numeric variable
elect_numeric

#discrimination, yes or no
AELSurveyData$discrimination <- AELSurveyData$Q32
AELSurveyData$discrimination[AELSurveyData$discrimination=="NA's"] <- NA
AELSurveyData$discrimination[AELSurveyData$discrimination == "Yes"] <- 1
AELSurveyData$discrimination[AELSurveyData$discrimination == "No"] <- 0
AELSurveyData$discrimination <- as.factor(AELSurveyData$discrimination)
levels(AELSurveyData$discrimination)
summary(AELSurveyData$discrimination)

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

AELSurveyData$yardsign <- as.factor(AELSurveyData$yardsign)
levels(AELSurveyData$yardsign)
summary(AELSurveyData$yardsign)

str(AELSurveyData$yardsign)
unique(AELSurveyData$yardsign)

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

#Group all of the discrimination questions so they are one variable, Q33 through Q44 omitting the attention check question (Q36)
#Do the same with participation
#Use the code below for a simple linear regression
#figure out how to get rid of warning messages that pop up when you run the line of code below
#you can't use a categorical variable as a predictor for a numeric variable
discrimination.lastelection.glm <- glm(lastelection ~ discrimination, data=AELSurveyData, family=binomial)
summary(discrimination.lastelection.glm)

#produces an index with all TSDS items
discrimination.index<- data.frame(AELSurveyData$ID, AELSurveyData$dishyperv, AELSurveyData$discworry, AELSurveyData$discisolate, AELSurveyData$discemotion, AELSurveyData$discnight, AELSurveyData$discunsafe, AELSurveyData$discavoid, AELSurveyData$discanxious, AELSurveyData$discafraid, AELSurveyData$discfunction, AELSurveyData$discembarrass, AELSurveyData$discannoy)

discrimination.index.numeric <- discrimination.index %>% mutate_if(is.factor, as.numeric)

discrimination.index.mean <- discrimination.index.numeric%>% 
  mutate(average = rowMeans(across(starts_with("AELSurveyData")) , na.rm=T))

discrimination.index.mean%>%
mutate(average = rowMeans(discrimination.index.numeric[,-1]), na.rm=T)

#produce an index for participation
participation.index<- data.frame(AELSurveyData$Q12_1, AELSurveyData$Q12_2, AELSurveyData$Q12_3, AELSurveyData$Q12_4, AELSurveyData$Q12_5)

#calculate Cronbach's alpha to ensure items are related
#disc alpha = 0.922
#part alpha = 0.652
install.packages("ltm")
library(ltm)
disc.alpha<-cronbach.alpha(discrimination.index, na.rm = TRUE)
part.alpha<-cronbach.alpha(participation.index, na.rm = TRUE)

#Since the non-voting participation items lack internal consistency, they shouldn't be in an index
#Instead, make separate plots for each form of participation, one for voting, one for going to a rally, one for displaying a yard sign, etc.

library(dplyr)

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


# Interaction plot
library(ggplot2)
df <- data.frame(
  treatment = c(0,0,0,1,1,1),
  covariate = c(1,2,3,1,2,3),
  outcome = c(6,8,10,12,14,16)
)
iplot <- lm(lastelection ~ discrimination*Q51, data = df)
# Extract the CATE for each level of the covariate
CATE <- predict(iplot, newdata = data.frame(treatment = c(0, 0, 0, 1, 1, 1),
                                           covariate = c(1, 2, 3, 1, 2, 3)))
# Combine the old data frame and the new data frame
df2 <- cbind(df, CATE)
# Make an interaction plot, one panel for the treatment group and one for the control group
ggplot(df2, aes(x = covariate, y = outcome, color = factor(treatment)))+
  geom_line()+
  facet_wrap(~treatment)+
  geom_hline(aes(yintercept = CATE), linetype = "dashed")



# Covariates of interests (race, ethnicity, age, gender, citizenship, voting eligibility)
covs <- c("Q49", "Q50", "Q51", "Q52", "Q46", "Q8")
summary(AELSurveyData[, covs])


#Now use 'DeclareDesign'to figure out which estimator to use
# Treat the data like a population, then re-sample from the population to assess the estimators
# Stratify based on race. This way, you'll be able to see if there were differences between whites and non-whites in the sample
pop <- declare_model(AELSurveyData)
sampling_plan <- declare_sampling(
  S = strata_rs(strata = AELSurveyData$race),
  legacy = FALSE
)
estimand1 <- declare_inquiry(diffprop = coef(lm(lastelection ~ discrimination))[[2]], label = "diffprop")
estimand2 <- declare_inquiry(logit = coef(glm(lastelection ~ discrimination, family = binomial), label = "logit")[[2]])
design <- pop + estimand1 + estimand2 + sampling_plan

# Samples half the people within each stratum
set.seed(150)
resampdata <- draw_data(design)
table(resampdata$race)
#R is saying that the numeric variable I created earlier isn't numeric....Not sure why that's the case
mean(resampdata$elect_numeric)
resampdata1 <- draw_data(design)
mean(resampdata1$elect_numeric)
model1 <- declare_estimator(elect_numeric ~ discrimination, .method = lm_robust, se_type = "classical", label = "est1")
model2 <- declare_estimator(elect_numeric ~ discrimination, .method = lm_robust, clusters = race, se_type = "CR0", label = "est2")
model3 <- declare_estimator(elect_numeric ~ discrimination, .method = glm, family = binomial, label = "logit")
des_plus_est <- design + estimator1 + estimator2 + estimator3
str(des_plus_est)
sims <- simulate_design(des_plus_est, sims = c(40, 40, 40, 40, 40, 40, 40))
head(sims)
sims %>%
  group_by(inquiry, estimator) %>%
  summarize(mean(estimate), unique(estimand))
diagnosis <- diagnose_design(des_plus_est, bootstrap_sims = 0, sims = c(1, 1, 1, 100, 1, 1, 1))
diagnosis
## diag_sims <- diagnosis$simulations_df



# Matching on age and gender, mahalanobis distance
install.packages("MatchIt")
library(MatchIt)

# estimate mahalanobis score
model_mahal <- glm(discrimination ~ AELSurveyData$Q49 + AELSurveyData$Q50, data = AELSurveyData, family = "binomial")
AELSurveyData$mscore <- predict(model_mahal, type = "response")

# perform matching
matched_data <- matchit(discrimination ~ mscore, data = AELSurveyData, method = "nearest")

# calculate CATE
CATE <- mean(matched_data$lastelection[matched_data$discrimination == 1] - matched_data$lastelection[matched_data$discrimination == 0])


# And now we do sensitivity analysis for full matching based on mahalanobis distance
res1_lm <- lm(lastelection ~ discrimination+fmatching, data = AELSurveyData, subset = !is.na(AELSurveyData$fmatching))
stopifnot(all.equal(coef(res1)[["discrimination"]],coef(res1_lm)[["discrimination"]]))
sens1 <-  sensemakr(model = res1_lm, treatment = "discrimination")
sens_analysis_1 <- sensemakr(estimate = coef(res1)[["discrimination"]], se = res1$std.error[["discrimination"]], treatment = "discrimination", dof = res1$df)
summary(sens_analysis_1)
plot(sens_analysis_1)
# shows adjusted treatment effect, which includes unobserved confounders
# sensitivity analysis tells you what the treatment effect is both with and without the unobserved confounders


# Pair matching
pmatching <- pairmatch(mhdist, remove.unmatchables = TRUE, data = AELSurveyData)
summary(pmatching)

AELSurveyData$pmatching <- factor(pmatching)
baltest <- balanceTest(lastelection ~ AELSurveyData$Q49 + AELSurveyData$Q50 + strata(pmatching), data = AELSurveyData)
zapsmall(baltest$results[, , "pmatching"])
zapsmall(baltest$overall[, ])


#Sensitivity analysis for pair matching
res2_lm <- lm(lastelection ~ discrimination+pmatching, data = AELSurveyData, subset = !is.na(AELSurveyData$pmatching))
stopifnot(all.equal(coef(res2)[["discrimination"]],coef(res2_lm)[["discrimination"]]))
sens1 <- sensemakr(model = res2_lm, treatment = "discrimination")
sens_analysis_2 <- sensemakr(estimate = coef(res2)[["discrimination"]], se = res2$std.error[["discrimination"]], treatment = "discrimination", dof = res2$df)
summary(sens_analysis_2)
plot(sens_analysis_2)



# This is the balance test for full matching
set.seed(200)
AELSurveyData <- AELSurveyData %>%
  group_by(fmatching) %>%
  mutate(Z = sample(discrimination)) %>%
  as.data.frame()
## This next is necessary because dplyr strips off row.names
row.names(AELSurveyData) <- AELSurveyData$discrimination
# Test the null hypothesis and the alternative hypothesis
xb_randomized <- balanceTest(Z ~ gender + Q50 + citizenship + strata(fmatching), data = AELSurveyData)
xb_randomized$overall["fmatching",,]
xb_randomized$results[,,"fmatching"]

xb_obs <- balanceTest(discrimination ~ gender + Q50 + citizenship+strata(fmatching), data = AELSurveyData, p.adjust.method="none")
xb_obs$overall["fmatching",]
xb_obs$results[,,"fmatching"]
p.adjust(xb_obs$results[, "p", "fmatching"], method = "holm")
p.adjust(xb_obs$results[, "p", "fmatching"], method = "bonferroni")
xb_obs$results[, "p", "fmatching"]

setmeanDiffs <- AELSurveyData %>%
  group_by(fmatching) %>%
  summarise(
    diffAboveHS = mean(nhAboveHS[discrimination == 1]) - mean(nhAboveHS[discrimination == 0]),
    nb = n(),
    pb = mean(discrimination),
    nbwt = nb / nrow(AELSurveyData),
    pbwt = pb * (1 - pb) * nbwt
  )
setmeanDiffs
test_stat1 <- with(setmeanDiffs, sum(diffAboveHS * pbwt / sum(pbwt)))
test_stat2 <- lm_robust(nhAboveHS ~ nhTrt, fixed_effects = ~fmatching, data=AELSurveyData)
coef(test_stat2)



# This is the balance test for pair matching
set.seed(200)
AELSurveyData <- AELSurveyData %>%
  group_by(pmatching) %>%
  mutate(Z = sample(discrimination)) %>%
  as.data.frame()
row.names(AELSurveyData) <- AELSurveyData$discrimination
# Test the null hypothesis and the alternative hypothesis
xb_randomized <- balanceTest(Z ~ gender + Q50 + citizenship + strata(pmatching), data = AELSurveyData)
xb_randomized$overall["pmatching",,]
xb_randomized$results[,,"pmatching"]

xb_obs <- balanceTest(discrimination ~ gender + Q50 + citizenship+strata(pmatching), data = AELSurveyData, p.adjust.method="none")
xb_obs$overall["pmatching",]
xb_obs$results[,,"pmatching"]
p.adjust(xb_obs$results[, "p", "pmatching"], method = "holm")
p.adjust(xb_obs$results[, "p", "pmatching"], method = "bonferroni")
xb_obs$results[, "p", "fmatching"]

setmeanDiffs <- AELSurveyData %>%
  group_by(pmatching) %>%
  summarise(
    diffelect_numeric = mean(elect_numeric[discrimination == 1]) - mean(elect_numeric[discrimination == 0]),
    nb = n(),
    pb = mean(discrimination),
    nbwt = nb / nrow(AELSurveyData),
    pbwt = pb * (1 - pb) * nbwt
  )
setmeanDiffs
test_stat1 <- with(setmeanDiffs, sum(diffelect_numeric * pbwt / sum(pbwt)))
test_stat2 <- lm_robust(elect_numeric ~ discrimination, fixed_effects = ~pmatching, data=AELSurveyData)
coef(test_stat2)


# Permutation test
# Generating data
set.seed(150)
d <- as.data.frame(cbind(rnorm(1:20, 500, 50), c(rep(0, 10), rep(1, 10))))

#Difference in means
original <- diff(tapply(lastelection, discrimination, mean))
mean(lastelection[treatment==1])-mean(lastelection[discrimination==0])

#Permutation
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

# Bootstrapping
library(boot)
boot_mean <- function(data, i) {
  mean(data[i])
}
boot_results <- boot(data = discrimination.index.numeric, statistic = boot_mean, R = 1000)
summary(boot_results)
hist(boot_results$t, main = "Bootstrap Distribution of Mean", xlab = "Mean")


# Independent samples t-test
# Extract a subset of the data where the discrimination variable is "Yes"
discrimination_yes <- subset(AELSurveyData, discrimination == "Yes")
# Extract a subset of the data where the discrimination variable is "No"
discrimination_no <- subset(AELSurveyData, discrimination == "No")
# Create two data frames
treatment_group <- discrimination_yes
control_group <- discrimination_no

# Check normality assumption for treatment group
shapiro.test(treatment_group$elect_numeric)
# Check normality assumption for control group
shapiro.test(control_group$elect_numeric)
# Check homogeneity of variances
bartlett.test(elect_numeric ~ group, data = AELSurveyData)
# Perform t-test for treatment group
treatment_ttest <- t.test(treatment_group$elect_numeric, control_group$elect_numeric)
# Perform t-test for control group
control_ttest <- t.test(control_group$elect_numeric, treatment_group$elect_numeric)
# Interpret results for treatment group
treatment_ttest
cohens_d(treatment_group$elect_numeric, control_group$elect_numeric)
# Interpret results for control group
control_ttest
cohens_d(control_group$elect_numeric, treatment_group$elect_numeric)


# This next part is to calculate the false discovery rate
# Generate some example p-values
p_values <- c(0.6, 0.8)
# Use the p.adjust() function with the method = "fdr" argument
fdr <- p.adjust(p_values, method = "fdr")
# Print the original p-values and the adjusted p-values
cbind(p_values, fdr)


# Calculate MSE
# Define the estimator
estimator <- function(discrimination) mean(AELSurveyData$lastelection)
mse <- mean((estimator(AELSurveyData$discrimination) - AELSurveyData$lastelection)^2)


# Bootstrapping to calculate bias
library(boot)
boot_mean <- function(AELSurveyData, i) {
  mean(AELSurveyData[i])
}
boot_results <- boot(AELSurveyData = discrimination.index.numeric, statistic = boot_mean, R = 1000)
summary(boot_results)
hist(boot_results$t, main = "Bootstrap Distribution of Mean", xlab = "Mean")


# create OLS model
OLS <- lm(lastelection ~ citizenship + ethnicity, data = AELSurveyData)
# print summary of OLS model
summary(OLS)
