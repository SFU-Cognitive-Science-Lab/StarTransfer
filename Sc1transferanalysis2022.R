#
# StarTransfer sc1 Analysis Script
#
# Author: Robin C. A. Barrett
#
# Purpose: See if sc1 experience impacts sc2 performance
# Expertise: Any_xp
#
# Reviewed: Joe Thompson
# Verified: Justin O'Camb
#
# Inputs: So long as the "Data" folder containing the OnRaceOffRace_OutputTable.txt is in the working directory, this script will run on its own.
#
# Outputs: Results for the SC1 transfer analysis. There is no reliably detectable correlation between SC1 experience and
#   performance in the first 3 games of SC2
#


### libraries ####

#install.packages('tidyverse')
library(tidyverse)
#install.packages('knitr')
require(knitr)
#install.packages('readr')
library(readr)
#install.packages('lme4')
library(lme4)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('dplyr')
library(dplyr)
#install.packages('DT')
library(DT)
#install.packages('xtable')
library(xtable)
#install.packages('finalfit')
library(finalfit)


## SC1 Analysis
# 
# This script is intended to analyze the impact of Starcraft 1 experience on the first 3 1v1 games a player has for Starcraft 2
# 
# This analysis will be conducted using a simple correlational test between SC1 experience and the avergae FAL for the first 3 1v1 games played within the first 25 games of SC2 overall.
# 
# Grab Data:

# This sets the working directory to the location of this script
wd = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

PerformanceData <- read_csv("Data/OnRaceOffRace_OutputTable.txt") # path based on github location 

#set PlayerID as factor
PerformanceData$PlayerID=as.factor(PerformanceData$PlayerID)


# Set the exclusion criteria
DropInsufficientData=1
DropAnyXPifover=25
HowMany1v1Games=3


## Apply the exclusion criteria and examine outputs

UniquePlayerID=unique(PerformanceData$PlayerID)

# initialize variables to store data and loop through players
PerformanceDataUSED=data.frame()
AnyXPforGames=data.frame()
PerformanceDataTable = as_tibble(PerformanceData)
numberof1v1games=data.frame()
DroppedPlayers=data.frame(matrix(ncol = 2, nrow = 0))

# Sort by Any_xp for ease of viewing individual game rows 
PerformanceDataTable=PerformanceDataTable[order(PerformanceDataTable$Any_XP & PerformanceDataTable$Any_XP),]

for(i in UniquePlayerID)
{
  CurrentPlayerData = filter(PerformanceDataTable, PlayerID == i)
  CurrentPlayerDataFirst = slice_head (CurrentPlayerData, n = HowMany1v1Games)
  
  # Get the AnyXP of when the last game selected was played (eg. the nth 1v1 game may be the 23rd game ever played by that player)
  maxanyXP = max(CurrentPlayerDataFirst$Any_XP)
  
  # Record for verification: how many xp are required to achieve enough 1v1 matches, and how many 1v1 games do they have in total.
  AnyXPforGames=rbind(AnyXPforGames, data.frame(i, maxanyXP, nrow(CurrentPlayerDataFirst))) 
  
  # check to make sure the player enough 1v1 games
  if (count(CurrentPlayerDataFirst)==HowMany1v1Games)
  {
    # then check if the total number of games required to reach HowMany1v1Games is less than threshold
    if (max(CurrentPlayerDataFirst$Any_XP) <= DropAnyXPifover)
    {
      #if all conditions are met save the player's data
      PerformanceDataUSED=rbind(PerformanceDataUSED, CurrentPlayerDataFirst) 
    }
    else
    {
      # If it takes too many games to reach enough 1v1 games, we drop that player
      TempDat=t(c(i, "Too many games required to reach desired number of 1v1 matches"))
      names(TempDat)=names(DroppedPlayers)
      DroppedPlayers=rbind(DroppedPlayers,TempDat)
    }
    
  }
  else # If there are not enough 1v1 games in total, we drop that player
  {
    
    DroppedPlayers=rbind(DroppedPlayers, c(i, "Less than enough 1v1 matches in total"))
  }
}

PerformanceData=PerformanceDataUSED

#need to drop old levels or R will try to fit them into model
counts<-table(PerformanceData$PlayerID)
PerformanceData$PlayerID=droplevels(PerformanceData$PlayerID, exclude=names(counts[counts==0]))

# Format and Display Dropped Players and Reason for Dropping
colnames(DroppedPlayers) <- c('PlayerID Dropped', 'Reason Dropped')
kable(DroppedPlayers)

# Now that we have the performance data of just the first 3 games for each player, we can find out how much sc1 xp they have

# First get the Survey Data

SurveyData <- read_csv("Data/BFLSurveyData.csv")



##CLEANING

#one level of variable was still playing sc1, so we recode that to 2013. We also set null values
SurveyData$sc1_stop[SurveyData$sc1_stop=='Still Playing SC1']='2013'
SurveyData$sc1_stop[SurveyData$sc1_stop=='Never Played SC1']='0'

#some participants did not answer the sc1start/stop question because they did not play. Recode these as 0.
SurveyData$sc1_stop[SurveyData$sc1_stop=='please choose' & SurveyData$sc1_experience=='No']=0

#recode anyone who just did not answer question as NULL
SurveyData$sc1_stop[SurveyData$sc1_stop=='please choose']='NULL'

# Any participant who did not play SC1 can be recoded as having 0 years experience
SurveyData$sc1_begin[SurveyData$sc1_begin=='Never Played SC1']='0'

# some participants did not answer the sc1start/stop question because they did not play. Recode these as 0.
SurveyData$sc1_begin[SurveyData$sc1_begin=='please choose' & SurveyData$sc1_experience=='No']=0

# recode anyone who just did not answer question as NULL
SurveyData$sc1_begin[SurveyData$sc1_begin=='please choose']='NULL'

# check work to see if any NAs present
unique(SurveyData$sc1_stop)
unique(SurveyData$sc1_begin)

# check for errors when converting to numeric
summary(as.numeric(SurveyData$sc1_stop))
summary(as.numeric(SurveyData$sc1_begin))

options(warn=1)
SurveyData$sc1_begin=as.numeric(SurveyData$sc1_begin)
SurveyData$sc1_stop=as.numeric(SurveyData$sc1_stop)


# drop NA values, report number of NA values
warning(paste(sum(is.na(SurveyData$sc1_begin)==TRUE), ' NAs in begin data'))
warning(paste(sum(is.na(SurveyData$sc1_stop)==TRUE), ' NAs in stop data'))
SurveyData=SurveyData[is.na(SurveyData$sc1_begin)==FALSE,]
SurveyData=SurveyData[is.na(SurveyData$sc1_stop)==FALSE,]

# check work to see if any NAs remaining
unique(SurveyData$sc1_stop)
unique(SurveyData$sc1_begin)



## Merge With Performance Data and Filter Dropped Players
Data=merge(PerformanceData,SurveyData, by=c("PlayerID"))

## Log players who do not have survey data
PerformanceData$InSurveyData <- c(PerformanceData$PlayerID %in% SurveyData$PlayerID)
PerformanceData[row.names(unique(PerformanceData[,c("PlayerID", "InSurveyData")])),]

surveycut <- PerformanceData[PerformanceData$InSurveyData == 'FALSE', ]

# Get Player Ids of missing players and attach reason for dropping
cutplayers = data.frame(cbind(as.character(unique(surveycut$PlayerID)), rep(c("not in survey data"),each=length(unique(surveycut$PlayerID)))))
# convert to dataframe
names(cutplayers)<- names(DroppedPlayers)

DroppedPlayers = rbind(DroppedPlayers,cutplayers)

#calculate number of sc1 years 
Data$numberofsc1years=Data$sc1_stop-Data$sc1_begin

# Now that we've filtered and merged the datasets, let's look at the counts of experience levels

hist(Data$numberofsc1years)


## Now we can aggregate the data to get just one row per player.

# Filter to just the columns we need
DataForAnalysis=Data[ ,c(1,5,6,7,22)]


# Gather UniquePlayerID that survived filtration
UniquePlayerID=unique(DataForAnalysis$PlayerID)

AggregatedDataForAnalysis = data.frame(matrix(ncol = 4, nrow = 0))

# Drop empty levels
UniquePlayerID=droplevels(UniquePlayerID)


for(i in UniquePlayerID)
{
  # For each player get the playerid
  CurrentPlayerData = filter(DataForAnalysis, PlayerID == i)
  # Get the mean for their FALs
  meanFAL = mean(CurrentPlayerData$falclassic)
  # Get the mean for their average league-equivalent score
  meanFALLeague = mean(CurrentPlayerData$Transformed_fal2League)
  # Get the number of years reported having played StarCraft 1
  SC1xp = unique(CurrentPlayerData$numberofsc1years)
  
  # Add to output matrix
  AggregatedDataForAnalysis = rbind(AggregatedDataForAnalysis, data.frame(i, as.numeric(meanFAL), as.numeric(meanFALLeague), as.numeric(SC1xp)))

}

# Every player should now only have 1 row of data per person.
# Aggregated Data can now be correlated and visualized
names(AggregatedDataForAnalysis) <- c('PlayerID', 'avg_FAL', 'avg_LeagueTransformed', 'SC1_xp')
AggregatedDataForAnalysis <- lapply(AggregatedDataForAnalysis,as.numeric)

## First, let's look at the sumary data
summary(AggregatedDataForAnalysis$avg_FAL)
summary(AggregatedDataForAnalysis$avg_LeagueTransformed)
summary(AggregatedDataForAnalysis$SC1_xp)

### Assumptions Checking

## Normality
hist(AggregatedDataForAnalysis$avg_FAL)
hist(AggregatedDataForAnalysis$avg_LeagueTransformed)
hist(AggregatedDataForAnalysis$SC1_xp)


## Linearity

# Graph avg_FAL by SC1_xp
plot(jitter(`avg_FAL`) ~ `SC1_xp`, data = AggregatedDataForAnalysis)

# Graph LeagueTransformed by SC1_xp
plot(jitter(`avg_LeagueTransformed`) ~ `SC1_xp`, data = AggregatedDataForAnalysis)


# Run linear regression function so we can check assumptions

# First with FAL
SC1.avg_FAL.lm <- lm(avg_FAL ~ SC1_xp, data = AggregatedDataForAnalysis)

summary(SC1.avg_FAL.lm)


## Check for Homoscedasticity
par(mfrow=c(2,2))
plot(SC1.avg_FAL.lm)
par(mfrow=c(1,1))


# Now with League
SC1.League.lm <- lm(avg_LeagueTransformed ~ SC1_xp, data = AggregatedDataForAnalysis)

summary(SC1.League.lm)


# Check for Homoscedasticity
par(mfrow=c(2,2))
plot(SC1.League.lm)
par(mfrow=c(1,1))


# Visualize
FAL.graph<-ggplot(as.data.frame(AggregatedDataForAnalysis), aes(x=`SC1_xp`, y=`avg_FAL`))+
  geom_point() + geom_smooth(method="lm", col="black") +
  theme_bw() +
  labs(title = "Average First Action Latency as a function of Years Played in StarCraft 1",
       x = "StarCraft 1 Experience (Years)",
       y = "Average First Action Latency (First 3 Games of StarCraft 2)")
FAL.graph

League.graph<-ggplot(as.data.frame(AggregatedDataForAnalysis), aes(x=`SC1_xp`, y=`avg_LeagueTransformed`))+
  geom_point() + geom_smooth(method="lm", col="black") + 
  theme_bw() +
  labs(title = "Average League Equivalent as a function of Years Played in StarCraft 1",
       x = "StarCraft 1 Experience (Years)",
       y = "League Equivalent Score (First 3 Games of StarCraft 2)")
League.graph

# Correlation Test League by SC1
cor.test(AggregatedDataForAnalysis$avg_LeagueTransformed, AggregatedDataForAnalysis$SC1_xp)

# Correlation Test FAL by SC1
cor.test(AggregatedDataForAnalysis$avg_FAL, AggregatedDataForAnalysis$SC1_xp)

##### Neither test is significant, and so we conclude that there is not a reliably detectable corr.


library('pwr')
pwr.r.test(n=93, r=.5, sig.level=.05)
