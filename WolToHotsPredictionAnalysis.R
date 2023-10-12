# Title: WolToHotsPredictionAnalysis.R
#
# Project: StarTransfer
#
# Purpose: This code creates simple linear regression models for each player, 
#   then compares the intercept of HOTS data to the predicted intercept based on 
#   Wings of Liberty (WOL) data.
#
# Author: Joe Thompson - updated Apr 25 2021
#   Reviewed: Justin O'Camb - June 14, 2021
#   Verified: Robin Barrett - May 24, 2021
#   Reverified: Robin Barrett - Jun 26, 2021
#
# Aug 22nd change: Added power for correlation test
#   Reviewed: Justin O'Camb 09/12/2021
#
# Usage: So long as the "Current Data" folder containing the wolvhos_OutputTable.txt is in the working directory, this script will run on its own.
#
# Notable Output: Prints graphs of each player's diagnostics and results to folder this code is saved in.

#### libraries ####

# install.packages('tidyverse')
require(tidyverse)
library(tidyr)
library(dplyr)

# install.packages('ggplot2')
library(ggplot2)

# install.packages('grid')
library(grid)

# install.packages('pwr')
library('pwr')

library(readr)

# This sets the working directory to the location of this script
wd = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

wolVhos_OutputTable <- read_csv("Data/wolVhos_OutputTable.txt")


#### Preamble ####

#Training is defined as Wol games in the Version analysis and HOTS is the test
wolVhos_OutputTable$IsTraining=as.factor(wolVhos_OutputTable$IsWoL)
PerformanceData=wolVhos_OutputTable
nrow(PerformanceData)


#create container to store info on all dropped data
Reason=as.character()
Dropn=as.integer()
DropStorage=data.frame(Reason,Dropn)

#create containers to store extra diagnostic information
OutputData=data.frame()


#Prep data analysis file
Wol_To_Hots_PredictionData=data.frame()

#get unique players
UniquePlayers=unique(PerformanceData$PlayerID)
for (i in UniquePlayers)
{
  
  #get current player data
  Currplayer=PerformanceData[PerformanceData$PlayerID==i,]
  
  
  #Insist on at least 20 WOL and HOTS
  #Insist on at least 20 Hots games
  GameCountCheck=sum(Currplayer$Expansion_level=='Heart of the Swarm')>20&sum(Currplayer$Expansion_level=='Wings of Liberty')>20
  if(!GameCountCheck)
  {
    Dropn=nrow(Currplayer)
    Reason=paste('player,', i, ' dropped for having too few Hots or Wol games')
    #this player is to be dropped. Log this
    DropStorage=rbind(DropStorage, data.frame(Reason,Dropn))
  }
  
  if(GameCountCheck)
  {
    #Some people swap versions from HOTS back to Wol, we drop those Wol games.
    #Find first HOTs game for this player
    FirstHotsDate=min(Currplayer$date_time[Currplayer$IsHoS==1])
    
    #drop any Wol games after FirstHotsDate. Do this by dropping anything that is not a HOTS game or is something before the first hots date (i.e., WOL)
    #log games to be dropped
    NumberOfLateWolGames=sum(Currplayer$date_time>FirstHotsDate&Currplayer$IsWoL==1)
    
    if (NumberOfLateWolGames>0)
    {
      
      Reason=paste('player,', i, ' has', NumberOfLateWolGames, ' late Wol games dropped')
      Dropn=NumberOfLateWolGames
      #this player is to be dropped. Log this
      DropStorage=rbind(DropStorage, data.frame(Reason,Dropn))
    }
    
    #Reinitialize current player by excluding any late WOL games
    Currplayer=Currplayer[Currplayer$date_time<=FirstHotsDate|Currplayer$IsHoS,]
    
    #Now calculate the number of days from the last game
    Currplayer$DaysBetweenGame = as.numeric(difftime(Currplayer$date_time, lag(Currplayer$date_time, n=1L), units = 'days'))
    
    
    #Build Regression for Wol & Hots seperately
    CurrHOTsData=Currplayer[Currplayer$Expansion_level=='Heart of the Swarm',]
    CurrWOLsData=Currplayer[Currplayer$Expansion_level=='Wings of Liberty',]
    
    HOTSModel=lm(Transformed_fal2League~Any_XP, data=CurrHOTsData)
    WOLModel=lm(Transformed_fal2League~Any_XP, data=CurrWOLsData)
    
    HOTSModel$coefficients
    WOLModel$coefficients
    
    #Use the Any_XP data from the earliest HOTs games, and make a prediction of this using the WOL model
    DataToPredict=CurrHOTsData[CurrHOTsData$Any_XP==min(CurrHOTsData$Any_XP),]
    Prediction.WOL=predict.lm(object=WOLModel,DataToPredict)
    Prediction.HOTS=predict.lm(object=HOTSModel,DataToPredict)
    
    #Get standard errors too.
    PredictionSE.WOL=as.numeric(predict.lm(object=WOLModel,DataToPredict,se.fit = TRUE)[2])
    PredictionSE.HOTs=as.numeric(predict.lm(object=HOTSModel,DataToPredict,se.fit = TRUE)[2])
    
    #Record output data
    Residual=Prediction.HOTS-Prediction.WOL
    
    #Find number of WolGames
    Wol_n_i=nrow(CurrWOLsData)
    
    #Export this information to a data frame
    Wol_To_Hots_PredictionData=rbind(Wol_To_Hots_PredictionData, cbind(i, Prediction.WOL, Prediction.HOTS, PredictionSE.WOL, PredictionSE.HOTs, Residual, Wol_n_i))
    
    #Prep scatter
    fig=ggplot(Currplayer, aes(x=Any_XP, y=Transformed_fal2League, color=Expansion_level)) +
      geom_point() + 
      geom_smooth(method=lm) + ylim(1,8) + geom_point(aes(size=Currplayer$DaysBetweenGame))
    
    #Print
    pdf(paste('HotsFigures Player ', i, '.pdf', sep=''))
    print(fig)
    dev.off()
    
    #Print diagnostics for HOTS and WOL model
    #Set to 4 figs per page
    par(mfrow = c(2, 2))
    
    pdf(paste('HotsFigures Player ', i, 'Diagnostics_HOTS.pdf', sep=''))
    print(plot(HOTSModel))
    dev.off()
    
    pdf(paste('HotsFigures Player ', i, 'Diagnostics_WOL.pdf', sep=''))
    print(plot(WOLModel))
    dev.off()
    
    #set to 1 fig per page
    par(mfrow = c(1, 1))
  }
}  

hist(Wol_To_Hots_PredictionData$Prediction.WOL)
hist(Wol_To_Hots_PredictionData$Prediction.HOTS)
hist(Wol_To_Hots_PredictionData$Residual)


#Do power calculations for a one sample t.test
power.t.test(nrow(Wol_To_Hots_PredictionData), delta=1, sd(Wol_To_Hots_PredictionData$Residual), 0.05, type='one.sample')
# Calculates a Power of 1 for d = 1.5755041, and 0.7822760 for d=0.5 which acceptable


#Build power plot
power.matrix <- matrix(0, nrow = 101, ncol = 2)
colnames(power.matrix) <- c('delta','power')
row.index <- 1
for (D in seq(from=0, to=1, by=.01)) {
  t.power <- power.t.test(nrow(Wol_To_Hots_PredictionData),
                          delta=D, sd(Wol_To_Hots_PredictionData$Residual),
                          0.05, type='one.sample')
  
  power.matrix[row.index,1] <- D
  power.matrix[row.index,2] <- t.power$power
  
  row.index <- row.index+1
}

plot(power.matrix[,1], power.matrix[,2])


#Perform t.test
t.test(Wol_To_Hots_PredictionData$Residual)  

#Look for a correlation between WoL skill and residual
plot(Wol_To_Hots_PredictionData$Wol_n_i, Wol_To_Hots_PredictionData$Residual)
plot(lm( Wol_To_Hots_PredictionData$Residual~Wol_To_Hots_PredictionData$Wol_n_i))

#Diagnostics: 
plot(Wol_To_Hots_PredictionData$Wol_n_i[c(-6,-25)], Wol_To_Hots_PredictionData$Residual[c(-6,-25)])
plot(lm( Wol_To_Hots_PredictionData$Residual[c(-6,-25)]~Wol_To_Hots_PredictionData$Wol_n_i[c(-6,-25)]))

cor.test(Wol_To_Hots_PredictionData$Wol_n_i, Wol_To_Hots_PredictionData$Residual)
cor.test(Wol_To_Hots_PredictionData$Wol_n_i[c(-6,-25)], Wol_To_Hots_PredictionData$Residual[c(-6,-25)])

#Power for correlational test
pwr.r.test(n=32, r=.5, sig.level=0.05, alternative='two.sided')



