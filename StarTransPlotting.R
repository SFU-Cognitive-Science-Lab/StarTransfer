#StarTrans Plot Formatting

#author: Justin
#reviewed: 
#verified: 

# Created: 07/25/2021

# Purpose: Creates and formats plots for the 3 startrans analyses





#install.packages("extrafont")
library(extrafont)
#install.packages("RMySQL")
library(RMySQL)
#install.packages("readr")
library(readr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)


# This sets the working directory to the location of this script
wd = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)



### Race Analysis ###

## Get Data
Data <- read_csv("Data/OnRaceOffRace_OutputTable.txt")

#Set factors
Data$PlayerID=as.factor(Data$PlayerID)
Data$GameID=as.factor(Data$GameID)
Data$IsTraining=as.factor(Data$IsTraining)

#Scale any_xp and falclassic (in case we want to see it in units of ms)
Data$Any_XP=Data$Any_XP/100
Data$falclassic=Data$falclassic/88.5*1000

## Outlier Exclusion
Data=Data[Data$falclassic<2500,]

#Drop players with fewer than 50 games
counts<-table(Data$PlayerID)

#Drop ANY players with less than 50 games

LowDataIdx=as.numeric(names(counts[counts<50&counts>0]))
for (i in LowDataIdx)
{

  Data=Data[Data$PlayerID!=i,]
  
  
  #understand racial proportions by player
  counts<-table(Data$race,Data$PlayerID)
  counts
  
  #drop anyone without at least 30 off race games
  for (i in unique(Data$PlayerID))
  {
    #find off race games for each player
    OffRaceGames=Data$IsOffRace_Protoss[Data$PlayerID==i]+Data$IsOffRace_Zerg[Data$PlayerID==i]+Data$IsOffRace_Terran[Data$PlayerID==i]
      
    #if player has fewer than 30 offrace games, drop them
    if(sum(OffRaceGames)<30)
    {
      Data=Data[Data$PlayerID!=i,]
    }
  }
}


#Player 16 looks to be playing a lot of random so it cannot really be said that they have a dominant race. Drop them.
Data=Data[Data$PlayerID!=16,]

#Aggregate data for plots
AggregatedData=aggregate(Data$Transformed_fal2League, by=list(Data$PlayerID,Data$IsTraining),FUN=function (x) {mean(x)})
#reverse order factors so istraining=1 comes first in plot
AggregatedData$Group.2 <- factor(AggregatedData$Group.2, levels=rev(levels(AggregatedData$Group.2)))
AggregatedData <- AggregatedData[order(AggregatedData$Group.1),]

# drop unused levels
Data$PlayerID=droplevels(Data$PlayerID, exclude=names(counts[counts==0]))
Max.AnyXP <- aggregate(Data$Any_XP, by=list(Data$PlayerID),FUN=function (x) {max(x)})
Max.AnyXP <- Max.AnyXP[order(Max.AnyXP$Group.1),]

# create dataframe for scatterplot
race.PlotData <- data.frame(League.MainRace=AggregatedData$x[AggregatedData$Group.2==1],
  League.OffRace=AggregatedData$x[AggregatedData$Group.2==0], player.Any_XP=Max.AnyXP)
race.PlotData$player.Any_XP.x <- race.PlotData$player.Any_XP.x*100

# set up vectors for polygon (has to be the same length as plot data)
race.shade.x <- c(-Inf,1,2,3,4,5,6,7,8,9,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf)
race.shade.y <- c(-Inf,1,2,3,4,5,6,7,8,9,Inf,Inf,Inf,Inf,Inf,Inf,Inf,-Inf)

# change names of isTraining for easier recognition
Data$IsTraining <- as.character(Data$IsTraining)
Data$IsTraining[Data$IsTraining==0] <- 'Off Race'
Data$IsTraining[Data$IsTraining==1] <- 'Main Race'



## Make individual plot
# currently player 78

# subset data
plot.dat.race <- Data[Data$PlayerID==78,]
plot.dat.race$Any_XP <- plot.dat.race$Any_XP * 100
individual.data <- race.PlotData[race.PlotData$player.Any_XP.Group.1==78,]
tempPlotDatOff <- plot.dat.race
tempPlotDatOff[tempPlotDatOff$IsTraining=='Main Race',] <- NA

Race.PlayerPlot <- ggplot(plot.dat.race, aes(x=Any_XP, y=Transformed_fal2League, color=IsTraining)) +
  geom_point(size=1)+
  geom_smooth(method = 'lm', se=F, size = 1.25, aes(group=IsTraining), color='black')+
  geom_smooth(method = 'lm', se=F)+
  geom_smooth(method = 'lm', se=F, size = 1.25, aes(x=tempPlotDatOff$Any_XP, y=tempPlotDatOff$Transformed_fal2League), color='black')+
  geom_smooth(method = 'lm', se=F, aes(x=tempPlotDatOff$Any_XP, y=tempPlotDatOff$Transformed_fal2League), color='#EE7733')+
  labs(title=NULL, x='Game Number', y='Estimated Performance', color=NULL)+
  theme_classic()+
  theme(aspect.ratio = 1)+
  scale_colour_manual(values = c('#33BBEE', '#EE7733'))+
  ggtitle('A')+
  theme(legend.position = c(.75,.25))+
  theme(legend.background = element_rect(fill=NA))+
  theme(axis.title = element_text(size=unit(11, 'points'), family='Arial', color='black'))+
  theme(legend.title = element_text(size=unit(11, 'points'), family='Arial', color='black'))+
  theme(legend.text = element_text(size=unit(9, 'points'), family='Arial', color='black'))+
  theme(axis.text = element_text(size=unit(9, 'points'), family='Arial', color='black'))+
  theme(plot.margin = unit(c(2,2,2,2), 'points'))



## Make Main Plot
Race.Plot <- ggplot(race.PlotData, aes(x=League.MainRace, y=League.OffRace)) +
  geom_polygon(aes(x=race.shade.x, y=race.shade.y), alpha=.3)+
  geom_point(aes(x=individual.data$League.MainRace, y=individual.data$League.OffRace), color='#CC3311', size=7) +
  geom_point(aes(size=player.Any_XP.x)) +
  labs(title=NULL, x='Est. Performance (Dom. Race)',
       y='Est. Performance (Off Race)', size='Total Number of Games')+
  theme_classic()+
  scale_x_continuous(limits = c(0,9), breaks = c(2,4,6,8))+
  scale_y_continuous(limits = c(0,9), breaks = c(2,4,6,8))+
  geom_abline(intercept = 0, slope = 1, size = 1)+
  theme(aspect.ratio = 1)+
  ggtitle('B')+
  theme(legend.position = c(.45,.8))+
  theme(legend.background = element_rect(fill=NA))+
  theme(axis.title = element_text(size=unit(11, 'points'), family='Arial', color='black'))+
  theme(legend.title = element_text(size=unit(11, 'points'), family='Arial', color='black'))+
  theme(legend.text = element_text(size=unit(9, 'points'), family='Arial', color='black'))+
  theme(axis.text = element_text(size=unit(9, 'points'), family='Arial', color='black'))+
  theme(plot.margin = unit(c(2,2,2,2), 'points'))



# ## Make Histograms
# Race.Delta <- race.PlotData$League.OffRace - race.PlotData$League.MainRace
# Race.Delta.Percent <- (race.PlotData$League.OffRace / race.PlotData$League.MainRace)*100
# 
# Race.Hist <- ggplot() +
#   geom_histogram(aes(Race.Delta)) +
#   labs(title=NULL, x='Change in Performance', y='Frequency')+
#   theme_classic()+
#   theme(aspect.ratio = 1)+
#   theme(axis.title = element_text(size=unit(12, 'points'), family='Arial', color='black'))+
#   theme(legend.title = element_text(size=unit(12, 'points'), family='Arial', color='black'))+
#   theme(legend.text = element_text(size=unit(10, 'points'), family='Arial', color='black'))+
#   theme(axis.text = element_text(size=unit(10, 'points'), family='Arial', color='black'))+
#   theme(plot.margin = unit(c(2,2,2,2), 'points'))
# 
# Race.Hist.Percent <- ggplot() +
#   geom_histogram(aes(Race.Delta.Percent)) +
#   labs(title=NULL, x='Percent of Performance Retained', y='Frequency')+
#   theme_classic()+
#   theme(aspect.ratio = 1)+
#   theme(axis.title = element_text(size=unit(12, 'points'), family='Arial', color='black'))+
#   theme(legend.title = element_text(size=unit(12, 'points'), family='Arial', color='black'))+
#   theme(legend.text = element_text(size=unit(10, 'points'), family='Arial', color='black'))+
#   theme(axis.text = element_text(size=unit(10, 'points'), family='Arial', color='black'))+
#   theme(plot.margin = unit(c(2,2,2,2), 'points'))



## Make plot of change in transfer by XP
race.PlotData$Race.Delta <- race.PlotData$League.OffRace - race.PlotData$League.MainRace
individual.data <- race.PlotData[race.PlotData$player.Any_XP.Group.1==78,]

Race.XPplot <- ggplot(race.PlotData, aes(x=player.Any_XP.x, y=Race.Delta)) +
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0), fill=alpha('grey',.3))+
  geom_abline(intercept = 0, slope = 0, size = 1)+
  geom_smooth(method = 'lm', se=F, color = '#0077BB')+
  geom_point(aes(x=individual.data$player.Any_XP.x, y=individual.data$Race.Delta), color='#CC3311', size=3) +
  geom_point(size=1) +
  labs(title=NULL, x='Total Number of Games',
       y='Change in Estimated Performance')+
  theme_classic()+
  theme(aspect.ratio = 1)+
  ggtitle('C')+
  theme(axis.title = element_text(size=unit(11, 'points'), family='Arial', color='black'))+
  theme(legend.title = element_text(size=unit(11, 'points'), family='Arial', color='black'))+
  theme(legend.text = element_text(size=unit(9, 'points'), family='Arial', color='black'))+
  theme(axis.text = element_text(size=unit(9, 'points'), family='Arial', color='black'))+
  theme(plot.margin = unit(c(2,2,2,2), 'points'))









### SC1 Analysis ###

## Get Data
PerformanceData <- read_csv("Data/OnRaceOffRace_OutputTable.txt")

#set playerid as factor
PerformanceData$PlayerID=as.factor(PerformanceData$PlayerID)

# Only keep first 50 games
PerformanceData=PerformanceData[PerformanceData$Any_XP<=50,]

#need to drop old levels or R will try to fit them into model
counts<-table(PerformanceData$PlayerID)
PerformanceData$PlayerID=droplevels(PerformanceData$PlayerID, exclude=names(counts[counts==0]))

# get SC1 info from SQL
SurveyData=read_csv("Data/BFLSurveyData.csv")


## Cleaning

#one level of variable was still playing sc1, so we recode that to 2013. We also set null values
SurveyData$sc1_stop[SurveyData$sc1_stop=='Still Playing SC1']='2013'
SurveyData$sc1_stop[SurveyData$sc1_stop=='Never Played SC1']='0'
#some participants did not answer the sc1start/stop question because they did not play. Recode these as 0.
SurveyData$sc1_stop[SurveyData$sc1_stop=='please choose' && SurveyData$sc1_experience=='No']=0
#recode anyone who just did not answer question as NULL
SurveyData$sc1_stop[SurveyData$sc1_stop=='please choose']='NULL'


SurveyData$sc1_begin[SurveyData$sc1_begin=='Never Played SC1']='0'
#some participants did not answer the sc1start/stop question because they did not play. Recode these as 0.
SurveyData$sc1_begin[SurveyData$sc1_begin=='please choose' && SurveyData$sc1_experience=='No']=0
#recode anyone who just did not answer question as NULL
SurveyData$sc1_begin[SurveyData$sc1_begin=='please choose']='NULL'

SurveyData$sc1_begin=as.numeric(SurveyData$sc1_begin)
SurveyData$sc1_stop=as.numeric(SurveyData$sc1_stop)

#drop NA values
SurveyData=SurveyData[is.na(SurveyData$sc1_begin)==FALSE,]
SurveyData=SurveyData[is.na(SurveyData$sc1_stop)==FALSE,]

#MERGING
Data=merge(PerformanceData,SurveyData, by=c("PlayerID"))

#calculate number of sc1 years 
Data$numberofsc1years=Data$sc1_stop-Data$sc1_begin

# create aggro data
AggregatedData=aggregate(Data$Transformed_fal2League, by=list(Data$PlayerID),FUN=function (x) {mean(x)})
names(AggregatedData)=c('PlayerID', 'Transformed_fal2League')
SurveyData$numberofsc1years=SurveyData$sc1_stop-SurveyData$sc1_begin
MergeAggroData=merge(AggregatedData,SurveyData, by=c("PlayerID"))



## Make Plot
SC1.Plot <- ggplot(MergeAggroData, aes(x=numberofsc1years, y=Transformed_fal2League)) +
  geom_point(size=1) +
  labs(title=NULL, x='Years of SC1', y='Performance (Transformed FAL)')+
  geom_smooth(method = 'lm')+
  theme_classic()+
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16))+
  scale_y_continuous(breaks = c(-6,-4,-2,0,2,4,6,8))+
  theme(axis.title = element_text(size=unit(12, 'points'), family='Arial', color='black'))+
  theme(legend.title = element_text(size=unit(12, 'points'), family='Arial', color='black'))+
  theme(legend.text = element_text(size=unit(10, 'points'), family='Arial', color='black'))+
  theme(axis.text = element_text(size=unit(10, 'points'), family='Arial', color='black'))+
  theme(plot.margin = unit(c(2,2,2,2), 'points'))










### WoL to HotS Analysis ###

## Get Data
wolVhos_OutputTable <- read_csv("Data/wolVhos_OutputTable.txt")

#Training is defined as Wol games in the Version analysis and HOTS is the test
wolVhos_OutputTable$IsTraining=as.factor(wolVhos_OutputTable$IsWoL)
PerformanceData=wolVhos_OutputTable

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
  
  if(GameCountCheck)
  {
    #Some people swap versions from HOTS back to Wol, we drop those Wol games.
    #Find first HOTs game for this player
    FirstHotsDate=min(Currplayer$date_time[Currplayer$IsHoS==1])
    
    #drop any Wol games after FirstHotsDate. Do this by dropping anything that is not a HOTS game or is something before the first hots date (i.e., WOL)
    #reinitialize current player by excluding any late WOL games
    Currplayer=Currplayer[Currplayer$date_time<=FirstHotsDate|Currplayer$IsHoS,]
    
    #now calculate the number of days from the last game
    Currplayer$DaysBetweenGame = as.numeric(difftime(Currplayer$date_time, lag(Currplayer$date_time, n=1L), units = 'days'))
    
    
    #BuildRegression for Wol & Hots seperately
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
    
    #get standard errors too.
    PredictionSE.WOL=as.numeric(predict.lm(object=WOLModel,DataToPredict,se.fit = TRUE)[2])
    PredictionSE.HOTs=as.numeric(predict.lm(object=HOTSModel,DataToPredict,se.fit = TRUE)[2])
    
    #record output data
    Residual=Prediction.HOTS-Prediction.WOL
    
    #find number of WolGames
    Wol_n_i=nrow(CurrWOLsData)
    
    #export this information to a data frame
    Wol_To_Hots_PredictionData=rbind(Wol_To_Hots_PredictionData, cbind(i, Prediction.WOL, Prediction.HOTS, PredictionSE.WOL, PredictionSE.HOTs, Residual, Wol_n_i))
   
  }
}

# reformat data for plot
data.violin <- data.frame(GameVersion=c(rep('WOL', nrow(Wol_To_Hots_PredictionData)),
    rep('HotS', nrow(Wol_To_Hots_PredictionData))),
  Prediction=c(Wol_To_Hots_PredictionData$Prediction.WOL,
    Wol_To_Hots_PredictionData$Prediction.HOTS))

# set up vectors for polygon (has to be the same length as plot data)
woltohots.shade.x <- c(-Inf,1,2,3,4,5,6,7,8,9,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,
                       Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf)
woltohots.shade.y <- c(-Inf,1,2,3,4,5,6,7,8,9,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,
                       Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,-Inf)


# create dataset for individual plot

# rename isTraining values
PerformanceData$IsTraining <- as.character(PerformanceData$IsTraining)

PerformanceData$IsTraining[PerformanceData$IsTraining==0] <- 'HoTS'
PerformanceData$IsTraining[PerformanceData$IsTraining==1] <- 'WoL'



## Make individual plot
# currently player 78

# subset data
plot.dat.WoltoHots <- PerformanceData[PerformanceData$PlayerID==78,]
individual.data.WtoH <- Wol_To_Hots_PredictionData[Wol_To_Hots_PredictionData$i==78,]

WoLtoHotS.PlayerPlot <- ggplot(plot.dat.WoltoHots, aes(x=Any_XP, y=Transformed_fal2League, color=IsTraining)) +
  geom_point(size=1)+
  geom_smooth(method = 'lm', se=F, size = 1.25, aes(group =IsTraining), color='black')+
  geom_smooth(method = 'lm', se=F)+
  labs(title=NULL, x='Game Number', y='Estimated Performance', color=NULL)+
  theme_classic()+
  theme(aspect.ratio = 1)+
  scale_colour_manual(values = c('#EE3377', '#009988'))+
  ggtitle('A')+
  theme(legend.position = c(.8,.25))+
  theme(legend.background = element_rect(fill=NA))+
  theme(axis.title = element_text(size=unit(11, 'points'), family='Arial', color='black'))+
  theme(legend.title = element_text(size=unit(11, 'points'), family='Arial', color='black'))+
  theme(legend.text = element_text(size=unit(9, 'points'), family='Arial', color='black'))+
  theme(axis.text = element_text(size=unit(9, 'points'), family='Arial', color='black'))+
  theme(plot.margin = unit(c(2,2,2,2), 'points'))



## Make Main Plot
WoLtoHotS.Plot <- ggplot(Wol_To_Hots_PredictionData, aes(x=Prediction.WOL, y=Prediction.HOTS)) +
  geom_polygon(aes(x=woltohots.shade.x, y=woltohots.shade.y), alpha=.3)+
  geom_point(aes(x=individual.data.WtoH$Prediction.WOL, y=individual.data.WtoH$Prediction.HOTS), color='#CC3311', size=6.5) +
  geom_point(aes(size=Wol_n_i)) +
  labs(title=NULL, x='Estimated Performance (WoL)',
    y='Estimated Performance (HotS)', size='Total WoL Games')+
  theme_classic()+
  scale_x_continuous(limits = c(0,9), breaks = c(2,4,6,8))+
  scale_y_continuous(limits = c(0,9), breaks = c(2,4,6,8))+
  geom_abline(intercept = 0, slope = 1, size = 1)+
  theme(aspect.ratio = 1)+
  ggtitle('B')+
  theme(legend.position = c(.35,.75))+
  theme(legend.background = element_rect(fill=NA))+
  theme(axis.title = element_text(size=unit(11, 'points'), family='Arial', color='black'))+
  theme(legend.title = element_text(size=unit(11, 'points'), family='Arial', color='black'))+
  theme(legend.text = element_text(size=unit(9, 'points'), family='Arial', color='black'))+
  theme(axis.text = element_text(size=unit(9, 'points'), family='Arial', color='black'))+
  theme(plot.margin = unit(c(2,2,2,2), 'points'))


 
# ## Make Histograms
# WoltoHotS.Delta <- Wol_To_Hots_PredictionData$Prediction.HOTS - Wol_To_Hots_PredictionData$Prediction.WOL
# WoltoHotS.Delta.Percent <- (Wol_To_Hots_PredictionData$Prediction.HOTS / Wol_To_Hots_PredictionData$Prediction.WOL)*100
# 
# WoLtoHotS.Hist <- ggplot() +
#   geom_histogram(aes(WoltoHotS.Delta)) +
#   labs(title=NULL, x='Change in Performance', y='Frequency')+
#   theme_classic()+
#   theme(aspect.ratio = 1)+
#   theme(axis.title = element_text(size=unit(12, 'points'), family='Arial', color='black'))+
#   theme(legend.title = element_text(size=unit(12, 'points'), family='Arial', color='black'))+
#   theme(legend.text = element_text(size=unit(10, 'points'), family='Arial', color='black'))+
#   theme(axis.text = element_text(size=unit(10, 'points'), family='Arial', color='black'))+
#   theme(plot.margin = unit(c(2,2,2,2), 'points'))
# 
# WoLtoHotS.Hist.Percent <- ggplot() +
#   geom_histogram(aes(WoltoHotS.Delta.Percent)) +
#   labs(title=NULL, x='Percent of Performance Retained', y='Frequency')+
#   theme_classic()+
#   theme(aspect.ratio = 1)+
#   theme(axis.title = element_text(size=unit(12, 'points'), family='Arial', color='black'))+
#   theme(legend.title = element_text(size=unit(12, 'points'), family='Arial', color='black'))+
#   theme(legend.text = element_text(size=unit(10, 'points'), family='Arial', color='black'))+
#   theme(axis.text = element_text(size=unit(10, 'points'), family='Arial', color='black'))+
#   theme(plot.margin = unit(c(2,2,2,2), 'points'))

## Make plot of change in transfer by XP
Wol_To_Hots_PredictionData$WoltoHotS.Delta <- Wol_To_Hots_PredictionData$Prediction.HOTS - Wol_To_Hots_PredictionData$Prediction.WOL
individual.data.WtoH <- Wol_To_Hots_PredictionData[Wol_To_Hots_PredictionData$i==78,]


WoLtoHotS.XPplot <- ggplot(Wol_To_Hots_PredictionData, aes(x=Wol_n_i, y=WoltoHotS.Delta)) +
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0), fill=alpha('grey',.3))+
  geom_abline(intercept = 0, slope = 0, size = 1)+
  geom_smooth(method = 'lm', se=F, color = '#0077BB')+
  geom_point(aes(x=individual.data.WtoH$Wol_n_i, y=individual.data.WtoH$WoltoHotS.Delta), color='#CC3311', size=3) +
  geom_point(size=1) +
  labs(title=NULL, x='Total WoL Games',
       y='Change in Estimated Performance')+
  theme_classic()+
  theme(aspect.ratio = 1)+
  ggtitle('C')+
  theme(axis.title = element_text(size=unit(11, 'points'), family='Arial', color='black'))+
  theme(legend.title = element_text(size=unit(11, 'points'), family='Arial', color='black'))+
  theme(legend.text = element_text(size=unit(9, 'points'), family='Arial', color='black'))+
  theme(axis.text = element_text(size=unit(9, 'points'), family='Arial', color='black'))+
  theme(plot.margin = unit(c(2,5,2,2), 'points'))










### Save Plots ###

## save to high res pngs, then convert to eps files in a photo editor (this is
##  the only way to preserve font)

# set working directory
setwd(paste0(wd,'/Plots'))


# Race
png('RacePlot.png', width = 7.5, height = 3.5, units = 'in', res = 600)
print(grid.arrange(Race.PlayerPlot, Race.Plot, Race.XPplot, ncol=3))
dev.off()



# SC1
png('SC1AnalysisPlot.png', width = 5, height = 3.5, units = 'in', res = 600)
print(SC1.Plot)
dev.off()



# WoL to HotS
png('WoLtoHotSplot.png', width = 7.5, height = 3.5, units = 'in', res = 600)
print(grid.arrange(WoLtoHotS.PlayerPlot, WoLtoHotS.Plot, WoLtoHotS.XPplot, ncol=3))
dev.off()

# This sets the working directory back to the location of this script in case you are running other code
wd = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)



### Old code

# # Race
# png('RaceAnalysisPlot.png', width = 5, height = 5, units = 'in', res = 600)
# print(Race.Plot)
# dev.off()
# 
# png('RaceAnalysisHist.png', width = 5, height = 5, units = 'in', res = 600)
# print(Race.Hist)
# dev.off()
# 
# png('RaceAnalysisHistPercent.png', width = 5, height = 5, units = 'in', res = 600)
# print(Race.Hist.Percent)
# dev.off()
# 
# png('RaceLeagueByXPPlot.png', width = 5, height = 5, units = 'in', res = 600)
# print(Race.XPplot)
# dev.off()
# 
# 
# 
# # WoL to HotS
# png('WoLtoHotSAnalysisPlot.png', width = 5, height = 5, units = 'in', res = 600)
# print(WoLtoHotS.Plot)
# dev.off()
# 
# png('WoLtoHotSAnalysisHist.png', width = 5, height = 5, units = 'in', res = 600)
# print(WoLtoHotS.Hist)
# dev.off()
# 
# png('WoLtoHotSAnalysisHistPercent.png', width = 5, height = 5, units = 'in', res = 600)
# print(WoLtoHotS.Hist.Percent)
# dev.off()
# 
# png('WoLtoHotSLeagueByXPPlot.png', width = 5, height = 5, units = 'in', res = 600)
# print(WoLtoHotS.XPplot)
# dev.off()




























