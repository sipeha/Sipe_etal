#WDFW raw data file:
wdfw2017<-read.csv("data/wdfw2017COLO.csv", header = T)
#column description:
# EffortDate: date of observation
#STARTTIME and ENDTIME: the start of the observation and when it concluded
# EffortSource: agency that conducted the observation, 'WDFW survey' were observations 
# made by WDFW biologists and 'Other Survey' were observations provided to the WDFW
#from biologists on private waterbodies (i.e., reservoirs)
#Latitude and Longitude: the location of the observation
#CountyName: Name of the county where the observation took place
#SiteName: Name of the site where the observation took place
#Quantity: Number of COLO observed 
#LifeStage: Life Stage of the individuals observed
#OCCPRODCODE: Type of observation
#OccProdCode_Desc: meaning of OCCPRODCODE


#Changing the WDFW observation codes (OCCPRODCODE) to 
#0: unoccupied, 1: occupied without breeding, 2: occupied with breeding
#create occupancy codes from occprod colummn
obsOCC<-numeric(length(wdfw2017[,1]))
wdfw2017<-cbind(wdfw2017, obsOCC)

#0 #3 nest occ unknown 
wdfw2017$obsOCC[which(wdfw2017$OCCPRODCODE==3)]<-0
#1 #4 single bird, nest unrepaired or pair not near nest
wdfw2017$obsOCC[which(wdfw2017$OCCPRODCODE==4)]<-1
#1 #5 nest occupied, act. unknwon
wdfw2017$obsOCC[which(wdfw2017$OCCPRODCODE==5)]<-1
#2 #6 nest occupied, active breeding
wdfw2017$obsOCC[which(wdfw2017$OCCPRODCODE==6)]<-2
#1 #7 nest occupied, inactive
wdfw2017$obsOCC[which(wdfw2017$OCCPRODCODE==7)]<-1
#0 #8 nest unoccupied, no birds, nest unrepaird
wdfw2017$obsOCC[which(wdfw2017$OCCPRODCODE==8)]<-0
#1 #50 productivity unsuccessful or nest empty
wdfw2017$obsOCC[which(wdfw2017$OCCPRODCODE==50)]<-1
#2 #60 productivity successful, number of young known
wdfw2017$obsOCC[which(wdfw2017$OCCPRODCODE==60)]<-2
#2 #70 productivity successful, number of young unknown
wdfw2017$obsOCC[which(wdfw2017$OCCPRODCODE==70)]<-2

library(lubridate)
#effort in minutes of time spent determined from STARTTIME and ENDTIME
duration_minutes<-numeric(length(wdfw2017[,1]))
for(i in 1:length(duration_minutes)){
  duration_minutes[i]<-((as.numeric(hm(wdfw2017$ENDTIME[i]))/(60*60))-(as.numeric(hm(wdfw2017$STARTTIME[i])))/(60*60))*60
}
#####
wdfw2017<-cbind(wdfw2017, duration_minutes)

#convert month and days
#for both change the format of the date
#sort by month then day
day<-month<-year<-Date<-numeric(length(wdfw2017[,1]))

day<-day(as.Date(wdfw2017$EffortDate, "%m /%d /%Y"))
month<-month(as.Date(wdfw2017$EffortDate, "%m /%d /%Y"))
year<-year(as.Date(wdfw2017$EffortDate, "%m /%d /%Y"))
for(i in 1:length(wdfw2017$EffortDate)){
  Date[i]<-paste(month[i], day[i], sep="")
}

wdfw2017<-cbind(wdfw2017, day, month, year)

#save csv file to use in ArcMap 
write.csv(wdfw2017, file="data/wdfw2017.csv")


#wdfw2017.csv data was then mapped in ArcMap where
#each of the sites were given the covariate site data from each of the 
#covariate datasets
#wdfw site covariate data can be found in SitesandCovs.csv file

