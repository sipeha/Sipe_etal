setwd("/Users/sipeh/Desktop/eBird dataset/FormattedData")

#output from ArcMap by protocol type and data source
#eBird
statdat<-read.csv("Occ data/statdat.csv")
areadat<-read.csv("Occ data/areadat.csv")
travdat<-read.csv("Occ data/travdat.csv")
#WDFW 
wdfw<-read.csv("/Users/sipeh/Desktop/WDFWloondat/wdfwelvhii.csv")

#columns of data:
# checklist_id (unique id for each submitted eBird checklist)
# Common.Loon (loon observation indicator (0/1))
# Location information: county, latitude,longitude, name of site   
# Date and time information: day, month, year
# Observation information: time of observation, 
  #protocol type (area, travelling, stationary, wdfw)
# duration of observation (minutes), distance traveled during observation (km)
#Site covariate information: area (sq_km), Perimeter (km),
    #elevation (m), human influence index (hii), NLCD landtype



# colnames(travdat)<-c( "X" , "checklist_id" ,"Common.Loon" , "county", "latitude","longitude",   
#                       "time_obser","protocol_t","duration_m","effort_dis","effort_are","month",       
#                       "year", "day","in20","LABsum","LABsPres","name",        
#                       "area_sq_km","Perimeter_km" ,"elevation","elevation_m","hii","distance" )
# 

#add column for distance for area and stat


# distance_a<-numeric(length(areadat[,1]))
# areadat<-cbind(areadat,distance_a)
# colnames(areadat)[which(names(areadat) == "distance_a")] <- "distance"
# distance_s<-numeric(length(statdat[,1]))
# statdat<-cbind(statdat, distance_s)
# colnames(statdat)[which(names(statdat) == "distance_s")] <- "distance"


#change ebird time to 00:00 instead of 00:00:00

all<-rbind(statdat, areadat, travdat)
#length(unique(all$checklist_id)) 
#remove those observations that are not unique checklists
length(unique(all$checklist_id)) #check to make sure there are duplicates
alluniq<-all[!duplicated(all$checklist_id), ]



#put in WDFW data here, use effort ID where checklist_id
# and NA columns for all those not available
# commonloon column will be observed occ codes
# protocol type = wdfw_survey
# in20,LabsPres,LabsSum all =0


alluniq<-alluniq[order(alluniq$year,alluniq$month,alluniq$day),]


Date<-numeric(length(alluniq[,1]))
alluniq<-cbind(alluniq, Date)

for(i in 1:length(alluniq$Date)){
  alluniq$Date[i]<-paste(alluniq$month[i], alluniq$day[i], sep="")
}

#add in a column for 1:92 of month and day for the array
#
mdnumeric<-seq(1,92,by=1)
unmd<-unique(alluniq2$Date[which(alluniq2$year==2017)])
mdnumeric<-cbind(mdnumeric, unmd)
MD<-numeric(length(alluniq2$Date))
alluniq2<-cbind(alluniq2,MD)
for(i in 1:length(alluniq2$Date)){
  for(j in 1:length(mdnumeric[,1])){
    if(alluniq2$Date[i]==mdnumeric[j,2]){
      alluniq2$MD[i]<-mdnumeric[j,1]
    }else{}
  }
}
#do for year too, from 1:18
yr<-numeric(length(alluniq2$X))
alluniq2<-cbind(alluniq2, yr)
yrs<-seq(1,18, by=1)
years<-cbind(yrs, unique(alluniq2$year))
for(i in 1:length(alluniq2$X)){
  for(j in 1:length(yrs)){
    if (alluniq2$year[i]==years[j,2]){
      alluniq2$yr[i]<-years[j,1]
    }else{}
  }
}

#####here
# wdfw2<-wdfw[ , -which(names(wdfw) %in% c("FID","TerrID", "EndTime", "EffortDate", "Field1"))]
# distance_w<-in20<-LABsum<-LabPres<-effort_are<-effort_dis<-numeric(length(wdfw[,1]))
# wdfw2<-cbind(wdfw2, distance_w, in20, LABsum, LabPres, effort_are, effort_dis)
# wdfw2$in20<-1 #surveys are right on the lake
# colnames(wdfw2)<-c("X","checklist_id", "protocol_t", "time_obser", "Common.Loon",
#                    "name","county", "latitude", "longitude","area_sq_km", "Perimeter_km", "elevation","day", "month","year",
#                    "yr","Date", "MD","duration_m","elevation_m", "hii","distance", "in20", "LABsum", "LABsPres", "effort_are", 
#                    "effort_dis" )
# wdfw2$checklist_id<-as.character(wdfw2$checklist_id)


al<-rbind(alluniq, wdfw)

#remove seconds from time interval
al$time_obser<-sub("*:*:00","", al$time_obser)
al$time_obser<-as.numeric(sub(":","", al$time_obser, fixed=FALSE))


al$name<-toupper(al$name)
#site name with county name
for(i in 1:length(al$name)){
  al$name[i]<-paste(al$name[i], al$county[i], sep="-")
}

al$name<-as.character(unlist(al$name))


sitenames<-as.character((unique(al$name)))
#siteIDs
siteid<-seq(1,length(sitenames), by=1)

sites<-cbind(siteid, sitenames)
# write.csv(sites, file="sites_ebd.csv")
# s<-read.csv("sites_ebd.csv")
#need to merge the sitename with the county it is located in

#include a column in alluniq for the siteids in alluniq2
sids<-length(al$X)
al<-cbind(al, sids)
for(i in 1:length(al$X)){
  for(j in 1:length(sites[,1])){
    if(al$name[i]==sites[j,2]){
      al$sids[i]<-sites[j,1]
    }else{}
  }
}
rm(list=ls())
al<-read.csv("all_occ_data.csv")
sites<-read.csv("sites.csv")
mdnumeric<-read.csv("mdnumeric.csv")

# write.csv(al, file="all_data_covs.csv")

# al<-read.csv("all_data_covs.csv")
# sites<-read.csv("/Users/sipeh/Desktop/eBird dataset/FormattedData/sites.csv")
# mdnumeric<-read.csv("/Users/sipeh/Desktop/eBird dataset/FormattedData/mdnumeric.csv")


#make numeric categories for protocol type
protocol_cat<-numeric(length(al$protocol_t))
al<-cbind(al, protocol_cat)
al$protocol_cat[which(al$protocol_t=="Area")]<-1
al$protocol_cat[which(al$protocol_t=="Other survey")]<-2
al$protocol_cat[which(al$protocol_t=="Stationary")]<-3
al$protocol_cat[which(al$protocol_t=="Traveling")]<-4
al$protocol_cat[which(al$protocol_t=="WDFW survey")]<-5


#write.csv(al, file="all_occ_data.csv")


