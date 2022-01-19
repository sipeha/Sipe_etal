#output from ArcMap by protocol type and data source
#with the spatially filtered observations and the site covariate information
#all<-read.csv("data/all_occ_data.csv", header=T)

#check to make sure there are no duplicates in eBird data
length(unique(all$checklist_id)) 
area<-all[(which(all$protocol_t=="Area")),]
length(which(duplicated(area$checklist_id)))
stat<-all[which(all$protocol_t=="Stationary"),]
length(which(duplicated(stat$checklist_id)))
travel<-all[which(all$protocol_t=="Traveling"),]
length(which(duplicated(travel$checklist_id)))

#make numeric categories for protocol type
protocol_cat<-numeric(length(all$protocol_t))
all<-cbind(all, protocol_cat)
all$protocol_cat[which(all$protocol_t=="Area")]<-1
all$protocol_cat[which(all$protocol_t=="Other survey")]<-2
all$protocol_cat[which(all$protocol_t=="Stationary")]<-3
all$protocol_cat[which(all$protocol_t=="Traveling")]<-4
all$protocol_cat[which(all$protocol_t=="WDFW survey")]<-5

#change name to be waterbody with county
all$name<-toupper(all$name)
#site name with county name
for(i in 1:length(all$name)){
  all$name[i]<-paste(all$name[i], all$county[i], sep="-")
}

all$name<-as.character(unlist(all$name))


sitenames<-as.character((unique(all$name)))
#siteIDs
siteid<-seq(1,length(sitenames), by=1)

sites<-cbind(siteid, sitenames)

#include a column for the siteids 
sids<-length(all$X)
all<-cbind(all, sids)
for(i in 1:length(all$X)){
  for(j in 1:length(sites[,1])){
    if(all$name[i]==sites[j,2]){
      all$sids[i]<-sites[j,1]
    }else{}
  }
}

#make numeric categories for protocol type
protocol_cat<-numeric(length(all$protocol_t))
all<-cbind(all, protocol_cat)
all$protocol_cat[which(all$protocol_t=="Area")]<-1
all$protocol_cat[which(all$protocol_t=="Other survey")]<-2
all$protocol_cat[which(all$protocol_t=="Stationary")]<-3
all$protocol_cat[which(all$protocol_t=="Traveling")]<-4
all$protocol_cat[which(all$protocol_t=="WDFW survey")]<-5


#write.csv(all, file="all_occ_data.csv")


