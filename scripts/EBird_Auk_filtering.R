library(auk)
library(dplyr)
library(devtools)
library(lubridate)
setwd("/Users/sipeh/Desktop/eBird dataset")

#AUK github and how tos: 

basicdat<-file("sipeh/Desktop/eBird_DATA/ebd_US-WA_relNov-2017/ebd_US-WA_relNov-2017.txt")
samplingdat<-file("/Users/sipeh/Desktop/eBird_DATA/ebd_sampling_relNov-2017/ebd_sampling_relNov-2017.txt")
ebd<-auk_ebd(file="/Users/sipeh/Desktop/eBird_DATA/ebd_US-WA_relNov-2017/ebd_US-WA_relNov-2017.txt", 
             file_sampling="/Users/sipeh/Desktop/eBird_DATA/ebd_sampling_relNov-2017/ebd_sampling_relNov-2017.txt") 
ebd_filters_all<- ebd %>%
  auk_country("US") %>%
  auk_extent(extent=c(-124.5, 44.5, -115.5, 50))%>%
  auk_date(date=c("2000-01-01","2017-12-31"))%>%
  auk_complete()
ebd_filters_all


#execute filters
ebdfilt_all<-auk_filter(ebd_filters_all,
                        file="ebd_filter.txt",
                        file_sampling="filter_sampling.txt", overwrite=TRUE)

XA<-read_ebd(ebdfilt_all)
#remove states that arent washington
XA<-XA[!XA$state_province=="Idaho",]
XA<-XA[!XA$state_province=="Montana",]
XA<-XA[!XA$state_province=="Oregon",]


#remove non-summer months
XA$observation_date<-as.Date(XA$observation_date)
monthA<-yearA<-dayA<-numeric(length((XA$observation_date)))
X_ALL<-as.data.frame(XA,month,year,day)
X_ALL$month<-month(XA$observation_date)
X_ALL$year<-year(XA$observation_date)
X_ALL$day<-day(XA$observation_date)


X_ALL<-X_ALL[!X_ALL$month<5,]
X_ALL<-X_ALL[!X_ALL$month>7,]

#remove any 'casual' observations, i.e. those not stationary, travelling, or area
X_ALLt<-X_ALL[which(X_ALL$protocol_type=="Traveling"),]
X_ALLa<-X_ALL[which(X_ALL$protocol_type=="Area"),]
X_ALLs<-X_ALL[which(X_ALL$protocol_type=="Stationary"),]

#combine stationary, travelling, and area protocol observations into a single 
# file to be used for spatially filtering in ArcMap

EBD<-rbind(X_ALLt, X_ALLa, X_ALLs)
A_EBD<-as.matrix(EBD)
#write.csv(A_EBD, file="EBD.csv")
