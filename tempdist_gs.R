##create a dataframe with temperature distribution
rm(list = ls())
graphics.off()

setwd("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me")

#load all relevant observations of each variable for each day in each county in the 38 years of observations
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Metpr")
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Mettmax")
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Mettmin")

#load the Data dataframe as a reference guide to format my dataframe
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Data_Metobs")

#Data_prism$latlon<- paste(Data_prism$lat, Data_prism$lon, sep=",")
#length(unique(Data_prism$latlon))

#load the day of the year of the beginning and end of the growing season 
#for each year (1979-2016) for each of the 1878 counties
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/MetGS_start")
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/MetGS_end")
#load the county and state data
load("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield_CodeCheck/Climate_CornYield-master/CountyANSI")
load("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield_CodeCheck/Climate_CornYield-master/StateANSI")
load("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield_CodeCheck/Climate_CornYield-master/ANSI")

countynum=1878
gslen= 185

#avgTmax_GS<-rep(NA,countynum*38)
#avgTmin_GS<-rep(NA,countynum*38)
#Pr_GS<-rep(NA,countynum*38)
#Tmax_GS<- matrix(NA, nrow= countynum*38, ncol= gslen) #already dealt with Tmax_GS, Tmin_GS
#Tmin_GS<- matrix(NA, nrow= countynum*38, ncol= gslen)

#for (i in 1:countynum){
#  m1=1
#  m2=1
#  for (j in 1:38){
#    k=365
#    if (j%%4==2){ #when exist Feb29
#      k=366
#    }
#    m2<-m1+k-1
#    index<-c(m1:m2)
#    if (!is.na(GS_start[i,j])){
#      GSindex<-index[GS_start[i,j]:GS_end[i,j]]
#      avgTmax_GS[(i-1)*38+j]<-mean(tmax[i,GSindex])
#      avgTmin_GS[(i-1)*38+j]<-mean(tmin[i,GSindex])
#      Pr_GS[(i-1)*38+j]<-sum(pr[i,GSindex])
#      #Tmax_GS[(i-1)*38+j,]<- tmax[i,GSindex]
#      #Tmin_GS[(i-1)*38+j,]<- tmin[i,GSindex]
#   }
#    m1=m2+1
# }
#}

#save(Tmax_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/Tmax_GS")
#save(Tmin_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/Tmin_GS")
#save(avgTmax_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/avgTmax_GS")
#save(avgTmin_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/avgTmin_GS")
#save(Pr_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/Pr_GS")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/avgTmax_GS")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/avgTmin_GS")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/Pr_GS")


load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/Tmax_GS")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/Tmin_GS")
#Calculate the amount of time spent in each 1 C interval
source("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/T_distribution.R") #function to be used

#add in latitude and longitude
library(ncdf4)
source("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield_CodeCheck/Climate_CornYield-master/latlong2county.R")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/geoCounty.rda")

low<- floor(min(Tmin_GS))
high<- ceiling(max(Tmax_GS))
T_interval=c(low, high)

#Tdist_GS<- matrix(NA, nrow= nrow(Tmax_GS), ncol= (high-low))

#create a matrix where each row is the amount of time spent in each 1C interval
#for each county and year
#for(i in 1:nrow(Tmax_GS)){
#    Tmax<- Tmax_GS[i,]
#    Tmin<- Tmin_GS[i,]
#    td<- T_distribution(Tmax= Tmax, Tmin= Tmin, T_interval= T_interval)
#    Tdist_GS[i,]<- td
#}

#save(Tdist_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/Tdist_GS")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/Tdist_GS")

S<-read.table("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield_CodeCheck/Climate_CornYield-master/harvest_area.csv",header=TRUE,sep=",")
area<-rep(NA,38*countynum)
for (i in 1:countynum){
  stateindex<-which(S$State.ANSI==StateANSI[i])
  countyindex<-which(S$County.ANSI==CountyANSI[i])
  yearindex<-intersect(stateindex,countyindex)
  year<-S$Year[yearindex]
  areatoget<-rep(NA,38)
  for (j in 1:38){
    ind<-which(year==1978+j)
    if (!identical(ind,integer(0))){
      areatoget[j]<-S$Value[yearindex[ind]]
    }
  }
  area[((i-1)*38+1):(i*38)]<-areatoget
}
W<-read.table("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield_CodeCheck/Climate_CornYield-master/yielddata.csv",header=TRUE,sep=",")
yield_anomaly<-rep(NA,38*countynum)
yield<-rep(NA,38*countynum)
for (i in 1:countynum){
  stateindex<-which(W$State.ANSI==StateANSI[i])
  countyindex<-which(W$County.ANSI==CountyANSI[i])
  yearindex<-intersect(stateindex,countyindex)
  year<-W$Year[yearindex]
  yieldtoget<-rep(NA,38)
  for (j in 1:38){
    ind<-which(year==1978+j)
    if (!identical(ind,integer(0))){
      yieldtoget[j]<-W$Value[yearindex[ind]]
    }
  }
  yield[((i-1)*38+1):(i*38)]<-yieldtoget
}

latit<- rep(NA, countynum)
longit<- rep(NA, countynum)

for(i in 1:countynum){
  latit[i]<- geoCounty$lat[which(geoCounty$fips==ANSI[i])]
  longit[i]<- geoCounty$lon[which(geoCounty$fips==ANSI[i])]
}

ogData<-data.frame(StateANSI=rep(StateANSI,each=38),countyANSI=rep(CountyANSI,each=38), fips=rep(ANSI,each=38),
                 year=rep(c(1:38),countynum),avgTmax_GS=avgTmax_GS,avgTmin_GS=avgTmin_GS,Pr_GS=Pr_GS,yield=yield,
                 area=area, lat= latit, lon= longit)
save(ogData, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/ogData")
metobs_data<- cbind(ogData, Tdist_GS)
metobs_data<-metobs_data[complete.cases(metobs_data), ] #Yield data are 1981-2012

#calculate yield anomaly based on fixed effects
metobs_data$StateANSI<-factor(metobs_data$StateANSI)
metobs_data$year= metobs_data$year+1978
metobs_data$year<-factor(metobs_data$year)

save(metobs_data,file = "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/metobs_data")
#load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/metobs_data")

