#created: 03/31/2021
#combine data sources into one data frame for the years 1981-2012
rm(list = ls())
graphics.off()

setwd("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me")

#load all relevant observations of each variable for each day in each county in the 38 years of observations
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Metpr")
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Mettmax")
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Mettmin")

#load the Data dataframe as a reference guide to format my dataframe
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Data_Metobs")

#load the day of the year of the beginning and end of the growing season 
#for each year (1979-2016) for each of the 1878 counties
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/MetGS_start")
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/MetGS_end")
load("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield_CodeCheck/Climate_CornYield-master/ANSI")

#create a vector that keeps track of the year each day is in
yeardays<- NA
for (i in 0:37){
  if(!((1979+i)%%4)) yeardays<- c(yeardays, rep(1979+i,366))
  if((1979+i)%%4) yeardays<- c(yeardays, rep(1979+i,365))
}
yeardays<- yeardays[-1]

#goal: create data frame where each row is for a county in a given year, 1981-2012
#need variable to represent amt of time spent in each 1 C interval during growing season
#growing season can be defined as March - August or 
#the 6 months after the 21 day moving average hits temp threshold
#also need as variables: year, fips, StateANSI, CountyANSI, Pr (during growing season)

#number of years, counties, growing seaon length
yrnum<- 38 #1979-2016
countynum<- 1878
gslen<- 185 #in days

countyyears=rep(ANSI,each=38)

#need a 38*1878 row dataframe (one row for each year in each county)
#df<- data.frame(
#  "year"= rep(1979:2016, countynum),
#  "fips"= countyyears
#)

#create a dataframe to hold the growing season daily maximum temperatures
tmax_gs_temps<- matrix(NA, nrow= countynum, ncol= gslen*yrnum)
tmin_gs_temps<- matrix(NA, nrow= countynum, ncol= gslen*yrnum)

#create a dataframe for the daily max and daily min temps during the growing season
#in each year in each county
for(i in 1:countynum){
  for(j in 1:yrnum){
    year<- which(yeardays==1978+j)
    tmx<- tmax[i,year]
    tmn<- tmin[i,year]
    tmx_GS<- tmx[c(GS_start[i,j]:GS_end[i,j])]
    tmn_GS<- tmn[c(GS_start[i,j]:GS_end[i,j])]
    tmax_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]<- tmx_GS
    tmin_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]<- tmn_GS
  }
}


##save growing season daily max and min temps
save(tmax_gs_temps, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/tmax_gs_temps")
save(tmin_gs_temps, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/tmin_gs_temps")

#after running lines 1-84, can comment out lines 50-84 and just run the lines below
#load growing season daily max and min temps
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/tmax_gs_temps")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/tmin_gs_temps")

#Find the max max temp and min min temp for all the growing seasons, set interval
low<- floor(min(tmin_gs_temps))
high<- ceiling(max(tmax_gs_temps))
T_interval=c(low, high)

#temp_dist_GS<- matrix(NA, nrow= 1, ncol= (high-low))

#Calculate the amount of time spent in each 1 C interval
source("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/T_distribution.R") #function to be used

temp_dist_GS<- matrix(NA, nrow= countynum*yrnum, ncol= (high-low))
for(i in 1:countynum){
  for(j in 1:yrnum){
    Tmax<- tmax_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]
    Tmin<- tmin_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]
    #temp_dist[i,(1+(j-1)*77):(77+(j-1)*77)]<- T_distribution(Tmax= Tmax, Tmin= Tmin, T_interval= T_interval)
    td<- T_distribution(Tmax= Tmax, Tmin= Tmin, T_interval= T_interval)
    #temp_dist_GS<- rbind(temp_dist_GS, td)
    temp_dist_GS[(i-1)*38+j,]<- td
  }
}
#temp_dist_GS<- temp_dist_GS[-1,]

save(temp_dist_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/temp_dist_GS")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/temp_dist_GS")

#precipitation stuff
#get the total precipitation for each county for each year during the growing season
totpr_GS<- rep(NA, countynum*yrnum)

for(i in 1:countynum){
  for(j in 1:yrnum){
    year<- which(yeardays==1978+j)
    prec<- pr[i,year]
    #totpr_GS<- c(totpr_GS, sum(prec[c(GS_start[i,j]:GS_end[i,j])]))
    totpr_GS[(i-1)*38+j]<- sum(prec[c(GS_start[i,j]:GS_end[i,j])])
  }
}
#totpr_GS<- as.vector(totpr_GS[-1])

save(totpr_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/totpr_GS")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/totpr_GS")

totpr_GS2<- totpr_GS^2
df<- cbind(totpr_GS, totpr_GS2, temp_dist_GS)


##Excerpt from METrawdata_dataframe.R by Haochen Ye (https://github.com/yhaochen/Climate_CornYield)
load("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield_CodeCheck/Climate_CornYield-master/CountyANSI")
load("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield_CodeCheck/Climate_CornYield-master/StateANSI")
load("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield_CodeCheck/Climate_CornYield-master/ANSI")

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
##End excerpt

df2<- data.frame(StateANSI=rep(StateANSI,each=38),countyANSI=rep(CountyANSI,each=38), fips=rep(ANSI,each=38),
                 year=rep(c(1:38),countynum),yield=yield,area=area)

df<- cbind(df2,df)
df<-df[complete.cases(df), ] #Yield data are 1981-2012
df$year=df$year+1978
save(df, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")

#rename the temperature distribution colums to correspond to the upper temperature of 
#each 1degree C temperature intervals i.e. the amount of time spent in (-29,-28) is called -28
#mininum temp: -29, maximum temp: 48

#columns 9-85 are amount of time spent in each 1 degree interval
tempnames<- seq(-28,48, by=1)
colnames(df)[9:85]<- as.character(tempnames)

colnames(df)[7:8]<- c("Pr_GS", "Pr_GS2")

#find unique state ansi values
states<- unique(df$StateANSI)

stateyears<- matrix(0, nrow= nrow(df), ncol= length(states))
stateyears<- as.data.frame(stateyears)
for(i in 1:24) colnames(stateyears)[i]<- paste("state",states[i],"yr",sep="")

for(i in 1:24){
  stateyears[which(df$StateANSI==states[i]),i]<- df$year[which(df$StateANSI==states[i])]-1980
}

stateyears_sq<- stateyears^2
for(i in 1:24) colnames(stateyears_sq)[i]<- paste("state",states[i],"yr_sq",sep="")

#columns 86-133 are stateyears and stateyears_sq
df<- cbind(df, stateyears, stateyears_sq)

#create the portion of the dataframe containing indicators for the fips codes
uniquefips<- unique(df$fips)
ufips<- as.character(uniquefips)

fipscode<- matrix(0, nrow= nrow(df), ncol= length(ufips))
fipscode<- as.data.frame(fipscode)

for(i in 1:length(ufips)) colnames(fipscode)[i]<- paste("fips",uniquefips[i],sep="")
for(i in 1:length(ufips)) fipscode[which(df$fips==uniquefips[i]),i]<- 1

df<- cbind(df[,1:133], fipscode)
save(df, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")