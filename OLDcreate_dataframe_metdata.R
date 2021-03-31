# dummy variables used as weights for the amount of time spent in each 1-degree temperature interval
# dummy variable for each county effect
# quadratic term for yearly precipitation
# quadratic time trend by state 

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

#create a vector that keeps track of the year each day is in
yeardays<- NA
for (i in 0:35){
  if(!((1981+i)%%4)) yeardays<- c(yeardays, rep(1981+i,366))
  if((1981+i)%%4) yeardays<- c(yeardays, rep(1981+i,365))
}
yeardays<- yeardays[-1]

#goal: create data frame where each row is for a county in a given year, 1981-2012
#need variable to represent amt of time spent in each 1 C interval during growing season
#growing season can be defined as March - August or 
#the 6 months after the 21 day moving average hits temp threshold
#also need as variables: year, fips, StateANSI, CountyANSI, Pr (during growing season)

#subset each dataframe to get rid of the years 1979 and 1980
tmax<- tmax[,-c(1:731)]
tmin<- tmin[,-c(1:731)]

GS_start<- GS_start[,-c(1:2)]
GS_end<- GS_end[,-c(1:2)]

#number of years, counties, growing seaon length
yrnum<- 36
countynum<- 1878
gslen<- 185

##create a vector that repeats each unique county ID (fips) 36 times
uniqueCounties<- levels(Data$fips)
countyyears<- NA
for(i in 1:countynum){
  countyyears<- c(countyyears, rep(uniqueCounties[i], yrnum))
}
countyyears<- countyyears[-1]

#need a 36*1878 row dataframe (one row for each year in each county)
df<- data.frame(
  "year"= rep(1981:2016, countynum),
  "fips"= countyyears
)

#the following variable is to keep track of which county years have yield data
df$fips_year<- paste(df$fips, df$year, sep="_")
Data$fips_year <- paste(Data$fips, Data$year, sep="_")

#create a dataframe to hold the growing season daily maximum temperatures
#tmax_gs_temps<- matrix(NA, nrow= countynum, ncol= gslen*yrnum)
#tmin_gs_temps<- matrix(NA, nrow= countynum, ncol= gslen*yrnum)

##create a dataframe for the daily max and daily min temps during the growing season
##in each year in each county
#for(i in 1:countynum){
#  for(j in 1:yrnum){
#    year<- which(yeardays==1980+j)
#    tmx<- tmax[i,year]
#    tmn<- tmin[i,year]
#    tmx_GS<- tmx[c(GS_start[i,j]:GS_end[i,j])]
#    tmn_GS<- tmn[c(GS_start[i,j]:GS_end[i,j])]
#    tmax_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]<- tmx_GS
#    tmin_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]<- tmn_GS
#  }
#}

##save growing season daily max and min temps
#save(tmax_gs_temps, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/tmax_gs_temps")
#save(tmin_gs_temps, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/tmin_gs_temps")

#after running lines 1-84, can comment out lines 50-84 and just run the lines below
#load growing season daily max and min temps
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/tmax_gs_temps")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/tmin_gs_temps")


#Find the max max temp and min min temp for all the growing seasons, set interval
low<- floor(min(tmin_gs_temps))
high<- ceiling(max(tmax_gs_temps))
T_interval=c(low, high)

#temp_dist<- matrix(NA, nrow= countynum, ncol= 36*77)
temp_dist_GS<- matrix(NA, nrow= 1, ncol= (high-low))

#Calculate the amount of time spent in each 1 C interval
source("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/T_distribution.R") #function to be used

for(i in 1:countynum){
  for(j in 1:yrnum){
    Tmax<- tmax_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]
    Tmin<- tmin_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]
    #temp_dist[i,(1+(j-1)*77):(77+(j-1)*77)]<- T_distribution(Tmax= Tmax, Tmin= Tmin, T_interval= T_interval)
    td<- T_distribution(Tmax= Tmax, Tmin= Tmin, T_interval= T_interval)
    temp_dist_GS<- rbind(temp_dist_GS, td)
  }
}
temp_dist_GS<- temp_dist_GS[-1,]

save(temp_dist_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/temp_dist_GS")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/temp_dist_GS")
#precipitation stuff
pr<- pr[,-c(1:731)]

#get the total precipitation for each county for each year during the growing season
totpr_GS<- NA

for(i in 1:countynum){
  for(j in 1:yrnum){
    year<- which(yeardays==1980+j)
    prec<- pr[i,year]
    totpr_GS<- c(totpr_GS, sum(prec[c(GS_start[i,j]:GS_end[i,j])]))
  }
}
totpr_GS<- as.vector(totpr_GS[-1])

save(totpr_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/totpr_GS")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/totpr_GS")


df<- cbind(df, temp_dist_GS)
df<- cbind(df, totpr_GS)
indx<- which(between(df$year,1981,2012))
df<- df[indx,]

save(df, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")
save(Data,file="/storage/work/svr5482/Countywise/Metdata/Metdataframe/Data_Metobs")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")

#**********EXTRA STUFF I CUT OUT****************#
#yeardays<- NA
#daysofyear<-NA
#for (i in 0:35){
#  if(!((1981+i)%%4)){
#    yeardays<- c(yeardays, rep(1981+i,366))
#    daysofyear<- c(daysofyear, seq(1,366,by=1))
#  }
#  if((1981+i)%%4){
#    yeardays<- c(yeardays, rep(1981+i,365))
#    daysofyear<- c(daysofyear, seq(1,365,by=1))
#  }
#}
#yeardays<- yeardays[-1]
#daysofyear<- daysofyear[-1]

#yearsanddays<- rbind(yeardays,daysofyear)
  
