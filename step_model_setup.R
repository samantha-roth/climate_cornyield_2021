#Step Function Model:
#group together temperature intervals of 3 degrees
#lump all time spent below 0C into [-1,0] category
#lump all tiem spent above 39C into [39,40] category
#this is done in S&R
#we will fit a full gaussian model for 1 year of data

rm(list = ls())
graphics.off()

setwd("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me")

#load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/metobs_data")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/Tmin_GS")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/Tmax_GS")

min<-floor(min(Tmin_GS)); max<-ceiling(max(Tmax_GS))

#create dataframe to hold amount of time in each 3 degree temperature interval
temps3<- matrix(NA, nrow= nrow(df), ncol= 15)

#compute the amount of time spent in each 3 degree interval and <0 and >39
#37= which(colnames(df)==0)
#77= which(colnames(df)==40)
#85= which(colnames(df)==48)

for(i in 1: nrow(df)){
  temps3[i,1]<- rowSums(df[i,9:37])
  temps3[i,15]<- rowSums(df[i,77:85])
  for(j in 1:13){
    temps3[i,(1+j)]<- rowSums(df[i,(37+j*3-2):(37+j*3)])
  }
}
temps3<- as.data.frame(temps3)
colnames(temps3)<- c("<0",seq(from=3, to=39, by=3), ">39")

save(temps3, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/temps3")

#can start code after running the above by commenting out lines 17-32
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/temps3")
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Data_Metobs")

#skips the fips variable since we are representing fips using a series of dummy variables
step3data<- cbind(df[,c(5,7:8)],temps3,df[,86:1954]) 
save(step3data, file="/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/step3data")

