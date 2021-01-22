#Step Function Model:
#group together temperature intervals of 3 degrees
#lump all time spent below 0C into [-1,0] category
#lump all tiem spent above 39C into [39,40] category
#this is done in S&R
#we will fit a full gaussian model for 1 year of data

rm(list = ls())
graphics.off()

setwd("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me")

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/metobs_data")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/Tmin_GS")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/Tmax_GS")

min<-floor(min(Tmin_GS)); max<-ceiling(max(Tmax_GS))

#create dataframe to hold amount of time in each 3 degree temperature interval
#temps3<- matrix(NA, nrow= nrow(metobs_data), ncol= 15)

#compute the amount of time spent in each 3 degree interval and <0 and >39
#for(i in 1: nrow(metobs_data)){
#  temps3[i,1]<- rowSums(metobs_data[i,10:38])
#  temps3[i,15]<- rowSums(metobs_data[i,78:86])
#  for(j in 1:13){
#    temps3[i,(1+j)]<- rowSums(metobs_data[i,(38+j*3-2):(38+j*3)])
#  }
#}
#colnames(temps3)<- c(seq(from=0, to=39, by=3), 40)

#save(temps3, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/temps3")

#can start code after running the above by commenting out lines 17-32
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/metobs_data")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/temps3")

step3data<- cbind(metobs_data[,1:11],temps3)
save(step3data, file="/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/step3data")



