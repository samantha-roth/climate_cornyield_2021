#add the other necessary variables to the dataframe
# fips, state, total prec^2, time^2
rm(list = ls())
graphics.off()
library(dplyr)

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")
#load the Data dataframe as a reference guide to format my dataframe
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Data_Metobs")

#want to get rid of the rows that don't exist in the Data dataframe
#i.e., we only want the rows for which yield data exists
#getrid<- rep(0, nrow(df))
#for(i in 1:nrow(df)){
#  for(j in 1:nrow(Data)){
#    if(df$fips_year[i]==Data$fips_year[j]) getrid[i]=j
#  }
#}
#save(getrid, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/getrid")

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/getrid")

#indx<- which(getrid!=0)
#df2<- df[which(getrid!=0),]
dfsized<- rep(NA, ncol(df))
for(j in 1:nrow(Data)){
  dfsized<-rbind(dfsized, df[which(getrid==j),])
}
dfsized<- dfsized[-1,]
save(dfsized, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/dfsized")

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/dfsized")

#df<- df[which(getrid!=0),]
#check to make sure the locations and years are the same for df and Data
equal<- Data$fips_year==dfsized$fips_year
##if equal comes back all true, add the StateANSI and countyANSI variables to df
dfsized$StateANSI<- Data$StateANSI
dfsized$countyANSI<- Data$countyANSI


save(dfsized, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/dfsized")

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/dfsized")
##EXTRA STUFF##
#fips<- as.character(Data$fips)
#fips<- as.numeric(fips)
#list<- NA
#for(i in 1:47528){
#  for(j in (i+1):47529){
#    if(fips[j]<fips[i]) list<- c(list, j)
#  }
#}