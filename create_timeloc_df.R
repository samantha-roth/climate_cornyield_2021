#create timeloc_df
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")

#save locations to be used in analysis in the same order in a more useful form
loc_df<- as.data.frame(df$fips)

library(housingData)
head(geoCounty)

loc_df$lat<- rep(NA,nrow(df))
loc_df$lon<- rep(NA,nrow(df))

ufips<- unique(df$fips)
ufips<- as.character(ufips)
fips<- as.character(df$fips)
geofips<- as.character(geoCounty$fips)

for(i in 1:length(ufips)){
  val<- ufips[i]
  loc_df$lat[which(fips==val)]<- geoCounty$lat[which(geofips==val)]
  loc_df$lon[which(fips==val)]<- geoCounty$lon[which(geofips==val)]
}
colnames(loc_df)[1]<- "fips"

save(loc_df,file="/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/loc_df")

timeloc_df<- cbind(loc_df,df$StateANSI,df$countyANSI,df$year)
colnames(timeloc_df)[4:6]<- c("StateANSI","countyANSI","year")
timeloc_df$StateANSI<- as.factor(timeloc_df$StateANSI)
timeloc_df$countyANSI<- as.factor(timeloc_df$countyANSI)

save(timeloc_df,file="/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_df")

