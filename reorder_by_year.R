#reorder pwl_norm by year instead of location
rm(list = ls())

library(tidyverse)

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_df")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/pwl_norm")

#format the data so it's in order of year instead of location

year<- as.character(timeloc_df$year)
year<- as.numeric(year)

year<- year[order(year)]

pwln.ord<- matrix(NA, ncol= ncol(pwl_norm), nrow= nrow(pwl_norm))
pwln.ord<- as.data.frame(pwln.ord)
colnames(pwln.ord)<- colnames(pwl_norm)

for(i in 1:32){
  inds<- which(timeloc_df$year==1980+i)
  pwln.ord[which(year==1980+i),]<- pwl_norm[inds,]
}

save(pwln.ord, file="/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/pwl_norm_yrorder")

#reorder timeloc_df so it's ordered by year

timeloc.ord<- matrix(NA, ncol= ncol(timeloc_df), nrow= nrow(timeloc_df))
timeloc.ord<- as.data.frame(timeloc.ord)
colnames(timeloc.ord)<- colnames(timeloc_df)

timeloc_df$fips<- as.character(timeloc_df$fips)
timeloc_df$StateANSI<- as.character(timeloc_df$StateANSI)
timeloc_df$year<- as.character(timeloc_df$year)

for(i in 1:32){
  inds<- which(timeloc_df$year==1980+i)
  timeloc.ord[which(year==1980+i),]<- timeloc_df[inds,]
}

save(timeloc.ord, file="/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

#reorder step3norm by year instead of location
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/step3norm")

#format the data so it's in order of year instead of location

year<- as.character(timeloc_df$year)
year<- as.numeric(year)

year<- year[order(year)]

step3n.ord<- matrix(NA, ncol= ncol(step3norm), nrow= nrow(step3norm))
step3n.ord<- as.data.frame(step3n.ord)
colnames(step3n.ord)<- colnames(step3norm)

for(i in 1:32){
  inds<- which(timeloc_df$year==1980+i)
  step3n.ord[which(year==1980+i),]<- step3norm[inds,]
}

save(step3n.ord, file="/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/step3norm_yrorder")
