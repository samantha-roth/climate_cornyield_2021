##spMvLM- applied to crop yield data

library(spBayes)
#load("C:/Users/saman/Dropbox/Climate_CornYield-me/SourceData/METDATA/step3norm_yrorder")
#load("C:/Users/saman/Dropbox/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/step3norm_yrorder")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

dat<- step3n.ord
rm(step3n.ord)
timeloc.ord$year<- as.numeric(timeloc.ord$year)

#compute number of observations per year
obsperyr<- rep(NA,32)

for(i in 1:32){
  ind<- which(timeloc.ord$year==1980+i)
  obsperyr[i]<- length(ind)
  #latlon<- timeloc.ord[ind,c("lat","lon")]
  #d<- dist(latlon, method = "euclidean")
}

csobs<- cumsum(obsperyr)

#compute distance matrix for each year and put into block diagonal matrix
dist_allyrs<- matrix(0,nrow= nrow(dat),ncol=nrow(dat))

ind<- which(timeloc.ord$year==1981)
latlon<- timeloc.ord[ind,c("lat","lon")]
d<- dist(latlon, method = "euclidean")
d<- as.matrix(d)
dist_allyrs[1:obsperyr[1],1:obsperyr[1]]<- d

for(i in 2:32){
  print(i)
  ind<- which(timeloc.ord$year==1980+i)
  latlon<- timeloc.ord[ind,c("lat","lon")]
  d<- dist(latlon, method = "euclidean")
  d<- as.matrix(d)
  dist_allyrs[(1+ csobs[i-1]):csobs[i],(1+ csobs[i-1]):csobs[i]]<- d
}

#save the distance matrix
save(dist_allyrs,file="/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/dist_allyrs")