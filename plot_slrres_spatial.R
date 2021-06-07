#mapping residuals in space for each year from simple linear regression model
rm(list = ls())
#load necessary packages
library(sp)
#library(rgeos)
library(gpclib)
library(maps)
library(maptools)
#library(ncdf4)
library(ggplot2)
library(usmap)
#library(precintcon)
#library(BMS)
#library(adaptMCMC)
#library(geoR) #for variogram
library(housingData) #for returning county centroid
#library(gstat)
#library(fields)
#library(binaryLogic)
library(stringr)

#load slr model with piecewise linear temperature function and residuals
load("/storage/work/svr5482/Climate_CornYield-me/prelim_models/pw_simplefit")
load("/storage/work/svr5482/Climate_CornYield-me/prelim_models/step_simplefit")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_df")
#load("C:/Users/saman/Dropbox/Climate_CornYield-me/prelim_models/output/slrmodel")
step_slr_res<- resid(simplefit)
pw_slr_res<- resid(pw_simplefit)

fips<- as.character(timeloc_df$fips)

#dat<- stepfix
#rm(stepfix)

#first<- which(colnames(dat)=="01001")
#last<- which(colnames(dat)=="55141")
#fips<- rep(NA, nrow(dat))
#for(i in first:last){
#  fips[which(dat[,i]==1)]<- colnames(dat)[i]
#}

slr_res.df<- as.data.frame(cbind(pw_slr_res,step_slr_res,fips,timeloc_df$year))
slr_res.df$pw_slr_res<- as.numeric(slr_res.df$pw_slr_res)
slr_res.df$step_slr_res<- as.numeric(slr_res.df$step_slr_res)
slr_res.df$fips<- as.numeric(slr_res.df$fips)
colnames(slr_res.df)[4]<- "year"
slr_res.df$year<- as.numeric(slr_res.df$year)

for(i in 1:32){
  filename<-paste("/storage/work/svr5482/Climate_CornYield-me/prelim_models/plots/spatial/res_stepslr_",1980+i,".jpeg",sep="")
  jpeg(file = filename,width = 800,height=800)
  res<- as.data.frame(slr_res.df[which(slr_res.df$year==(1980+i)),])
  a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
                                            "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
                                            "LA","AR","IA"), data=res, values="step_slr_res") +
    labs(title = paste(1980+i," Yield Resid.- Step SLR",sep=""))+
    scale_fill_gradient2(low = "blue",mid="white", high ="red", limits=c(min(slr_res.df$step_slr_res),max(slr_res.df$step_slr_res)), name="Residual")+
    theme(plot.title = element_text(size=14))
  plot(a)
  dev.off()

  filename<-paste("/storage/work/svr5482/Climate_CornYield-me/prelim_models/plots/spatial/res_pwslr_",1980+i,".jpeg",sep="")
  jpeg(file = filename,width = 800,height=800)
  res<- as.data.frame(slr_res.df[which(slr_res.df$year==(1980+i)),])
  a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
                                            "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
                                            "LA","AR","IA"), data=res, values="pw_slr_res") +
    labs(title = paste(1980+i," Yield Resid.- PWSLR",sep=""))+
    scale_fill_gradient2(low = "blue",mid="white", high ="red", limits=c(min(slr_res.df$pw_slr_res),max(slr_res.df$pw_slr_res)), name="Residual")+
    theme(plot.title = element_text(size=14))
  plot(a)
  dev.off()
}

#compute the variogram for each year's residuals
#load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")

#slr_res.df$lat<- timeloc_df$lat; slr_res.df$lon<- timeloc_df$lon

#dir.create("C:/Users/saman/Dropbox/Climate_CornYield-me/prelim_models/plots/slrresids_vg")
#for(i in 1:32){
#  loc_sub<- cbind(slr_res.df$lat[which(slr_res.df$year==(1980+i))], slr_res.df$lon[which(slr_res.df$year==(1980+i))])
#  data_sub<- df$yield[which(slr_res.df$year==(1980+i))]
#  fips_sub<- slr_res.df$fips[which(slr_res.df$year==(1980+i))]
#  prism_sub<- slr_res.df[slr_res.df$year==(1980+i),]
#  gd_prism_sub<- as.geodata(prism_sub, coords.col= 6:7, data.col = 5, covar.col = c("fips", "year"))
#  vg<- variog(geodata= gd_prism_sub)
#  filename<-paste("C:/Users/saman/Dropbox/Climate_CornYield-me/prelim_models/plots/slrresids_vg/res_freqns_",1980+i,".jpeg",sep="")
#  jpeg(file = filename,width = 800,height=800)
#  plot(vg)
#  dev.off()
#}

#mindist<- rep(NA,32)
#for(i in 1:32){
#  loc_sub<- cbind(slr_res.df$lat[which(slr_res.df$year==i)], slr_res.df$lon[which(slr_res.df$year==i)])
#  dists<- dist(loc_sub,method= "euclidean")
#  dists<- as.matrix(dists)
#  diag(dists)<- 3000
#  mindist[i]<- min(dists)
#}
