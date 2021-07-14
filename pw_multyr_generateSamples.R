#generateSamples with Schlenker and Roberts mean function
#and block diagonal covariance structure
rm(list=ls())
library(rdist); library(mvtnorm) ; library(classInt)
source(file = "/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/PICAR/source/sharedFunctions.R")

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/pwl_norm_yrorder")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")
#dist_allyrs is a block diagonal matrix with each block containing the distances
#between counties for each year
#load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/dist_allyrs")

dat<- pwln.ord
dat<- dat[,-which(colnames(dat)=="fips01001")]
#eliminate one county to avoid multicollinearity
#dat<- dat[,-which(colnames(dat)=="-29")] 
#all growing seasons have same length
#so the amount of time across all categories in any growing season= 185days*24hrs
#this doesn't matter since we don't have to estimate a coefficient for each bin!!

#need to first format the data so that we have the proper model
#create a vector of half-degree values
deg1<- seq(from= -28.5, to= 47.5, by= 1)
deg2<- seq(from= .5, to= 18.5, by= 1)
ind1<- which(colnames(dat)=="-29")
ind2<- which(colnames(dat)=="28")
ind3<- which(colnames(dat)=="47")

ifyouaintfirst<- as.matrix(dat[,ind1:ind3])
yourelast<- as.matrix(dat[,(ind2+1):ind3])

wsum1<- ifyouaintfirst%*%deg1
wsum2<- yourelast%*%deg2

ind4<- which(colnames(dat)=="state55yr")
dat<- cbind(dat[,1:3],wsum1,wsum2,dat[,(ind3+1):(ind4-1)])

inds1<- which(colnames(dat)=="state1yr")
inds54<- which(colnames(dat)=="state54yr")

for(j in inds1:inds54){
  temp.ind<- which(dat[,j]!=0)
  dat[temp.ind,j]<- 1
}

#timeloc.ord$year<- as.numeric(timeloc.ord$year)
#nyrs= 5
nyrs= 2

#compute number of observations per year
obsperyr<- rep(NA,nyrs)

for(i in 1:nyrs){
  ind<- which(timeloc.ord$year==1980+i)
  obsperyr[i]<- length(ind)
  #latlon<- timeloc.ord[ind,c("lat","lon")]
  #d<- dist(latlon, method = "euclidean")
}
csobs<- cumsum(obsperyr)

#set state with State ANSI code 55 to be default to avoid multicollinearity
#so we only go to column 41, not 42
dat<- dat[1:csobs[nyrs],]
rm(pwln.ord)

# create training set, test set

#holding out 10% of the data from each year ensures we're not
#training our model based on some years more than others

test.sizes<- round(obsperyr/10)
train.sizes<- obsperyr-test.sizes
cs.sizes<- cumsum(test.sizes)
cs.trsizes<- cumsum(train.sizes)
test_indx<- rep(NA, cs.sizes[nyrs])
#test_indList<-list() 

for(i in 1:nyrs){
  if(i==1){
    cvind<- c(1:csobs[1])
    set.seed(i)
    test_indx[1:cs.sizes[1]]<- sample(cvind, test.sizes[i], replace = FALSE, prob = NULL)
    #set.seed(i)
    #test_indList[[i]]<- sample(cvind, test.sizes[i], replace = FALSE, prob = NULL)
  } 
  if(i>1){
    cvind<-c( (csobs[i-1]+1):csobs[i])
    set.seed(i)
    test_indx[(cs.sizes[i-1]+1):cs.sizes[i]]<- sample(cvind, test.sizes[i], replace = FALSE, prob = NULL)
    #set.seed(i)
    #test_indList[[i]]<- sample(cvind, test.sizes[i], replace = FALSE, prob = NULL)
  } 
}

#test_indx2<- c(test_indList[[1]],test_indList[[2]],test_indList[[3]],test_indList[[4]],test_indList[[5]])

#test_dataList<- list()
#for(i in 1:nyrs){
#  test_dataList[[i]]<- as.matrix(dat[test_indList[[i]],])
#}

test_data<- dat[test_indx,]
train_data<- dat[-test_indx,]

n= nrow(train_data); modInd= 1:n; cvInd= (n+1):nrow(dat)

# set the grid locations for training, testing data. Combine
timeloc.dat<- timeloc.ord[1:csobs[nyrs],]; rm(timeloc.ord)

gridLocation<- cbind(timeloc.dat$lon[-test_indx], timeloc.dat$lat[-test_indx])
CVgridLocation<- cbind(timeloc.dat$lon[test_indx], timeloc.dat$lat[test_indx])
comboLocation<-rbind(gridLocation,CVgridLocation)
save(comboLocation,file="/storage/work/svr5482/Climate_CornYield-me/PICAR/pw_multyrs-comboLocation")

##calculate the distances between the locations within each year
distMatModList<- list()
distMatCVList<- list()

for(i in 1:nyrs){
  if(i==1){
    distMatModList[[i]]<- as.matrix(rdist(gridLocation[1:cs.trsizes[i],],metric="euclidean"))
    distMatCVList[[i]]<- as.matrix(rdist(CVgridLocation[1:cs.sizes[i],],metric="euclidean"))
  }
  if(i>1){
    distMatModList[[i]]<- as.matrix(rdist(gridLocation[(cs.trsizes[i-1]+1):cs.trsizes[i],],metric="euclidean"))
    distMatCVList[[i]]<- as.matrix(rdist(CVgridLocation[(cs.sizes[i-1]+1):cs.sizes[i],],metric="euclidean"))
  }
}
#distMatMod<- as.matrix(rdist(gridLocation))
#distMatCV<- as.matrix(rdist(CVgridLocation))
#distMatFull<-as.matrix(rdist(comboLocation))

# Observations (using log yield instead of yield)
obsFullLinear<- c(train_data$yield, test_data$yield)
#obsFullLinear<- log(spdata2$yield)
# Observations that we will use to create the model
obsModLinear<-obsFullLinear[modInd] 
obsCVLinear<- obsFullLinear[cvInd]

# Covariates
XMat<-cbind(rep(1, nrow(train_data)), train_data[,2:ncol(train_data)])
colnames(XMat)[1]<- "ones"
XMatCV<-cbind(rep(1, nrow(test_data)), test_data[,2:ncol(test_data)])
colnames(XMatCV)[1]<- "ones"

XMat<-as.matrix(XMat)
XMatCV<- as.matrix(XMatCV)


save.image(file="/storage/work/svr5482/Climate_CornYield-me/PICAR/Crop_multyrs_pwSpatialData.RData") # Save Data
#save.image(file="C:/Users/saman/Dropbox/Climate_CornYield-me/PICAR/output/Crop1981_SpatialData.RData") # Save Data 
