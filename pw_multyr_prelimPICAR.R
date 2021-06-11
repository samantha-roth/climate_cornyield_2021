rm(list=ls())
library(fields) ; library(mvtnorm) ; library(INLA)
#setwd("C:/Users/saman/Dropbox/climate and crop research- summer 2020/PICAR/PICAR")
setwd("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/PICAR") #me
load("/storage/work/svr5482/Climate_CornYield-me/PICAR/Crop_multyrs_pwSpatialData.RData")
source(file= "/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/PICAR/source/sharedFunctions.R")
source(file= "/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/PICAR/source/batchmeans.R")
source(file= "/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/PICAR/source/nimbleSource.R")
#source(file= "source/sharedFunctions.R")

MLE_FindRank_Linear<-function(XMat,XMatCV,dimSeq,AMat,AMatCV,obsCV,obsMod,MoransOperatorEig){
  CVMSPE<-vector("numeric")
  betaMatList<-list() # Contains Beta Parameters
  for(ij in 1:ncol(XMat)){betaMatList[[ij]]<-matrix(NA,nrow=length(dimSeq),ncol=3)}
  for(jk in 1:length(dimSeq)){
    if(jk%%50==0){print(jk)}
    keepM<-1:dimSeq[jk]
    mBase<-(AMat%*%MoransOperatorEig$vectors[,keepM])
    mBaseCV<-(AMatCV%*%MoransOperatorEig$vectors[,keepM])
    XMat<- as.data.frame(XMat)
    st1<- which(colnames(XMat)=="state1yr")
    st54<- which(colnames(XMat)=="state54yr")
    for(col in st1:st54) XMat[,col]<- as.factor(XMat[,col])
    mbXM<- cbind(as.data.frame(as.matrix(mBase)),XMat[,-1]) #get rid of column of ones
    lm1<-glm(obsMod~.,data=mbXM,family = "gaussian")
    coeffs<-lm1$coefficients
    estMean<-coeffs[-(1:ncol(mBase))]
    lowCI<-estMean-1.975*sqrt(diag(vcov(lm1)))[-(1:ncol(mBase))]
    highCI<-estMean+1.975*sqrt(diag(vcov(lm1)))[-(1:ncol(mBase))]
    for(k in 1:length(betaMatList)){betaMatList[[k]][jk,]<-rbind(estMean,lowCI,highCI)[,k]}
    XMatCV<- as.data.frame(XMatCV)
    for(col in st1:st54) XMatCV[,col]<- as.factor(XMatCV[,col])
    mbXMCV<- cbind(as.data.frame(as.matrix(mBaseCV)),XMatCV[,-1])
    predCV<- predict(lm1,mbXMCV)
    CVMSPE[jk]<-mean((predCV-obsCV)^2)
  }
  return(list(CVMSPE,betaMatList))
}

# Make Mesh using INLA

#put a zero in front of the cumulative sum vectors
#to make for loops easier to deal with
cs.obs.train<- c(0,cs.trsizes)
cs.obs.test<- c(0,cs.sizes)

# First, need to divide up grid locations so we have one grid per year
meshList<- list()
AMatList<- list()
AMatCVList<- list()
mBaseList<- list()
MList<- list()
MOEList<- list()

for(i in 1:nyrs){
  #generate mesh
  mesh<- inla.mesh.2d(gridLocation[(cs.obs.train[i]+1):cs.obs.train[i+1],], 
                      max.edge=c(1),
                      min.angle= 5, #smaller minimum triangle angle
                      cutoff = 0.005, #smaller minimum distance between points
                      offset=c(0.1, 0.1))
  meshList[[i]]<- mesh
  
  # Projector Matrix 
  AMat <- inla.spde.make.A(mesh, loc=gridLocation[(cs.obs.train[i]+1):cs.obs.train[i+1],])  # model-fitting locations
  AMatList[[i]]<- AMat
  AMatCV <- inla.spde.make.A(mesh, loc=CVgridLocation[(cs.obs.test[i]+1):cs.obs.test[i+1],]) # validation locations
  AMatCVList[[i]]<- AMatCV
  
  # Figure for Mesh and observation locations 
  filename<- paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/plot_pw_mesh2_yr",1980+i,".png",sep="")
  png(filename, width = 1000, height = 700)
  par(mfrow=c(1,1),mar=c(2,2,2,2))
  plot(mesh,main="")
  points(x=mesh$loc[,1], y=mesh$loc[,2],col="black",pch=16,cex=0.4)
  points(x=gridLocation[(cs.obs.train[i]+1):cs.obs.train[i+1],1], 
         y=gridLocation[(cs.obs.train[i]+1):cs.obs.train[i+1],2],col="blue",pch=16,cex=.8)
  points(x=CVgridLocation[(cs.obs.test[i]+1):cs.obs.test[i+1],1], 
         y=CVgridLocation[(cs.obs.test[i]+1):cs.obs.test[i+1],2],col="red",pch=18,cex=1.2)
  mtext("Mesh 2: Piecewise Linear Heat Function",cex=2)
  legend("topright", legend=c("Model Fitting" , "Validation" , "Mesh Vertices"),
         col=c("blue","red","black"), pch=c(16,16,16),cex = 0.5)
  dev.off()
  
  # Generate Moran's Basis Functions
  # See Hughes and Haran (2012) or Lee and Haran (2020+) for details
  wp<-ncol(AMat)
  DMat<-diag(apply(mesh$graph$vv,1,sum))
  WeightMat<-mesh$graph$vv
  PrecMat<-DMat-WeightMat  
  Nnew<-nrow(WeightMat)
  OrthSpace<-diag(Nnew)-(rep(1,Nnew)%*%t(rep(1,Nnew)))/Nnew
  MoransOperator<-OrthSpace%*%(WeightMat%*%OrthSpace)# Moran's Operator
  # Moran's Basis functions
  MoransOperatorEig<-eigen(MoransOperator) # Eigenvectors of the Moran's Operator
  MOEList[[i]]<-MoransOperatorEig
  
  #Select Rank for PICAR
  dimSeq<-seq(2, 100, by=1) # ADjust the endpoint (50) and the resolution (1)
  heuristicResults<-MLE_FindRank_Linear(XMat=XMat[(cs.obs.train[i]+1):cs.obs.train[i+1],],
                                        XMatCV=XMatCV[(cs.obs.test[i]+1):cs.obs.test[i+1],],
                                        dimSeq=dimSeq,
                                        AMat=AMat,
                                        AMatCV=AMatCV,
                                        obsCV=obsCVLinear[(cs.obs.test[i]+1):cs.obs.test[i+1]],
                                        obsMod=obsModLinear[(cs.obs.train[i]+1):cs.obs.train[i+1]],
                                        MoransOperatorEig = MoransOperatorEig)
  
  heur_res_name<- paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/plot_pw50_heur_res_cvmspe",1980+i,".png",sep="")
  png(heur_res_name, width = 1000, height = 720)
  plot(x=dimSeq,y=heuristicResults[[1]],typ="l" , main="PW CVMSPE") # CVMSPE
  dev.off()
  
  # Select Rank of Moran's Basis Functions
  # We choose the rank that yields the lowest CVMSPE based on the above
  pBase<-dimSeq[which.min(heuristicResults[[1]])] 
  print(pBase)
  mBase<-MoransOperatorEig$vectors[,1:pBase] # Moran's Basis Function
  p<-ncol(mBase) #p is the number of bases functions
  #p<- 20
  M<-as.matrix(AMat%*%mBase)
  AMatList[[i]]<- AMat
  mBaseList[[i]]<- mBase
  MList[[i]]<-M
}

save(AMatCVList,AMatList,mBaseList,meshList,MList,MOEList,file="/storage/work/svr5482/Climate_CornYield-me/PICAR/Crop_pw_multyrs_bases.RData") 
#save(AMatCVList,AMatList,mBaseList,meshList,MList,MOEList,file="/storage/work/svr5482/Climate_CornYield-me/PICAR/Crop_pw_multyrs_max50bases.RData") 
# Save Data
