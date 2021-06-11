rm(list=ls())
library(fields) ; library(mvtnorm) ; library(nimble)
#load data
load("/storage/work/svr5482/Climate_CornYield-me/PICAR/Crop_multyrs_pwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/PICAR/Crop_pw_multyrs_bases.RData")
#load("/storage/work/svr5482/Climate_CornYield-me/PICAR/Crop_pw_multyrs_max50bases.RData")
#source(file= "/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/PICAR/source/sharedFunctions.R")
#source(file= "/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/PICAR/source/batchmeans.R")
#source(file= "/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/PICAR/source/nimbleSource.R")
source(file="/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/bayesian_spacetime/linearspatialmodel_PICAR/source/nimblefunctions.R")

#put a zero in front of the cumulative sum vectors
#to make for loops easier to deal with
cs.obs.train<- c(0,cs.trsizes)
cs.obs.test<- c(0,cs.sizes)

#get the covariance matrices for each year
CList<- list()

for(i in 1:nyrs){
  M<- MList[[i]]
  CList[[i]]<- M%*%t(M)
}

#create the covariance matrix
CMat<- matrix(0,nrow= sum(train.sizes),ncol= sum(train.sizes))
for(i in 1:nyrs){
  C<- CList[[i]]
  Cadd<- C + diag(x=.00000001,nrow=nrow(C))
  CMat[(cs.obs.train[i]+1):cs.obs.train[i+1],(cs.obs.train[i]+1):cs.obs.train[i+1]]<- Cadd
}

#create the precision matrix
precMat<- matrix(0,nrow= sum(train.sizes),ncol= sum(train.sizes))
for(i in 1:nyrs){
  print(i)
  C<- CList[[i]]
  Cadd<- C + diag(x=.00000001,nrow=nrow(C))
  precMat[(cs.obs.train[i]+1):cs.obs.train[i+1],(cs.obs.train[i]+1):cs.obs.train[i+1]]<- solve(Cadd)
}
save(CMat,file="/storage/work/svr5482/Climate_CornYield-me/PICAR/semiPICAR/pw/CMat_5yrs.RData")

simplefit<- lm(yield~.,data=train_data)
init.betas<- as.numeric(coef(simplefit))

#Gaussian linear spatial model for yield
linear_model_string <- nimbleCode({
  # Data Model
  #Y[1:n] ~ dmnorm(mean = mn[1:n], prec = fullPrecMat[1:n,1:n])
  Y[1:n] ~ dmvn(mu= mn[1:n], invSigma= fullPrecMat[1:n,1:n], detSigma= detCovMat)
  
  #Constant and Covariance Matrix
  #tau2 = nugget
  detCovMat<- (sigma2^n)*detCMat
  fullPrecMat[1:n,1:n]<- (1/sigma2)*PrecMat[1:n,1:n]
  mn[1:n]<- X[1:n,1:n.betas]%*%beta[1:n.betas] #mean vector
  #covariance function defined above
  
  # Parameter Model
  # Set different priors
  sigma2   ~  dinvgamma(shape= .2, rate= .2)
  #set this to be so the mode is var(Y)/2
  mnb[1:n.betas]<- rep(0,n.betas)
  betaPrecMat[1:n.betas,1:n.betas]<- (1/10)*diag(n.betas)
  beta[1:n.betas] ~ dmnorm(mean= mnb[1:n.betas], prec= betaPrecMat[1:n.betas,1:n.betas])
})

#set constants, initial values
constants <- list(n= length(obsModLinear), n.betas= ncol(XMat))
#data <- list(Y= obsModLinear, PrecMat= precMat, X=XMat)
data <- list(Y= obsModLinear, PrecMat= precMat, X=XMat, detCMat= det(CMat))
inits <- list(beta= init.betas, sigma2= var(obsModLinear))
set.seed(26)

# Build Model
Rmodel<-nimbleModel(code=linear_model_string, data = data,  constants=constants, inits = inits)
Cmodel <- compileNimble(Rmodel)
modelConf <- configureMCMC(Rmodel, print = TRUE, multivariateNodesAsScalars=TRUE)
modelConf$addMonitors(c("beta", "sigma2"))
modelMCMC <- buildMCMC(modelConf)

# Run the mcmc algorithm
niter=100
CmodelMCMC <- compileNimble(modelMCMC)
set.seed(0)
pt<-proc.time()
CmodelMCMC$run(niter)
ptFinal<-proc.time()-pt
ptFinal_pw_mesh2p100yrm_100<- ptFinal
samples <- as.matrix(CmodelMCMC$mvSamples)
samples_pw_mesh2p100yrm_100<- samples
save(samples_pw_mesh2p100yrm_100,file="/storage/work/svr5482/Climate_CornYield-me/PICAR/semiPICAR/mydmvn/pw/samples_pw_mesh2p100yrm_100.RData")
save(ptFinal_pw_mesh2p100yrm_100, file="/storage/work/svr5482/Climate_CornYield-me/PICAR/semiPICAR/mydmvn/pw/ptFinal_pw_mesh2p100yrm_100.RData")

#ptFinal_pw_mesh2p50yrm_100<- ptFinal
#samples <- as.matrix(CmodelMCMC$mvSamples)
#samples_pw_mesh2p50yrm_100<- samples
#save(samples_pw_mesh2p50yrm_100,file="/storage/work/svr5482/Climate_CornYield-me/PICAR/semiPICAR/pw/samples_pw_mesh2p50yrm_100.RData")
#save(ptFinal_pw_mesh2p50yrm_100, file="/storage/work/svr5482/Climate_CornYield-me/PICAR/semiPICAR/pw/ptFinal_pw_mesh2p50yrm_100.RData")


#ptFinal_pw_mesh2yrm_1k<- ptFinal
#samples <- as.matrix(CmodelMCMC$mvSamples)
#samples_pw_mesh2yrm_1k<- samples
#save(samples_pw_mesh2yrm_1k,file="/storage/work/svr5482/Climate_CornYield-me/PICAR/semiPICAR/pw/samples_pw_mesh2yrm_1k.RData")
#save(ptFinal_pw_mesh2yrm_1k, file="/storage/work/svr5482/Climate_CornYield-me/PICAR/semiPICAR/pw/ptFinal_pw_mesh2yrm_1k.RData")
