#simple linear regression model with piecewise linear function of heat
#in the mean function

rm(list=ls())

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/pwl_data")

dat<- pwl_data
dat<- dat[,-which(colnames(dat)=="fips01001")]
#eliminate one county to avoid multicollinearity
#dat<- dat[,-which(colnames(dat)=="-29")] 
#all growing seasons have same length
#so the amount of time across all categories in any growing season= 185days*24hrs
#this doesn't matter since we don't have to estimate a coefficient for each bin!!
dat$logyield<- log(dat$yield)
dat<- dat[,-which(colnames(dat)=="yield")]

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

ind4<- which(colnames(dat)=="logyield")
dat<- cbind(dat[,1:2],wsum1,wsum2,dat[,(ind3+1):ind4])

#to look at the residuals and covariate effect effects of the model with logyield, 
#comment out the lines with the model with yield
pw_simplefit<- lm(logyield~ ., data= dat)
save(pw_simplefit, file= "/storage/work/svr5482/Climate_CornYield-me/prelim_models/log_pw_simplefit")
pwsf_summary<- summary(pw_simplefit)
pwsf_summary
betas<-coef(pw_simplefit)
betanames<- names(betas)


#using yield instead of logyield
pw_simplefit<- lm(yield~ ., data= pwl_data)
save(pw_simplefit, file= "/storage/work/svr5482/Climate_CornYield-me/prelim_models/pw_simplefit")
pwsf_summary<- summary(pw_simplefit)
pwsf_summary
betas<-coef(pw_simplefit)
betanames<- names(betas)

#the coefficients for wsum1 and wsum2 are both highly significant.


x= seq(from= -29, to= 48, by= .1)
resp<- function(x,cutoff){
  y= rep(NA, length(x))
  for(i in 1:length(x)){
    if(x[i]<=cutoff) y[i]= betas[4]*x[i]
    if(x[i]>cutoff) y[i]= betas[4]*x[i] + betas[5]*(x[i]-cutoff)
  }
  return(y)
}

y<- resp(x,29)
#plot(x,y,type="l",main="Temperature effect on log(yield)",xlab="Degrees Celsius",ylab="Coefficient for exposure to 1C interval")
plot(x,y,type="l",main="Temperature effect on yield",xlab="Degrees Celsius",ylab="Coefficient for exposure to 1C interval")

#filename<- "/storage/work/svr5482/Climate_CornYield-me/prelim_models/plots/tempeffect_logyield_pwslr.png"
filename<- "/storage/work/svr5482/Climate_CornYield-me/prelim_models/plots/tempeffect_yield_pwslr.png"
png(filename, width = 1000, height = 720)
#plot(x,y,type="l",main="Temperature effect on log(yield)",xlab="Degrees Celsius",ylab="Coefficient for exposure to 1C interval")
plot(x,y,type="l",main="Temperature effect on yield",xlab="Degrees Celsius",ylab="Coefficient for exposure to 1C interval")
dev.off()
#this looks similar to S&R plot again, good!!

#look for patterns in the residuals
yhat=pw_simplefit$fitted #get the fitted values of y
resids=pw_simplefit$resid #get the residuals
plot(yhat,resids)
abline(h=0,col="red")

#look at qqplot of standardized residuals
stdresids= rstandard(pw_simplefit)
#png(paste("/storage/work/svr5482/Climate_CornYield-me/prelim_models/plots/qqplot_logyield_pwslr.png",sep=""), width = 1000, height = 720)
png(paste("/storage/work/svr5482/Climate_CornYield-me/prelim_models/plots/qqplot_yield_pwslr.png",sep=""), width = 1000, height = 720)
qqnorm(stdresids, ylab="Standardized Residuals", xlab="Normal Scores", main="QQPlot") 
qqline(stdresids)
dev.off()
