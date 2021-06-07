#fit a full-fledged linear regression model 
rm(list=ls())
library(mvtnorm);library(fields); library(invgamma); library(ggplot2)
setwd("C:/Users/saman/Dropbox/Climate_CornYield-me")
#setwd("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me")
#load("C:/Users/saman/Dropbox/Climate_CornYield-me/SourceData/METDATA/step3data")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/step3data")

#get rid of fips code 01001, temp <0 so we don't have linearly dependent regressors
dat<- step3data
dat<- dat[,-which(colnames(dat)=="fips01001")]
dat<- dat[,-which(colnames(dat)=="<0")] 
dat$logyield<- log(dat$yield)
dat<- dat[,-which(colnames(dat)=="yield")]

simplefit<- lm(logyield~ ., data= dat)
save(simplefit, file= "/storage/work/svr5482/Climate_CornYield-me/prelim_models/log_step_simplefit")
summary(simplefit)
betas<-coef(simplefit)
betanames<- names(betas)

tempbetas<- betas[4:17]
plot(seq(0,39,by=3),tempbetas, type= "l", xlab= "Degrees Celsius", ylab= "Coefficient Estimate", main= "Effect of Amount of Time Spent in Each 3C Temperature Interval on log(Yield)")

png(paste("/storage/work/svr5482/Climate_CornYield-me/prelim_models/plots/tempeffect_logyield_stepslr.png",sep=""), width = 1000, height = 720)
plot(seq(0,39,by=3),tempbetas, xlab= "Degrees Celsius", ylab= "Coefficient Estimate", main= "Effect of Amount of Time Spent in Each 3C Temperature Interval on log(Yield)")
lines(seq(0,39,by=3),tempbetas)
dev.off()

#look for patterns in the residuals
yhat=simplefit$fitted #get the fitted values of y
resids=simplefit$resid #get the residuals
plot(yhat,resids)
abline(h=0,col="red")

#look at qqplot of standardized residuals
stdresids= rstandard(simplefit)
png(paste("/storage/work/svr5482/Climate_CornYield-me/prelim_models/plots/qqplot_logyield_stepslr.png",sep=""), width = 1000, height = 720)
qqnorm(stdresids, ylab="Standardized Residuals", xlab="Normal Scores", main="QQPlot") 
qqline(stdresids)
dev.off()

#plot(simplefit,resid=T,pch=2,lwd=3,cex=.015)

#the residuals show a reverse fanning pattern, so we'll transform log yield back to yield
dat2<- dat[,-which(colnames(dat)=="logyield")]
dat2$yield<- step3data$yield

simplefit<- lm(yield~., data= dat2)
save(simplefit, file= "/storage/work/svr5482/Climate_CornYield-me/prelim_models/step_simplefit")
summary(simplefit)

betas2<-coef(simplefit)
betanames2<- names(betas2)
tempbetas2<- betas2[4:17]

png(paste("/storage/work/svr5482/Climate_CornYield-me/prelim_models/plots/tempeffect_yield_stepslr.png",sep=""), width = 1000, height = 720)
plot(seq(0,39,by=3),tempbetas2, type= "l",xlab= "Degrees Celsius", ylab= "Coefficient Estimate", main= "Effect of Amount of Time Spent in Each 3C Temperature Interval on Yield")
dev.off()
#The same pattern exists in the coefficients of the amount of time spent in each 3 degree temperature interval

#plot the residuals vs the predicted values
yhat=simplefit$fitted #get the fitted values of y
resids=simplefit$resid #get the residuals
plot(yhat,resids)
abline(h=0,col="red")

#plot standardized residuals
stdresids= rstandard(simplefit)
plot(yhat,stdresids)
abline(h=0,col="red")
#We see less reverse fanning of the residuals when we model yield instead of logyield
#There does not appear to be autocorrelation of the residuals either

#Next, we'll look at the qqplot

#We also need to check for normality of the residuals

#We next check to see whether the mean relationship is correctly specified
#plot(simplefit,resid=T,pch=2,lwd=3,cex=.015)

png(paste("/storage/work/svr5482/Climate_CornYield-me/prelim_models/plots/qqplot_yield_stepslr.png",sep=""), width = 1000, height = 720)
qqnorm(stdresids, ylab="Standardized Residuals", xlab="Normal Scores", main="QQPlot") 
qqline(stdresids)
dev.off()

#The distribution here has heavier tails than the normal distribution, 
#as indicated by the normal QQ-plot of the standardized residuals.
#I think this is something we should be concerned with since we have a very large sample.

plot(yhat, dat2$yield)
abline(lm(dat2$yield~yhat), col="red")

#it seems as though the mean function is correctly specified
