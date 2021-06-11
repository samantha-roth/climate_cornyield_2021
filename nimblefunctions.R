#creating my own nimble function to calculate the multivariate normal density
#of the yield observations


dmvn<- nimbleFunction(
  run = function(x=double(1), mu=double(1), invSigma=double(2), detSigma= double(0),
                 log = integer(0, default = 0)) { # type declarations
    returnType(double(0))  # return type declaration
    k <- dim(invSigma)[1] #number of observations
    p1<- (-k/2)*log(2*pi)+(-1/2)*log(detSigma)
    #p2<- (-1/2)*t(x-mu)%*%invSigma%*%(x-mu))
    p2<- (-1/2)*inprod((x-mu),invSigma%*%(x-mu))
    logProb <- p1+p2
    if(log) return(logProb)
    else return(exp(logProb)) 
  } )


#rmyexp <- nimbleFunction(
#  run = function(n = integer(0), mu=double(1), invSigma=double(2), detSigma= double(0)) {
#    returnType(double(0))
#    if(n != 1) print("rmyexp only allows n = 1; using n = 1.")
#    dev <- runif(1, 0, 1)
#    return(-log(1-dev) / rate)
#  })


#dmvn<- nimbleFunction(
#  run = function(x=double(1), mu=double(1), invSigma=double(2), detSigma= double(0)) { # type declarations
#    returnType(double(0))  # return type declaration
#    k <- dim(invSigma)[1] #number of observations
#    p1<- ((2*pi)^(-k/2))*(detSigma^(-1/2))
#    p2<- t(x-mu)%*%invSigma%*%(x-mu)
#    p3<- exp(-p2/2)
#    dens <- p1*p3
#    return(dens)
#  } )

#obs<- rnorm(5,mean=0,sd=1)
#mean= rep(0,5)
#Sigma<- diag(1,nrow=5)
#invSigma<- solve(Sigma)
#detSigma<- det(Sigma)

#dmvn(obs,mean,invSigma,detSigma)
#library(mnormt)
#dmnorm(x=obs, mean = rep(0, 5), varcov=Sigma, log = FALSE)
#yay my function works
