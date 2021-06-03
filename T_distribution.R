#This is a function that calculates the exposure time (in hours) of in each 1-degree interval
T_distribution<-function(Tmax,Tmin,T_interval) {
  #Input includes a vector of daily Tmax, Tmin in degrees C (should be in the same length), 
  # and T_interval is the upper bound and lower bound of temperature to cover (both bounds should be integers) 
  # (e.g. T_interval=c(0,30) returns the temperature distribution from 0 to 30)
  if (length(Tmax)!=length(Tmin)){
    print("Tmax and Tmin must have the same length")
    exit
  }
  if (!all(Tmax>=Tmin)){
    print("Tmax cannot be less than Tmin")
    exit
  }
  if (length(T_interval)!=2){
    print("T_interval must have a length of 2")
    exit
  }
  if (!(all.equal(T_interval, as.integer(T_interval)))){
    print("T_interval must be integers")
    exit
  }
  
  T_up<-max(T_interval) #T upper and lower bound
  T_low<-min(T_interval)
  days<-length(Tmax) #Total days
  T_dis<-rep(0,T_up-T_low) #Total time in each 1-deg interval to calculate
  
  for (i in 1:days){
    tmax<-Tmax[i]
    tmin<-Tmin[i]
    tavg<-(tmax+tmin)*0.5 #center of cosine curve
    thalfrange<-(tmax-tmin)*0.5 #amplitude of cosine curve
    #Assume each day's T distribution is sinusoid with tmax and tmin
    for (j in 1:(T_up-T_low)){
      T1<-T_low+j-1
      T2<-T_low+j
      if ((T2>tmin)&(T1<tmax)){ #add the time exposure if this 1-deg interval is within this day's temperature range
        #if one side of the 1-deg interval falls outside the temperature range, set it equal to the boundary
        if (T2>tmax){
          T2<-tmax
        }
        if (T1<tmin){
          T1<-tmin
        }
        T_dis[j]<-T_dis[j]+(acos(pmin(pmax((T1-tavg)/thalfrange,-1.0),1.0))*12/pi
                            -acos(pmin(pmax((T2-tavg)/thalfrange,-1.0),1.0))*12/pi)*2
      }
    }
  }
  return(T_dis)
}
  