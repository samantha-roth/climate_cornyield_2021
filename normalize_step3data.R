#normalize the data for improved computation
load("C:/Users/saman/Dropbox/Climate_CornYield-me/SourceData/METDATA/step3data")

n.pr<- normalize(step3data$Pr_GS, method = "standardize")
n.pr2<- n.pr^2

ind1<- which(colnames(step3data)=="state1yr")
ind2<- which(colnames(step3data)=="state55yr")
ind3<- which(colnames(step3data)=="state55yr_sq")

n.styr<- step3data[,ind1:ind2]

for(i in 1:ncol(n.styr)){
  n.styr[which(n.styr[,i]>0),i]<- normalize(n.styr[which(n.styr[,i]>0),i], method = "standardize")
}

n.styr2<- n.styr^2

ind4<- which(colnames(step3data)=="<0")
ind5<- which(colnames(step3data)==">39")
n.temp<- step3data[,ind4:ind5]
for(i in 1:ncol(n.temp)){
  n.temp[,i]<- normalize(n.temp[,i], method="standardize")
}

step3norm<- step3data
step3norm[,ind1:ind3]<- cbind(n.styr,n.styr2)
step3norm[,ind4:ind5]<- n.temp
step3norm[,2:3]<- cbind(n.pr,n.pr2)

save(step3norm,file="C:/Users/saman/Dropbox/Climate_CornYield-me/SourceData/METDATA/step3norm")
