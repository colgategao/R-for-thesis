SEMtoSD<-function(SEM, N)
{
  SD<-SEM * sqrt(N)
  return(SD)
}
#post weaning data
mydata<-read.csv("~/R/MY WORK/postwean.csv")
SD<-array(dim = length(mydata$N))
for (i in 1:length(mydata$N)) {
  SD[i]<-SEMtoSD(mydata$SEM[i], mydata$N[i])
}
SD

simdata<-numeric()

repli=5000 #replication
for ( r in 1:repli) {
  for (i in 1:length(mydata$N)) {
    x<-numeric(length = mydata$N[i])
    #x<-rtruncnorm(mydata$N[i],a=4.4, b=5.5,mean=mydata$BMean[i],sd=SD_D[i])
    x<-rnorm(mydata$N[i],mydata$Mean[i],SD[i])
    simdata<-c(x,simdata)
  }
}
mean(simdata)
sd(simdata)
weight<-matrix(nrow=2, ncol=3, dimnames = list(c("mean", "sd"),c("BW", "pre weaning", "post weaing")))
weight[,3]<-c(mean(simdata), sd(simdata))
