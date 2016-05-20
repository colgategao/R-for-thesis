library("Hmisc")
library("truncnorm")

#--------------------------
mydata<-read.csv("DMproduction.csv")
dimnames(mydata)[[1]]<-c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")


#---------------set up parameters-----------
anmean<-11
ansd<-0.38
rep1<-100


#--------------initial data structure
antotal<-array(0, dim = rep1)

DailyRate<-matrix(0, nrow=rep1, ncol= length(mydata$Mean))
results<-matrix(0, nrow = rep1, ncol = length(mydata$Mean))
colnames(DailyRate)<-c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")

monthlytotal<-matrix(0, nrow = rep1, ncol = length(mydata$Mean))
colnames(monthlytotal)<-c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")


#---------------generate randomness-------------
for (i in 1:rep1) {
  

  ##---------------generate Yearly Scale----------
YS<-rtruncnorm(1, a=0, mean = anmean, sd = ansd)
  #YS

  ##---------------deviation---------------------
Monthmean<-mydata$Mean-YS

  ##---------------generate random value in 1 year

  results[i,]<-rtruncnorm(1, a = -YS, mean = Monthmean, sd=mydata$SD)
  DailyRate[i,]<-results[i,] + YS
  monthlytotal[i,]<-DailyRate[i,]*mydata$Nodate
  antotal[i]<-sum(monthlytotal[i,])
}

mm<-colMeans(monthlytotal)
boxplot(monthlytotal)
lines(mm, col="red")
Daily<-as.vector(t(DailyRate))

write.csv(Daily, file = "DMG.CSV", row.names = F, col.names = F)