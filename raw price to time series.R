mydata<-read.csv("may22.csv")

#find the weighted mean of Prime lambs based on sold number
A<-subset(mydata, CLASS=="PRIME LAMBS")
weighted.mean(A$PRICEHD_AVE, A$SOLD)

#set factor as date
A$SaleDate<-as.Date(A$SaleDate, "%d-%b-%y")

#delete CLASS, MIN and MAX
A$CLASS<-NULL
A$PRICEHD_MAX<-NULL
A$PRICEHD_MIN<-NULL

#find weighted mean based in time series
library(plyr)
Price_ts<-ddply(A, .(SaleDate), function(x) data.frame(Avg_price=weighted.mean(x$PRICEHD_AVE,x$SOLD)))
Price_ts
