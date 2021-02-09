setwd("D:/Desktop/Dissertation")
ts.df<-read.csv("timedata.csv")
ts.df<-ts.df[1:1584,]
ts.df[is.na(ts.df)] <- 0

#par(mfrow=c(3,1))
#plot(Reg_mean~yearBP,data=ts.df)
#plot(num_stor~yearBP,data=ts.df)
#plot(meanelev~yearBP,data=ts.df)

#plot(Reg_mean~yearBP,data=ts.df)
#plot(meanCDr~yearBP,data=ts.df)
#plot(meanVS_tot~yearBP,data=ts.df)

#plot(Reg_mean~yearBP,data=ts.df)
#plot(meanVS_fp~yearBP,data=ts.df)
#plot(meanVS_tot~yearBP,data=ts.df)

precip<-predict(loess(Reg_mean~yearBP,data=ts.df,span=.17),se.fit=T)
num<-predict(loess(num_stor~yearBP,data=ts.df,span=.1),se.fit=T)
elev<-predict(loess(meanelev~yearBP,data=ts.df,span=.1),se.fit=T)
cd<-predict(loess(meanCDr~yearBP,data=ts.df,span=.1),se.fit=T)
vsfp<-predict(loess(meanVS_fp~yearBP,data=ts.df,span=.1),se.fit=T)
vstot<-predict(loess(meanVS_tot~yearBP,data=ts.df,span=.1),se.fit=T)

#plot(precip~yearBP,data=ts.df,type="l")
#plot(num~yearBP,data=ts.df,type="l")
#plot(elev~yearBP,data=ts.df,type="l")

#plot(precip~yearBP,data=ts.df,type="l")
#plot(cd~yearBP,data=ts.df,type="l")
#plot(vstot~yearBP,data=ts.df,type="l")

#plot(precip~yearBP,data=ts.df,type="l")
#plot(vsfp~yearBP,data=ts.df,type="l")
#plot(vstot~yearBP,data=ts.df,type="l")


par(mfrow=c(4,1), mai=c(0.4,0.7,0.2,.1))
#plot(precip~yearBP,data=ts.df,type="l")
#plot(num~yearBP,data=ts.df,type="l")
#plot(cd~yearBP,data=ts.df,type="l")
#plot(vstot~yearBP,data=ts.df,type="l")

#panel 1
library(raster)
library(RColorBrewer)
library(maps)
library(Cairo)


#moving average
ts.df<-read.csv("timedata.csv")
ts.df<-ts.df[1:1584,]
ts.df[is.na(ts.df)] <- 0
mean<-ts.df
mean1<-mean[500:1500,]
library(RColorBrewer)
ma <- function(arr, n=15){
  res = arr
  for(i in n:length(arr)){
    res[i] = mean(arr[(i-n):i])
  }
  res
}

RB<-brewer.pal(n = 11, name = 'RdBu')
pal = colorRampPalette(RB)
mean1$order = findInterval(mean1$Reg_mean, sort(mean1$Reg_mean))

plot(Reg_mean~yearBP,data=mean1, type="p", pch=19, col=pal(nrow(mean1))[mean1$order], lwd=1, ylab="Mean Precip (mm)", xlab="Year BP",cex.lab=1.3)

ma7yr<-ma(mean$Reg_mean, n=30) #7 year running average following Benson et al. 2013

lines(ma7yr~mean$yearBP, col="black", lwd=2)

legend(x=450, y=20, bty="n", legend= "30 Year Average", lty=1, lwd=2, col="black", cex=1, xpd=NA)



abline(v=800, lty=2, lwd=3, col="grey")
abline(v=980, lty=2, lwd=3, col="grey")


abline(v=800, lty=2, lwd=3, col="grey")
abline(v=980, lty=2, lwd=3, col="grey")
text(x=1450,y=475, labels = "A",cex=2)
#abline(v=809, lty=2, lwd=3, col="grey")

par(mai=c(0,0.7,0,.1))
plot(num[500:1500]~yearBP,data=mean[500:1500,],type="l",lwd=2,xlab="",ylab="Num of Storage Features",xaxt='n',cex.lab=1.3)
abline(v=800, lty=2, lwd=3, col="grey")
abline(v=980, lty=2, lwd=3, col="grey")
text(x=1450,y=25, labels = "B",cex=2)

plot(cd[500:1500]~yearBP,data=mean[500:1500,],type="l",lwd=2,xlab="",ylab="Mean Cost Dist (mins)",xaxt='n',cex.lab=1.3)
abline(h=absmeanCD,col=1, lty=3,lwd=1)
abline(v=800, lty=2, lwd=3, col="grey")
abline(v=980, lty=2, lwd=3, col="grey")
text(x=550,y=75, labels = "Mean Cost Dist of Random Points",cex=1.3)
text(x=1450,y=160, labels = "C",cex=2)
par(mai=c(0.7,0.7,0,.1))

plot(vstot[500:1500]~yearBP,data=mean[500:1500,],type="l",lwd=2,xlab="Years Before Present (BP)",ylab="Mean Viewshed",cex.lab=1.3)
text(x=570,y=40000+5000, labels = "Mean Viewshed Size of Random Points",cex=1.3)
abline(h=absmeanVS,col=1, lty=3,lwd=1)
abline(v=800, lty=2, lwd=3, col="grey")
abline(v=980, lty=2, lwd=3, col="grey")
text(x=1450,y=70000, labels = "D",cex=2)




##Scaling and shit like that
ts.df$scaled_Reg<-scale(ts.df$Reg_mean)

ts.df$summed<-cumsum(ts.df$scaled_Reg)
ts.df<-ts.df[1:1584,]
ts.df[is.na(ts.df)] <- 0
par(mfrow=c(4,1), mai=c(0,0.7,0.2,.1))
plot(summed~yearBP,data=ts.df[500:1500,], type="l", ylab="Mean Precip (mm)", xlab="",xaxt='n',cex.lab=1.3,lwd=2)

abline(h=0,lty=2,col="gray")

#legend(x=50, -20, bty="n", legend=c("Annual", "7 Year Average"), lty=c(1,1), lwd=c(1,2), col=c("black", "red"), cex=.75, xpd=NA)


abline(v=800, lty=2, lwd=3, col="grey")
abline(v=980, lty=2, lwd=3, col="grey")
text(x=1450,y=475, labels = "A",cex=2)
#abline(v=809, lty=2, lwd=3, col="grey")

par(mai=c(0,0.7,0,.1))
plot(num[500:1500]~yearBP,data=mean[500:1500,],type="l",lwd=2,xlab="",ylab="Num of Storage Features",xaxt='n',cex.lab=1.3)
abline(v=800, lty=2, lwd=3, col="grey")
abline(v=980, lty=2, lwd=3, col="grey")
text(x=1450,y=25, labels = "B",cex=2)

plot(cd[500:1500]~yearBP,data=mean[500:1500,],type="l",lwd=2,xlab="",ylab="Mean Cost Dist (mins)",xaxt='n',cex.lab=1.3)
#polygon(x=mean[500:1500,]$yearBP,y=cd[500:1500])
abline(h=absmeanCD,col=1, lty=3,lwd=1)
abline(v=800, lty=2, lwd=3, col="grey")
abline(v=980, lty=2, lwd=3, col="grey")
text(x=550,y=75, labels = "Mean Cost Dist of Random Points",cex=1.3)
text(x=1450,y=160, labels = "C",cex=2)
par(mai=c(0.7,0.7,0,.1))

plot(vstot[500:1500]~yearBP,data=mean[500:1500,],type="l",lwd=2,xlab="Years Before Present (BP)",ylab="Mean Viewshed",cex.lab=1.3)
text(x=570,y=40000+5000, labels = "Mean Viewshed Size of Random Points",cex=1.3)
abline(h=absmeanVS,col=1, lty=3,lwd=1)
abline(v=800, lty=2, lwd=3, col="grey")
abline(v=980, lty=2, lwd=3, col="grey")
text(x=1450,y=70000, labels = "D",cex=2)

##cool... not very pretty though
plot(cd~summed,data=ts.df)