library("highfrequency")
library("xts")

# setwd("~/../Dropbox/Veli/actualR")
mTable<-read.table("EURJPY",header=F,sep=",")
sapply(mTable,class)
#unique(mTable[,4])
head(mTable,200)

pdat<- mTable[,c(1,2,7,8)]
head(pdat)
pdat[,1]<-as.character(pdat[,1])
pdat[,2]<-as.character(pdat[,2])
#pdat[,3]<-as.character(pdat[,3])

head(pdat)

op <- options(digits.secs = 3)
options(op)
den<-paste(pdat[,c(1)],pdat[,c(2)])
den<-gsub(pattern="/",replacement="-",x=den)

Sys.setenv(TZ = "GMT")

myTime<-strptime(den, "%Y-%m-%d %H:%M:%OS", tz="GMT")

# data("sample_tdata")
# head("sample_tdata")
# head(sample_tdata$PRICE)
# head(aggregatePrice(sample_tdata$PRICE,on="secs",k=30));

prix<-xts(pdat[,3],order.by=myTime)
prix<-prix[!(is.na(myTime))]
quantities<-xts(pdat[,4],order.by=myTime)
quantities<-quantities[!(is.na(myTime))]
head(prix)


aggprix01min<-aggregatets(prix,on="mins",k=01,weights=quantities,dropna=T)
aggprix05min<-aggregatets(prix,on="mins",k=05,weights=quantities,dropna=T)
aggprix15min<-aggregatets(prix,on="mins",k=15,weights=quantities,dropna=T)
aggprix30min<-aggregatets(prix,on="mins",k=30,weights=quantities,dropna=T)
aggprix60min<-aggregatets(prix,on="mins",k=60,weights=quantities,dropna=T)
aggprix6hours<-aggregatets(prix,on="hours",k=6,weights=quantities,dropna=T)
aggprix12hours<-aggregatets(prix,on="hours",k=12,weights=quantities,dropna=T)
aggprix01day<-aggregatets(prix,on="days",k=1,weights=quantities,dropna=T)

## Plotly graph 
library(plotly)
y = diff(unclass(aggprix05min)[,1],log=T)**2
x= index(aggprix05min)[-1]
p <- plot_ly(y=y, x=x , type="scatter", mode="markers+lines")
p



plot(diff(log(prix)))
plot(makeReturns(aggprix12hours))


# setwd("~/../Dropbox/Makalelerim_New/HFT_FX")
pdf("sd30min.pdf",width=7,height=4.5)
plot(rollapply(makeReturns(aggprix30min),width=30,FUN=sd ))
abline(h=sd(makeReturns(aggprix15min),na.rm=T), col="red")
dev.off()
#

pdf("intradayVol.pdf",width=7,height=4.5)
ts.plot(ts(medRV(aggprix05min,makeReturns=T)),
        ts(medRV(aggprix15min,makeReturns=T)),
        ts(medRV(aggprix30min,makeReturns=T)),
        col=c("red", "blue", "black", "blue"),
        main="Intraday volatility estimation according to Andersen(2009)")
legend("topright",legend=c("5 mins", "15 mins", "30mins"),lty=1,bty="n",
       col=c("red", "blue", "black", "blue"))


dev.off()


pdf("intradayVol_BN.pdf",width=7,height=4.5)
ts.plot(ts(rBPCov(aggprix05min,makeReturns=T)),
        ts(rBPCov(aggprix15min,makeReturns=T)),
        ts(rBPCov(aggprix30min,makeReturns=T)),
        col=c("red", "blue", "black", "blue"),
        main="Intraday volatility estimation according to \n Barndorff-Nielsen and Shephard (2004)")
legend("topright",legend=c("5 mins", "15 mins", "30mins"),lty=1,bty="n",
       col=c("red", "blue", "black", "blue"))


dev.off()


pdf("intradayVolsComp.pdf",width=7,height=4.5)



ts.plot(
ts(rBPCov(prix, makeReturns=T)), 
ts(medRV(prix, makeReturns=T)),
ts(rKernelCov(prix, makeReturns=T)),
ts(rCov(prix, makeReturns=T)),
col=c("red", "blue", "green", "gray"),
main="Intraday volatility estimations for different measures")
legend("topright",legend=c("rBP", "medRV", "rKernelCov", "rcov"),lty=1,bty="n",
       col=c("red", "blue", "black", "gray"))


dev.off()
#

Sys.setenv(TZ="GMT")

out<-spotVol(aggprix05min, periodicvol="TML",k=5,marketopen = "00:00:00", 
             marketclose = "23:59:59 ")
head(out)

sd(makeReturns(ts=aggprix05min))
sd(makeReturns(ts=aggprix15min))
sd(makeReturns(ts=aggprix30min))
sd(makeReturns(ts=aggprix01day))
sd(makeReturns(ts=aggprix05min))
sd(makeReturns(ts=aggprix15min))
sd(makeReturns(ts=aggprix30min))
sd(makeReturns(ts=aggprix01day))


rAVGCov(aggprix05min['2011-03-28'],makeReturns=T)

plot(rBPCov(aggprix05min,makeReturns=TRUE))
rHYCov(list(prix[c('2011-03-28','2011-03-29')],prix['2011-03-28']))


spotVol(aggprix05min,P1=6,P2=4,periodicvol="wsd",dummies=FALSE)

data("sample_real5rixminprices");

aggprix15min[is.na(aggprix15min)]<-rnorm(sum(is.na(aggprix15min)),110,.001)

# Compute and plot intraday periodicity:
out = spotVol((aggprix15min),
              dailyvol="medrv", P1=6,P2=6,periodicvol="OLS",k=30, on="minutes",dummies=TRUE) 
head(out);

x=harModel(makeReturns(aggprix05min), periods = c(1,5,10), periodsJ=c(1,5,10),
         RVest = c("rCov","rBPCov"), type="HARRVCJ",transform="sqrt")
summary(x)

plot(medRV(makeReturns(aggprix05min)))
plot(rOWCov(prix,makeReturns=T))




pdf("returns.pdf",width=8, height=5)
par(mfrow=c(2,2), mar=c(2.0,2.0,1.5,1.5))
plot(makeReturns(aggprix01min), main="",xaxt="n")
plot(makeReturns(aggprix05min), main="",xaxt="n")
plot(makeReturns(aggprix15min), main="")
plot(makeReturns(aggprix60min), main="")
dev.off()

getTail<-function(x) tail(x,1)

aggprix01min<-aggregatets(prix,on="mins",k=01,FUN=getTa, weights=quantities,dropna=T)
aggprix05min<-aggregatets(prix,on="mins",k=05,weights=quantities,dropna=T)
aggprix15min<-aggregatets(prix,on="mins",k=15,weights=quantities,dropna=T)
aggprix30min<-aggregatets(prix,on="mins",k=30,weights=quantities,dropna=T)
aggprix60min<-aggregatets(prix,on="mins",k=60,weights=quantities,dropna=T)
aggprix6hours<-aggregatets(prix,on="hours",k=6,weights=quantities,dropna=T)
aggprix12hours<-aggregatets(prix,on="hours",k=12,weights=quantities,dropna=T)
aggprix01day<-aggregatets(prix,on="days",k=1,weights=quantities,dropna=T)




aggprix01min<-aggregatets(prix,on="mins",k=01,FUN="getTail", dropna=T)
aggprix05min<-aggregatets(prix,on="mins",k=05,FUN="getTail", dropna=T)
aggprix15min<-aggregatets(prix,on="mins",k=15,FUN="getTail", dropna=T)
aggprix30min<-aggregatets(prix,on="mins",k=30,FUN="getTail", dropna=T)
aggprix60min<-aggregatets(prix,on="mins",k=60,FUN="getTail", dropna=T)
aggprix6hours<-aggregatets(prix,on="hours",k=6,FUN="getTail", dropna=T)
aggprix12hours<-aggregatets(prix,on="hours",k=12,FUN="getTail", dropna=T)





pdf("sdsLast.pdf",width=8, height=5)
par(mfrow=c(2,2), mar=c(1.0,2.0,1.5,1.5))
plot(rollapply(makeReturns(aggprix01min),width=30,FUN=sd, na.rm=T), main="",xaxt="n")
abline(h=sd(makeReturns(aggprix01min),na.rm=T), col="red")
plot(rollapply(makeReturns(aggprix05min),width=30,FUN=sd, na.rm=T), main="",xaxt="n")
abline(h=sd(makeReturns(aggprix05min),na.rm=T), col="red")
par(mar=c(2.0,2.0,1.5,1.5))
plot(rollapply(makeReturns(aggprix15min),width=30,FUN=sd, na.rm=T), main="")
abline(h=sd(makeReturns(aggprix15min),na.rm=T), col="red")
plot(rollapply(makeReturns(aggprix30min),width=30,FUN=sd, na.rm=T), main="")
abline(h=sd(makeReturns(aggprix30min),na.rm=T), col="red")

dev.off()
#





