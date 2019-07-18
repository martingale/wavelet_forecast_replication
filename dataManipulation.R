#########################  Settings and required libraries ###

library("data.table")
library("highfrequency")
library("xts")
library("xtable")
library("forecast")
library("tsDyn")
library("wmtsa")
library("fGarch")
library("data.table")
library("MSBVAR")
library("fMarkovSwitching")


# theDir<-ifelse(test = Sys.info()[['sysname']]!="Windows", "~/Dropbox/Veli/actualR/Forecasting Codes",
#                "~/../Dropbox/Veli/actualR/Forecasting Codes")

# tryCatch(setwd("C:/Users/ege.yazgan/Dropbox/Ege - Fuat/High Frequency Trading/ReplicationFiles/"),
#          error= function(e) setwd("C:/Users/fuadcan/Dropbox/FuatHarun/HFT/RcodesFuat/"))

source("ForecastF_2.R")
source("Estimation.R")
source("haarDecomp.R")
source("mclapplyhack.R")
source("nodesToPeriods.R")
source("uForecast.R")


#########################  End of settings and required libraries ###


############################### Reading the EURJPY data from source  #####################
dataMnp <- function(tm=05,type="PrevTick",interval="min", tau=tau){
mTable <- fread("EURJPY",header=F,sep=",")
mTable <- data.frame(mTable)


pdat<- mTable[,c(1,2,7,8)]

pdat[,1]<-as.character(pdat[,1])
pdat[,2]<-as.character(pdat[,2])

cat("data is read \n")
op <- options(digits.secs = 3)
options(op)
timepoints<-paste(pdat[,c(1)],pdat[,c(2)])
timepoints<-gsub(pattern="/",replacement="-",x=timepoints)

myTime<-strptime(timepoints, "%Y-%m-%d %H:%M:%OS",tz="EET")


prix<-xts(pdat[,3],order.by=myTime)
prix<-prix[!is.na(myTime)]
quantities<-xts(pdat[,4],order.by=myTime)
quantities<-quantities[!is.na(myTime)]

cat(" 'quantities' is constructed \n")

if(type %in% c("mean","average","weighted")){
  aggprix05min <- aggregatets(prix,on="mins",k=tm,weights=quantities,dropna=T)} else
 {aggprix05min <- aggregatets(prix,FUN = "previoustick",on = "minutes",k=tm,dropna=T)}


cat("data is aggregated \n")  

myData<-diff(aggprix05min,na.pad = F,log = T)
ind<-index(myData)

cat("Just index ... \n")

# Setting the knot points through the series.


d1<-c(strptime("2011-02-06 00:00:00",format ="%Y-%m-%d %H:%M:%OS",tz = 'GMT'),
      strptime("2011-02-12 00:00:00",format ="%Y-%m-%d %H:%M:%OS",tz = 'GMT'))

d2<-c(strptime("2011-03-13 00:00:00",format ="%Y-%m-%d %H:%M:%OS",tz = 'GMT'),
      strptime("2011-03-19 00:00:00",format ="%Y-%m-%d %H:%M:%OS",tz = 'GMT'))

d3<-c(strptime("2011-04-10 00:00:00",format ="%Y-%m-%d %H:%M:%OS",tz = 'GMT'),
      strptime("2011-04-16 00:00:00",format ="%Y-%m-%d %H:%M:%OS",tz = 'GMT'))


# d1<-c(strptime("2011-03-06 00:00:00",format ="%Y-%m-%d %H:%M:%OS",tz = 'GMT'),
#       strptime("2011-03-12 00:00:00",format ="%Y-%m-%d %H:%M:%OS",tz = 'GMT'))

# d2<-c(strptime("2011-03-13 00:00:00",format ="%Y-%m-%d %H:%M:%OS",tz = 'GMT'),
#       strptime("2011-03-19 00:00:00",format ="%Y-%m-%d %H:%M:%OS",tz = 'GMT'))


# d0<- ind[1]
# d1<-(strptime("2011-03-09 00:00:00",format ="%Y-%m-%d %H:%M:%OS"))
# d2<-(strptime("2011-03-18 00:00:00",format ="%Y-%m-%d %H:%M:%OS"))
# d3<-(strptime("2011-04-25 00:00:00",format ="%Y-%m-%d %H:%M:%OS"))
# d4<-ind[length(ind)]

cat("Periodized \n")

# nodesL<-list(d0,d1,d2,d3,d4)
nodesL<-list(d1,d2,d3)
# nodesL<-list(d1,d2)
den <- nodesToPeriods2(nodesL,tau=tau,ind=ind)
# den <- nodesToPeriods3(nodesL,tau=tau,ind=ind)

# start estim  test
# [1,]   750  1732  2020
# [2,]  7221  8318  8606
# [3,] 12555 13628 13916

# browser()
# nodesL<-list(d0,d1)
# den is the matrix which keeps the estimation and test periods for each block
# den<-data.matrix(nodesToPeriods(nodesL,tau=tau,ind=ind))
# den <- data.matrix(nodesToPeriods(nodesL,tau=tau,ind=ind))
# den<-data.matrix(den)
cat("Finished \n")
return(list(myData,den))

}

############# End of reading data ###################

