# data<-wav_all[[1]][[1]]
# addArgs<-theargs
# setwd("~/Dropbox/Veli/actualR")
library("forecast")
library("tsDyn")
library("MSBVAR")
library("fGarch")
library("fMarkovSwitching")

estim <- function(data,umodel,naFail=F,addArgs){
  addArgs<- list( m=1,size=3,xreg=NULL,formula=as.formula(object="~arma(1,0)+garch(1,1)"),
               include="none",order.max=1,h=2,p=1,stationary=TRUE,stepwise=F, 
               seasonal=FALSE,max.order=1,max.q=1,max.p=1,trace=F, gammaInt=c(1,200))
  
  cat(umodel, ' Estimdeyiz\n')
  
  # data<-na.omit(data)
  # cat(str(head(data)))
  require("tsDyn")
  require("MSBVAR")
  # assign(formalArgs(umodel)[1], as.matrix(data))
  data <-x <-y <-X<-Y<- matrix(data,ncol = 1)
  cat('Hello babalar.')
  # browser()
  
  if(umodel=="lstar"){addArgs$include="const"; addArgs$m<-addArgs$m+1}
  if(umodel=="setar"){addArgs$include="const"}
  # addArgs$m = 2 
  # if(is.null(addArgs$m)) {
  #  addArgs$m=2 
  #  }
  addArgs$mH<-addArgs$mL<-addArgs$mM<-addArgs$m;
  addArgs$ML = seq_len(addArgs$mL); addArgs$MM = seq_len(addArgs$mM);addArgs$MH = seq_len(addArgs$mH)
  
  if(is.null(addArgs$d)) addArgs$d<-1
  addArgs$steps<-addArgs$d
  
  
  myArgs<-addArgs
  #print(myArgs)
  allVar<-as.list(environment())
  allVar<-c(allVar, myArgs)
  

  if(umodel=="ar")  fullArgs<-formals(auto.arima) else   fullArgs<-formals(umodel)
  fullArgs1<-modifyList(as.list(fullArgs),allVar)
  fullArgs<-fullArgs1[names(fullArgs)]
  #class(fullArgs)<-"alist"
  #fullArgs<-as.pairlist(fullArgs)
  # print((fullArgs$xreg))
  #if(umodel=="garchFit") fullArgs$data<- as.numeric(fullArgs$data)
  
  if(umodel=="ar"){
    # print(length(fullArgs$xreg))
    auto.arima(x = data,d = 0,seasonal=F,max.q=0,ic = "aic",allowdrift = T,
               stationary = T,stepwise = F,max.order = 1,max.p=1)
  }
  
  else if(umodel=="auto.arima"){
    auto.arima(x = data,d = 0,seasonal=F,max.q=1,ic = "aic",allowdrift = T,
               stationary = T,stepwise = F,max.order = 2,max.p=1)
  }
  else if(umodel=="nnetTs"){
    nnetTs(x=data, m=4, size=4)
    
  }
  else if(umodel=="garchFit"){
   #  browser()
    
    garchFit(~ garch(1,1), data =data,trace = FALSE )
    
  }
  
  else{
    # write.file = 'progress.txt'
    # file.create(write.file)
    # fileConn<-file(write.file)
    # writeLines('Here are the args: \n', fileConn)
    # writeLines(str(fullArgs), fileConn)
    # close(fileConn)
    # cat(str(fullArgs))
    if(umodel=="lstar"){
      fullArgs$starting.control=list(gammaInt=c(1,1000))
    }
    tryCatch(do.call(what=umodel,args = fullArgs),
             error=function(e){
               if(umodel=="msvar"){
                 
                 return(tryCatch(do.call(what=fMarkovSwitching::MS_Regress_Fit,
                                         args = list(dep=tail(data, -1),indep=head(data,-1),S=c(1)))
                                 # ,error=function(e){return(NA)}
                                 ))
               }
               else{
                 cat("kotu haber: ",umodel)
                 return(0)
               }
             })
  }
}

# #rm(d,steps)
#  y<-arima.sim(model = list(ar=c(.63)),n=4000)+10
# y<-rnorm(1000)
# 
# #fit<-estim(y,umodel="setar",F,theargs )
# # (fit)
# # undebug(estim)
# # m=2
# # rm(m)
# # setar(rnorm(1000), m=2,d=2)
# skipForecast(y = y,model = "garchFit",tau = 100,step = 50,args = list(formula=formula , cond.dist = "norm", trace=F) )
# 
# fit<-garchFit(formula = formula, data = y)
# debug(estim)
