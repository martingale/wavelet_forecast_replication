library("forecast")
# library("tsDyn")
library("MSBVAR")

msvarForecast<-function(mod_msvar,newdata=NULL,steps=1){
  data<-mod_msvar$init.model$y
  newdata<-tail(newdata,1)
  Bk<-mod_msvar$hreg$Bk
  P<-mod_msvar$fp[nrow(mod_msvar$fp),]
  Q1<-mod_msvar$Q
  if(is.null(newdata)){
  b<-matrix(c(data[nrow(data),],1),ncol=ncol(data)+1)
  }
  else{
    b<-matrix(c(newdata,1),ncol=ncol(data)+1)
  }
  
  a<-matrix(0,nrow=steps,ncol=ncol(data))
  for(i in 1:steps){
    P<-P%*%Q1
    s1<-b%*%matrix(Bk[,,1],nrow=2)
    s2<-b%*%matrix(Bk[,,2],nrow=2)
    a[i,] <- P[1]*s1 + P[2]*s2
    b <- matrix(c(a[i,],1),ncol=ncol(data)+1)
  }
  return (a)
}

uForecast<-function(model, ...){
  modelClass<-class(model)[1]
  if(sum(modelClass==c("setar","lstar","nnetTS"))){
    return(predict(model, ...))
  }
  else if(modelClass=="Arima"){
    return(fitted(model,...))
  }
  else if(modelClass=="MSVAR"){
    return(msvarForecast(model,...))
  }
}


# y<-arima.sim(model = list(ar=.5,ma=c(.9,-.4)),n = 1000,n.start = 500)
# 
# 
# undebug(estim)
# a<-estim(data=y,model="msvar",h=2,p=1)
# class(a)
# 
# 
# b<-uForecast(1)
# b







# a<-estim(y,"arima", order = c(3, 0,2))
# modar<-ar(y)
# modar$series
# a$series
# 
# forecast::forecast(modar,newdata = 1:100)
# 
# class(arima(rnorm(100), c(2,0,2)))
# class(a)
# 
# stats::predict(a, n.ahead=10, newxreg = 1:50)
# 
# uForecast(a,newdata=(10:2000))
# stats::predict(a,n.ahead = 10)$pred
# forecast::forecast(a,newdata=10:1000)
# 
# summary(a)
# a$series
# 
# new.data<-1000:1009
# model <- auto.arima(y,max.order = 12,stepwise = F)
# newfit <- Arima(c(y,new.data), model=model)
# newfit$series
# (onestep.for <- fitted(newfit)[1001:1010])
# 
# predict(newfit,n.ahead=10)$pred
# predict(model,n.ahead=10)$pred
