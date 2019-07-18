skipForecastF<-function(p,model,tau,step,args){
  if(model=="garchFit") {p<-p*1e+00}
  p<-as.matrix(p)
  #print(as.list(match.call()))
  ey<-p[1:(length(p)-tau)]
  ley<-length(ey)
  
  if(tolower(model) %in% c("arima","ar","arma", "auto.arima")){
    fValue<-NULL
    if(tolower(model)=="ar"){
      for(i in 1:(tau/step)){
        wavs<- unlist(lapply(0:1, function(l) haarDecomp(p[1:(ley+step*(i-1))],l)),recursive = F)
        fits<-lapply(wavs, function(w) estim(w,umodel = "ar",addArgs=args))
        newfits <- lapply(fits, function(f) Arima(p[1:(ley+step*(i))], model=f))
        print(length(newfit$xreg))
        ftemp <-sapply(1:length(wavs), function(x) tail(fitted(newfits[[x]]),n = step))
        fValue<- rbind(fValue, ftemp)
      }
    }
    else if(model %in% c("Arma", "arma", "auto.arima")){
      for(i in 1:(tau/step)){
        wavs<- unlist(lapply(0:1, function(l) haarDecomp(p[1:(ley+step*(i-1))],l)),recursive = F)
        fits<-lapply(wavs, function(w) estim(w,umodel = "auto.arima",addArgs=args))
        newfits <- lapply(fits, function(f) Arima(p[1:(ley+step*(i))], model=f))
        print(length(newfit$xreg))
        ftemp <-sapply(1:length(wavs), function(x) tail(fitted(newfits[[x]]),n = step))
        fValue<- rbind(fValue, ftemp)
      }
    }
  }
  else
  {    
    if(!is.null(args$xreg) && model=="msvar"){
      dep<-ey
      indep<-cbind(dep-lag(dep),args$xreg)
      fit<-MS_Regress_Fit(ey,indep,S = c(1,1), k = 2)
    }
    else{
      fit<-estim(ey,umodel=model,,args)
    }
    
    fValue<-NULL
    for(i in 1:tau){
      if((i %% step)==0 & i!=tau) {
        if(!is.null(args$xreg) && model=="msvar"){
          dep<-p[1:(length(ey)+i)]
          indep<-cbind(dep-lag(dep),args$xreg)
          fit<-MS_Regress_Fit(dep,indep,S=c(1,1),k=2)
        }
        else{
          fit<-estim(p[1:(length(ey)+i)],umodel=model,addArgs = args)
        }
      }
      if(model=="msvar"){
        if(!is.null(args$xreg)){
          fValue<-MS_Regress_For(myModel = fit,newIndep = tail(indep,1))
        }
        else{
          tryCatch(fValue[i]<-msvarForecast(fit,newdata=p[(length(ey)+i)]),
                   error=function(e){
                     fValue[i]<-fMarkovSwitching::MS_Regress_For(myModel = fit,newIndep = p[(length(ey)+i)])$condMean
                   })
        }
      }else if(model=="garchFit"){
        fit@data<-p[1:(length(ey)+i)]
        fValue[i]<-predict(fit,100)$meanForecast[1]/1e+00
      }
      else{
        
        fValue[i]<-predict(fit,newdata=p[1:(length(ey)+i)])
      }
    }
    
  }
  return(fValue)
}
