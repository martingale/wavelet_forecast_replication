
skipForecast<-function(y,model,tau,step,args){
  if(model=="garchFit") {y<-y*1e+00}
  y<-as.matrix(y)
  #print(as.list(match.call()))
  ey<-y[1:(length(y)-tau)]
  
  if(tolower(model) %in% c("arima","ar","arma", "auto.arima")){
    fValue<-NULL
    if(tolower(model)=="ar"){
      for(i in 1:(tau/step)){
        fit<-estim(y[1:(length(ey)+step*(i-1))],umodel = "ar",addArgs=args)
        newfit <- Arima(y[1:(length(ey)+step*(i))], model=fit,xreg = args$xreg[1:(length(ey)+step*(i))])
        print(length(newfit$xreg))
        fValue[(1:step)+(i-1)*step] <- tail(fitted(newfit),n = step)
      }
    }
    else if(model %in% c("Arma", "arma", "auto.arima")){
      for(i in 1:(tau/step)){
        fit<-estim(y[1:(length(ey)+step*(i-1))],umodel="auto.arima",,addArgs=args)
        newfit <- Arima(y[1:(length(ey)+step*(i))], model=fit,xreg = args$xreg[1:(length(ey)+step*(i))])
        print(length(newfit$xreg))
        fValue[(1:step)+(i-1)*step] <- tail(fitted(newfit),n = step)
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
          dep<-y[1:(length(ey)+i)]
          indep<-cbind(dep-lag(dep),args$xreg)
          fit<-MS_Regress_Fit(dep,indep,S=c(1,1),k=2)
        }
        else{
          fit<-estim(y[1:(length(ey)+i)],umodel=model,addArgs = args)
        }
      }
      if(model=="msvar"){
        if(!is.null(args$xreg)){
          fValue<-MS_Regress_For(myModel = fit,newIndep = tail(indep,1))
        }
        else{
        tryCatch(fValue[i]<-msvarForecast(fit,newdata=y[(length(ey)+i)]),
                 error=function(e){
                   fValue[i]<-fMarkovSwitching::MS_Regress_For(myModel = fit,newIndep = y[(length(ey)+i)])$condMean
                 })
        }
      }else if(model=="garchFit"){
        fit@data<-y[1:(length(ey)+i)]
        fValue[i]<-predict(fit,100)$meanForecast[1]/1e+00
      }
      else{
        
        fValue[i]<-predict(fit,newdata=y[1:(length(ey)+i)])
      }
    }
    
  }
  return(fValue)
}
#skipForecast(y[1:500],"setar",tau,10,theargs)
