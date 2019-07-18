theargs<- list( m=1,size=3,xreg=NULL,formula=as.formula(object="~arma(1,0)+garch(1,1)"),
                include="none",order.max=1,h=2,p=1,stationary=TRUE,stepwise=F, 
                seasonal=FALSE,max.order=1,max.q=1,max.p=1,trace=F)


args<- list( m=1,size=3,xreg=NULL,formula=as.formula(object="~arma(1,0)+garch(1,1)"),
                include="none",order.max=1,h=2,p=1,stationary=TRUE,stepwise=F, 
                seasonal=FALSE,max.order=1,max.q=1,max.p=1,trace=F)

ForecastF<-function(w,model,step,args,MLT=1, no=1, tau=1){
  # cat(as.character(length(w)),'\n')
  # cat('w: ',w[11:12],'\n')
  # browser()
  decomp_level = substr(names(w)[1],1,2)
  data_desc = (no %%2 + no ) /2
  
  suffix = paste0('_', data_desc, decomp_level, tau, collapse = '_')
  model_name = paste0(model, suffix, '_fit.rds')
  
  # Inputs #
  # w: series to be forecasted
  # model: model to be used to forecast
  # step: forecast step
  # args: theargs 
  if(model %in% c("garchFit","setar","lstar","nnetTs")) {w <- w*MLT}
  w<-as.matrix(w)
  
  if( model%in% c("arima","ar","arma", "auto.arima")){
    fValue<-NULL
    if(model=="ar"){
      if (model_name %in% list.files('./Results/')){
        print(paste0('./Results/',model_name))
        fit <- readRDS(paste0('./Results/',model_name))
        
      } else {
        fit    <- estim(w,umodel = "ar",addArgs=args)
        saveRDS(fit, (paste0('./Results/',model_name)))
        
      }
      # fit <- Arima(w, model=fit)
      # browser()
      fValue <- predict(fit,n.ahead=step)$pred[step]
      
    } else if(model %in% c("Arma", "arma", "auto.arima")){
      if (model_name %in% list.files('./Results/')){
        print(paste0('./Results/',model_name))
        fit <- readRDS(paste0('./Results/',model_name))
        
      } else {
        fit  <- estim(w,umodel = "auto.arima",addArgs=args)
        saveRDS(fit, (paste0('./Results/',model_name)))
      }
      fit <- Arima(w, model=fit)
      fValue <- predict(fit,n.ahead=step)$pred[step]
      # cat(fValue)
      
    }} else if(model=="garchFit"){
      if (model_name %in% list.files('./Results/')){
        fit <- readRDS(paste0('./Results/',model_name))
        
      } else {
        fit      <- estim(w,umodel=model,addArgs = args)
        saveRDS(fit, (paste0('./Results/',model_name)))
      }
      or_l <- length(fit@data)
      fit@data[(or_l-10):or_l ] <- tail(as.numeric(w), 10) # reuse fit
      fValue   <- predict(fit,n.ahead=step)$meanForecast[step]/MLT 
      # cat(fValue)
      # cat(str(fValue))
    
    } else if(model %in% c("setar","lstar","nnetTs")){
      
      cat(model_name,"\n")
      if (model_name %in% list.files('./Results/')){
        
        fit <- readRDS(paste0('./Results/',model_name))
        
      } else {
        fit      <- estim(w, umodel=model, naFail=F, args)
        # browser()
        saveRDS(fit, (paste0('./Results/',model_name)))
      }
      # fValue   <- predict(fit, n.ahead = step, newdata=as.numeric(w))[step]/MLT
      # browser()
      fValue   <- predict(fit, newdata=as.numeric(w),n.ahead=step)[step]/MLT
    }
   else if(model=="msvar"){
     if (model_name %in% list.files('./Results/')){
       fit <- readRDS(paste0('./Results/',model_name))
       
     } else {
    # browser()
     fit <- estim(w, umodel="msvar", naFail=F, args)
     saveRDS(fit, (paste0('./Results/',model_name)))
     }
     fValue     <- tryCatch(c(msvarForecast(fit,steps = step, newdata=as.numeric(w)))[step],
                            error=function(e){
                            print(model_name);
                            MS_Regress_For(fit,tail(w,1))$condMean
                              })
   }
  return(fValue)
}

  
  
