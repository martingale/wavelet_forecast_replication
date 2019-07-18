modifyResults<-function(type,step,model){
# step<-10;model<- "garchFit";type <- "Mean"
declMax <- 8;tau=100; tm=05
models<- c("ar","arma","setar","lstar","nnetTs","msvar","garchFit")

# forec<-get(load(file="C:/Users/fuat.beylunioglu/Dropbox/FuatHarun/HFT/Results/resultsPrevTick_st10/forecsAll-st10.rda"))

# Preperation!!

if(type=="Mean"){
  aggprix05min <- aggregatets(prix,on="mins",k=tm,weights=quantities,dropna=T)} else 
  if(type=="PrevTick"){aggprix05min <- aggregatets(prix,FUN = "previoustick",on = "minutes",k=tm,dropna=T)} else
   {stop("type hatalý")}

ind <- ind<-index(aggprix05min)

myData<-diff(aggprix05min,na.pad = F,log = T)
ind<-index(myData)

# Setting the knot points through the series.
d0<- ind[1]
d1<-(strptime("2011-03-9 00:00:00",format ="%Y-%m-%d %H:%M:%OS"))
d2<-(strptime("2011-03-18 00:00:00",format ="%Y-%m-%d %H:%M:%OS"))
d3<-(strptime("2011-04-25 00:00:00",format ="%Y-%m-%d %H:%M:%OS"))
d4<-ind[length(ind)]

# checking the length of each block of series
diff(sapply( list(d0,d1,d2,d3,d4), function(x) which(ind==x)))

nodesL<-list(d0,d1,d2,d3,d4)
# den is an important matrix which keeps the estimation and test periods for each block
den<-as.matrix(nodesToPeriods(nodesL,tau=100))
den<-as.matrix(den)


forecsAll <- get(load(paste0("C:/Users/",Sys.info()[6],"/Dropbox/FuatHarun/HFT/Results/results",type,"_st",step,"/forecsAll.rda")))
inds   <- lapply(1:(2*declMax+1), function(w) which(is.na(forecsAll[[which(models==model)]][[w]]),arr.ind = T))
wind   <- na.omit(sapply(1:17, function(x){if(nrow(inds[[x]])==0){NA} else {x}}))

perData<- lapply(1:nrow(den), function(x) (as.matrix(myData))[den[x,1]:den[x,3]])


forecMainSingle <- function(pd, tstep, wavind){
  tstep <- tau - tstep
  # Step 1: decomp. Y - Tau to s1, d1, Y0
  pestY   <- head(pd,-tstep)
  
  wav_all <- unlist(lapply(0:declMax, function(i) haarDecomp(pestY,level=i)),recursive = F)
  wav_all <- wav_all[!duplicated(names(wav_all))]
  wav_all <- wav_all[sort(names(wav_all))]
  
  
  # Step 2: Forecast
  forec <- tryCatch(ForecastF(wav_all[[wavind]],model,step,theargs,),
           error= function(e)  tryCatch(ForecastF(wav_all[[wavind]],model,step,theargs,1e3),
           error= function(e)  tryCatch(ForecastF(wav_all[[wavind]],model,step,theargs,1e6),
           error= function(e)  tryCatch(ForecastF(wav_all[[wavind]],model,step,theargs,1e8), 
           error= function(e)  tryCatch(ForecastF(wav_all[[wavind]],model,step,theargs,1e9),
           error= function(e)  tryCatch(ForecastF(wav_all[[wavind]],model,step,theargs,1e2),
           error= function(e)  NA))))))
  
  return(forec)
}

forecN <- lapply(wind, function(w) simplify2array(mclapply.hack(1:nrow(inds[[w]]), function(x){cat(paste0(inds[[w]][x,]),"\n"); forecMainSingle(perData[[inds[[w]][x,2]]],inds[[w]][x,1]-step,w)})))
forecsAll[[which(models==model)]]->forec

for(w in wind){
    forec[[w]][inds[[w]]] <- forecN[[which(wind==w)]]
}

forec <- forec[1:(2*declMax+1)]

forec <- lapply(1:tau , function(t) lapply(1:(2*declMax+1), function(w) forec[[w]][t,]))
forec <- lapply(forec, function(t) t(sapply(1:(2*declMax+1), function(w) t[[w]])))
forec <- lapply(forec, function(t) rbind(t,apply(t[1:declMax,],2,cumsum) + t[(declMax+1):(2*declMax),]))
forec <- lapply(1:(3*declMax+1)  , function(w) t(sapply(1:tau, function(t) forec[[t]][w,])))

forecsAll[[which(models==model)]]<-forec
save(forecsAll, file=paste0("C:/Users/",Sys.info()[6],"/Dropbox/FuatHarun/HFT/Results/results",type,"_st",step,"/forecsAll.rda"))

}
