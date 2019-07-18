
source("dataManipulation.R")

forecAnalysis <- function(tmFreq,interval,type,tau,step){
# Inputs:
  # tmLen    :: Time frequency,
  # Interval :: Time interval type, e.g. min or hour
  # type     :: aggregation type, mean or prevTick
  # tau      :: length of the forecast zone
  # step     :: forecast horizon
  

###### Data and initial settings ######
tmLen=5
myData <- dataMnp(5,type,interval)
den <- myData[[2]]
myData <- myData[[1]]


# Obtaining the return series from the price series by taking diff of log-prices.

################## Decomposition & Forecasting  ######################

theargs<- list( m=1,size=3,xreg=NULL,formula=as.formula(object="~arma(1,0)+garch(1,1)"),
                include="none",order.max=1,h=2,p=1,stationary=TRUE,stepwise=F, 
                seasonal=FALSE,max.order=1,max.q=1,max.p=1,trace=F)

#### Function for forecast ###

# Structure
# Tau > Period > Levels > Models


## Inputs  : perData, tau, step
## Outputs : forecasted point for each period, model and for s1,d1,y0
### Note   : Function calculates for all models and periods defined earlier.

# tau is the number of data points for the forecasting test period, step is the forecast step

declMax <- 8 # maximum decomposition level


# perdata will be a list consisting of three matrices
perData <- lapply(1:nrow(den), function(x) (as.matrix(myData))[den[x,1]:den[x,3]])


forecMain <- function(perData, tstep, models){
  tstep <- tau - tstep
  # Step 1: decomp. Y - Tau to s1, d1, Y0
  pestY   <- lapply(perData, function(x) head(x,-tstep))
  
  wav_all <- lapply(pestY, function(x) unlist(lapply(0:declMax, function(i) haarDecomp(x,level=i)),recursive = F))
  wav_all <- lapply(wav_all, function(w) w[!duplicated(names(w))])
  wav_all <- lapply(wav_all, function(w) w[sort(names(w))])
  
  
  # Step 2: Forecast
  lw<-length(wav_all[[1]]); lp <- length(perData); lm<-length(models)
  forecs <- lapply(models, function(m) sapply(1:lp, function(p) sapply(wav_all[[p]], 
                  function(w)  tryCatch(ForecastF(w,m,step,theargs,1),
           error= function(e)  tryCatch(ForecastF(w,m,step,theargs,1e3),
           error= function(e)  tryCatch(ForecastF(w,m,step,theargs,1e6)))))))

  dcums  <- lapply(forecs, function(f) apply(matrix(f[1:declMax,],,lp),2,cumsum))
  forecs <- lapply(1:lm,   function(f) rbind(forecs[[f]], forecs[[f]][(declMax+1):(declMax+8),] + dcums[[f]]))
  forecs <- lapply(forecs, function(f){rownames(f) <- c(head(rownames(f),-declMax),paste0("comp.",1:declMax));return(f)})
  forecs
  
  # Output: forecs includes tables, for each model, having one step forecasts for y0, d1, etc., and for each time intervals.

}

### End: Function ###

#### Analysis for each Tau ####

tst   <- tau-step
fromm <- -step+1


fhd<- paste0(getwd(),"/Results/results",type,"_st",step)
dir.create(path=fhd,showWarnings = F,recursive = T)



models     <- c("ar","arma")
forecsARMA <- mclapply(fromm:tst, function(t) forecMain(perData,t,models))
save(forecsARMA, file= paste0(fhd, "/forecARMA-","st",step,"-",Sys.Date(),".rda")) 

models     <- c("nnetTs","msvar","garchFit")
cat(str(perData))
forecsNMG <- mclapply(fromm:tst, function(t) forecMain(perData,t, models))
save(forecsNMG, file= paste0(fhd, "/forecNMG-","st",step,"-",Sys.Date(),".rda")) 



models     <- c("setar")
forecsS <- mclapply(fromm:tst, function(t) forecMain(perData,t,models))
save(forecsS, file= paste0(fhd, "/forecS-","st",step,"-",Sys.Date(),".rda")) 

models     <- c("lstar")
forecsL <- mclapply(fromm:tst, function(t) forecMain(perData,t,models))
save(forecsL, file= paste0(fhd, "/forecL-","st",step,"-",Sys.Date(),".rda")) 



cat(str(forecsARMA[[1]][[1]]))
forecsS    <- lapply(1,   function(m) lapply(1:25, function(w) t(sapply(1:tau, function(t) forecsS[[t]][[m]][w,]))))
forecsL    <- lapply(1,   function(m) lapply(1:25, function(w) t(sapply(1:tau, function(t) forecsL[[t]][[m]][w,]))))
forecsNMG  <- lapply(1:3, function(m) lapply(1:25, function(w) t(sapply(1:tau, function(t) forecsNMG[[t]][[m]][w,]))))
forecsARMA <- lapply(1:2, function(m) lapply(1:25, function(w) t(sapply(1:tau, function(t) forecsARMA[[t]][[m]][w,]))))


forecsAll <- c(forecsARMA,forecsS,forecsL,forecsNMG)

save(forecsAll, file= paste0(fhd, "/forecsAll-",type,"_st",step,"-",Sys.Date(),".rda")) 


#  Output: forecs over 1:tau. Reported in tables, for each model, period and decomposition type


models<- c("ar","arma","setar","lstar","nnetTs","msvar","garchFit")


# Step 3: Evaluation
lp <- length(perData); lm<-length(models)

evalData   <- sapply(perData, function(x) tail(x,tau))

RMSE       <- lapply(forecsAll, function(m) lapply(m, function(w) (w-evalData)^2))
RMSE       <- lapply(RMSE, function(m) t(sapply(m, function(w) (apply(w,2,function(x) mean(x,na.rm=T)))*1e7)))

# Report!!

capTable<- t(matrix(c("low (1)", "low (1)",
             "low (1)","high",
             "high","high",
             "high","low (2)",
             "low (2)","low (2)",
             "low (2)","medium",
             "medium","medium"),2,))

reps <- lapply(1:lm, function(m){
lapply(1:lp, function(x){
  rep     <- matrix(,declMax+1,2)
  rep[,1] <- RMSE[[m]][,x][(declMax*2+1):(declMax*3+1)]
  rep[,2] <- c(NA,RMSE[[m]][,x][(declMax+1):(declMax*2)])
  dzz     <- RMSE[[m]][,x][1:declMax]
  dmat    <- matrix(,declMax,declMax)
  dmat[upper.tri(dmat,T)]<-unlist(sapply(1:declMax, function(d) dzz[1:d]))
  dmat <- rbind(rep(NA,declMax),t(dmat))
  rep <- cbind(rep,dmat)
  mind <- which(rep==min(rep,na.rm = T),arr.ind = T)
  rep <- round(rep,4)
  rep[mind] <- paste0("\\textbf{",rep[mind],"}")
  rep <- data.frame(rep)
  rep <- cbind(rep(models[m]),0:declMax, rep)
  colnames(rep) <- c("model","level","comp.","$s_l$", paste0("$d_",1:declMax,"$"))
  rownames(rep) <- c()
  return(rep)
  })
})
reps <- lapply(1:lp, function(p) lapply(1:lm, function(m) reps[[m]][[p]]))
reps <- lapply(reps, function(rep) do.call(rbind,rep))

# Output: Table consists of RMSE scores for each method presented consecutively in the rows.

# Exporting to LaTeX

fhd <- paste0(getwd(),"/latex/")
dir.create(path=fhd,showWarnings = F,recursive = T)

filName <- paste0(fhd,"results",type,"-st",step)
dir.create(filName,recursive=T)


doc.start<-"\\documentclass[10pt,a4paper]{article}
\\usepackage[latin1]{inputenc}
\\usepackage{amsmath}
\\usepackage{amsfonts}
\\usepackage{amssymb}
\\usepackage{graphicx}
\\begin{document}"

cat(doc.start,file=paste0(filName,"/resultsAll-",type,"-st",step,"-tau",tau ,".tex"),append=T)
for(i in 1:lp){
xtb <- xtable(reps[[i]],digits=4,caption=paste0("MSE scores for Estimation Zone ",capTable[i,1]," and Forecast Zone ",capTable[i,2]," $ \\times 10^{-7}$"))
print.xtable(xtb,file = paste0(filName,"/resultsAll-",type,"-st",step,".tex"),append = T,caption.placement="top",hline.after=c(-1,0,(1+declMax)*1:lm),sanitize.text.function = identity)
}
cat("\\end{document}",file=paste0(filName,"/resultsAll-",type,"-st",step,".tex"),append=T)

}

