library("wmtsa")
#library("multicore")
options(mc.cores=14)

haarDecomp<-function(y,level){
  if(level==0){return(list(y0=y))} else{
    result   <- wavMODWT(y, wavelet="haar", n.levels=level)

  return(result$data)}}

haarDecompMc<-function(y,n.level){
  #res<-mclapply(1:n.level, function(x) haarDecomp(y,x))
  res<-lapply(1:n.level, function(x) haarDecomp(y,x))
  return(res)
}

rmseBench<-function(data,tau,dates,models){
  res<-NULL
  cnames<-NULL
  for(j in 1:length(models)){
    
    res<-cbind(res,sapply(0:1, function(i) sapply(1:length(dates), function(x) RMSE(data,dates[x],tau,i,models[j]))))
    cnames<-c(cnames,paste(models[j],"-tau"),paste(models[j],"+tau"))
  }
  res<-data.frame(res,row.names = dates)
  colnames(res)<-cnames
  return(res)
}
