nodesToPeriods<-function(nodesL, tau=100, as.index=T, ind){
  require("data.table")
  
  nodeInds<-sapply( nodesL, function(x) which(ind==x))
  #   nodeInds<-sapply(nodesL, function(x) which(as.Date(ind)==as.Date(x))[1])
  resPlus=NULL
  for(i in 1: (length(nodeInds)-1)) {
    resPlus= rbind(resPlus,c( nodeInds[i], nodeInds[i+1],nodeInds[i+1]+tau) )
  }
  resMinus<-NULL
  for(i in 1: (length(nodeInds)-1) ){
    resMinus= rbind(resMinus,c( nodeInds[i], nodeInds[i+1]-tau,nodeInds[i+1]) )
  }
  
  res<-rbind(resMinus, resPlus)
  
  remove<-apply(res,1, function(x) any(x>max(nodeInds)))
  res<-res[!remove, ]
  res<-data.table(res, key="V1") #sort according to the first element
  setnames(res, c("start", "estim", "test"))
  
  
  
  # tr<-  apply(res, c(1,2), function(x) ind[x])
  # as.POSIXlt(tr,origin="1970-01-01")
  if (!as.index) apply(res, c(1,2), as.POSIXlt, origin= "1970-01-01" ) else return(res)
}


nodesToPeriods2<-function(nodesL, tau=100, as.index=T, ind){
  require("data.table")
  res<-matrix(0,nrow =  length(nodesL),ncol=3)
  k=1
  for (x in nodesL){
    start <- which(ind > x[1])[1]
    test <- which(ind > x[2])[1]-1
    estim <- test -tau
    res[k,] <- c(start,estim,test)
    k = k+1
  }
  colnames(res)<- c("start", "estim", "test")
  return(res)
}


nodesToPeriods3<-function(nodesL, tau=100, as.index=T, ind){
  require("data.table")
  res<-matrix(0,nrow =  length(nodesL),ncol=3)
  k=1
  for (x in nodesL){
    start <- which(ind > x[1])[1]
    estim <- which(ind > x[2])[1]-1
    test <- estim + tau
    
    res[k,] <- c(start,estim,test)
    k = k+1
  }
  colnames(res)<- c("start", "estim", "test")
  return(res)
}
