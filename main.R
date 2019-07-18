setwd("~/wavelet_forecast/ReplicationFiles/")
rm(list = ls(all.names = T))
source("forecAnalysis_h.R")
# system()
options(cores=14)
tau = 60 * 24 / 5  # number of 5 minutes in a day
# tau = 50 
forecAnalysis(5, "min", "mean", tau = tau, 1)
# forecAnalysis(5, "min", "mean", tau = tau, 5)
# forecAnalysis(5,"min","mean",tau = tau,5)
# forecAnalysis(5,"min","mean",tau = tau,10)
# forecAnalysis(5,"min","PrevTick",tau = tau,1)
# forecAnalysis(5,"min","PrevTick",tau = tau,5)
# forecAnalysis(5,"min","PrevTick",tau = tau,10)
