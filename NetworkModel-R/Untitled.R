rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3A_PrepPlotExperimentData.R")

load("output/__RData/FixedDelta06Sigma01Eta7100reps.Rdata")


# bind task distributions into mega dataframe
taskDist <- unlist(groups_taskDist, recursive = FALSE)
taskDistTot <- do.call("rbind", taskDist) 

taskDistTot[ ,1:2] <- taskDistTot[ ,1:2] * 10000
taskDistTot <- taskDistTot %>% 
  mutate(Set = paste0(n, "-", replicate)) %>% 
  arrange(n, replicate)

sets <- unique(taskDistTot$Set)
entropy <- lapply(sets, function(i) {
  
})