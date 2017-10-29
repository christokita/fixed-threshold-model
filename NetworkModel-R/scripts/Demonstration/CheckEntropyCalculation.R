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
  # Grab task dist dataframe for set
  taskDist <- taskDistTot[taskDistTot$Set == i, ]
  n <- taskDist$n[1]
  replicate <- taskDist$replicate[1]
  # Calcualte entropy
  taskDist <- taskDist[ ,1:2]
  entropy <- mutualEntropy(taskDist)
  entropy$n <- n
  entropy$replicate <- replicate
  return(entropy)
})