################################################################################
#
# Check for evidence of redundancy
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3_PrepPlotExperimentData.R")

load("output/__RData/MSrevision_FixedDelta06_DetThreshDetUpdate100reps.Rdata")

# Set variable  
filename <- "Fixed_Delta06Sigma01Eta7"

####################
# Frequency of task not being performed
####################
noTaskPerf <- lapply(groups_taskTally, function(group_size) {
  # Loop through replicates within group size
  within_groupTaskPerf <- lapply(group_size, function(replicate) {
    # Get basics and counts of instances in which there isn't anyone performing task
    to_return <- data.frame(n = unique(replicate$n), 
                           replicate = unique(replicate$n),
                           noTask1 = sum(replicate$Task1 == 0),
                           noTask2 = sum(replicate$Task2 == 0))
    #  Quantify length of no-performance bouts
    for (task in c("Task1", "Task2")) {
      bout_lengths <- rle(replicate[ , task])
      bout_lengths <- as.data.frame(do.call("cbind", bout_lengths))
      bout_lengths <- bout_lengths %>% 
        filter(values == 0)
      avg_nonPerformance <- mean(bout_lengths$lengths)
      if(task == "Task1") {
        to_return$noTask1Length = avg_nonPerformance
      } 
      else {
        to_return$noTask2Length = avg_nonPerformance
      }
    }
    # Get averages
    to_return <- to_return %>% 
      mutate(noTaskAvg = (noTask1 + noTask2) / 2,
             noTaskLengthAvg = (noTask1Length + noTask2Length) / 2)
    # Return
    return(to_return)
  })
  # Bind and return
  within_groupTaskPerf <- do.call("rbind", within_groupTaskPerf)
  return(within_groupTaskPerf)
})
# Bind
noTaskPerf <- do.call("rbind", noTaskPerf)

# Plot
gg_noTask <- ggplot(data = noTaskPerf, aes(x = n, y = noTask1)) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks = unique(noTaskPerf$n)) +
  scale_y_continuous(limits = c(0, 6800)) +
  xlab("Group Size") +
  ylab("Instances of No Task 1 Performance")
  # ylab("Avg. Length of No Task 1 Performance")
gg_noTask



