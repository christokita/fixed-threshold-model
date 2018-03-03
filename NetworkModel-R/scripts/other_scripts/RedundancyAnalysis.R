################################################################################
#
# Check for evidence of redundancy
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3_PrepPlotExperimentData.R")

load("output/__RData/FixedDelta06Sigma01Eta7100reps.Rdata")

# Set variable  
filename <- "Fixed_Delta06Sigma01Eta7"

# Palette without single individuals
palette <- c("#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78", "#bdbdbd", "#525252")


####################
# Frequency of task not being performed
####################
noTaskPerf <- lapply(groups_taskTally, function(group_size) {
  # Loop through replicates within group size
  within_groupTaskPerf <- lapply(group_size, function(replicate) {
    # Get basics and counts of instances in which there isn't anyone performing task
    to_return <- data.frame(n = unique(replicate$n), 
                           replicate = unique(replicate$replicate),
                           Set = paste0(unique(replicate$n), "-", unique(replicate$replicate)),
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
  geom_point(size = 0.5, alpha = 0.3) +
  theme_classic() +
  scale_x_continuous(breaks = unique(noTaskPerf$n)) +
  scale_y_continuous(limits = c(0, 8000)) +
  xlab("Group Size") +
  ylab("Instances of No Task 1 Performance")
  # ylab("Avg. Length of No Task 1 Performance")
gg_noTask


####################
# Frequency of task not being performed vs. specialization
####################
# Load specialization
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2)
taskCorrTot <- taskCorrTot %>% 
  mutate(Set = paste0(n, "-", replicate)) %>% 
  select(n, TaskMean, Task1, Task2, Set) 

# Merge
merged_specperf <- merge(taskCorrTot, noTaskPerf, by = c("Set", "n"))

# Plot
gg_specPerf <- ggplot(data = merged_specperf) +
  geom_point(aes(x = TaskMean, y = noTask1, colour = as.factor(n))) +
  theme_classic() +
  facet_wrap(~n, scales = "free") +
  scale_color_manual(values = palette) +
  theme(legend.position = "none") +
  ylab("Instances of Task 1 Neglect") +
  xlab("Rank Correlation")
gg_specPerf


# Normalized within group
normalizedNeglect <- merged_specperf %>% 
  mutate(noTaskAvg = noTaskAvg / 10000) %>% 
  group_by(n) %>% 
  mutate(noTaskAvgMin = min(noTaskAvg),
         noTaskAvgMax = max(noTaskAvg),
         TaskMeanMin = min(TaskMean),
         TaskMeanMax = max(TaskMean)) %>% 
  mutate(noTaskAvgNorm = (noTaskAvg - noTaskAvgMin) / (noTaskAvgMax - noTaskAvgMin),
         TaskMeanNorm = (TaskMean - TaskMeanMin) / (TaskMeanMax - TaskMeanMin)) 

palette <- c("#83343E", "#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78", "#bdbdbd", "#525252")

gg_specPerfNorm <- ggplot(data = normalizedNeglect) +
  geom_point(aes(x = TaskMeanNorm,
                 colour = as.factor(n),
                 y = noTaskAvgNorm), 
             # colour = "#F23619",
             size = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("Normalized task neglect") +
  xlab("Normalized specialization") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     expand = c(0, 0.005)) +
  scale_x_continuous(breaks = seq(-1, 1, 0.2), 
                     expand = c(0, 0.005)) +
  scale_color_manual(values = palette) +
  theme(legend.position = "none",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 6),
        legend.text.align = 0,
        # legend.box.background = element_rect(),
        axis.text.y = element_text(size = 8, margin = margin(5, 6, 5, -2), color = "black"),
        axis.text.x = element_text(size = 8, margin = margin(6, 5, -2, 5), color = "black"),
        axis.title = element_text(size = 10, margin = margin(0, 0, 0, 0)),
        axis.ticks.length = unit(-0.1, "cm"),
        aspect.ratio = 1)

gg_specPerfNorm

ggsave(filename = "output/FitnessPlots/NormalizedTaskNegelct.png", width = 2, height = 2, units = "in", dpi = 600)


####################
# Frequency of task not being performed vs. specialization
####################
# Load distribution
taskDist <- unlist(groups_taskDist, recursive = FALSE)
taskDistTot <- do.call("rbind", taskDist)
taskDistTot <- taskDistTot %>% 
  mutate(Set = paste0(n, "-", replicate)) %>% 
  group_by(Set) %>% 
  mutate(minTask1 = min(Task1),
         minTask2 = min(Task2),
         maxTask1 = max(Task1),
         maxTask2 = max(Task2)) %>% 
  group_by(n) %>% 
  mutate(minTask1norm = (minTask1 - min(minTask1)) / (max(minTask1) - min(minTask1))) %>% 
  select(n, Set, minTask1, minTask2, maxTask1, maxTask2) %>% 
  unique()
  
# Merge
merged_specperf <- merge(merged_specperf, taskDistTot, by = c("Set", "n"))
normalizedNeglect <- merged_specperf %>% 
  mutate(noTaskAvg = noTaskAvg / 10000) %>% 
  group_by(n) %>% 
  mutate(noTaskAvgMin = min(noTaskAvg),
         noTaskAvgMax = max(noTaskAvg),
         TaskMeanMin = min(TaskMean),
         TaskMeanMax = max(TaskMean)) %>% 
  mutate(noTaskAvgNorm = (noTaskAvg - noTaskAvgMin) / (noTaskAvgMax - noTaskAvgMin),
         TaskMeanNorm = (TaskMean - TaskMeanMin) / (TaskMeanMax - TaskMeanMin)) 

# Plot
gg_min <- ggplot(data = normalizedNeglect, aes(x = maxTask2, y = TaskMeanNorm, colour = as.factor(n))) +
  geom_point() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("Normalized specialization") +
  xlab("Min task 1 frequency") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     expand = c(0, 0.005)) +
  # scale_x_continuous(breaks = seq(-1, 1, 0.05), 
  #                    limits = c(0, 0.33),
  #                    expand = c(0, 0.005)) +
  scale_color_manual(values = palette) +
  theme(legend.position = "none",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 6),
        legend.text.align = 0,
        # legend.box.background = element_rect(),
        axis.text.y = element_text(size = 8, margin = margin(5, 6, 5, -2), color = "black"),
        axis.text.x = element_text(size = 8, margin = margin(6, 5, -2, 5), color = "black"),
        axis.title = element_text(size = 10, margin = margin(0, 0, 0, 0)),
        axis.ticks.length = unit(-0.1, "cm"),
        aspect.ratio = 1)

gg_min

####################
# Check stimulus level at time of neglect
####################
neglectStim <- lapply(1:length(groups_taskTally), function(i) {
  group_size <- groups_taskTally[[i]]
  size_stim <- groups_stim[[i]]
  # Loop through replicates within group size
  within_groupPerfStim <- lapply(1:length(group_size), function(j) {
    replicate <- group_size[[j]]
    stim <- size_stim[[j]]
    # Get which time steps had non-performance
    # Task 1
    steps <- which(replicate$Task1 == 0)
    steps <- steps[steps > 15]
    stim_levels <- stim$s1[steps+1]
    zscore1 <- ( stim_levels - mean(stim$s1) ) / sd(stim$s1)
    # Task 2
    steps <- which(replicate$Task2 == 0)
    steps <- steps[steps > 15]
    stim_levels <- stim$s2[steps+1]
    zscore2 <- ( stim_levels - mean(stim$s2) ) / sd(stim$s2)
    to_return <- data.frame(n = unique(replicate$n), Zscore = c(zscore1, zscore2) )
    return(to_return)
  })
  # Bind and return
  within_groupPerfStim <- do.call('rbind', within_groupPerfStim)
  return(within_groupPerfStim)
})
# Bind
neglectStim <- do.call('rbind', neglectStim)

# Graph
palette <- c("#83343E", "#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78", "#bdbdbd", "#525252")
gg_neglectStim <- ggplot(neglectStim, aes(x = Zscore, 
                                          color = as.factor(n),
                                          fill = as.factor(n))) +
  geom_histogram(bins = 50) +
  theme_classic() +
  xlab("Zscore of Stim Relative to Mean") +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) +
  facet_wrap(~n, scales = "free_y") +
  theme(legend.position = "none")
gg_neglectStim
