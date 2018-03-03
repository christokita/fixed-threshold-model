################################################################################
#
# Check within group size specialization vs stim fluctuation
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3_PrepPlotExperimentData.R")

load("output/__RData/MSrevision_FixedDelta06_DetThreshWithSigmaDetUpdateDetQuit100reps.Rdata")

# Set variable  
filename <- "CompleteDeterministicWithSigma"

# Palette without single individuals
#palette <- c("#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78", "#bdbdbd", "#525252")

# Palette without single individuals
palette <- c("#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78", "#bdbdbd", "#525252")


####################
# Task Rank Correlation
####################
# Unlist
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2)

# Manipulate and bind with Yuko data
taskCorrTot <- taskCorrTot %>% 
  mutate(Set = paste0(n, "-", replicate)) %>% 
  select(n, TaskMean, Set) 

####################
# Stimulus and task Fluctuation
####################
# Unlist stims
stims <- unlist(groups_stim, recursive = FALSE)
stims <- do.call("rbind", stims)

# Unlist task tallies
tallies <- unlist(groups_taskTally, recursive = FALSE)
tallies <- do.call("rbind", tallies)

# Loop through time windows of interest
fluctuations <- lapply(c(200, 1), function(step) {
  #### Stimulus fluctuations ###
  # Normalize and Summarise by "day" (i.e., time window) and calculate difference
  stimFluct <- stims %>%
    select(-delta1, -delta2) %>%
    mutate(Set = paste0(n, "-", replicate)) %>%
    group_by(Set) %>%
    mutate(t = 0:(length(Set)-1)) %>%
    mutate(Window = t %/% step) %>%
    filter(t != 0) %>%
    group_by(n, Set, Window) %>%
    summarise(s1 = mean(s1),
              s2 = mean(s2)) %>%
    mutate(s1Diff = abs(s1 - lag(s1)),
           s2Diff = abs(s2 - lag(s2)),
           BeginSet = !duplicated(Set))
  # Make sure first diff row of each new set is NA
  sets <- which(stimFluct$BeginSet == TRUE)
  stimFluct$s1Diff[sets] <- NA
  stimFluct$s2Diff[sets] <- NA
  # Summarise by colony/set
  stimFluct <- stimFluct %>% 
    group_by(n, Set) %>% 
    summarise(s1Fluct = mean(s1Diff, na.rm = TRUE),
              s2Fluct = mean(s2Diff, na.rm = TRUE)) %>% 
    mutate(GroupSizeFactor = factor(n, levels = sort(unique(n))),
           stimFluct = (s1Fluct + s2Fluct) / 2)
  #### Task Fluctuations ###
  # Normalize and Summarise by "day" (i.e., time window) and calculate difference
  tallyFluct <- tallies %>%
    mutate(Task1 = Task1 / n,
           Task2 = Task2 / n,
           Inactive = Inactive / n,
           Set = paste0(n, "-", replicate),
           Window = t %/% step) %>%
    group_by(n, Set, Window) %>%
    summarise(Task1 = mean(Task1),
              Task2 = mean(Task2),
              Inactive = mean(Inactive)) %>%
    mutate(Task1Diff = abs(Task1 - lag(Task1)),
           Task2Diff = abs(Task2 - lag(Task2)),
           InactiveDiff = abs(Inactive - lag(Inactive)),
           BeginSet = !duplicated(Set))
  # Make sure first diff row of each new set is NA
  sets <- which(tallyFluct$BeginSet == TRUE)
  tallyFluct$Task1Diff[sets] <- NA
  tallyFluct$Task2Diff[sets] <- NA
  tallyFluct$InactiveDiff[sets] <- NA
  # Summarise by colony/set
  tallyFluct <- tallyFluct %>% 
    group_by(n, Set) %>% 
    summarise(Task1Fluct = mean(Task1Diff, na.rm = TRUE),
              Task2Fluct = mean(Task2Diff, na.rm = TRUE),
              InactiveFluct = mean(InactiveDiff, na.rm = TRUE)) %>% 
    mutate(GroupSizeFactor = factor(n, levels = sort(unique(n))),
           taskFluct = (Task1Fluct + Task2Fluct) / 2)
  #### Merge and retur  ###
  # Merge
  merged_specstim <- merge(taskCorrTot, stimFluct, by = c("Set", "n"))
  merged_specstim <- merge(merged_specstim, tallyFluct, by = c("Set", "n"))
  return(merged_specstim)
})

####################
# Plot
####################
#### 200 time steps ####
merged_specstim <- fluctuations[[1]]

# Plot - Stimulus vs Specialization by Group Size
gg_compareStim200 <- ggplot(merged_specstim, aes(x = TaskMean, y = s1Fluct, colour = as.factor(n))) +
  geom_point() +
  theme_classic() +
  scale_color_manual(values = palette) +
  theme(legend.position = "none") +
  facet_wrap(~ n, scales = "free") +
  xlab("Rank Correlation") +
  ylab("Stim 1 Fluctuations (200 steps)")

# Plot - Task Fluctuation vs Specialization by Group Size
gg_compareTask200 <- ggplot(merged_specstim, aes(x = TaskMean, y = Task1Fluct, colour = as.factor(n))) +
  geom_point() +
  theme_classic() +
  scale_color_manual(values = palette) +
  theme(legend.position = "none") +
  facet_wrap(~ n, scales = "free") +
  xlab("Rank Correlation") +
  ylab("Task 1 Fluctuations (200 steps)")

#### 1 time steps ####
merged_specstim <- fluctuations[[2]]

# Plot - Stimulus vs Specialization by Group Size
gg_compareStim1 <- ggplot(merged_specstim, aes(x = TaskMean, y = s1Fluct, colour = as.factor(n))) +
  geom_point() +
  theme_classic() +
  scale_color_manual(values = palette) +
  theme(legend.position = "none") +
  facet_wrap(~ n, scales = "free") +
  xlab("Rank Correlation") +
  ylab("Stim 1 Fluctuations (1 steps)")

# Plot - Task Fluctuation vs Specialization by Group Size
gg_compareTask1 <- ggplot(merged_specstim, aes(x = TaskMean, y = Task1Fluct, colour = as.factor(n))) +
  geom_point() +
  theme_classic() +
  scale_color_manual(values = palette) +
  theme(legend.position = "none") +
  facet_wrap(~ n, scales = "free") +
  xlab("Rank Correlation") +
  ylab("Task 1 Fluctuations (1 steps)")


# Plot together
png(filename = paste0("output/RevisionChecks/WithinGroupFluctuations/", filename, ".png"), 
    width = 10, height = 7, units = "in", res = 300)
multiplot(gg_compareStim200, gg_compareTask200, gg_compareStim1, gg_compareTask1, cols = 2)
dev.off()
