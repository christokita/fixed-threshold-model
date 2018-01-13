################################################################################
#
# Check within group size specialization vs stim fluctuation
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3A_PrepPlotExperimentData.R")

load("output/__RData/FixedDelta06Sigma01Eta7100reps.Rdata")

# Set variable  
filename <- "Fixed_Delta06Sigma01Eta7"

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
# Stimulus Fluctuation
####################
# Unlist
stims <- unlist(groups_stim, recursive = FALSE)
stims <- do.call("rbind", stims)

#### 200 Time steps ####
# Normalize and Summarise by "day" (i.e., time window) and calculate difference
stimFluct <- stims %>% 
  select(-delta1, -delta2) %>% 
  mutate(Set = paste0(n, "-", replicate)) %>% 
  group_by(Set) %>% 
  mutate(t = 0:(length(Set)-1)) %>% 
  mutate(Window = t %/% 200) %>% 
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


####################
# Merge and plot
####################
# Merge
merged_specstim <- merge(taskCorrTot, stimFluct, by = c("Set", "n"))

# Plot
gg_compareTot <- gg_compare <- ggplot(merged_specstim, aes(x = TaskMean, y = s1Fluct, colour = as.factor(n))) +
  geom_point() +
  theme_classic() +
  scale_color_manual(values = palette, name = "Group Size") +
  xlab("Task Correlation") +
  ylab("Stim 1 Fluctuations")
gg_compareTot

# Plot
gg_compare <- ggplot(merged_specstim, aes(x = TaskMean, y = s1Fluct, colour = as.factor(n))) +
  geom_point() +
  theme_classic() +
  scale_color_manual(values = palette) +
  theme(legend.position = "none") +
  facet_wrap(~ n) +
  xlab("Task Correlation") +
  ylab("Stim 1 Fluctuations")
gg_compare



