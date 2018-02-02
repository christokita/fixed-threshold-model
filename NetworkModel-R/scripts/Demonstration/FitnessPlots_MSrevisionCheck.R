################################################################################
#
# Fitness Plots
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3_PrepPlotExperimentData.R")
library(RColorBrewer)
library(scales)

load("output/__RData/MSrevision_FixedDelta06_DetThreshDetUpdateDetQuit100reps.Rdata")

filename <- "DetThreshDetUpdateDetQuit"

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
  mutate(GroupSizeFactor = factor(n, levels = sort(unique(n))))

# Summarise by n
stimSumFluct <- stimFluct %>% 
  group_by(n, GroupSizeFactor) %>% 
  summarise(s1FluctMean = mean(s1Fluct, na.rm = TRUE),
            s1FluctSE = sd(s1Fluct, na.rm = TRUE) / sqrt(length(s1Fluct)),
            s2FluctMean = mean(s2Fluct, na.rm = TRUE),
            s2FluctSE = sd(s2Fluct, na.rm = TRUE) / sqrt(length(s2Fluct)))
stimSumFluct <- as.data.frame(stimSumFluct)
stimSumFluct <- stimSumFluct %>% 
  mutate(GroupSizeFactor = factor(GroupSizeFactor, levels = sort(unique(n))))

# Plot
gg_stimfluct200 <- ggplot() +
  geom_point(data = stimFluct, 
             aes(x = n, y = s1Fluct),
             fill = "grey50", 
             colour = "grey50", 
             position = position_jitter(width = 0.1),
             size = 0.7, 
             alpha = 0.4,
             stroke = 0) +
  # geom_line(data = stimSumFluct,
  #           aes(x = n, y = s1FluctMean),
  #           size = 0.3) +���
  theme_classic() +
  labs(x = "Group size",
       y = "Stimulus fluctuation") +
  scale_x_continuous(breaks = unique(stimFluct$n)) +
  scale_y_continuous(breaks = seq(0, 2, 0.4),
                     limits = c(0, 1.85),
                     expand = c(0, 0)) +
  theme(legend.position = "none") +
  # Mean and SE portion of plot
  geom_errorbar(data = stimSumFluct, 
                aes(x = n, 
                    ymin = s1FluctMean - s1FluctSE, 
                    ymax = s1FluctMean + s1FluctSE),
                colour = "black",
                size = 0.25) +
  geom_point(data = stimSumFluct, 
             aes(x = n, y = s1FluctMean),
             colour = "black",
             size = 1.5) +
  theme(legend.position = "none",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 6),
        legend.text.align = 0,
        # legend.box.background = element_rect(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10, margin = margin(0, 0, 0, 0)))


#### 1 Time steps ####
# Normalize and Summarise by "day" (i.e., time window) and calculate difference
stimFluct <- stims %>% 
  select(-delta1, -delta2) %>% 
  mutate(Set = paste0(n, "-", replicate)) %>% 
  group_by(Set) %>% 
  mutate(t = 0:(length(Set)-1)) %>% 
  mutate(Window = t %/% 1) %>% 
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
  mutate(GroupSizeFactor = factor(n, levels = sort(unique(n))))

# Summarise by n
stimSumFluct <- stimFluct %>% 
  group_by(n, GroupSizeFactor) %>% 
  summarise(s1FluctMean = mean(s1Fluct, na.rm = TRUE),
            s1FluctSE = sd(s1Fluct, na.rm = TRUE) / sqrt(length(s1Fluct)),
            s2FluctMean = mean(s2Fluct, na.rm = TRUE),
            s2FluctSE = sd(s2Fluct, na.rm = TRUE) / sqrt(length(s2Fluct)))
stimSumFluct <- as.data.frame(stimSumFluct)
stimSumFluct <- stimSumFluct %>% 
  mutate(GroupSizeFactor = factor(GroupSizeFactor, levels = sort(unique(n))))

# Plot
gg_stimfluct1 <- ggplot() +
  geom_point(data = stimFluct, 
             aes(x = n, y = s1Fluct),
             fill = "grey50", 
             colour = "grey50", 
             position = position_jitter(width = 0.1),
             size = 0.7, 
             alpha = 0.4,
             stroke = 0) +
  # geom_line(data = stimSumFluct,
  #           aes(x = n, y = s1FluctMean),
  #           size = 0.3) +���
  theme_classic() +
  labs(x = "Group size",
       y = "Stimulus fluctuation") +
  scale_x_continuous(breaks = unique(stimFluct$n)) +
  scale_y_continuous(breaks = seq(0, 2, 0.2),
                     limits = c(0, 0.85),
                     expand = c(0, 0)) +
  theme(legend.position = "none") +
  # Mean and SE portion of plot
  geom_errorbar(data = stimSumFluct, 
                aes(x = n, 
                    ymin = s1FluctMean - s1FluctSE, 
                    ymax = s1FluctMean + s1FluctSE),
                colour = "black",
                size = 0.25) +
  geom_point(data = stimSumFluct, 
             aes(x = n, y = s1FluctMean),
             colour = "black",
             size = 1.5) +
  theme(legend.position = "none",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 6),
        legend.text.align = 0,
        # legend.box.background = element_rect(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10, margin = margin(0, 0, 0, 0)))

####################
# Task Performance Fluctuation
####################
# Unlist
tallies <- unlist(groups_taskTally, recursive = FALSE)
tallies <- do.call("rbind", tallies)

#### 200 Time steps ####
# Normalize and Summarise by "day" (i.e., time window) and calculate difference
tallyFluct <- tallies %>% 
  mutate(Task1 = Task1 / n,
         Task2 = Task2 / n,
         Inactive = Inactive / n,
         Set = paste0(n, "-", replicate),
         Window = t %/% 200) %>% 
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
  mutate(GroupSizeFactor = factor(n, levels = sort(unique(n))))

# Summarise by n
tallySumFluct <- tallyFluct %>% 
  group_by(n, GroupSizeFactor) %>% 
  summarise(Task1FluctMean = mean(Task1Fluct, na.rm = TRUE),
            Task1FluctSE = sd(Task1Fluct) / sqrt(length(Task1Fluct)),
            Task2FluctMean = mean(Task2Fluct, na.rm = TRUE),
            Task2FluctSE = sd(Task2Fluct, na.rm = TRUE) / sqrt(length(Task2Fluct)),
            InactiveFluctMean = mean(InactiveFluct, na.rm = TRUE),
            InactiveFluctSE = sd(InactiveFluct, na.rm = TRUE) / sqrt(length(InactiveFluct)))
tallySumFluct <- as.data.frame(tallySumFluct)
tallySumFluct <- tallySumFluct %>% 
  mutate(GroupSizeFactor = factor(GroupSizeFactor, levels = sort(unique(n))))


# Plot
gg_fluct200 <- ggplot() +
  geom_point(data = tallyFluct, 
             aes(x = n, y = Task1Fluct),
             fill = "grey50", 
             colour = "grey50", 
             size = 0.7, 
             position = position_jitter(width = 0.1),
             alpha = 0.4,
             stroke = 0) +
  theme_classic() +
  labs(x = "Group size",
       y = "Task fluctuation") +
  scale_x_continuous(breaks = unique(tallyFluct$n)) +
  scale_y_continuous(breaks = seq(0, 0.2, 0.01),
                     limits = c(0, 0.069),
                     expand = c(0, 0)) +
  theme(legend.position = "none") +
  # Mean and SE portion of plot
  geom_errorbar(data = tallySumFluct, 
                aes(x = n, 
                    ymin = Task1FluctMean - Task1FluctSE, 
                    ymax = Task1FluctMean + Task1FluctSE),
                colour= "black",
                size = 0.25) +
  geom_point(data = tallySumFluct, 
             aes(x = n, y = Task1FluctMean),
             colour = "black",
             size = 1.5) +
  theme(legend.position = "none",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 6),
        legend.text.align = 0,
        # legend.box.background = element_rect(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10, margin = margin(0, 0, 0, 0)))

#### 1 Time steps ####
# Normalize and Summarise by "day" (i.e., time window) and calculate difference
tallyFluct <- tallies %>% 
  mutate(Task1 = Task1 / n,
         Task2 = Task2 / n,
         Inactive = Inactive / n,
         Set = paste0(n, "-", replicate),
         Window = t %/% 1) %>% 
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
  mutate(GroupSizeFactor = factor(n, levels = sort(unique(n))))

# Summarise by n
tallySumFluct <- tallyFluct %>% 
  group_by(n, GroupSizeFactor) %>% 
  summarise(Task1FluctMean = mean(Task1Fluct, na.rm = TRUE),
            Task1FluctSE = sd(Task1Fluct) / sqrt(length(Task1Fluct)),
            Task2FluctMean = mean(Task2Fluct, na.rm = TRUE),
            Task2FluctSE = sd(Task2Fluct, na.rm = TRUE) / sqrt(length(Task2Fluct)),
            InactiveFluctMean = mean(InactiveFluct, na.rm = TRUE),
            InactiveFluctSE = sd(InactiveFluct, na.rm = TRUE) / sqrt(length(InactiveFluct)))
tallySumFluct <- as.data.frame(tallySumFluct)
tallySumFluct <- tallySumFluct %>% 
  mutate(GroupSizeFactor = factor(GroupSizeFactor, levels = sort(unique(n))))


# Plot
gg_fluct1 <- ggplot() +
  geom_point(data = tallyFluct, 
             aes(x = n, y = Task1Fluct),
             fill = "grey50", 
             colour = "grey50", 
             size = 0.7, 
             position = position_jitter(width = 0.1),
             alpha = 0.4,
             stroke = 0) +
  theme_classic() +
  labs(x = "Group size",
       y = "Task fluctuation") +
  scale_x_continuous(breaks = unique(tallyFluct$n)) +
  scale_y_continuous(breaks = seq(0, 0.22, 0.02),
                     limits = c(0, 0.155),
                     expand = c(0, 0)) +
  theme(legend.position = "none") +
  # Mean and SE portion of plot
  geom_errorbar(data = tallySumFluct, 
                aes(x = n, 
                    ymin = Task1FluctMean - Task1FluctSE, 
                    ymax = Task1FluctMean + Task1FluctSE),
                colour= "black",
                size = 0.25) +
  geom_point(data = tallySumFluct, 
             aes(x = n, y = Task1FluctMean),
             colour = "black",
             size = 1.5) +
  theme(legend.position = "none",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 6),
        legend.text.align = 0,
        # legend.box.background = element_rect(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10, margin = margin(0, 0, 0, 0)))

####################
# Plot
####################
png(filename = paste0("output/", filename, ".png"), width = 4, height = 4, units = "in", res = 300)
multiplot(gg_stimfluct200, gg_stimfluct1, gg_fluct200, gg_fluct1, cols = 2)
dev.off()
