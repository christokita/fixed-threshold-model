################################################################################
#
# Short term task fluctuations vs. group size by model type
#
################################################################################


rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3_PrepPlotExperimentData.R")
library(RColorBrewer)
library(scales)


# Entirely Deterministic 
load("output/__RData/MSrevision_FixedDelta06_DetThreshDetUpdateDetQuit100reps.Rdata")
tallies <- unlist(groups_taskTally, recursive = FALSE)
tallies <- do.call("rbind", tallies)
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
sets <- which(tallyFluct$BeginSet == TRUE)
tallyFluct$Task1Diff[sets] <- NA
tallyFluct$Task2Diff[sets] <- NA
tallyFluct$InactiveDiff[sets] <- NA
tallyFluct <- tallyFluct %>% 
  group_by(n, Set) %>% 
  summarise(Task1Fluct = mean(Task1Diff, na.rm = TRUE),
            Task2Fluct = mean(Task2Diff, na.rm = TRUE),
            InactiveFluct = mean(InactiveDiff, na.rm = TRUE)) %>% 
  mutate(GroupSizeFactor = factor(n, levels = sort(unique(n))),
         TaskFluct = (Task1Fluct + Task2Fluct) / 2)
tallySumFluct <- tallyFluct %>% 
  group_by(n, GroupSizeFactor) %>% 
  summarise(Task1FluctMean = mean(Task1Fluct, na.rm = TRUE),
            Task1FluctSE = sd(Task1Fluct) / sqrt(length(Task1Fluct)),
            Task2FluctMean = mean(Task2Fluct, na.rm = TRUE),
            Task2FluctSE = sd(Task2Fluct, na.rm = TRUE) / sqrt(length(Task2Fluct)),
            InactiveFluctMean = mean(InactiveFluct, na.rm = TRUE),
            InactiveFluctSE = sd(InactiveFluct, na.rm = TRUE) / sqrt(length(InactiveFluct)),
            TaskFluctMean = mean(TaskFluct, na.rm = TRUE),
            TaskFluctSE = sd(TaskFluct, na.rm = TRUE) / sqrt(length(TaskFluct))) %>% 
  mutate(Source = "Deterministic")
tallySumFluct <- as.data.frame(tallySumFluct)
tallySumFluct <- tallySumFluct %>% 
  mutate(GroupSizeFactor = factor(GroupSizeFactor, levels = sort(unique(n))))

tallySumFluct_all <- tallySumFluct

# Probabilistic thresholds
load("output/__RData/MSrevision_FixedDelta06_DetUpdateDetQuit100reps.Rdata")
tallies <- unlist(groups_taskTally, recursive = FALSE)
tallies <- do.call("rbind", tallies)
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
sets <- which(tallyFluct$BeginSet == TRUE)
tallyFluct$Task1Diff[sets] <- NA
tallyFluct$Task2Diff[sets] <- NA
tallyFluct$InactiveDiff[sets] <- NA
tallyFluct <- tallyFluct %>% 
  group_by(n, Set) %>% 
  summarise(Task1Fluct = mean(Task1Diff, na.rm = TRUE),
            Task2Fluct = mean(Task2Diff, na.rm = TRUE),
            InactiveFluct = mean(InactiveDiff, na.rm = TRUE)) %>% 
  mutate(GroupSizeFactor = factor(n, levels = sort(unique(n))),
         TaskFluct = (Task1Fluct + Task2Fluct) / 2)
tallySumFluct <- tallyFluct %>% 
  group_by(n, GroupSizeFactor) %>% 
  summarise(Task1FluctMean = mean(Task1Fluct, na.rm = TRUE),
            Task1FluctSE = sd(Task1Fluct) / sqrt(length(Task1Fluct)),
            Task2FluctMean = mean(Task2Fluct, na.rm = TRUE),
            Task2FluctSE = sd(Task2Fluct, na.rm = TRUE) / sqrt(length(Task2Fluct)),
            InactiveFluctMean = mean(InactiveFluct, na.rm = TRUE),
            InactiveFluctSE = sd(InactiveFluct, na.rm = TRUE) / sqrt(length(InactiveFluct)),
            TaskFluctMean = mean(TaskFluct, na.rm = TRUE),
            TaskFluctSE = sd(TaskFluct, na.rm = TRUE) / sqrt(length(TaskFluct))) %>% 
  mutate(Source = "Prob. Thresholds")
tallySumFluct <- as.data.frame(tallySumFluct)
tallySumFluct <- tallySumFluct %>% 
  mutate(GroupSizeFactor = factor(GroupSizeFactor, levels = sort(unique(n))))


tallySumFluct_all <- rbind(tallySumFluct_all, tallySumFluct)


# Probabilistic quitting
load("output/__RData/MSrevision_FixedDelta06_DetThreshDetUpdate100reps.Rdata")
stims <- unlist(groups_stim, recursive = FALSE)
stims <- do.call("rbind", stims)
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
sets <- which(stimFluct$BeginSet == TRUE)
stimFluct$s1Diff[sets] <- NA
stimFluct$s2Diff[sets] <- NA
stimFluct <- stimFluct %>% 
  group_by(n, Set) %>% 
  summarise(s1Fluct = mean(s1Diff, na.rm = TRUE),
            s2Fluct = mean(s2Diff, na.rm = TRUE)) %>% 
  mutate(GroupSizeFactor = factor(n, levels = sort(unique(n))),
         sFluct = (s1Fluct + s2Fluct) / 2)
stimSumFluct <- stimFluct %>% 
  group_by(n, GroupSizeFactor) %>% 
  summarise(sFluctMean = mean(sFluct, na.rm = TRUE),
            sFluctSE = sd(sFluct, na.rm = TRUE) / sqrt(length(sFluct))) %>% 
  mutate(Source = "Prob. Quitting")
stimSumFluct <- as.data.frame(stimSumFluct)

stimFluct_all <- rbind(stimFluct_all, stimSumFluct)


# Probabilistic Updating
load("output/__RData/MSrevision_FixedDelta06_DetThreshDetQuit100reps.Rdata")
stims <- unlist(groups_stim, recursive = FALSE)
stims <- do.call("rbind", stims)
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
sets <- which(stimFluct$BeginSet == TRUE)
stimFluct$s1Diff[sets] <- NA
stimFluct$s2Diff[sets] <- NA
stimFluct <- stimFluct %>% 
  group_by(n, Set) %>% 
  summarise(s1Fluct = mean(s1Diff, na.rm = TRUE),
            s2Fluct = mean(s2Diff, na.rm = TRUE)) %>% 
  mutate(GroupSizeFactor = factor(n, levels = sort(unique(n))),
         sFluct = (s1Fluct + s2Fluct) / 2)
stimSumFluct <- stimFluct %>% 
  group_by(n, GroupSizeFactor) %>% 
  summarise(sFluctMean = mean(sFluct, na.rm = TRUE),
            sFluctSE = sd(sFluct, na.rm = TRUE) / sqrt(length(sFluct))) %>% 
  mutate(Source = "Prob. Updating")
stimSumFluct <- as.data.frame(stimSumFluct)

stimFluct_all <- rbind(stimFluct_all, stimSumFluct)


# Threshold Variation
load("output/__RData/MSrevision_FixedDelta06_DetThreshWithSigmaDetUpdateDetQuit100reps.Rdata")
stims <- unlist(groups_stim, recursive = FALSE)
stims <- do.call("rbind", stims)
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
sets <- which(stimFluct$BeginSet == TRUE)
stimFluct$s1Diff[sets] <- NA
stimFluct$s2Diff[sets] <- NA
stimFluct <- stimFluct %>% 
  group_by(n, Set) %>% 
  summarise(s1Fluct = mean(s1Diff, na.rm = TRUE),
            s2Fluct = mean(s2Diff, na.rm = TRUE)) %>% 
  mutate(GroupSizeFactor = factor(n, levels = sort(unique(n))),
         sFluct = (s1Fluct + s2Fluct) / 2)
stimSumFluct <- stimFluct %>% 
  group_by(n, GroupSizeFactor) %>% 
  summarise(sFluctMean = mean(sFluct, na.rm = TRUE),
            sFluctSE = sd(sFluct, na.rm = TRUE) / sqrt(length(sFluct))) %>% 
  mutate(Source = "Threshold Variation")
stimSumFluct <- as.data.frame(stimSumFluct)

stimFluct_all <- rbind(stimFluct_all, stimSumFluct)



# Original model
load("output/__RData/FixedDelta06Sigma01Eta7100reps.Rdata")
stims <- unlist(groups_stim, recursive = FALSE)
stims <- do.call("rbind", stims)
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
sets <- which(stimFluct$BeginSet == TRUE)
stimFluct$s1Diff[sets] <- NA
stimFluct$s2Diff[sets] <- NA
stimFluct <- stimFluct %>% 
  group_by(n, Set) %>% 
  summarise(s1Fluct = mean(s1Diff, na.rm = TRUE),
            s2Fluct = mean(s2Diff, na.rm = TRUE)) %>% 
  mutate(GroupSizeFactor = factor(n, levels = sort(unique(n))),
         sFluct = (s1Fluct + s2Fluct) / 2)
stimSumFluct <- stimFluct %>% 
  group_by(n, GroupSizeFactor) %>% 
  summarise(sFluctMean = mean(sFluct, na.rm = TRUE),
            sFluctSE = sd(sFluct, na.rm = TRUE) / sqrt(length(sFluct))) %>% 
  mutate(Source = "Full Model")
stimSumFluct <- as.data.frame(stimSumFluct)

stimFluct_all <- rbind(stimFluct_all, stimSumFluct)


# Plot
stimFluct_all$Source <- factor(stimFluct_all$Source, levels = c("Deterministic",
                                                                "Prob. Updating",
                                                                "Prob. Quitting",
                                                                "Prob. Thresholds",
                                                                "Threshold Variation",
                                                                "Full Model"))



# Plot all
gg_models <- ggplot(data = stimFluct_all) +
  geom_line(aes(x = n, y = sFluctMean, colour = Source)) +
  geom_errorbar(aes(x = n, ymin = sFluctMean - sFluctSE, ymax = sFluctMean + sFluctSE, colour = Source),
                width = 1) +
  geom_point(aes(x = n, y = sFluctMean, colour = Source),
             size = 1.5) +
  theme_classic() +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = unique(stimFluct_all$n)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.95), breaks = seq(0, 1, 0.2)) +
  xlab("Group size") +
  ylab("Stimulus fluctuation") +
  theme(legend.position = "none",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 8),
        legend.text.align = 0,
        # legend.box.background = element_rect(),
        axis.text.y = element_text(size = 8, margin = margin(5, 6, 5, -2), color = "black"),
        axis.text.x = element_text(size = 8, margin = margin(6, 5, -2, 5), color = "black"),
        axis.title = element_text(size = 10, margin = margin(0, 0, 0, 0)),
        axis.ticks.length = unit(-0.1, "cm"),
        aspect.ratio = 1)

gg_models


ggsave("output/StochasticElements/StimFluctByModelType.png", width = 2, height = 2, dpi = 600, unit = "in")
