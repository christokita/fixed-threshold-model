################################################################################
#
# Behavioral variation vs. group size by model type
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3_PrepPlotExperimentData.R")
library(RColorBrewer)
library(scales)

# Entirely Deterministic 
load("output/__RData/MSrevision_FixedDelta06_DetThreshDetUpdateDetQuit100reps.Rdata")

# load and manipulate
taskDist <- unlist(groups_taskDist, recursive = FALSE)
taskDistTot <- do.call("rbind", taskDist)
taskDistTot <- taskDistTot %>% 
  mutate(Set = paste0(n, "-", replicate))

taskVarMean <- taskDistTot %>% 
  mutate(n = as.character(n)) %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(n, replicate, Set) %>% 
  summarise(SD1 = sd(Task1),
            SD2 = sd(Task2),
            Mean = mean(Task1)) %>% 
  mutate(Source = "Determinstic",
         SD = (SD1 + SD2) / 2) %>% 
  group_by(n, Source) %>% 
  summarise(VariationMean = mean(SD, na.rm = T),
            VaraiationSE = sd(SD, na.rm = T) / sqrt(length(SD)))
taskVarMean[is.na(taskVarMean)] <- 0


taskVarMean_all <- taskVarMean



# Probabilistic thresholds
load("output/__RData/MSrevision_FixedDelta06_DetUpdateDetQuit100reps.Rdata")

# load and manipulate
taskDist <- unlist(groups_taskDist, recursive = FALSE)
taskDistTot <- do.call("rbind", taskDist)
taskDistTot <- taskDistTot %>% 
  mutate(Set = paste0(n, "-", replicate))

taskVarMean <- taskDistTot %>% 
  mutate(n = as.character(n)) %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(n, replicate, Set) %>% 
  summarise(SD1 = sd(Task1),
            SD2 = sd(Task2),
            Mean = mean(Task1)) %>% 
  mutate(Source = "Prob. Thresholds",
         SD = (SD1 + SD2) / 2) %>% 
  group_by(n, Source) %>% 
  summarise(VariationMean = mean(SD, na.rm = T),
            VaraiationSE = sd(SD, na.rm = T) / sqrt(length(SD)))
taskVarMean[is.na(taskVarMean)] <- 0

taskVarMean_all <- rbind(taskVarMean_all, taskVarMean)

# Probabilistic quitting
load("output/__RData/MSrevision_FixedDelta06_DetThreshDetUpdate100reps.Rdata")

# load and manipulate
taskDist <- unlist(groups_taskDist, recursive = FALSE)
taskDistTot <- do.call("rbind", taskDist)
taskDistTot <- taskDistTot %>% 
  mutate(Set = paste0(n, "-", replicate))

taskVarMean <- taskDistTot %>% 
  mutate(n = as.character(n)) %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(n, replicate, Set) %>% 
  summarise(SD1 = sd(Task1),
            SD2 = sd(Task2),
            Mean = mean(Task1)) %>% 
  mutate(Source = "Prob. Quitting",
         SD = (SD1 + SD2) / 2) %>% 
  group_by(n, Source) %>% 
  summarise(VariationMean = mean(SD, na.rm = T),
            VaraiationSE = sd(SD, na.rm = T) / sqrt(length(SD)))
taskVarMean[is.na(taskVarMean)] <- 0


taskVarMean_all <- rbind(taskVarMean_all, taskVarMean)

# Probabilistic Updating
load("output/__RData/MSrevision_FixedDelta06_DetThreshDetQuit100reps.Rdata")

# load and manipulate
taskDist <- unlist(groups_taskDist, recursive = FALSE)
taskDistTot <- do.call("rbind", taskDist)
taskDistTot <- taskDistTot %>% 
  mutate(Set = paste0(n, "-", replicate))

taskVarMean <- taskDistTot %>% 
  mutate(n = as.character(n)) %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(n, replicate, Set) %>% 
  summarise(SD1 = sd(Task1),
            SD2 = sd(Task2),
            Mean = mean(Task1)) %>% 
  mutate(Source = "Prob. Updating",
         SD = (SD1 + SD2) / 2) %>% 
  group_by(n, Source) %>% 
  summarise(VariationMean = mean(SD, na.rm = T),
            VaraiationSE = sd(SD, na.rm = T) / sqrt(length(SD)))
taskVarMean[is.na(taskVarMean)] <- 0


taskVarMean_all <- rbind(taskVarMean_all, taskVarMean)

# Plot before adding threshold variation model
gg_models_noVar <- ggplot(data = taskVarMean_all) +
  geom_errorbar(aes(x = n, ymin = VariationMean - VaraiationSE, ymax = VariationMean + VaraiationSE, colour = Source),
                width = 1.5,
                position = position_dodge(width = 1)) +
  geom_point(aes(x = n, y = VariationMean, colour = Source),
             size = 2,
             position = position_dodge(width = 1)) +
  geom_line(aes(x = n, y = VariationMean, colour = Source),
            position = position_dodge(width = 1)) +
  theme_classic() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = unique(taskVarMean_all$n)) +
  xlab("Group Size") +
  ylab("Behavioral Variation (SD)") +
  theme(legend.position = "right",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 10),
        legend.text.align = 0,
        # legend.box.background = element_rect(),
        axis.text.y = element_text(size = 10, margin = margin(5, 6, 5, -2), color = "black"),
        axis.text.x = element_text(size = 10, margin = margin(6, 5, -2, 5), color = "black"),
        axis.title = element_text(size = 11, margin = margin(0, 0, 0, 0)),
        axis.ticks.length = unit(-0.1, "cm"),
        aspect.ratio = 1)

gg_models_noVar

# Threshold Variation
load("output/__RData/MSrevision_FixedDelta06_DetThreshWithSigmaDetUpdateDetQuit100reps.Rdata")

# load and manipulate
taskDist <- unlist(groups_taskDist, recursive = FALSE)
taskDistTot <- do.call("rbind", taskDist)
taskDistTot <- taskDistTot %>% 
  mutate(Set = paste0(n, "-", replicate))

taskVarMean <- taskDistTot %>% 
  mutate(n = as.character(n)) %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(n, replicate, Set) %>% 
  summarise(SD1 = sd(Task1),
            SD2 = sd(Task2),
            Mean = mean(Task1)) %>% 
  mutate(Source = "Threshold Variation",
         SD = (SD1 + SD2) / 2) %>% 
  group_by(n, Source) %>% 
  summarise(VariationMean = mean(SD, na.rm = T),
            VaraiationSE = sd(SD, na.rm = T) / sqrt(length(SD)))
taskVarMean[is.na(taskVarMean)] <- 0


taskVarMean_all <- rbind(taskVarMean_all, taskVarMean)

# Original model
load("output/__RData/FixedDelta06Sigma01Eta7100reps.Rdata")

# load and manipulate
taskDist <- unlist(groups_taskDist, recursive = FALSE)
taskDistTot <- do.call("rbind", taskDist)
taskDistTot <- taskDistTot %>% 
  mutate(Set = paste0(n, "-", replicate))

taskVarMean <- taskDistTot %>% 
  mutate(n = as.character(n)) %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(n, replicate, Set) %>% 
  summarise(SD1 = sd(Task1),
            SD2 = sd(Task2),
            Mean = mean(Task1)) %>% 
  mutate(Source = "All (Original)",
         SD = (SD1 + SD2) / 2) %>% 
  group_by(n, Source) %>% 
  summarise(VariationMean = mean(SD, na.rm = T),
            VaraiationSE = sd(SD, na.rm = T) / sqrt(length(SD)))
taskVarMean[is.na(taskVarMean)] <- 0


taskVarMean_all <- rbind(taskVarMean_all, taskVarMean)

taskVarMean_all$Source <- factor(taskVarMean_all$Source, levels = c("Determinstic",
                                                                    "Prob. Quitting",
                                                                    "Prob. Thresholds",
                                                                    "Prob. Updating",
                                                                    "Threshold Variation",
                                                                    "All (Original)"))

# Plot all
gg_models <- ggplot(data = taskVarMean_all) +
  geom_errorbar(aes(x = n, ymin = VariationMean - VaraiationSE, ymax = VariationMean + VaraiationSE, colour = Source),
                width = 1.5,
                position = position_dodge(width = 1)) +
  geom_point(aes(x = n, y = VariationMean, colour = Source),
             size = 2,
             position = position_dodge(width = 1)) +
  geom_line(aes(x = n, y = VariationMean, colour = Source),
            position = position_dodge(width = 1)) +
  theme_classic() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = unique(taskVarMean_all$n)) +
  scale_y_continuous(breaks = (-1, 1, 0.2)) +
  xlab("Group Size") +
  ylab("Behavioral Variation (SD)") +
  theme(legend.position = "right",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 10),
        legend.text.align = 0,
        # legend.box.background = element_rect(),
        axis.text.y = element_text(size = 10, margin = margin(5, 6, 5, -2), color = "black"),
        axis.text.x = element_text(size = 10, margin = margin(6, 5, -2, 5), color = "black"),
        axis.title = element_text(size = 11, margin = margin(0, 0, 0, 0)),
        axis.ticks.length = unit(-0.1, "cm"),
        aspect.ratio = 1)

gg_models
