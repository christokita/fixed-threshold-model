################################################################################
#
# Comparing Specialization
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3_PrepPlotExperimentData.R")
library(RColorBrewer)
library(scales)

# Entirely Deterministic 
load("output/__RData/MSrevision_FixedDelta06_DetThreshDetUpdateDetQuit100reps.Rdata")
entropy <- unlist(groups_entropy, recursive = FALSE)
entropy <- do.call("rbind", entropy)  %>% 
  mutate(set = paste(n, replicate, sep = "-"))%>% 
  select(-Dsym, -Dyx) %>% 
  filter(n != 1)
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2) %>% 
  mutate(set = paste(n, replicate, sep = "-"))%>% 
  select(-Task1, -Task2)
taskSpec <- groups_specialization %>% 
  group_by(n, replicate) %>% 
  summarise(SpecMean = mean(TransSpec))  %>% 
  mutate(set = paste(n, replicate, sep = "-")) 
metrics <- merge(taskCorrTot, entropy)
metrics <- merge(metrics, taskSpec)
# Summarise
metrics <- metrics %>% 
  melt(., id.vars = c("n", "replicate", "set"))
names(metrics) <- c("n", "replicate", "set", "metric", "value")
metrics <- metrics %>% 
  group_by(n, metric) %>% 
  summarise(Mean = mean(value, na.rm = T),
            SE = sd(value, na.rm = T) / sqrt(length(value))) %>% 
  mutate(Source = "Deterministic")
metrics_all <- metrics


# Probabilistic thresholds
load("output/__RData/MSrevision_FixedDelta06_DetUpdateDetQuit100reps.Rdata")
entropy <- unlist(groups_entropy, recursive = FALSE)
entropy <- do.call("rbind", entropy)  %>% 
  mutate(set = paste(n, replicate, sep = "-"))%>% 
  select(-Dsym, -Dyx) %>% 
  filter(n != 1)
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2) %>% 
  mutate(set = paste(n, replicate, sep = "-"))%>% 
  select(-Task1, -Task2)
taskSpec <- groups_specialization %>% 
  group_by(n, replicate) %>% 
  summarise(SpecMean = mean(TransSpec))  %>% 
  mutate(set = paste(n, replicate, sep = "-")) 
metrics <- merge(taskCorrTot, entropy)
metrics <- merge(metrics, taskSpec)
# Summarise
metrics <- metrics %>% 
  melt(., id.vars = c("n", "replicate", "set"))
names(metrics) <- c("n", "replicate", "set", "metric", "value")
metrics <- metrics %>% 
  group_by(n, metric) %>% 
  summarise(Mean = mean(value, na.rm = T),
            SE = sd(value, na.rm = T) / sqrt(length(value))) %>% 
  mutate(Source = "Prob. Thresholds")
metrics_all <- rbind(metrics_all, metrics)


# Probabilistic quitting
load("output/__RData/MSrevision_FixedDelta06_DetThreshDetUpdate100reps.Rdata")
entropy <- unlist(groups_entropy, recursive = FALSE)
entropy <- do.call("rbind", entropy)  %>% 
  mutate(set = paste(n, replicate, sep = "-"))%>% 
  select(-Dsym, -Dyx) %>% 
  filter(n != 1)
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2) %>% 
  mutate(set = paste(n, replicate, sep = "-"))%>% 
  select(-Task1, -Task2)
taskSpec <- groups_specialization %>% 
  group_by(n, replicate) %>% 
  summarise(SpecMean = mean(TransSpec))  %>% 
  mutate(set = paste(n, replicate, sep = "-")) 
metrics <- merge(taskCorrTot, entropy)
metrics <- merge(metrics, taskSpec)
# Summarise
metrics <- metrics %>% 
  melt(., id.vars = c("n", "replicate", "set"))
names(metrics) <- c("n", "replicate", "set", "metric", "value")
metrics <- metrics %>% 
  group_by(n, metric) %>% 
  summarise(Mean = mean(value, na.rm = T),
            SE = sd(value, na.rm = T) / sqrt(length(value))) %>% 
  mutate(Source = "Prob. Quitting")
metrics_all <- rbind(metrics_all, metrics)

# Probabilistic Updating
load("output/__RData/MSrevision_FixedDelta06_DetThreshDetQuit100reps.Rdata")
entropy <- unlist(groups_entropy, recursive = FALSE)
entropy <- do.call("rbind", entropy)  %>% 
  mutate(set = paste(n, replicate, sep = "-"))%>% 
  select(-Dsym, -Dyx) %>% 
  filter(n != 1)
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2) %>% 
  mutate(set = paste(n, replicate, sep = "-"))%>% 
  select(-Task1, -Task2)
taskSpec <- groups_specialization %>% 
  group_by(n, replicate) %>% 
  summarise(SpecMean = mean(TransSpec))  %>% 
  mutate(set = paste(n, replicate, sep = "-")) 
metrics <- merge(taskCorrTot, entropy)
metrics <- merge(metrics, taskSpec)
# Summarise
metrics <- metrics %>% 
  melt(., id.vars = c("n", "replicate", "set"))
names(metrics) <- c("n", "replicate", "set", "metric", "value")
metrics <- metrics %>% 
  group_by(n, metric) %>% 
  summarise(Mean = mean(value, na.rm = T),
            SE = sd(value, na.rm = T) / sqrt(length(value))) %>% 
  mutate(Source = "Prob. Updating")
metrics_all <- rbind(metrics_all, metrics)


# Threshold Variation
load("output/__RData/MSrevision_FixedDelta06_DetThreshWithSigmaDetUpdateDetQuit100reps.Rdata")
entropy <- unlist(groups_entropy, recursive = FALSE)
entropy <- do.call("rbind", entropy)  %>% 
  mutate(set = paste(n, replicate, sep = "-"))%>% 
  select(-Dsym, -Dyx) %>% 
  filter(n != 1)
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2) %>% 
  mutate(set = paste(n, replicate, sep = "-"))%>% 
  select(-Task1, -Task2)
taskSpec <- groups_specialization %>% 
  group_by(n, replicate) %>% 
  summarise(SpecMean = mean(TransSpec))  %>% 
  mutate(set = paste(n, replicate, sep = "-")) 
metrics <- merge(taskCorrTot, entropy)
metrics <- merge(metrics, taskSpec)
# Summarise
metrics <- metrics %>% 
  melt(., id.vars = c("n", "replicate", "set"))
names(metrics) <- c("n", "replicate", "set", "metric", "value")
metrics <- metrics %>% 
  group_by(n, metric) %>% 
  summarise(Mean = mean(value, na.rm = T),
            SE = sd(value, na.rm = T) / sqrt(length(value))) %>% 
  mutate(Source = "Thresh. Variation")
metrics_all <- rbind(metrics_all, metrics)


# Original model
load("output/__RData/FixedDelta06Sigma01Eta7100reps.Rdata")
entropy <- unlist(groups_entropy, recursive = FALSE)
entropy <- do.call("rbind", entropy)  %>% 
  mutate(set = paste(n, replicate, sep = "-"))%>% 
  select(-Dsym, -Dyx) %>% 
  filter(n != 1)
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2) %>% 
  mutate(set = paste(n, replicate, sep = "-"))%>% 
  select(-Task1, -Task2)
taskSpec <- groups_specialization %>% 
  group_by(n, replicate) %>% 
  summarise(SpecMean = mean(TransSpec))  %>% 
  mutate(set = paste(n, replicate, sep = "-")) 
metrics <- merge(taskCorrTot, entropy)
metrics <- merge(metrics, taskSpec)
# Summarise
metrics <- metrics %>% 
  melt(., id.vars = c("n", "replicate", "set"))
names(metrics) <- c("n", "replicate", "set", "metric", "value")
metrics <- metrics %>% 
  group_by(n, metric) %>% 
  summarise(Mean = mean(value, na.rm = T),
            SE = sd(value, na.rm = T) / sqrt(length(value))) %>% 
  mutate(Source = "All (Original)")
metrics_all <- rbind(metrics_all, metrics)



# Plot all
metrics_all$Source <- factor(metrics_all$Source, levels = c("Deterministic",
                                                            "Prob. Quitting",
                                                            "Prob. Thresholds",
                                                            "Prob. Updating",
                                                            "Thresh. Variation",
                                                            "All (Original)"))
plot_all <- metrics_all %>% filter(metric == "Dxy")

gg_models <- ggplot(data = plot_all) +
  geom_line(aes(x = n, y = Mean, colour = Source),
            position = position_dodge(width = 1)) +
  geom_errorbar(aes(x = n, ymin = Mean - SE, ymax = Mean + SE, colour = Source),
                width = 1.5,
                position = position_dodge(width = 1)) +
  geom_point(aes(x = n, y = Mean, colour = Source),
             size = 2,
             position = position_dodge(width = 1)) +
  theme_classic() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = unique(plot_all$n)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) +
  xlab("Group Size") +
  ylab("DOL Entropy") +
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
