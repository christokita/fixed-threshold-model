################################################################################
#
# Comparing various specialization plots
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3A_PrepPlotExperimentData.R")
library(RColorBrewer)
library(scales)

load("output/SpecializationMetrics/Rdata/FixedDelta06Sigma01Eta7100reps.Rdata")

####################
# Compare entropies
####################
# Unlist
entropy <- unlist(groups_entropy, recursive = FALSE)
entropy <- do.call("rbind", entropy)

# Summarise




####################
# Task Rank Correlation
####################
# Unlist
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2)

####################
# Load in larger groups and bind
####################
load("output/SpecializationMetrics/Rdata/FixedDelta06Sigma01Eta7LargerSizes100reps.Rdata")

# Unlist
entropy1 <- unlist(groups_entropy, recursive = FALSE)
entropy1 <- do.call("rbind", entropy1)
entropy <- rbind(entropy, entropy1)

# Unlist
taskCorrTot1 <- do.call("rbind", groups_taskCorr)
taskCorrTot1 <- taskCorrTot1 %>% 
  mutate(TaskMean = (Task1 + Task2) / 2)
taskCorrTot <- rbind(taskCorrTot, taskCorrTot1)

####################
# Scatterplot
####################
# Speclialization vs Entropy at colony level
entropy <- entropy %>% 
  mutate(colony = paste0(n, "-", replicate)) %>% 
  group_by(colony) %>% 
  summarize(Dsym = mean(Dsym),
            Dyx = mean(Dyx),
            Dxy = mean(Dxy))
taskEntrCorr <- taskCorrTot %>% 
  mutate(colony = paste0(n, "-", replicate)) %>% 
  merge(entropy) %>% 
  select(colony, n, replicate, TaskMean, Dxy) %>% 
  mutate(groupsize = factor(paste0("n = ", n), 
                            levels = c("n = 2", "n = 4", "n = 6", "n = 8", "n = 12", "n = 16", "n = 32", "n = 100"))) %>% 
  filter(n %in% c(2, 16, 32, 100))

palette <- c("#F00924", "#4C0E78", "#bdbdbd", "#525252")
gg_entrcorr <- ggplot(data = taskEntrCorr, aes(x = Dxy, y = TaskMean, col = groupsize)) +
  geom_hline(data = taskCorrTot, 
             aes(yintercept = 0),
             colour = "grey30",
             size = 0.25) +
  geom_point(alpha = 0.5,
             size = 0.2) +
  theme_bw() +
  xlab("Task Entropy") +
  ylab("Rank Correlation") +
  scale_colour_manual(name = "Group Size", 
                      values = palette) +
  scale_x_continuous(limits = c(0, 0.4),
                     breaks = seq(0, 0.4, 0.2)) +
  scale_y_continuous(limits = c(-0.5, 1)) +
  theme(legend.position = "none", 
        legend.title = element_text(size = 7),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1),
        panel.grid = element_blank(),
        strip.text = element_text(size = 7, face = "italic"),
        strip.background = element_rect(fill = NA, colour = NA),
        panel.spacing = unit(0.5, "cm")) +
  facet_grid(.~ groupsize) 

gg_entrcorr

ggsave(filename = "output/SpecializationMetrics/Plots/EntropyVsCorrelationLargerSizes.png", width = 6, height = 2, units = "in", dpi = 600)

# ggsave(filename = "output/SpecializationMetrics/Plots/EntropyVsCorrelationFacet.png", width = 12, height = 2, units = "in", dpi = 300)


# Speclialization vs Correlation at colony level
# taskCorrSpec <- merge(taskCorrTot, taskSpec)
# taskCorrSpec <- taskCorrSpec %>% 
#   mutate(colony = paste0(n, "-", replicate))
# 
# palette <- c("#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78", "#bdbdbd", "#525252")
# 
# gg_corrspec <- ggplot(data = taskCorrSpec, aes (x = SpecMean, y = TaskMean, col = as.factor(n))) +
#   geom_hline(aes(yintercept = 0), 
#              colour = "grey30") +
#   geom_vline(aes(xintercept = 0), 
#              colour = "grey30") +
#   geom_point(alpha = 0.5,
#              size = 0.2) +
#   theme_bw() +
#   xlab("Task Consistency") +
#   ylab("Rank Correlation") +
#   scale_colour_manual(name = "Group Size", 
#                       values = palette) +
#   scale_x_continuous(limits = c(-0.2, 1)) +
#   scale_y_continuous(limits = c(-0.2, 1)) +
#   theme(legend.position = "right", 
#         legend.title = element_text(size = 7),
#         legend.key.height = unit(0.3, "cm"),
#         legend.key.width= unit(0.4, "cm"),
#         legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
#         legend.text = element_text(size = 6),
#         axis.text = element_text(size = 8),
#         axis.title = element_text(size = 10),
#         axis.ticks = element_line(size = 0.5),
#         panel.border = element_rect(fill = NA, size = 1),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(0.5, "cm")) 
# 
# gg_corrspec
# 
# ggsave(filename = "output/SpecializationMetrics/Plots/SpecializationVsCorrelation.png", width = 3, height = 2, units = "in", dpi = 300)
# 

# Speclialization vs Correlation at inidividual level
taskCorrSpec <- merge(groups_specialization, taskCorrTot)
taskCorrSpec <- taskCorrSpec %>% 
  mutate(colony = paste0(n, "-", replicate),
         groupsize = factor(paste0("n = ", n), 
                            levels = c("n = 2", "n = 4", "n = 6", "n = 8", "n = 12", "n = 16")))

gg_corrspec <- ggplot(data = taskCorrSpec, aes (x = TransSpec, y = TaskMean, col = groupsize)) +
  geom_hline(aes(yintercept = 0), 
             colour = "grey30",
             size = 0.25) +
  geom_vline(aes(xintercept = 0), 
             colour = "grey30",
             size = 0.25) +
  geom_point(alpha = 0.5,
             size = 0.2) +
  theme_bw() +
  scale_colour_manual(name = "Group Size", 
                      values = palette) +
  scale_x_continuous(limits = c(-0.4, 1)) +
  scale_y_continuous(limits = c(-0.4, 1)) +
  theme(legend.position = "none") +
  xlab("Task Consistency") +
  ylab("Rank Correlation") +
  theme(legend.position = "none", 
        legend.title = element_text(size = 7),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 12),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1),
        panel.grid = element_blank(),
        strip.text = element_text(size = 7, face = "italic"),
        strip.background = element_rect(fill = NA, colour = NA),
        panel.spacing = unit(0.5, "cm")) +
  facet_wrap(~ groupsize) 

gg_corrspec

ggsave(filename = "output/SpecializationMetrics/Plots/SpecializationVsCorrelationFacet.png", width = 4, height = 3, units = "in", dpi = 600)


# Speclialization vs Total Activity at inidividual level
taskDist <- unlist(groups_taskDist, recursive = FALSE)
taskDistTot <- do.call("rbind", taskDist)
taskDistSpec <- taskDistTot %>% 
  mutate(Active = Task1 + Task2) %>% 
  merge(groups_specialization) %>% 
  filter(n > 1) %>% 
  mutate(groupsize = factor(paste0("n = ", n), 
                             levels = c("n = 2", "n = 4", "n = 6", "n = 8", "n = 12", "n = 16")))

gg_actspec <- ggplot(data = taskDistSpec, aes(x = Active, y = TransSpec, colour = groupsize)) +
  geom_hline(aes(yintercept = 0), 
             colour = "grey30",
             size = 0.25) +
  geom_point(alpha = 0.5, 
             size = 0.2) +
  theme_bw() +
  scale_colour_manual(name = "Group Size", 
                      values = palette) +
  scale_y_continuous(limits = c(-0.2, 1)) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  theme(legend.position = "none") +
  xlab("Activity Level") +
  ylab("Task Consistency") +
  theme(legend.position = "none", 
        legend.title = element_text(size = 7),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 12),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1),
        panel.grid = element_blank(),
        strip.text = element_text(size = 7, face = "italic"),
        strip.background = element_rect(fill = NA, colour = NA),
        panel.spacing = unit(0.5, "cm")) +
  facet_wrap(~ groupsize)
  
gg_actspec

ggsave(filename = "output/SpecializationMetrics/Plots/SpecializationVsActivity.png", width = 4, height = 3, units = "in", dpi = 600)


myPalette <- colorRampPalette(brewer.pal(9, "Blues"))

gg_taskspec <- ggplot(data = taskDistSpec, aes(x = Task1, y = Task2, colour = TransSpec)) +
  geom_point(alpha = 0.5,
             size = 0.2) +
  theme_bw() +
  scale_color_gradientn(name = "Task\nConsistency",
                        colours = myPalette(5), values = c(0, 0.1, 0.3, 0.5, 1), oob = squish) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5)) +
  theme(legend.position = "none") +
  xlab("Task 1 Activity") +
  ylab("Task 2 Activity") +
  theme(legend.position = "right", 
        legend.title = element_text(size = 7),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 12),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1),
        panel.grid = element_blank(),
        strip.text = element_text(size = 7, face = "italic"),
        strip.background = element_rect(fill = NA, colour = NA),
        panel.spacing = unit(0.5, "cm")) +
  facet_wrap(~ groupsize)

gg_taskspec

ggsave(filename = "output/SpecializationMetrics/Plots/TasksVsSpecialization.png", width = 4.5, height = 3, units = "in", dpi = 600)



####################
# Summary plots
####################
# Correlation vs Entropy at colony level
taskEntrCorr <- taskCorrTot %>% 
  mutate(colony = paste0(n, "-", replicate)) %>% 
  merge(entropy) %>% 
  select(colony, n, replicate, TaskMean, Dxy) %>% 
  melt(id.vars = c("colony", "n", "replicate"))
names(taskEntrCorr) <- c("colony", "n", "replicate", "metric", "value")
taskEntrCorr <- taskEntrCorr %>% 
  group_by(n, metric) %>% 
  summarise(Mean = mean(value),
            SE   = sd(value) / sqrt(length(value)))


gg_meanCorrEntr <- ggplot(data = taskEntrCorr, aes(x = n, group = metric)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 1.5,
                position = position_dodge(width = 1)) +
  geom_line(aes(y = Mean, linetype = metric),
            position = position_dodge(width = 1)) +
  geom_point(aes(y = Mean, fill = metric), 
             size = 1.5,
             shape = 21,
             position = position_dodge(width = 1)) +
  theme_classic() +
  xlab("Group Size") +
  ylab("Value") +
  scale_y_continuous(#limits = c(0, 0.65),
                     breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = unique(taskEntrCorr$n)) +
  scale_fill_manual(name = "Metric",
                    values = c("black", "white"),
                    labels = c("Rank Correlation",
                               "DOL Entropy")) +
  scale_linetype_discrete(name = "Metric",
                          labels = c("Rank Correlation",
                                     "DOL Entropy")) +
  theme(legend.position = "right", 
        legend.title = element_text(size = 7, face = "bold"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.ticks = element_line(size = 0.5))

gg_meanCorrEntr

ggsave(plot = gg_meanCorrEntr, filename = "output/SpecializationMetrics/Plots/EntropyCorrMeans.png", width = 3, height = 2, units = "in", dpi = 600)


# Correlation vs Specialization at colony level
taskCorrSpec <- merge(taskCorrTot, taskSpec)
taskCorrSpec <- taskCorrSpec %>%
  select(-Task1, -Task2) %>% 
  melt(id.vars = c("replicate", "n"))
names(taskCorrSpec) <- c("replicate", "n", "metric", "value")
taskCorrSpec <- taskCorrSpec %>% 
  group_by(n, metric) %>% 
  summarise(Mean = mean(value),
            SE   = sd(value) / sqrt(length(value)))

gg_meanCorrSpec <- ggplot(data = taskCorrSpec, aes(x = n, group = metric)) +
  geom_hline(aes(yintercept = 0), 
             colour = "grey30",
             size = 0.25) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 1.5,
                position = position_dodge(width = 1)) +
  geom_line(aes(y = Mean, linetype = metric),
            position = position_dodge(width = 1)) +
  geom_point(aes(y = Mean, fill = metric), 
             size = 1.5,
             shape = 21,
             position = position_dodge(width = 1)) +
  theme_classic() +
  xlab("Group Size") +
  ylab("Value") +
  scale_y_continuous(limits = c(-0.05, 0.65),
                     breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = unique(taskEntrCorr$n)) +
  scale_fill_manual(name = "Metric",
                    values = c("black", "white"),
                    labels = c("Rank Correlation",
                               "Task Consistency")) +
  scale_linetype_manual(name = "Metric",
                        values = c("solid", "dotted"),
                          labels = c("Rank Correlation",
                                     "Task Consistency")) +
  theme(legend.position = "right", 
        legend.title = element_text(size = 7, face = "bold"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.ticks = element_line(size = 0.5))

gg_meanCorrSpec

ggsave(plot = gg_meanCorrSpec, filename = "output/SpecializationMetrics/Plots/SpecCorrMeans.png", width = 3, height = 2, units = "in", dpi = 600)

