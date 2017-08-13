################################################################################
#
# Plot figures for manuscripts
#
################################################################################

####################
# Parameter Space Exploration
####################
rm(list = ls())
source("scripts/__Util__MASTER.R")
library(RColorBrewer)
library(scales)

# load
load("output/ParameterExploration/Rdata/FixedDelta06_SigmaSlopeExploration.Rdata")
improve <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3)


##### Absolute Slope #####
# Fit surface
spec.loess <- loess(Increase ~ sigma * threshSlope, data = improve, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(sigma = seq(0, max(improve$sigma), (max(improve$sigma) - min(improve$sigma)) / 1000), 
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)

spec.fit$spec <- as.numeric(z)

# start building large dataframe for calculating total fit
totalfit <- spec.fit[ , 1:2]
totalfit$slope.fit <- spec.fit$spec
# Find which z are within range 0.95 - 1.05? (relative to data)

# Try custom color
# colPal <- c("#810f7c", "#8856a7", "#8c96c6", "#b3cde3", "#ffffff")
myPalette <- colorRampPalette(brewer.pal(6, "YlOrRd"))
colPal <- c(myPalette(6), "#800026")

# Graph 
gg_abslope <- ggplot(spec.fit, aes(x = sigma, y = threshSlope, fill = spec)) +
  geom_tile() +
  stat_contour(aes(z = spec), 
               size = 0.25,
               alpha = 1,
               colour = "white",
               breaks = c(0.2926124,  0.3576374)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.005, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradientn(name = "Specialization Increase",
                       colors = colPal,
                       breaks = seq(0, 0.5, 0.1),
                       colours = colPal,
                       limits = c(0, 0.5),
                       oob = squish) +
  xlab(expression(sigma)) +
  ylab(expression(eta)) +
  theme(legend.position = "none", 
        # legend.key.height = unit(0.84, "cm"),
        legend.key.height = unit(0.17, "npc"),
        legend.key.width= unit(0.2, "cm"),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 6, margin = margin(5, 2, 5, -2)),
        axis.text.x = element_text(size = 6, margin = margin(2, 5, -2, 5)),
        axis.title = element_text(size = 9),
        axis.ticks.length = unit(0, "cm"),
        panel.border = element_rect(fill = "NA", size = 1))

ggsave("output/MSFigures/ParameterSpaceDelta06.png", width = 2.7, height = 2, units = "in", dpi = 600)



####################
# Specialization Plots - Fixed Probabilistic
####################
rm(list = ls())
source("scripts/__Util__MASTER.R")
library(RColorBrewer)
library(scales)
library(ggthemes)

# Load and prep experimental data
source("scripts/3A_PrepPlotExperimentData.R")
yukoCorr <- yukoCorr %>% 
  mutate(Sigma = NA)

# Load and prep fixed probabilistic sigma = 0.1
load("output/__RData/FixedDelta06Sigma01Eta7100reps.Rdata")

taskCorrTot <- do.call("rbind", groups_taskCorr)
fixedprob_01 <-  taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2) %>% 
  mutate(Sigma = 0.1, 
         Source = "Model") %>% 
  select(n, TaskMean, Source, Sigma) 

# Load and prep fixed probabilistic sigma = 0.15
load("output/__RData/Fixed_Delta06Sigma005Eta25.Rdata")

taskCorrTot <- do.call("rbind", groups_taskCorr)
fixedprob_005 <-  taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2) %>% 
  mutate(Sigma = 0.05, 
         Source = "Model") %>% 
  select(n, TaskMean, Source, Sigma)

# Load and prep fixed probabilistic sigma = 0.02
load("output/__RData/Fixed_Delta06Sigma002Eta7.Rdata")

taskCorrTot <- do.call("rbind", groups_taskCorr)
fixedprob_002 <-  taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2) %>% 
  mutate(Sigma = 0.02, 
         Source = "Model") %>% 
  select(n, TaskMean, Source, Sigma)

# Load and prep fixed probabilistic sigma = 0.3, eta = 2
load("output/__RData/Fixed_Delta06Sigma03.Rdata")

taskCorrTot <- do.call("rbind", groups_taskCorr)
fixedprob_03 <-  taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2) %>% 
  mutate(Sigma = 0.3, 
         Source = "Model") %>% 
  select(n, TaskMean, Source, Sigma)

# Bind into large dataframe
allFixedProbCorr <- fixedprob_01 %>% 
  rbind(fixedprob_002) %>% 
  rbind(fixedprob_005) %>%
  rbind(fixedprob_03) %>% 
  rbind(yukoCorr) %>% 
  mutate(Source = as.factor(Source)) %>% 
  group_by(Source, n, Sigma) %>% 
  summarise(SpecMean = mean(TaskMean),
            SpecSE = sd(TaskMean) / sqrt(length(TaskMean)),
            SpecCI = 1.96 * SpecSE) %>% 
  mutate(Set = paste0(Source, Sigma)) %>% 
  mutate(Set = factor(Set, levels = c("ExperimentNA", "Model0.02", "Model0.3", "Model0.1", "Model0.05"))) 

# Get increase in specialization


# Set pallete
fixedProbpalette <- c("grey45", "#F9D76E", "#FD792C", "#F23619", "#97031B")
fillPalette <- c("#ffffff","#F9D76E", "#FD792C", "#F23619", "#97031B")

# Plot with experimental data
gg_fixedProb <- ggplot(data = allFixedProbCorr) +
  theme_classic() +
  labs(x = "Group Size",
       y = "Specialization") +
  scale_x_continuous(breaks = unique(taskCorrTot$n)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.1), 
                     limits = c(0, 0.85),
                     expand = c(0, 0)) +
  scale_colour_manual(values = fixedProbpalette, 
                      labels = c("Experiment", 
                                 expression(paste(sigma, " = 0.02, ", eta, " = 7")),
                                 expression(paste(sigma, " = 0.1, ", eta, " = 7")),
                                 expression(paste(sigma, " = 0.3, ", eta, " = 2")),
                                 expression(paste(sigma, " = 0.05, ", eta, " = 25")))) +
  scale_fill_manual(values = fillPalette,
                    labels = c("Experiment", 
                               expression(paste(sigma, " = 0.02, ", eta, " = 7")),
                               expression(paste(sigma, " = 0.1, ", eta, " = 7")),
                               expression(paste(sigma, " = 0.3, ", eta, " = 2")),
                               expression(paste(sigma, " = 0.05, ", eta, " = 25")))) +
  scale_shape_manual(values = c(21, 22, 21, 25, 24),
                     labels = c("Experiment", 
                                expression(paste(sigma, " = 0.02, ", eta, " = 7")),
                                expression(paste(sigma, " = 0.1, ", eta, " = 7")),
                                expression(paste(sigma, " = 0.3, ", eta, " = 2")),
                                expression(paste(sigma, " = 0.05, ", eta, " = 25")))) +
  # Mean and SE portion of plot
  geom_errorbar(aes(x = n, ymin = SpecMean - SpecSE, ymax = SpecMean + SpecSE, colour = Set, width = 1.5),
                position = position_dodge(width = 0.5),
                size = 0.25) +
  geom_line(aes(x = n, y = SpecMean,  colour = Set),
            size = 0.3,
            position = position_dodge(width = 0.5)) +
  geom_point(aes(x = n, y = SpecMean, colour = Set, fill = Set, shape = Set),
             position = position_dodge(width = 0.5),
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
        axis.text.y = element_text(size = 6, margin = margin(5, 6, 5, -2)),
        axis.text.x = element_text(size = 6, margin = margin(6, 5, -2, 5)),
        axis.title = element_text(size = 6, margin = margin(0, 0, 0, 0)),
        axis.ticks.length = unit(-0.1, "cm"))

# svg("output/MSFigures/FixedProbSpecializationFits.svg", width = 2.65, height = 2.05)
svg("output/MSFigures/FixedProbSpecializationFits.svg", width = 2.71, height = 2.05)
gg_fixedProb
dev.off()

####################
# Sample stimuli over time
####################
rm(list = ls())
source("scripts/__Util__MASTER.R")
library(RColorBrewer)
library(scales)

# load data
load("output/__RData/FixedDelta06Sigma01Eta7100reps.Rdata")

# Unlist
stims <- unlist(groups_stim, recursive = FALSE)
stims <- do.call("rbind", stims)

# Select out example colonies
stimSet <- stims %>% 
  filter(n %in% c(2, 16)) %>% 
  filter(replicate == 1) %>% 
  group_by(n) %>% 
  mutate(timestep = 0:(length(n)-1),
         groupsize = factor(paste0("n = ", n), 
                            levels = c("n = 2", "n = 16")))

# Plot
gg_stimEx <- ggplot(data = stimSet, aes(x = timestep, y = s1)) +
  geom_line(size = 0.2, colour = "#4eb3d3") +
  theme_classic() +
  xlab("Timestep") +
  ylab("Stimulus") +
  scale_x_continuous(breaks = seq(0, 10000, 2500),
                     limits = c(0, 10500),
                     expand = c(0, 0),
                     labels = comma) +
  scale_y_continuous(breaks = seq(0, 20, 5), 
                     limits = c(0, 16),
                     expand = c(0, 0)) +
  theme(axis.text.y = element_text(size = 6, margin = margin(5, 6, 5, -2)),
        axis.text.x = element_text(size = 6, margin = margin(6, 5, -2, 5)),
        axis.title = element_text(size = 6),
        axis.ticks.length = unit(-0.1, "cm"),
        strip.text = element_blank(),
        strip.background = element_rect(fill = NA, colour = NA),
        panel.spacing = unit(0.25, "cm")) +
  facet_wrap(~ groupsize, ncol = 1, scale = "free")

svg("output/MSFigures/ExampleStimulusOverTime.svg", width = 2.66, height = 2.05)
gg_stimEx
dev.off()


####################
# Rank Correlation vs Task Consistency
####################
rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3A_PrepPlotExperimentData.R")
library(RColorBrewer)
library(scales)

load("output/SpecializationMetrics/Rdata/FixedDelta06Sigma01Eta7100reps.Rdata")


# Unlist
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2)

taskSpec <- groups_specialization %>% 
  group_by(n, replicate) %>% 
  summarise(SpecMean = mean(TransSpec))


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
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                size = 0.25,
                width = 0.65,
                position = position_dodge(width = 1)) +
  geom_line(aes(y = Mean, linetype = metric),
            position = position_dodge(width = 1),
            size = 0.3) +
  geom_point(aes(y = Mean, fill = metric), 
             size = 1,
             shape = 21,
             position = position_dodge(width = 1)) +
  theme_classic() +
  ylab("Value") +
  xlab("Group Size")
scale_y_continuous(limits = c(-0.1, 0.7),
                   breaks = seq(-0.1, 1, 0.1),
                   expand = c(0, 0)) +
  scale_x_continuous(breaks = unique(taskCorrSpec$n)) +
  scale_fill_manual(name = "Metric",
                    values = c("black", "white"),
                    labels = c("Rank Correlation",
                               "Task Consistency")) +
  scale_linetype_manual(name = "Metric",
                        values = c("solid", "dashed"),
                        labels = c("Rank Correlation",
                                   "Task Consistency")) +
  theme(legend.position = c(0.5, 0.5), 
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.5, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size = 6, margin = margin(5, 6, 5, -2)),
        axis.text.x = element_text(size = 6, margin = margin(6, 5, -2, 5)),
        axis.title = element_text(size = 6),
        axis.ticks.length = unit(-0.1, "cm"))


svg("output/MSFigures/CorrVsConsistency.svg", width = 2.71, height = 2.0275)
gg_meanCorrSpec
dev.off()


gg_meanCorrSpecSide <- ggplot(data = taskCorrSpec, aes(x = n, group = metric)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                size = 0.25,
                width = 0.65,
                position = position_dodge(width = 1)) +
  geom_line(aes(y = Mean, linetype = metric),
            position = position_dodge(width = 1),
            size = 0.3) +
  geom_point(aes(y = Mean, fill = metric), 
             size = 1.5,
             shape = 21,
             position = position_dodge(width = 1)) +
  theme_classic() +
  ylab("Value") +
  xlab("Group Size") +
  scale_y_continuous(#limits = c(-0.1, 0.7),
    breaks = seq(-0.1, 1, 0.1),
    expand = c(0.1, 0)) +
  scale_x_continuous(breaks = unique(taskCorrSpec$n)) +
  scale_fill_manual(name = "Metric",
                    values = c("black", "white"),
                    labels = c("Rank Correlation",
                               "Task Consistency")) +
  scale_linetype_manual(name = "Metric",
                        values = c("solid", "dashed"),
                        labels = c("Rank Correlation",
                                   "Task Consistency")) +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.5, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size = 6, margin = margin(5, 6, 5, -2)),
        axis.text.x = element_text(size = 6, margin = margin(6, 5, -2, 5)),
        axis.title = element_text(size = 6),
        axis.ticks.length = unit(-0.1, "cm"),
        strip.text = element_blank(),
        strip.background = element_rect(fill = NA, colour = NA),
        panel.spacing = unit(0.25, "cm")) +
  facet_wrap(~metric, scale = "free")

gg_meanCorrSpecSide

svg("output/MSFigures/CorrVsConsistencySide.svg", width = 2.62, height = 2.05)
gg_meanCorrSpecSide
dev.off()


