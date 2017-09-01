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


##### Delta 06 #####
# load
load("output/ParameterExploration/Rdata/FixedDelta06_SigmaSlopeExplorationEXTRA.Rdata")
improve1 <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3)

load("output/ParameterExploration/Rdata/FixedDelta06_SigmaSlopeExplorationEXTRA2.Rdata")
improve2 <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3)

load("output/ParameterExploration/Rdata/FixedDelta06_SigmaSlopeExploration.Rdata")
improve <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3)

improve06 <- rbind(improve, improve1, improve2)
rm(improve, improve1, improve2)

# Filter to size
improve06 <- improve06 %>% 
  filter(!sigma %in% c(0.075, 0.125, 0.175, 0.225, 0.275, 0.325)) 


##### Absolute Slope #####
# Colors
myPalette <- colorRampPalette(brewer.pal(6, "YlOrRd"))
colPal <- c(myPalette(6), "#800026")

# Fit surface
spec.loess <- loess(Increase ~ sigma * threshSlope, data = improve06, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(sigma = seq(0, max(improve06$sigma), (max(improve06$sigma) - min(improve06$sigma)) / 1000), 
                             threshSlope = seq(1, max(improve06$threshSlope), (max(improve06$threshSlope) - min(improve06$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)
spec.fit$spec <- as.numeric(z)
spec.fit$CloseIncrease <- ifelse(spec.fit$spec >= 0.2926124 & spec.fit$spec <= 0.3576374, 0.5, 0) 

# Graph 
gg_abslope <- ggplot() +
  geom_raster(data = improve06, 
              aes(x = sigma, 
                  y = threshSlope, 
                  fill = Increase)) +
  stat_contour(data = spec.fit,
               aes(x = sigma,
                   y = threshSlope,
                   z = spec),
               size = 0.35,
               colour = "white",
               breaks = c(0.2926124)) +
  stat_contour(data = spec.fit,
               aes(x = sigma,
                   y = threshSlope,
                   z = spec),
               size = 0.35,
               colour = "white",
               linetype = "dashed",
               breaks = c(0.3576374)) +
  theme_bw() +
  scale_x_continuous(expand = c(0, -0.002)) +
  scale_y_continuous(expand = c(0, -0.2), breaks = c(1, seq(10, 30, 10))) +
  scale_fill_gradientn(name = "Specialization\nIncrease",
                       colors = colPal,
                       breaks = seq(0, 0.5, 0.1),
                       colours = colPal,
                       limits = c(0, 0.5),
                       oob = squish) +
  scale_colour_gradientn(name = "Specialization\nIncrease",
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

gg_abslope

ggsave("output/MSFigures/ParameterSpaceDelta06wContourfill.png", width = 2.7, height = 2, units = "in", dpi = 600)



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

# Load and prep fixed probabilistic sigma = 0.03
load("output/__RData/Fixed_Delta06Sigma003Eta3.Rdata")

taskCorrTot <- do.call("rbind", groups_taskCorr)
fixedprob_003 <-  taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2) %>% 
  mutate(Sigma = 0.03, 
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
  rbind(fixedprob_003) %>% 
  rbind(fixedprob_005) %>%
  rbind(fixedprob_03) %>% 
  rbind(yukoCorr) %>% 
  mutate(Source = as.factor(Source)) %>% 
  group_by(Source, n, Sigma) %>% 
  summarise(SpecMean = mean(TaskMean),
            SpecSE = sd(TaskMean) / sqrt(length(TaskMean)),
            SpecCI = 1.96 * SpecSE) %>% 
  mutate(Set = paste0(Source, Sigma)) %>% 
  mutate(Set = factor(Set, levels = c("ExperimentNA", "Model0.03", "Model0.3", "Model0.1", "Model0.05"))) 

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
                                 expression(paste(sigma, " = 0.03, ", eta, " = 3")),
                                 expression(paste(sigma, " = 0.3, ", eta, " = 2")),
                                 expression(paste(sigma, " = 0.1, ", eta, " = 7")),
                                 expression(paste(sigma, " = 0.05, ", eta, " = 25")))) +
  scale_fill_manual(values = fillPalette,
                    labels = c("Experiment", 
                               expression(paste(sigma, " = 0.03, ", eta, " = 3")),
                               expression(paste(sigma, " = 0.3, ", eta, " = 2")),
                               expression(paste(sigma, " = 0.1, ", eta, " = 7")),
                               expression(paste(sigma, " = 0.05, ", eta, " = 25")))) +
  scale_shape_manual(values = c(21, 22, 25, 21, 24),
                     labels = c("Experiment", 
                                expression(paste(sigma, " = 0.03, ", eta, " = 3")),
                                expression(paste(sigma, " = 0.3, ", eta, " = 2")),
                                expression(paste(sigma, " = 0.1, ", eta, " = 7")),
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
# rm(list = ls())
# source("scripts/__Util__MASTER.R")
# library(RColorBrewer)
# library(scales)
# 
# # load data
# load("output/__RData/FixedDelta06Sigma01Eta7100reps.Rdata")
# 
# # Unlist
# stims <- unlist(groups_stim, recursive = FALSE)
# stims <- do.call("rbind", stims)
# 
# # Select out example colonies
# stimSet <- stims %>% 
#   filter(n %in% c(2, 16)) %>% 
#   filter(replicate == 1) %>% 
#   group_by(n) %>% 
#   mutate(timestep = 0:(length(n)-1),
#          groupsize = factor(paste0("n = ", n), 
#                             levels = c("n = 2", "n = 16")))
# 
# # Plot
# gg_stimEx <- ggplot(data = stimSet, aes(x = timestep, y = s1)) +
#   geom_line(size = 0.2, colour = "#4eb3d3") +
#   theme_classic() +
#   xlab("Timestep") +
#   ylab("Stimulus") +
#   scale_x_continuous(breaks = seq(0, 10000, 2500),
#                      limits = c(0, 10500),
#                      expand = c(0, 0),
#                      labels = comma) +
#   scale_y_continuous(breaks = seq(0, 20, 5), 
#                      limits = c(0, 16),
#                      expand = c(0, 0)) +
#   theme(axis.text.y = element_text(size = 6, margin = margin(5, 6, 5, -2)),
#         axis.text.x = element_text(size = 6, margin = margin(6, 5, -2, 5)),
#         axis.title = element_text(size = 6),
#         axis.ticks.length = unit(-0.1, "cm"),
#         strip.text = element_blank(),
#         strip.background = element_rect(fill = NA, colour = NA),
#         panel.spacing = unit(0.25, "cm")) +
#   facet_wrap(~ groupsize, ncol = 1, scale = "free")
# 
# svg("output/MSFigures/ExampleStimulusOverTime.svg", width = 2.66, height = 2.05)
# gg_stimEx
# dev.off()


####################
# Stimulus Fluctuation
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

#### Time steps ####
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




#### Variance ####
# Normalize and Summarise by "day" (i.e., time window)
stimFluct <- stims %>% 
  select(-delta1, -delta2) %>% 
  mutate(Set = paste0(n, "-", replicate)) %>% 
  group_by(n, Set) %>% 
  summarise(s1mean = mean(s1),
            s1Fluct = var(s1),
            s2mean = mean(s2),
            s2Fluct = var(s2)) %>% 
  mutate(GroupSizeFactor = n) 

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
palette <- c("#83343E", "#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78")


gg_stimfluct <- ggplot() +
  geom_point(data = stimFluct, 
             aes(x = n, y = s1Fluct),
             fill = "grey50", 
             colour = "grey50", 
             size = 0.7, 
             alpha = 0.4,
             stroke = 0) +
  geom_line(data = stimSumFluct,
            aes(x = n, y = s1FluctMean),
            size = 0.3) +
  theme_classic() +
  labs(x = "Group Size",
       y = "Variance in Task Stimulus") +
  scale_x_continuous(breaks = unique(stimFluct$n)) +
  scale_y_continuous(breaks = seq(0, 22, 5),
                     limits = c(0, 22),
                     expand = c(0, 0)) +
  scale_fill_manual(values = palette) +
  scale_colour_manual(values = palette) +
  theme(legend.position = "none") +
  # Mean and SE portion of plot
  geom_errorbar(data = stimSumFluct, 
                aes(x = n, 
                    ymin = s1FluctMean - s1FluctSE, 
                    ymax = s1FluctMean + s1FluctSE, 
                    colour = GroupSizeFactor),
                size = 0.25) +
  geom_point(data = stimSumFluct, 
             aes(x = n, y = s1FluctMean, colour = GroupSizeFactor, fill = GroupSizeFactor),
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

gg_stimfluct

ggsave("output/MSFigures/StimulusFluctuations.png", width = 2.82, height = 2.05, units = "in", dpi = 600)


####################
# Task Performance Fluctuation
####################
load("output/SpecializationMetrics/Rdata/FixedDelta06Sigma01Eta7100reps.Rdata")

# Unlist
tallies <- unlist(groups_taskTally, recursive = FALSE)
tallies <- do.call("rbind", tallies)

#### Time steps ####
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
            Task1FluctSE = sd(Task1Fluct, na.rm = TRUE) / sqrt(length(Task1Fluct)),
            Task2FluctMean = mean(Task2Fluct, na.rm = TRUE),
            Task2FluctSE = sd(Task2Fluct, na.rm = TRUE) / sqrt(length(Task2Fluct)),
            InactiveFluctMean = mean(InactiveFluct, na.rm = TRUE),
            InactiveFluctSE = sd(InactiveFluct, na.rm = TRUE) / sqrt(length(InactiveFluct)))
tallySumFluct <- as.data.frame(tallySumFluct)
tallySumFluct <- tallySumFluct %>% 
  mutate(GroupSizeFactor = factor(GroupSizeFactor, levels = sort(unique(n))))


#### Variance ####
# Normalize and Summarise by "day" (i.e., time window)
tallyFluct <- tallies %>% 
  mutate(Task1 = Task1 / n,
         Task2 = Task2 / n,
         Inactive = Inactive / n,
         Set = paste0(n, "-", replicate)) %>% 
  group_by(n, Set) %>% 
  summarise(task1mean = mean(Task1),
            task1var = var(Task1),
            task2mean = mean(Task2),
            task2var = var(Task2)) %>% 
  mutate(GroupSizeFactor = n) %>% 
  mutate(Task1Fluct = task1var)

# Summarise by n
tallySumFluct <- tallyFluct %>% 
  group_by(n, GroupSizeFactor) %>% 
  summarise(Task1FluctMean = mean(Task1Fluct, na.rm = TRUE),
            Task1FluctSE = sd(Task1Fluct, na.rm = TRUE) / sqrt(length(Task1Fluct)))
tallySumFluct <- as.data.frame(tallySumFluct)
tallySumFluct <- tallySumFluct %>% 
  mutate(GroupSizeFactor = factor(GroupSizeFactor, levels = sort(unique(n))))


# Plot
palette <- c("#83343E", "#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78")


gg_fluct <- ggplot() +
  geom_point(data = tallyFluct, 
             aes(x = n, y = Task1Fluct),
             fill = "grey50", 
             colour = "grey50", 
             size = 0.7, 
             position = position_dodge(width = 1),
             alpha = 0.4,
             stroke = 0) +
  theme_classic() +
  labs(x = "Group Size",
       y = "Mean Fluctuation In\nTask Performance") +
  scale_x_continuous(breaks = unique(tallyFluct$n)) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.02),
                     limits = c(0, 0.25),
                     expand = c(0, 0)) +
  scale_fill_manual(values = palette) +
  scale_colour_manual(values = palette) +
  theme(legend.position = "none") +
  # Mean and SE portion of plot
  geom_errorbar(data = tallySumFluct, 
                aes(x = n, 
                    ymin = Task1FluctMean - Task1FluctSE, 
                    ymax = Task1FluctMean + Task1FluctSE, 
                    colour = GroupSizeFactor),
                size = 0.25) +
  geom_point(data = tallySumFluct, 
             aes(x = n, y = Task1FluctMean, colour = GroupSizeFactor, fill = GroupSizeFactor),
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

gg_fluct

ggsave("output/MSFigures/TaskPerformanceFluctuations_VARIANCE.png", width = 2.82, height = 2.05, units = "in", dpi = 600)
svg("output/MSFigures/TaskPerformanceFluctuations.svg", width = 2.82, height = 2.05)
gg_fluct
dev.off()
