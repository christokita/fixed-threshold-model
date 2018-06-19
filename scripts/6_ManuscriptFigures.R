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
# Approximated +/- 10% of experimental data
# Experimental slope: 0.5915000 - 0.2663750 = 0.325125
spec.fit$CloseIncrease <- ifelse(spec.fit$spec >= 0.2926124 & spec.fit$spec <= 0.3576374, 0.5, 0) 

# Graph 
gg_abslope <- ggplot() +
  # geom_raster(data = improve06, 
  #             aes(x = sigma, 
  #                 y = threshSlope, 
  #                 fill = Increase)) +
  geom_tile(data = improve06, 
              aes(x = sigma, 
                  y = threshSlope, 
                  fill = Increase,
                  color = Increase)) +
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
        # legend.key.height = unit(0.5, "cm"),
        legend.key.height = unit(0.195, "npc"),
        legend.key.width= unit(0.2, "cm"),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 6, margin = margin(t = 0, r = 3, b = 0, l = -5), color = "black"),
        axis.text.x = element_text(size = 6, margin = margin(3, 0, -4, 0), color = "black"),
        axis.title = element_text(size = 9, margin = margin(0, 0, 0, 0)),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
        axis.ticks.length = unit(0, "cm"),
        panel.border = element_rect(colour = "black")) 

gg_abslope

ggsave("output/MSFigures/ParameterSpaceDelta06wContour_OneColumn.svg", width = 39, height = 44, units = 'mm')
ggsave("output/MSFigures/ParameterSpaceDelta06wContour.svg", width = 50, height = 38.5, units = 'mm')
ggsave("output/MSFigures/ParameterSpaceDelta06wContour_ThreePerRow.svg", width = 36, height = 33.8, units = 'mm')


####################
# Specialization Plots - Fixed Probabilistic
####################
rm(list = ls())
source("scripts/__Util__MASTER.R")
library(RColorBrewer)
library(scales)
library(ggthemes)

# Load and prep experimental data
source("scripts/3_PrepPlotExperimentData.R")
yukoCorr <- yukoCorr %>% 
  mutate(Sigma = NA)

# Load and prep fixed probabilistic sigma = 0.1
# load("output/__RData/FixedDelta06Sigma01Eta7100reps.Rdata")
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
  labs(x = "Group size",
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
                size = 0.2) +
  geom_line(aes(x = n, y = SpecMean,  colour = Set),
            size = 0.2,
            position = position_dodge(width = 0.5)) +
  geom_point(aes(x = n, y = SpecMean, colour = Set, fill = Set, shape = Set),
             position = position_dodge(width = 0.5),
             size = 1) +
  theme(legend.position = "none",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 6),
        legend.text.align = 0,
        # legend.box.background = element_rect(),
        axis.text.y = element_text(size = 6, margin = margin(0, 5, 0, -4), color = "black"),
        axis.text.x = element_text(size = 6, margin = margin(5, 0, -4, 0), color = "black"),
        axis.title = element_text(size = 7, margin = margin(0, 0, 0, 0)),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
        axis.ticks = element_line(colour = "black", size = 0.3),
        axis.ticks.length = unit(-0.06, "cm"),
        axis.line = element_line(size = 0.3))


ggsave("output/MSFigures/FixedProbSpecializationFits_OneColumn.svg", width = 44.5, height = 45, units = "mm")
ggsave("output/MSFigures/FixedProbSpecializationFits.svg", width = 60, height = 40, units = "mm")
ggsave("output/MSFigures/FixedProbSpecializationFits_ThreePerRow.svg", width = 45, height = 35, units = 'mm')


####################
# Task Distribution 
####################
rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3_PrepPlotExperimentData.R")

load("output/__RData/FixedDelta06Sigma01Eta7100reps.Rdata")

# Set variable  
filename <- "Fixed_Delta06Sigma01Eta7"

# Palette with single individuals
palette <- c("#83343E", "#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78", "#bdbdbd", "#525252")
# palette <- c("#bababa", "#4d4d4d", "#bababa", "#4d4d4d", "#bababa", "#4d4d4d", "#bababa") #for black and white theme


# Bind together
taskDist <- unlist(groups_taskDist, recursive = FALSE)
taskDistTot <- do.call("rbind", taskDist)

# Choose random subset of replicates to show
set.seed(323)
to_show <- sample(1:100, 10, replace = F)

# Manipulate
taskDistTot <- taskDistTot %>% 
  filter(replicate %in% to_show) %>% 
  mutate(set = paste0(n, "-", replicate)) %>% 
  mutate(set = factor(set, 
                      levels = mixedsort(unique(set))),
         n = as.factor(n))

taskSum <- taskDistTot %>% 
  group_by(n) %>% 
  summarise(taskMean1 = mean(Task1),
            taskMean2 = mean(Task2))

# Plot
plot_TaskMat <- as.data.frame(taskDistTot)
gg_dist <- ggplot(data = plot_TaskMat, aes(y = Task1, x = set)) +
  geom_point(aes(colour = n), size = 0.1, stroke = 0.4) +
  theme_classic() +
  labs(x = "Group size",
       y = "Task 1 performance freq.") +
  scale_color_manual(values = palette) +
  scale_y_continuous(limits = c(0, 0.72), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
  theme( axis.text.y = element_text(size = 6, margin = margin(0, 5, 0, -4), color = "black"),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(), 
         axis.title.y = element_text(size = 7, margin = margin(0, 4, 0, 0)),
         axis.title.x = element_text(size = 7, margin = margin(11, 0, 0, 0)),
         axis.ticks.length = unit(-0.06, "cm"),
         axis.ticks = element_line(colour = "black", size = 0.3),
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
         axis.line = element_line(size = 0.3),
         legend.position = "none")

# svg("output/MSFigures/TaskDistExample.svg", width = 2.8, height = 2.07)
ggsave("output/MSFigures/TaskDistExample_OneColumn.svg", width = 45, height = 35, units = "mm")
ggsave("output/MSFigures/TaskDistExample.svg", width = 50.5, height = 30, units = "mm")
ggsave("output/MSFigures/TaskDistExample_threeperrow.svg", width = 45, height = 35, units = "mm")

####################
# Task Neglect 
####################
rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3_PrepPlotExperimentData.R")

load("output/__RData/FixedDelta06Sigma01Eta7100reps.Rdata")

# Frequency of no task being performed
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

# Summarise
neglectSum <- noTaskPerf %>% 
  group_by(n) %>% 
  mutate(noTaskAvg = (noTask1 + noTask2) / 2 ) %>% 
  summarise(Task1NegelectMean = mean(noTask1, na.rm = TRUE) / 10000,
            Task1NegelectSE = ( sd(noTask1) / sqrt(length(noTask1)) ) / 1000,
            Task2NegelectMean = mean(noTask2, na.rm = TRUE) / 10000,
            Task2NegelectSE = ( sd(noTask2) / sqrt(length(noTask2)) ) / 10000,
            TaskNegelectMean = mean(noTaskAvg, na.rm = TRUE) / 10000,
            TaskNegelectSE = ( sd(noTaskAvg) / sqrt(length(noTaskAvg)) ) / 10000)


# Plot
gg_noTask <- ggplot(data = neglectSum) +
  geom_errorbar(aes(x = n, 
                    ymin = TaskNegelectMean - TaskNegelectSE, 
                    ymax = TaskNegelectMean + TaskNegelectSE),
                colour = "black",
                size = 0.25,
                width = 0.1) +
  geom_point(aes(x = n, y = TaskNegelectMean),
             colour = "black",
             size = 0.7) +
  theme_classic() +
  labs(x = "Group size",
       y = "Avg. task neglect") +
  scale_x_continuous(breaks = unique(neglectSum$n),
                     labels = c(1, "", 4, "", 8, 12, 16)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     labels = c("0.0", "", "0.2", "", "0.4", "", "0.6", "", "0.8", "", ""),
                     limits = c(0, 0.825),
                     expand = c(0, 0)) +
  theme(legend.position = "none",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.2, "cm"),
        legend.text = element_text(size = 6),
        legend.text.align = 0,
        # legend.box.background = element_rect(),
        axis.text.y = element_text(size = 6, margin = margin(t = 0, r =  5, b =  0, l = -4), color = "black"),
        axis.text.x = element_text(size = 6, margin = margin(5, 0, -4, 0), color = "black"),
        axis.title = element_text(size = 7, margin = margin(0, 0, 0, 0)),
        axis.ticks.length = unit(-0.06, "cm"),
        axis.ticks = element_line(colour = "black", size = 0.3),
        axis.line = element_line(size = 0.3),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))

# svg("output/MSFigures/TaskNeglectCombined.svg",  width = 1.44, height = 2.08)
ggsave("output/MSFigures/TaskNeglectCombined_OneColumn.svg",  width = 22.5, height = 45, units = "mm")
ggsave("output/MSFigures/TaskNeglectCombined.svg",  width = 23.5, height = 29.5, units = "mm")
ggsave("output/MSFigures/TaskNeglectCombined_ThreePerRow.svg", width = 45, height = 35, units = 'mm')

####################
# Task Neglect vs. Specialization within group
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
merged_specperf <- merged_specperf %>% 
  mutate(noTaskAvg = noTaskAvg / 10000) %>% 
  group_by(n) %>% 
  mutate(noTaskAvgMin = min(noTaskAvg),
         noTaskAvgMax = max(noTaskAvg),
         TaskMeanMin = min(TaskMean),
         TaskMeanMax = max(TaskMean)) %>% 
  mutate(noTaskAvgNorm = (noTaskAvg - noTaskAvgMin) / (noTaskAvgMax - noTaskAvgMin),
         TaskMeanNorm = (TaskMean - TaskMeanMin) / (TaskMeanMax - TaskMeanMin)) 

# Plot
# Palette with single individuals
palette <- c("#83343E", "#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78", "#bdbdbd", "#525252")

gg_specPerfNorm <- ggplot(data = merged_specperf) +
  geom_point(aes(x = TaskMeanNorm,
                 colour = as.factor(n),
                 y = noTaskAvgNorm), 
             size = 0.1,
             stroke = 0.4) +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("Normalized task neglect") +
  xlab("Norm. specialization") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     expand = c(0, 0.01)) +
  scale_x_continuous(breaks = seq(0, 1, 0.5), 
                     expand = c(0, 0.01)) +
  scale_color_manual(values = palette) +
  theme(axis.text.y = element_text(size = 6, margin = margin(0, 5, 0, -4), color = "black"),
        axis.text.x = element_text(size = 6, margin = margin(5, 0, -4, 0), color = "black"),
        axis.title = element_text(size = 7, margin = margin(0, 0, 0, 0)),
        axis.ticks.length = unit(-0.06, "cm"),
        axis.ticks = element_line(colour = "black", size = 0.3),
        axis.line = element_line(size = 0.3),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
gg_specPerfNorm

# svg("output/MSFigures/TaskNeglect_WithinGroup_Normalized_Color.svg",  width = 1.45, height = 2.068)
ggsave("output/MSFigures/TaskNeglect_WithinGroup_Normalized_Color_OneColumn.svg",  width = 22.5, height = 45, units = "mm")
ggsave("output/MSFigures/TaskNeglect_WithinGroup_Normalized_Color.svg",  width = 23.5, height = 30, units = "mm")
ggsave("output/MSFigures/TaskNeglect_WithinGroup_Normalized_Color_threeperrow.svg",  width = 45, height = 35, units = "mm")


####################
# Experiments: Min RMSD
####################
rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3_PrepPlotExperimentData.R")

min_rmsd <- read.csv(file = "data/minRMSD.csv", header = TRUE)

min_rmsd_summary <- min_rmsd %>% 
  group_by(GroupSize) %>% 
  summarise(MeanMinRMSD = mean(minRMSD),
            MeanMinRMSDSE = ( sd(minRMSD) / sqrt(length(minRMSD))))
blank_row <- data.frame("colonyID" = 'blah', "GroupSize" = 1.0, "minRMSD" = NA)
min_rmsd <- rbind(min_rmsd, blank_row)

gg_RMSD <- ggplot() +
  geom_point(data = min_rmsd, 
             aes(x = GroupSize, y = minRMSD),
             col = "grey60", 
             size = 0.4,
             shape = 8,
             stroke = 0.2) +
  geom_errorbar(data = min_rmsd_summary, 
                aes(x = GroupSize, 
                    ymin = MeanMinRMSD - MeanMinRMSDSE, 
                    ymax = MeanMinRMSD + MeanMinRMSDSE),
                colour = "black",
                size = 0.3, 
                width = 0) +
  geom_point(data = min_rmsd_summary, 
             aes(x = GroupSize, y = MeanMinRMSD),
             size = 0.7) +
  theme_classic() +
  xlab("Group Size") +
  ylab("Min. RMSD (mm)") +
  scale_y_continuous(breaks = seq(9, 19, 1),
                     expand = c(0, 0.05),
                     labels = c('', '10', '', '12', '', '14', '', '16', '', '18', '')) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 12, 16),
                     expand = c(0, 0.0),
                     labels = c('', '2', '', '6', '', '12', '16')) +
  scale_color_manual(values = palette) +
  theme(axis.text.y = element_text(size = 6, margin = margin(0, 5, 0, -4), color = "black"),
        axis.text.x = element_text(size = 6, margin = margin(5, 0, -4, 0), color = "black"),
        axis.title = element_text(size = 7, margin = margin(0, 0, 0, 0)),
        axis.ticks.length = unit(-0.06, "cm"),
        axis.ticks = element_line(colour = "black", size = 0.3),
        axis.line = element_line(size = 0.3),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
gg_RMSD

ggsave("output/MSFigures/MinRMSDFigure.svg",  width = 23.5, height = 29.5, units = "mm")
ggsave("output/MSFigures/MinRMSDFigure_threeperrow.svg", width = 45, height = 35, units = "mm")
