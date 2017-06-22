################################################################################
#
# Plot ensemble model outputs in single plot 
#
################################################################################

source("scripts/__Util__MASTER.R")



# Set variable
filename <- "Fixed_11_Sigma03Tau03"

# Palette without single individuals
palette <- c("#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78", "#bdbdbd", "#525252")

# Palette without single individuals
palette <- c("#83343E", "#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78", "#bdbdbd", "#525252")

####################
# Final task distributions
####################
# Bind together
taskDist <- unlist(groups_taskDist, recursive = FALSE)
taskDistTot <- do.call("rbind", taskDist)

# Manipulate
taskDistTot <- taskDistTot %>% 
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
  geom_point(aes(colour = n), size = 0.3) +
  theme_classic() +
  labs(x = "Group Size",
       y = "Frequency Task 1") +
  scale_color_manual(values = palette) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme(axis.text.x = element_blank()) 



####################
# Task variance by group size
####################
# Prep
taskVarMean <- taskDistTot %>% 
  mutate(n = as.character(n)) %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(n, replicate) %>% 
  summarise(SD = sd(Task1),
            Mean = mean(Task1)) 
taskVarMean$SD[is.na(taskVarMean$SD)] <- 0 #fix for single individuals

# Calculate means and SE
taskVarMeans <- taskVarMean %>% 
  group_by(n) %>% 
  summarise(MeanMean = mean(Mean),
            MeanSE = sd(Mean) / sqrt(length(Mean)),
            SDMean = mean(SD),
            SDSE = sd(SD) / sqrt(length(SD)))

# Calculate CI for resampling
resampledCI <- resampleCI(TaskDistList = groups_taskDist)
resampledCI <- as.data.frame(resampledCI)
resampledCI[is.na(resampledCI)] <- 0

# Plot variance and mean by group size
gg_var <- ggplot() +
  # Add resampled CI
  geom_line(data = resampledCI,
            aes(x = n, y = SD1_avg),
            colour = "#93cdff",
            size = 0.8) +
  geom_ribbon(data = resampledCI,
              aes(x = n, ymin = SD1_avg - SD1_95CI, ymax = SD1_avg + SD1_95CI),
              fill = "#93cdff",
              alpha = 0.4) +
  # Add data
  geom_point(data = taskVarMean, 
             aes(x = n, y = SD),
             colour = "grey70", 
             size = 0.8) +
  theme_classic() +
  xlab("Group Size") +
  ylab("Behavioral Variation (SD)") +
  scale_x_continuous(breaks = unique(taskVarMean$n)) +
  scale_y_continuous(breaks = seq(0, 1, 0.025)) +
  # Mean and SE portion of plot
  geom_errorbar(data = taskVarMeans, 
                aes(x = n, ymin = SDMean - SDSE, ymax = SDMean + SDSE)) +
  geom_point(data = taskVarMeans, 
             aes(x = n, y = SDMean)) +
  geom_line(data = taskVarMeans,
            aes(x = n, y = SDMean)) 


gg_mean <- ggplot() +
  # Add resampled CI
  # geom_line(data = resampledCI, 
  #           aes(x = n, y = Mean1_avg), 
  #           colour = "#93cdff",
  #           size = 0.5) +
  # geom_ribbon(data = resampledCI,
  #             aes(x = n, ymin = Mean1_avg - Mean1_95CI, ymax = Mean1_avg + Mean1_95CI),
  #             fill = "#93cdff",
  #             alpha = 0.4) +
  # Add data
  geom_point(data = taskVarMean,
             aes(x = n, y = Mean),
             colour = "grey70", 
             size = 0.8) +
  theme_classic() +
  xlab("Group Size") +
  ylab("Behavioral Mean") +
  scale_x_continuous(breaks = unique(taskVarMean$n)) +
  scale_y_continuous(breaks = seq(0, 1, 0.025)) +
  # Mean and SE portion of plot
  geom_errorbar(data = taskVarMeans, 
                aes(x = n, ymin = MeanMean - MeanSE, ymax = MeanMean + MeanSE)) +
  geom_point(data = taskVarMeans, 
             aes(x = n, y = MeanMean)) +
  geom_line(data = taskVarMeans,
            aes(x = n, y = MeanMean)) 




####################
# Plot task tallies over time
####################
# Unlist
tallies <- unlist(groups_taskTally, recursive = FALSE)
tallies <- do.call("rbind", tallies)

# Normalize
tallies <- tallies %>% 
  mutate(Task1 = Task1 / n,
         Task2 = Task2 / n,
         Inactive = Inactive / n,
         n = factor(n)) %>% 
  select(-replicate) %>% 
  melt(id.vars = c("n", "t")) %>% 
  rename(Task = variable, Freq = value) %>% 
  group_by(n, t, Task) %>% 
  summarise(MeanFreq = mean(Freq))

# Plot
colPal <- colorRampPalette(c("#d3afff", "#1f0242"))
numbers <- length(unique(tallies$n))

gg_tally <- ggplot(data = tallies, aes(x = t)) +
  geom_line(aes(y = MeanFreq, group = n, colour = n)) +
  geom_smooth(method = "lm", 
              aes(y = MeanFreq), 
              se = FALSE, 
              colour = "black",
              size = 0.5) +
  theme_classic() +
  scale_color_manual(values = palette) +
  scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 10000, 200)) +
  xlab("Time") +
  ylab("Mean Proportion of Colony") +
  facet_grid(n ~ Task) +
  theme(legend.position = "none",
        axis.text.x = element_blank())


####################
# Task Rank Correlation
####################
# Unlist
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2)

# Kronauer data
yukoCorr <- read.csv("data/Specialization_RawData.csv", header = TRUE)
yukoCorr <- yukoCorr %>% 
  mutate(n = GroupSize, TaskMean = Specialization, Source = "Experiment") %>% 
  select(n, TaskMean, Source) %>% 
  filter(!is.na(TaskMean))
taskCorrTot <- taskCorrTot %>% 
  mutate(Source = "Model") %>% 
  select(n, TaskMean, Source) %>% 
  rbind(yukoCorr) %>% 
  mutate(Source = as.factor(Source))

# Calculate means and SE
taskCorrMeans <- taskCorrTot %>% 
  group_by(Source, n) %>% 
  summarise(SpecMean = mean(TaskMean),
            SpecSE = sd(TaskMean) / sqrt(length(TaskMean)),
            SpecCI = 1.96 * SpecSE)

# Plot
gg_corr <- ggplot() +
  geom_hline(data = taskCorrTot, 
             aes(yintercept = 0),
             colour = "grey30") +
  geom_point(data = taskCorrTot, 
             aes(x = n, y = TaskMean, fill = Source, colour = Source), 
             size = 0.8, 
             position = position_dodge(width = 1),
             alpha = 0.5) +
  theme_classic() +
  labs(x = "Group Size",
       y = "Specialization") +
  scale_x_continuous(breaks = unique(taskCorrTot$n)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) +
  scale_fill_manual(values = c( "indianred2", "black")) +
  scale_colour_manual(values = c( "indianred2", "black")) +
  theme(legend.position = "none") +
  # Mean and SE portion of plot
  geom_errorbar(data = taskCorrMeans, 
                aes(x = n, ymin = SpecMean - SpecCI, ymax = SpecMean + SpecCI, colour = Source),
                position = position_dodge(width = 1)) +
  geom_point(data = taskCorrMeans, 
             aes(x = n, y = SpecMean, fill = Source, colour = Source),
             position = position_dodge(width = 1)) +
  geom_line(data = taskCorrMeans,
            aes(x = n, y = SpecMean,  colour = Source),
            position = position_dodge(width = 1))


####################
# Plot stimulus
####################
# Unlist
stims <- unlist(groups_stim, recursive = FALSE)
stims <- do.call("rbind", stims)

# Prep for plot
stims <- stims %>% 
  mutate(n = factor(n)) %>% 
  group_by(n, replicate) %>% 
  mutate(t = 0:(length(n) - 1)) 

stims <- stims %>% 
  select(-delta1, -delta2) %>% 
  group_by(n, t) %>% 
  summarise(Stimulus1 = mean(s1),
            Stimulus2 = mean(s2)) %>% 
  melt(id.vars = c("n", "t")) %>%
  rename(Stim = variable)

# Plot
gg_stim <- ggplot(data = stims, aes(x = t)) +
  geom_line(aes(y = value, group = n, colour = n)) +
  # geom_smooth(method = "lm", 
  #             aes(y = value), 
  #             se = FALSE, 
  #             colour = "black",
  #             size = 0.5) +
  theme_classic() +
  scale_color_manual(values = palette) +
  scale_y_continuous(breaks = seq(0, 100, 1)) +
  scale_x_continuous(breaks = seq(0, 10000, 200)) +
  xlab("Time") +
  ylab("Mean Stimulus") +
  facet_grid(n ~ Stim) +
  theme(legend.position = "none",
        axis.text = element_blank()) 


####################
# Plot all
####################


# MultiPlot
png(filename = paste0("output/_ComprehnsivePlots/", filename, ".png"), width = 12, height = 8, units = "in", res = 300)
multiplot(gg_dist, gg_mean,  gg_corr, gg_var, gg_tally, gg_stim, cols = 3)  
dev.off()



####################
# Save all
####################
save(groups_entropy, groups_stim, groups_taskCorr, groups_taskDist, groups_taskStep, groups_taskTally, taskCorrTot, 
     file = paste0("output/__RData/", filename, ".Rdata"))

