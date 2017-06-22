################################################################################
#
# Evaluate ensemble model outputs
#
################################################################################

source("scripts/__Util__MASTER.R")

# Set variable
main_title <- "Epsilon = 0.2\nSmall Var Threshold Means\nScale Free Network\nUneven Exhaust"
filename <- "HighEx_Epsilon_0.2_ScaleFree.png"

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
  geom_point(aes(colour = n)) +
  theme_classic() +
  labs(title = main_title,
       x = "Group Size",
       y = "Frequency Task 1") +
  #scale_color_brewer(palette = "Paired") +
  scale_color_manual(values = c("#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme(axis.text.x = element_blank())
gg_dist

ggsave(filename = paste0("output/FrequencyBySize/", filename), width = 4, height = 4, dpi = 300)


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

# Calculate means and SE
taskVarMeans <- taskVarMean %>% 
  group_by(n) %>% 
  summarise(MeanMean = mean(Mean),
            MeanSE = sd(Mean) / sqrt(length(Mean)),
            SDMean = mean(SD),
            SDSE = sd(SD) / sqrt(length(SD)))

# Plot variance and mean by group size
gg_var <- ggplot() +
  geom_point(data = taskVarMean, 
             aes(x = n, y = SD),
             colour = "grey70", 
             size = 0.8) +
  theme_classic() +
  xlab("Group Size") +
  ylab("Behavioral Variation (SD)") +
  scale_x_continuous(breaks = unique(taskVarMean$n)) +
  # Mean and SE portion of plot
  geom_errorbar(data = taskVarMeans, 
                aes(x = n, ymin = SDMean - SDSE, ymax = SDMean + SDSE)) +
  geom_point(data = taskVarMeans, 
             aes(x = n, y = SDMean)) +
  geom_line(data = taskVarMeans,
            aes(x = n, y = SDMean))
gg_var

gg_mean <- ggplot() +
  geom_point(data = taskVarMean,
             aes(x = n, y = Mean),
             colour = "grey70", 
             size = 0.8) +
  theme_classic() +
  xlab("Group Size") +
  ylab("Behavioral Mean") +
  scale_x_continuous(breaks = unique(taskVarMean$n)) +
  # Mean and SE portion of plot
  geom_errorbar(data = taskVarMeans, 
                aes(x = n, ymin = MeanMean - MeanSE, ymax = MeanMean + MeanSE)) +
  geom_point(data = taskVarMeans, 
             aes(x = n, y = MeanMean)) +
  geom_line(data = taskVarMeans,
            aes(x = n, y = MeanMean))
gg_mean

# MultiPlot
png(filename = paste0("output/VarAndMean/", filename), width = 8, height = 4, units = "in", res = 300)
multiplot(gg_mean, gg_var, cols = 2)
dev.off()

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
  theme_classic() +
  scale_color_manual(values = colPal(numbers)) +
  scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  xlab("Time") +
  ylab("Mean Frequency") +
  facet_grid(n ~ Task) +
  theme(legend.position = "none",
        axis.text.x = element_blank())
gg_tally

ggsave(filename = paste0("output/TaskFrequencyByTime/", filename), width = 4, height = 4, dpi = 300)

####################
# Task Rank Correlation
####################
# Unlist
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2 )

# Calculate means and SE
taskCorrMeans <- taskCorrTot %>% 
  group_by(n) %>% 
  summarise(SpecMean = mean(TaskMean),
            SpecSE = sd(TaskMean) / sqrt(length(TaskMean)))

# Plot
gg_corr <- ggplot() +
  geom_hline(data = taskCorrTot, aes(yintercept = 0),
             colour = "grey30") +
  geom_point(data = taskCorrTot, aes(x = n, y = TaskMean), 
             colour = "grey70", 
             size = 0.8) +
  theme_classic() +
  labs(title = main_title,
       x = "Group Size",
       y = "Specialization") +
  scale_x_continuous(breaks = unique(taskCorrTot$n)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  # Mean and SE portion of plot
  geom_errorbar(data = taskCorrMeans, 
                aes(x = n, ymin = SpecMean - SpecSE, ymax = SpecMean + SpecSE)) +
  geom_point(data = taskCorrMeans, 
             aes(x = n, y = SpecMean)) +
  geom_line(data = taskCorrMeans,
            aes(x = n, y = SpecMean))
gg_corr

ggsave(filename = paste0("output/SpecializationCorrelation/", filename), width = 4, height = 4, dpi = 300)


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
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu") +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  xlab("Time") +
  ylab("Mean Stimulus") +
  facet_grid(n ~ Stim) +
  theme(legend.position = "none",
        axis.text = element_blank())
gg_stim

ggsave(filename = paste0("output/StimByTime/", filename), width = 4, height = 4, dpi = 300)

####################
# Compare entropies
####################
# Unlist
entropy <- unlist(groups_entropy, recursive = FALSE)
entropy <- do.call("rbind", entropy)

# Summarise
entropy <- entropy %>% 
  group_by(n) %>% 
  summarize(Dsym = mean(Dsym),
            Dyx = mean(Dyx),
            Dxy = mean(Dxy))


####################
# Compare model fits
####################

# Plot density plots
gg_dens <- ggplot(data = taskCorrTot, aes(x = TaskMean)) +
  geom_density(aes(fill = Source)) +
  theme_classic() +
  scale_fill_manual(values = c( "indianred2", "grey30")) +
  facet_grid(Source ~ n) +
  xlab("Specialization Value") +
  ylab("Density")
gg_dens

ggsave(filename = "output/SpecializationDensity/StimLow06ThreshLowVarHighExhaust_Epsilon_0_Complete.png", 
       width = 9, height = 3, dpi = 150)


