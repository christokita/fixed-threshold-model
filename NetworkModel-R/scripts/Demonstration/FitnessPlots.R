################################################################################
#
# Fitness Plots
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3A_PrepPlotExperimentData.R")
library(RColorBrewer)
library(scales)

load("output/SpecializationMetrics/Rdata/FixedDelta06Sigma01Eta7100reps.Rdata")

####################
# Sample stimuli over time
####################
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
  geom_line(colour = "#4eb3d3", 
            size = 0.2) +
  theme_classic() +
  xlab("Timestep") +
  ylab("Stimulus") +
  scale_x_continuous(breaks = seq(0, 10000, 5000),
                     labels = comma) +
  theme(plot.margin = margin(0.25, 0.4, 0.25, 0.25, "cm"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        strip.text = element_text(size = 7, face = "italic"),
        strip.background = element_rect(fill = NA, colour = NA),
        panel.spacing = unit(0.5, "cm")) +
  facet_grid(. ~ groupsize)

gg_stimEx

ggsave(filename = "output/FitnessPlots/StimOverTimeExample.png", width = 4.2, height = 2, units = "in", dpi = 600)


####################
# Sample stimuli over time
####################
# Unlist
tallies <- unlist(groups_taskTally, recursive = FALSE)
tallies <- do.call("rbind", tallies)

# Normalize
tallyEx <- tallies %>% 
  filter(n %in% c(2, 6, 16)) %>% 
  filter(replicate == 1) %>% 
  mutate(Task1 = Task1 / n,
         Task2 = Task2 / n,
         Inactive = Inactive / n,
         n = factor(n)) %>% 
  melt(id.vars = c("n", "t", "replicate")) %>% 
  rename(Task = variable, Freq = value) %>% 
  mutate(timestep = 0:(length(n)-1),
         groupsize = factor(paste0("n = ", n), 
                            levels = c("n = 2", "n = 6", "n = 16")))
levels(tallyEx$Task) <- c("Task 1", "Task 2", "Inactive")


# Plot
cols <- c("#F00924", "#FDD545", "#4C0E78")

gg_taskEx <- ggplot(data = tallyEx, aes(x = t, y = Freq, colour = groupsize)) +
  geom_line(alpha = 1) +
  theme_classic() +
  xlab("Timestep") +
  ylab("Proportion of Colony") +
  scale_x_continuous(limits = c(0, 300),
                     breaks = seq(0, 1000, 100)) +
  scale_color_manual(values = cols) +
  theme(legend.position = "none", 
        legend.title = element_text(size = 7, face = "bold"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        strip.text = element_text(size = 7, face = "italic"),
        strip.background = element_rect(fill = NA, colour = NA),
        panel.spacing = unit(0.5, "cm")) +
  facet_grid(Task ~ groupsize)

gg_taskEx

ggsave(filename = "output/FitnessPlots/TasksOverTimeExample.png", width = 5, height = 4, units = "in", dpi = 600)

