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

load("/Users/ChrisTokita/Documents/Research/Tarnita Lab/Evolution of DOL/Fixed_Delta06Sigma01Eta7LargerGroups100reps.Rdata")

####################
# Sample stimuli over time
####################
# Unlist
stims <- unlist(groups_stim, recursive = FALSE)
stims <- do.call("rbind", stims)

# Select out example colonies
stimSet <- stims %>% 
  filter(n %in% c(2, 16, 32, 100)) %>% 
  filter(replicate == 1) %>% 
  group_by(n) %>% 
  mutate(timestep = 0:(length(n)-1),
         groupsize = factor(paste0("Group size ", n), 
                            levels = c("Group size 2", "Group size 16", "Group size 32", "Group size 100")))

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
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7),
        axis.title = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        strip.text = element_text(size = 8, face = "italic"),
        strip.background = element_rect(fill = NA, colour = NA),
        panel.spacing = unit(0.5, "cm")) +
  facet_grid(. ~ groupsize)

gg_stimEx

ggsave(filename = "output/FitnessPlots/StimOverTimeExampleLargerGroupos.png", width = 4, height = 1.5, units = "in", dpi = 800)

