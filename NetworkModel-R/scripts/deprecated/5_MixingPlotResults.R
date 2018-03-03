################################################################################
#
# Plot ensemble model outputs in single plot 
#
################################################################################

source("scripts/__Util__MASTER.R")



# Set variable
filename <- "Mixed_4-6Thresh_DifferentStimByLine"

# Palette for clonal lines
paletteLines <- c("#F00924", "#1D10F9")

####################
# Final task distributions
####################
# Bind together
taskDist <- lapply(groups_taskDist, function(x) { 
  unlisted <- unlist(x, recursive = FALSE)
})
taskDist <- unlist(taskDist, recursive = FALSE)
taskDistTot <- do.call("rbind", taskDist)

# Manipulate
taskDistTot <- taskDistTot %>% 
  mutate(set = paste0(Mix, replicate)) %>% 
  mutate(set = factor(set, 
                      levels = mixedsort(unique(set))),
         n = as.factor(n),
         Line = as.factor(Line)) 

taskSum <- taskDistTot %>% 
  group_by(n) %>% 
  summarise(taskMean1 = mean(Task1),
            taskMean2 = mean(Task2))

# Plot
plot_TaskMat <- as.data.frame(taskDistTot)
gg_dist <- ggplot(data = plot_TaskMat, aes(y = Task1, x = set)) +
  geom_point(aes(colour = Line), size = 0.8) +
  theme_classic() +
  labs(y = "Frequency Task 1") +
  scale_color_manual(values = paletteLines) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())  +
  facet_grid(. ~ n) 

gg_dist

ggsave(paste0("output/MixedLinePlots/", filename, ".png"), width = 8, height = 4, units = "in", dpi = 150)
