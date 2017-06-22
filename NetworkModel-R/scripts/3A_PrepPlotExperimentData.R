################################################################################
#
# Prep data from experiments for plotting
#
################################################################################

####################
# Prep Yuko RMSD data
####################
# Load and manipulate
yukoData <- read.csv("data/GSAB_ants.csv", header = T, stringsAsFactors = FALSE)
yukoData <- yukoData %>% 
  mutate(RMSD = as.numeric(RMSD),
         RMSDnorm = RMSD / 25,
         n = GS,
         replicate = gsub(".*_([0-9]+)", "\\1", boxID)) %>% 
  mutate(set = paste0(CloneLine, n, "-", replicate)) %>% 
  mutate(set = factor(set, levels = mixedsort(unique(set))),
         n = as.factor(n))

# Separate
yukoDataA <- yukoData %>% 
  filter(CloneLine == "A")
yukoDataB <- yukoData %>% 
  filter(CloneLine == "B")

# Create summary tables for plotting
yukoDataSummary <- yukoData %>% 
  mutate(n = as.character(n)) %>% 
  mutate(n = as.numeric(n),
         replicate = as.integer(replicate)) %>%
  group_by(n, replicate, CloneLine) %>% 
  summarise(SD = sd(RMSDnorm, na.rm = TRUE),
            Mean = mean(RMSDnorm, na.rm = TRUE))  %>% 
  mutate(Source = "Experiment")
yukoDataSummary$SD[is.na(yukoDataSummary$SD)] <- 0 #fix for single individuals
yukoDataSummary <- yukoDataSummary %>% 
  filter(!is.na(Mean)) %>% 
  select(-CloneLine)


####################
# Prep Yuko correlation data
####################
# Kronauer data
yukoCorr <- read.csv("data/Specialization_RawData.csv", header = TRUE)
yukoCorr <- yukoCorr %>% 
  mutate(n = GroupSize, TaskMean = Specialization, Source = "Experiment") %>% 
  select(n, TaskMean, Source) %>% 
  filter(!is.na(TaskMean))


####################
# TaskDist
####################
# Plot
gg_dist_expA <- ggplot(data = yukoDataA, aes(y = RMSDnorm, x = set, colour = n)) +
  geom_point(size = 0.3) +
  theme_classic() +
  labs(x = "Group Size",
       y = "Normalized RMSD") +
  scale_color_manual(values = palette) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme(axis.text.x = element_blank()) 


gg_dist_expB <- ggplot(data = yukoDataB, aes(y = RMSDnorm, x = set, colour = n)) +
  geom_point(size = 0.3) +
  theme_classic() +
  labs(x = "Group Size",
       y = "Normalized RMSD") +
  scale_color_manual(values = palette) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme(axis.text.x = element_blank()) 

