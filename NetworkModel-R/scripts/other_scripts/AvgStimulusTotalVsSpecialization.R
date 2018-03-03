stims_avg <- stims %>%
  group_by(n, replicate) %>% 
  summarise(avgS1 = mean(s1),
            avgS2 = mean(s2)) %>% 
  mutate(Set = paste0(n, "-", replicate),
         avgS = (avgS1 + avgS2) / 2)


merged_specstim <- merge(taskCorrTot, stims_avg, by = c("Set", "n"))


palette <- c("#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78", "#bdbdbd", "#525252")


qplot(data = merged_specstim, x = TaskMean, y = avgS, col = as.factor(n)) + 
  theme_classic() +
  scale_color_manual(values = palette) +
  facet_wrap(~n)
 



task_avg <- tallies %>%
  mutate(Task1 = Task1 / n,
         Task2 = Task2 / n,
         Inactive = Inactive / n) %>% 
  group_by(n, replicate) %>% 
  summarise(avgTask1 = mean(Task1),
            avgTask2 = mean(Task2)) %>% 
  mutate(Set = paste0(n, "-", replicate),
         avgTask = (avgTask1 + avgTask2) / 2)


merged_taskstim <- merge(taskCorrTot, task_avg, by = c("Set", "n"))


palette <- c("#F00924", "#F7A329", "#FDD545", "#027C2C", "#1D10F9", "#4C0E78", "#bdbdbd", "#525252")


qplot(data = merged_taskstim, x = TaskMean, y = avgTask, col = as.factor(n)) + 
  theme_classic() +
  scale_color_manual(values = palette) +
  facet_wrap(~n)
