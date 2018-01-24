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
 