#
# Testing out stimulus threshold from BONABEAU, THERAULAZ, & DENEUBOURG (1996/1998)
#
rm(list = ls())
library(reshape2)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

graphThresholds <- function(thresholds) {
  #Loop through thresholds 
  to_plot <- lapply(1:length(thresholds), function(j) {
    # Run one threshold and loop through stimulus level
    s <- seq(0.01, 1, 0.01)
    prob <- rep(NA, length(s))
    for (i in 1:length(s)) {
      prob[i] <- s[i]^2 / (s[i]^2 + thresholds[j]^2)
    }
    return(prob)
  })
  names(to_plot) <- paste0("threshold_", thresholds)
  to_plot <- do.call("cbind", to_plot) 
  # Melt for plotting
  to_plot <- to_plot %>% 
    melt() %>% 
    mutate(stimulus = Var1 - 1,
           thresholdlevel = Var2, 
           probability = value) %>% 
    select(stimulus, thresholdlevel, probability)
  # Color Function
  getPalette <- colorRampPalette(brewer.pal(9, "PuBu"))
  threshColors <- getPalette(length(unique(to_plot$thresholdlevel)))
  # Plot
  gg_thresh <- ggplot(data = to_plot, aes(x = stimulus, y = probability)) +
    geom_line(aes(colour = thresholdlevel)) +
    theme_classic() +
    scale_color_manual(values = threshColors) +
    scale_x_continuous(trans = "log10",
                       limits = c(NA, 1)) +
    xlab("\nStimulus Level") +
    ylab("Probability of Performing Task\n")
  gg_thresh
}


thresholds <- c(seq(0, 1, 0.1))

graphThresholds(thresholds = thresholds)
