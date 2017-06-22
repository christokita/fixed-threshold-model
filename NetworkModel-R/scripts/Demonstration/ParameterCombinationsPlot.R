################################################################################
#
# Plot ot show parameter combinations tested
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")
library(parallel)
library(snowfall)
library(rlecuyer)


####################
# Set global variables
####################
sigmas         <- c(0, 0.01, 0.02, 0.03, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275, 0.3, 0.325, 0.35, 0.4, 0.45, 0.5)
threshSlopes   <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 17, 20, 22, 25, 27, 30) #exponent parameter for threshold curve shape  

combos <- expand.grid(sigma = sigmas, threshSlope = threshSlopes)

####################
# Plot
####################

gg_param <- ggplot(data = combos, aes(x = sigma, y = threshSlope)) +
  geom_point(size = 0.1) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.ticks = element_line(size = 0.5), 
        panel.border = element_rect(fill = NA, size = 1)) +
  xlab(expression(sigma)) +
  ylab(expression(eta))

gg_param


ggsave(plot = gg_param, filename = "output/OtherFigures/ParameterCombinationsTested.png", width = 2, height = 2, units = "in", dpi = 600)
