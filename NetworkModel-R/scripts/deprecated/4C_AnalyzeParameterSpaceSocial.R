################################################################################
#
# Test the specilization fit (% increase) over sigma and n-slope parameter space
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")
library(scales)

####################
# Prep and Plot
####################
# load
load("output/ParameterExploration/Rdata/SocialDelta06_CSlopeExploration.Rdata")
improve <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         StimDiffSmall   = (Task1DiffSmall + Task2DiffSmall) / 2,
         StimDiffLarge   = (Task1DiffLarge + Task2DiffLarge) / 2) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3)

# Set file names
filename <- "SocialDelta06"

# Exp data: % increase = 1.220554
# Exp data: Slope = 0.02322321
# Exp data: n16 = 0.5915000
# Exp data: n2 = 0.2663750

##### Slope #####
# Fit surface
spec.loess <- loess(relativeSlope ~ c * threshSlope, data = improve, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(c = seq(0, max(improve$c), (max(improve$c) - min(improve$c)) / 1000), 
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)

spec.fit$spec <- as.numeric(z)
# Find which z are within range 0.95 - 1.05? (relative to data)

# Graph 
gg_slope <- ggplot(spec.fit, aes(x = c, y = threshSlope, fill = spec)) +
  geom_tile() +
  theme_bw() +
  geom_hline(mapping = aes(yintercept = 2), 
             linetype = "dashed", size = 0.3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradient2(name = "Fit to Data",
                       low = "#0571b0", 
                       mid = "white", 
                       high = "#ca0020", 
                       midpoint = 0, 
                       limits = c(-0.5, 0.5), 
                       oob = squish) +
  xlab(expression(c)) +
  ylab(expression(eta)) +
  theme(legend.position = c(0.95, 0.95), 
        legend.justification = c(1, 1),
        legend.background = element_rect(color = "black", size = 0.2, linetype = "solid"),
        legend.title = element_text(size = 4, face = "bold"),
        legend.key.size = unit(0.2, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 4),
        title = element_text(size = 4),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

##### N = 2 #####
# Fit surface
spec.loess <- loess(relativeSmall ~ c * threshSlope, data = improve, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(c = seq(0, max(improve$c), (max(improve$c) - min(improve$c)) / 1000), 
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)

spec.fit$spec <- as.numeric(z)
# Find which z are within range 0.95 - 1.05? (relative to data)

# Graph 
gg_small <- ggplot(spec.fit, aes(x = c, y = threshSlope, fill = spec)) +
  geom_tile() +
  theme_bw() +
  geom_hline(mapping = aes(yintercept = 2), 
             linetype = "dashed", size = 0.3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradient2(name = "Fit to Data",
                       low = "#0571b0", 
                       mid = "white", 
                       high = "#ca0020", 
                       midpoint = 0, 
                       limits = c(-0.5, 0.5), 
                       oob = squish) +
  xlab(expression(c)) +
  ylab(expression(eta)) +
  theme(legend.position = c(0.95, 0.95), 
        legend.justification = c(1, 1),
        legend.background = element_rect(color = "black", size = 0.2, linetype = "solid"),
        legend.title = element_text(size = 4, face = "bold"),
        legend.key.size = unit(0.2, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 4),
        title = element_text(size = 4),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))


##### N = 16 #####
# Fit surface
spec.loess <- loess(relativeLarge ~ c * threshSlope, data = improve, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(c = seq(0, max(improve$c), (max(improve$c) - min(improve$c)) / 1000), 
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)

spec.fit$spec <- as.numeric(z)
# Find which z are within range 0.95 - 1.05? (relative to data)
# Normalize spec values for coloring
maxVal <- max(spec.fit$spec)
vals <- c(0 / maxVal, 0.5 / maxVal, 0.9 / maxVal, 1.1 / maxVal, 1.5 / maxVal, 2.0 / maxVal, maxVal)

# Graph 
gg_large <- ggplot(spec.fit, aes(x = c, y = threshSlope, fill = spec)) +
  geom_tile() +
  theme_bw() +
  geom_hline(mapping = aes(yintercept = 2), 
             linetype = "dashed", size = 0.3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradient2(name = "Fit to Data",
                       low = "#0571b0", 
                       mid = "white", 
                       high = "#ca0020", 
                       midpoint = 0, 
                       limits = c(-0.5, 0.5), 
                       oob = squish) +
  xlab(expression(c)) +
  ylab(expression(eta)) +
  theme(legend.position = c(0.95, 0.95), 
        legend.justification = c(1, 1),
        legend.background = element_rect(color = "black", size = 0.2, linetype = "solid"),
        legend.title = element_text(size = 4, face = "bold"),
        legend.key.size = unit(0.2, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 4),
        title = element_text(size = 4),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))


##### Plot #####
# output
png(paste0("output/ParameterExploration/Plot/", filename, "_SlopeandSize.png"), width = 6, height = 2, units = "in", res = 300)
multiplot(gg_slope, gg_small, gg_large, cols = 3)
dev.off()


##### Fit #####
# Fit surface
spec.loess <- loess(fit ~ c * threshSlope, data = improve, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(c = seq(0, max(improve$c), (max(improve$c) - min(improve$c)) / 1000), 
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)

spec.fit$spec <- as.numeric(z)
# Find which z are within range 0.95 - 1.05? (relative to data)

# Try custom color
colPal <- c("#b2182b", "#ffffff", "#000000")

# Graph 
gg_fit <- ggplot(spec.fit, aes(x = c, y = threshSlope, fill = spec)) +
  geom_tile() +
  theme_bw() +
  geom_hline(mapping = aes(yintercept = 2), 
             linetype = "dashed", size = 0.3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  # scale_fill_gradient2(name = "Fit to Data",
  #                      low = "#4575b4",
  #                      mid = "#ffffff",
  #                      high = "#d73027",
  #                      midpoint = 0,
  #                      limits = c(-1, 1),
  #                      oob = squish) +
  scale_fill_gradientn(name = "Total Fit",
                       values = c(0, 0.5, 1),
                       colours = colPal,
                       limits = c(0, 1),
                       oob = squish) +
  xlab(expression(c)) +
  ylab(expression(eta)) +
  theme(legend.position = c(0.95, 0.95), 
        legend.justification = c(1, 1),
        legend.background = element_rect(color = "black", size = 0.2, linetype = "solid"),
        legend.title = element_text(size = 4, face = "bold"),
        legend.key.size = unit(0.2, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 4),
        title = element_text(size = 4),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave(paste0("output/ParameterExploration/Plot/", filename, "_fit.png"), width = 2, height = 2, units = "in", dpi = 300)


##### Stm differences #####
# Fit surface
stim.loess <- loess(StimDiffSmall ~ c * threshSlope, data = improve, degree = 2, span = 0.1)
stim.fit <- expand.grid(list(c = seq(0, max(improve$c), (max(improve$c) - min(improve$c)) / 1000), 
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
z <- predict(stim.loess, newdata = stim.fit)

stim.fit$stim <- as.numeric(z)

# Graph 
gg_stimSmall <- ggplot(stim.fit, aes(x = c, y = threshSlope, fill = stim)) +
  geom_tile() +
  theme_bw() +
  geom_hline(mapping = aes(yintercept = 2), 
             linetype = "dashed", size = 0.3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradient2(name = "Change in\nStimulus",
                       low = "#542788", 
                       mid = "white", 
                       high = "#e08214", 
                       midpoint = 0, 
                       limits = c(-1, 1), 
                       oob = squish) +
  xlab(expression(c)) +
  ylab(expression(eta)) +
  theme(legend.position = c(0.95, 0.95), 
        legend.justification = c(1, 1),
        legend.background = element_rect(color = "black", size = 0.2, linetype = "solid"),
        legend.title = element_text(size = 4, face = "bold"),
        legend.key.size = unit(0.2, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 4),
        title = element_text(size = 4),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

# Fit surface
stim.loess <- loess(StimDiffLarge ~ c * threshSlope, data = improve, degree = 2, span = 0.1)
stim.fit <- expand.grid(list(c = seq(0, max(improve$c), (max(improve$c) - min(improve$c)) / 1000), 
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
z <- predict(stim.loess, newdata = stim.fit)

stim.fit$stim <- as.numeric(z)

# Graph 
gg_stimLarge <- ggplot(stim.fit, aes(x = c, y = threshSlope, fill = stim)) +
  geom_tile() +
  theme_bw() +
  geom_hline(mapping = aes(yintercept = 2), 
             linetype = "dashed", size = 0.3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradient2(name = "Change in\nStimulus",
                       low = "#542788", 
                       mid = "white", 
                       high = "#e08214", 
                       midpoint = 0, 
                       limits = c(-1, 1), 
                       oob = squish) +
  xlab(expression(c)) +
  ylab(expression(eta)) +
  theme(legend.position = c(0.95, 0.95), 
        legend.justification = c(1, 1),
        legend.background = element_rect(color = "black", size = 0.2, linetype = "solid"),
        legend.title = element_text(size = 4, face = "bold"),
        legend.key.size = unit(0.2, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 4),
        title = element_text(size = 4),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

##### Plot #####
# output
png(paste0("output/ParameterExploration/Plot/", filename, "_FitAndStim.png"), width = 6, height = 2, units = "in", res = 300)
multiplot(gg_fit, gg_stimSmall, gg_stimLarge, cols = 3)
dev.off()


##### Difference of Fit Between Delta 08 and Delta 06 #####
# Find relative difference from data fit at delta 08 relative to delta 06
diff.fit <- spec.fit
diff.fit$spec <- (abs(spec.fit$spec) - abs(spec.fit1$spec))

# Graph 
gg_diff <- ggplot(diff.fit, aes(x = c, y = threshSlope, fill = spec)) +
  geom_tile() +
  theme_bw() +
  geom_hline(mapping = aes(yintercept = 2), 
             linetype = "dashed", size = 0.3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradient2(name = "Improvement\nin Fit",
                       low = "#4d9221", 
                       mid = "white", 
                       high = "#c51b7d", 
                       midpoint = 0, 
                       limits = c(-0.5, 0.5), 
                       oob = squish) +
  xlab(expression(c)) +
  ylab(expression(eta)) +
  theme(legend.position = c(0.95, 0.95), 
        legend.justification = c(1, 1),
        legend.background = element_rect(color = "black", size = 0.2, linetype = "solid"),
        legend.title = element_text(size = 4, face = "bold"),
        legend.key.size = unit(0.2, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 4),
        title = element_text(size = 4),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("output/ParameterExploration/Plot/Self_DifferenceOfFit.png", width = 2, height = 2, units = "in", dpi = 300)


png("output/ParameterExploration/Plot/SelfDiffOfFitDelta08Comparison.png", width = 4, height = 2, units = "in", res = 300)
multiplot(gg_fit, gg_diff, cols = 2)
dev.off()
