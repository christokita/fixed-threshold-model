################################################################################
#
# Test the specilization fit (% increase) over sigma and n-slope parameter space
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")
library(scales)
library(RColorBrewer)

####################
# Prep and Plot
####################
# load
load("output/ParameterExploration/Rdata/FixedDelta06_SigmaSlopeExploration.Rdata")
improve <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3)

# Set file names
filename <- "FixedDelta08"

# Exp data: % increase = 1.220554
# Exp data: Slope = 0.02322321
# Exp data: n16 = 0.5915000
# Exp data: n2 = 0.2663750

#######################################################
# Figures for main text
#######################################################

##### Absolute Slope #####
# Fit surface
spec.loess <- loess(Increase ~ sigma * threshSlope, data = improve, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(sigma = seq(0, max(improve$sigma), (max(improve$sigma) - min(improve$sigma)) / 1000), 
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)

spec.fit$spec <- as.numeric(z)

# start building large dataframe for calculating total fit
totalfit <- spec.fit[ , 1:2]
totalfit$slope.fit <- spec.fit$spec
# Find which z are within range 0.95 - 1.05? (relative to data)

# Try custom color
# colPal <- c("#810f7c", "#8856a7", "#8c96c6", "#b3cde3", "#ffffff")
myPalette <- colorRampPalette(brewer.pal(6, "YlOrRd"))
colPal <- c(myPalette(6), "#800026")

# Graph 
gg_abslope <- ggplot(spec.fit, aes(x = sigma, y = threshSlope, fill = spec)) +
  geom_tile() +
  stat_contour(aes(z = spec), 
               size = 0.25,
               alpha = 1,
               colour = "white",
               breaks = c(0.2926124,  0.3576374)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.005, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradientn(name = "Specialization\nIncrease",
                       colors = colPal,
                       breaks = seq(0, 0.5, 0.1),
                       colours = colPal,
                       limits = c(0, 0.5),
                       oob = squish) +
  xlab(expression(sigma)) +
  ylab(expression(eta)) +
  theme(legend.position = "right", 
        legend.title = element_text(size = 7),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1))

ggsave(plot = gg_abslope, filename = paste0("output/ParameterExploration/Plot/", filename, "_absoluteslope.png"), width = 2.9, height = 2, units = "in", dpi = 600)

##### Relative Slope #####
# Fit surface
spec.loess <- loess(relativeSlope ~ sigma * threshSlope, data = improve, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(sigma = seq(0, max(improve$sigma), (max(improve$sigma) - min(improve$sigma)) / 1000), 
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)

spec.fit$spec <- as.numeric(z)

# start building large dataframe for calculating total fit
totalfit <- spec.fit[ , 1:2]
totalfit$slope.fit <- spec.fit$spec
# Find which z are within range 0.95 - 1.05? (relative to data)

myPalette <- colorRampPalette(rev(brewer.pal(9, "RdBu")))
colPal <- myPalette(9)

# Graph 
gg_slope <- ggplot(spec.fit, aes(x = sigma, y = threshSlope, fill = spec)) +
  geom_tile() +
  stat_contour(aes(z = spec), 
               size = 0.15,
               alpha = 1,
               colour = "black",
               breaks = c(-0.1, 0.1)) +
  theme_bw() +
  geom_hline(mapping = aes(yintercept = 2), 
             linetype = "dashed", size = 0.3) +
  scale_x_continuous(expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.005, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradientn(name = "Fit to Data",
                       colors = colPal,
                       #values = rescale(x = c(0, 0.01, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.6)),
                       breaks = seq(-0.5, 0.5, 0.25),
                       colours = colPal,
                       limits = c(-0.5, 0.5),
                       oob = squish) +
  xlab(expression(sigma)) +
  ylab(expression(eta)) +
  theme(legend.position = "right", 
        legend.title = element_text(size = 10),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1))

ggsave(paste0("output/ParameterExploration/Plot/", filename, "_relslope.png"), width = 2.9, height = 2, units = "in", dpi = 800)



#######################################################
# Figures for SI
#######################################################

##### Relative Slope #####
# Fit surface
spec.loess <- loess(relativeSlope ~ sigma * threshSlope, data = improve, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(sigma = seq(0, max(improve$sigma), (max(improve$sigma) - min(improve$sigma)) / 1000), 
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)

spec.fit$spec <- as.numeric(z)

# start building large dataframe for calculating total fit
totalfit <- spec.fit[ , 1:2]
totalfit$slope.fit <- spec.fit$spec
# Find which z are within range 0.95 - 1.05? (relative to data)

myPalette <- colorRampPalette(rev(brewer.pal(9, "RdBu")))
colPal <- myPalette(9)

# Graph 
gg_slope <- ggplot(spec.fit, aes(x = sigma, y = threshSlope, fill = spec)) +
  geom_tile() +
  stat_contour(aes(z = spec), 
               size = 0.25,
               alpha = 1,
               colour = "black",
               breaks = c(-0.1, 0.1)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.005, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradientn(name = "Fit to Data",
                       colors = colPal,
                       #values = rescale(x = c(0, 0.01, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.6)),
                       breaks = seq(-0.5, 0.5, 0.25),
                       colours = colPal,
                       limits = c(-0.5, 0.5),
                       oob = squish) +
  xlab(expression(sigma)) +
  ylab(expression(eta)) +
  theme(legend.position = "none",
        legend.justification = c(1, 1),
        legend.background = element_rect(color = "black", size = 0.2, linetype = "solid"),
        legend.title = element_text(size = 4, face = "bold"),
        legend.key.size = unit(0.15, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 3),
        title = element_text(size = 4),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))


##### N = 2 #####
# Fit surface
spec.loess <- loess(relativeSmall ~ sigma * threshSlope, data = improve, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(sigma = seq(0, max(improve$sigma), (max(improve$sigma) - min(improve$sigma)) / 1000), 
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)

spec.fit$spec <- as.numeric(z)


# start building large dataframe for calculating total fit
totalfit$small.fit <- spec.fit$spec
# Find which z are within range 0.95 - 1.05? (relative to data)

# Normalize spec values for coloring
maxVal <- max(spec.fit$spec)
vals <- c(0 / maxVal, 0.5 / maxVal, 0.9 / maxVal, 1.1 / maxVal, 1.5 / maxVal, 2.0 / maxVal, maxVal)

# Graph 
gg_small <- ggplot(spec.fit, aes(x = sigma, y = threshSlope, fill = spec)) +
  geom_tile() +
  stat_contour(aes(z = spec), 
               size = 0.25,
               alpha = 1,
               colour = "black",
               breaks = c(-0.1, 0.1)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.005, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradientn(name = "Fit to Data",
                       colors = colPal,
                       #values = rescale(x = c(0, 0.01, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.6)),
                       breaks = seq(-0.5, 0.5, 0.25),
                       colours = colPal,
                       limits = c(-0.5, 0.5),
                       oob = squish) +
  xlab(expression(sigma)) +
  ylab(expression(eta)) +
  theme(legend.position = "none",
        legend.justification = c(1, 1),
        legend.background = element_rect(color = "black", size = 0.2, linetype = "solid"),
        legend.title = element_text(size = 4, face = "bold"),
        legend.key.size = unit(0.15, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 3),
        title = element_text(size = 4),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))


##### N = 16 #####
# Fit surface
spec.loess <- loess(relativeLarge ~ sigma * threshSlope, data = improve, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(sigma = seq(0, max(improve$sigma), (max(improve$sigma) - min(improve$sigma)) / 1000), 
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)

spec.fit$spec <- as.numeric(z)

# start building large dataframe for calculating total fit
totalfit$large.fit <- spec.fit$spec
# Find which z are within range 0.95 - 1.05? (relative to data)
# Normalize spec values for coloring
maxVal <- max(spec.fit$spec)
vals <- c(0 / maxVal, 0.5 / maxVal, 0.9 / maxVal, 1.1 / maxVal, 1.5 / maxVal, 2.0 / maxVal, maxVal)

# Graph 
gg_large <- ggplot(spec.fit, aes(x = sigma, y = threshSlope, fill = spec)) +
  geom_tile() +
  stat_contour(aes(z = spec), 
               size = 0.25,
               alpha = 1,
               colour = "black",
               breaks = c(-0.1, 0.1)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.005, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradientn(name = "Fit to Data",
                       colors = colPal,
                       #values = rescale(x = c(0, 0.01, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.6)),
                       breaks = seq(-0.5, 0.5, 0.25),
                       colours = colPal,
                       limits = c(-0.5, 0.5),
                       oob = squish) +
  xlab(expression(sigma)) +
  ylab(expression(eta)) +
  theme(legend.position = "right",
        # legend.justification = c(1, 1),
        legend.background = element_rect(color = "black", size = 0.2, linetype = "solid"),
        legend.title = element_text(size = 4, face = "bold"),
        legend.key.size = unit(0.15, "cm"),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 3),
        title = element_text(size = 4),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))




##### Plot #####
# output
png(paste0("output/ParameterExploration/Plot/", filename, "_SlopeandSize.png"), width = 6, height = 2, units = "in", res = 800)
multiplot(gg_slope, gg_small, gg_large, cols = 3)
dev.off()


##### Fit #####
# Fit surface
spec.loess <- loess(fit ~ sigma * threshSlope, data = improve, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(sigma = seq(0, max(improve$sigma), (max(improve$sigma) - min(improve$sigma)) / 1000),
                             threshSlope = seq(1, max(improve$threshSlope), (max(improve$threshSlope) - min(improve$threshSlope)) / 1000)))
# spec.fit <- expand.grid(list(sigma = seq(0, 0.5, 0.01),
#                              threshSlope = seq(1, 30, 0.5)))

z <- predict(spec.loess, newdata = spec.fit)

spec.fit$spec <- as.numeric(z)

# start building large dataframe for calculating total fit
# Find which z are within range 0.95 - 1.05? (relative to data)

# Try custom color
# colPal <- c("#810f7c", "#8856a7", "#8c96c6", "#b3cde3", "#ffffff")
myPalette <- colorRampPalette((brewer.pal(9, "BuPu")))
colPal <- c("#ffffff", myPalette(9))

# Graph 
gg_fit <- ggplot(spec.fit, aes(x = sigma, y = threshSlope, fill = spec)) +
  geom_tile() +
  # stat_contour(aes(z = spec), 
  #              size = 0.1,
  #              alpha = 0.7,
  #              colour = "black") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradientn(name = "Overall\nFit",
                       colors = colPal,
                       #values = c(0, 0.5, 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c("Best", 0.25, 0.5, 0.75, "Worst"),
                       colours = colPal,
                       limits = c(0, 1),
                       oob = squish) +
  xlab(expression(sigma)) +
  ylab(expression(eta)) +
  theme(legend.position = "right", 
        legend.title = element_text(size = 10),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1))


ggsave(paste0("output/ParameterExploration/Plot/", filename, "_fit.png"), width = 2.8, height = 2, units = "in", dpi = 600)


##### Difference of Fit Between Delta 08 and Delta 06 #####
# Find relative difference from data fit at delta 08 relative to delta 06
diff.fit <- spec.fit
diff.fit$spec <- (abs(spec.fit$spec) - abs(spec.fit1$spec))

# Graph 
gg_diff <- ggplot(diff.fit, aes(x = sigma, y = threshSlope, fill = spec)) +
  geom_tile() +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradient2(name = "Improvement\nin Fit",
                       low = "#4d9221", 
                       mid = "white", 
                       high = "#c51b7d", 
                       midpoint = 0, 
                       limits = c(-0.5, 0.5), 
                       oob = squish) +
  xlab(expression(sigma)) +
  ylab(expression(eta)) +
  theme(legend.position = "right", 
        legend.title = element_text(size = 8),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1))

ggsave("output/ParameterExploration/Plot/Fixed_0806DifferenceOfFit.png", width = 3, height = 2, units = "in", dpi = 600)


png("output/ParameterExploration/Plot/DifferenceOfFitDelta08Comparison.png", width = 5.5, height = 2, units = "in", res = 800)
multiplot(gg_fit, gg_diff, cols = 2)
dev.off()
