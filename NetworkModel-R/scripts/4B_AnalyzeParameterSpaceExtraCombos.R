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
load("output/ParameterExploration/Rdata/FixedDelta08_SigmaSlopeExplorationEXTRA.Rdata")
improve1 <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  filter(sigma != 0.35) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3)

load("output/ParameterExploration/Rdata/FixedDelta08_SigmaSlopeExplorationEXTRA2.Rdata")
improve2 <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3)

load("output/ParameterExploration/Rdata/FixedDelta08_SigmaSlopeExploration.Rdata")
improve <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3)

improve <- rbind(improve, improve1, improve2)
rm(improve1, improve2)

# Filter to size
improve <- improve %>% 
  filter(!sigma %in% c(0.075, 0.125, 0.175, 0.225, 0.275, 0.325))

# Set file names
filename <- "Delta08"

# Exp data: % increase = 1.220554
# Exp data: Slope = 0.02322321
# Exp data: n16 = 0.5915000
# Exp data: n2 = 0.2663750
# Exp data: Increase = 0.325125

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

# Try custom color
# colPal <- c("#810f7c", "#8856a7", "#8c96c6", "#b3cde3", "#ffffff")
myPalette <- colorRampPalette(brewer.pal(6, "YlOrRd"))
colPal <- c(myPalette(6), "#800026")

# Graph 
gg_abslope <- ggplot(spec.fit, aes(x = sigma, y = threshSlope, fill = spec)) +
  geom_raster() +
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
  geom_raster() +
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
# Heatmap style
#######################################################
# Colors
myPalette <- colorRampPalette(brewer.pal(6, "YlOrRd"))
colPal <- c(myPalette(6), "#800026")

# Graph 
gg_abslopeHeat <- ggplot() +
  geom_raster(data = improve, 
              aes(x = sigma, 
                  y = threshSlope, 
                  fill = fit)) +
  # stat_contour(data = spec.fit,
  #              aes(x = sigma, 
  #                   y = threshSlope,
  #                   z = spec),
  #              size = 0.25,
  #              alpha = 1,
  #              colour = "white",
  #              breaks = c(0.2926124,  0.3576374)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.00, 0)) +
  scale_y_continuous(expand = c(0.00, 0), breaks = c(0, 2, seq(10, 30, 10))) +
  scale_fill_gradientn(name = "Specialization\nIncrease",
                       colors = colPal,
                       breaks = seq(0, 0.5, 0.1),
                       colours = colPal,
                       limits = c(0, 0.5),
                       oob = squish) +
  scale_colour_gradientn(name = "Specialization\nIncrease",
                       colors = colPal,
                       breaks = seq(0, 0.5, 0.1),
                       colours = colPal,
                       limits = c(0, 0.5),
                       oob = squish) +
  xlab(expression(sigma)) +
  ylab(expression(eta)) +
  theme(legend.position = "none", 
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

gg_abslopeHeat

ggsave(plot = gg_abslopeHeat, filename = paste0("output/ParameterExploration/Plot/", filename, "_Heatabsoluteslope.png"), width = 2.7, height = 2, units = "in", dpi = 600)



