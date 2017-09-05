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

##### Delta 06 #####
# load
load("output/ParameterExploration/Rdata/FixedDelta06_SigmaSlopeExplorationEXTRA.Rdata")
improve1 <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3,
         fitRMSE = sqrt( (relativeLarge^2 + relativeSmall^2 + relativeSlope^2) / 3 ))

load("output/ParameterExploration/Rdata/FixedDelta06_SigmaSlopeExplorationEXTRA2.Rdata")
improve2 <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3,
         fitRMSE = sqrt( (relativeLarge^2 + relativeSmall^2 + relativeSlope^2) / 3 ))

load("output/ParameterExploration/Rdata/FixedDelta06_SigmaSlopeExploration.Rdata")
improve <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3,
         fitRMSE = sqrt( (relativeLarge^2 + relativeSmall^2 + relativeSlope^2) / 3 ))

improve06 <- rbind(improve, improve1, improve2)
rm(improve, improve1, improve2)

# Filter to size
improve06 <- improve06 %>% 
  filter(!sigma %in% c(0.075, 0.125, 0.175, 0.225, 0.275, 0.325))


##### Delta 08 #####
# load
load("output/ParameterExploration/Rdata/FixedDelta08_SigmaSlopeExplorationEXTRA.Rdata")
improve1 <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3,
         fitRMSE = sqrt( (relativeLarge^2 + relativeSmall^2 + relativeSlope^2) / 3 ))

load("output/ParameterExploration/Rdata/FixedDelta08_SigmaSlopeExplorationEXTRA2.Rdata")
improve2 <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3,
         fitRMSE = sqrt( (relativeLarge^2 + relativeSmall^2 + relativeSlope^2) / 3 ))

load("output/ParameterExploration/Rdata/FixedDelta08_SigmaSlopeExploration.Rdata")
improve <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3,
         fitRMSE = sqrt( (relativeLarge^2 + relativeSmall^2 + relativeSlope^2) / 3 ))

improve08 <- rbind(improve, improve1, improve2)
rm(improve, improve1, improve2)

# Filter to size
improve08 <- improve08 %>% 
  filter(!sigma %in% c(0.075, 0.125, 0.175, 0.225, 0.275, 0.325))


# Exp data: % increase = 1.220554
# Exp data: Slope = 0.02322321
# Exp data: n16 = 0.5915000
# Exp data: n2 = 0.2663750
# Exp data: Increase = 0.325125

#######################################################
# Main Text Figures
#######################################################

##### Absolute Slope #####
# Colors
myPalette <- colorRampPalette(brewer.pal(6, "YlOrRd"))
colPal <- c(myPalette(6), "#800026")

# Fit surface
spec.loess <- loess(Increase ~ sigma * threshSlope, data = improve06, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(sigma = seq(0, max(improve06$sigma), (max(improve06$sigma) - min(improve06$sigma)) / 1000), 
                             threshSlope = seq(1, max(improve06$threshSlope), (max(improve06$threshSlope) - min(improve06$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)
spec.fit$spec <- as.numeric(z)

# Graph 
gg_abslope <- ggplot() +
  geom_raster(data = improve06, 
              aes(x = sigma, 
                  y = threshSlope, 
                  fill = Increase)) +
  stat_contour(data = spec.fit,
               aes(x = sigma,
                    y = threshSlope,
                    z = spec),
               size = 0.3,
               alpha = 1,
               colour = "white",
               breaks = c(0.2926124,  0.3576374)) +
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

gg_abslope

ggsave(plot = gg_abslope, filename = "output/ParameterExploration/Plot/Delta06_Heatabsoluteslope.png", width = 2.7, height = 2, units = "in", dpi = 600)


#######################################################
# SI Figures
#######################################################

# Colors
myPalette <- colorRampPalette(brewer.pal(6, "YlOrRd"))
colPal <- c(myPalette(6), "#800026")

##### Relative Slope #####
# Fit surface
spec.loess <- loess(relativeSlope ~ sigma * threshSlope, data = improve06, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(sigma = seq(0, max(improve06$sigma), (max(improve06$sigma) - min(improve06$sigma)) / 1000), 
                             threshSlope = seq(1, max(improve06$threshSlope), (max(improve06$threshSlope) - min(improve06$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)

spec.fit$spec <- as.numeric(z)

# start building large dataframe for calculating total fit
totalfit <- spec.fit[ , 1:2]
totalfit$slope.fit <- spec.fit$spec
# Find which z are within range 0.95 - 1.05? (relative to data)

myPalette <- colorRampPalette(rev(brewer.pal(9, "RdBu")))
colPal <- myPalette(9)

# Graph 
gg_relslope <- ggplot() +
  geom_raster(data = improve06, 
              aes(x = sigma, 
                  y = threshSlope, 
                  fill = relativeSlope)) +
  stat_contour(data = spec.fit,
               aes(x = sigma,
                   y = threshSlope,
                   z = spec),
               size = 0.4,
               alpha = 1,
               colour = "black",
               breaks = c(-0.1,  0.1)) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(1, seq(10, 30, 10))) +
  scale_fill_gradientn(name = "Fit to Data",
                       colors = colPal,
                       #values = rescale(x = c(0, 0.01, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.6)),
                       breaks = seq(-1, 1, 0.5),
                       colours = colPal,
                       limits = c(-1, 1),
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
        axis.title = element_text(size = 12),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1))


##### N = 2 #####
# Fit surface
spec.loess <- loess(relativeSmall ~ sigma * threshSlope, data = improve06, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(sigma = seq(0, max(improve06$sigma), (max(improve06$sigma) - min(improve06$sigma)) / 1000), 
                             threshSlope = seq(1, max(improve06$threshSlope), (max(improve06$threshSlope) - min(improve06$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)
spec.fit$spec <- as.numeric(z)

# Graph 
gg_small <- ggplot() +
  geom_raster(data = improve06, 
              aes(x = sigma, 
                  y = threshSlope, 
                  fill = relativeSmall)) +
  stat_contour(data = spec.fit,
               aes(x = sigma,
                   y = threshSlope,
                   z = spec),
               size = 0.4,
               alpha = 1,
               colour = "black",
               breaks = c(-0.1,  0.1)) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(1, seq(10, 30, 10))) +
  scale_fill_gradientn(name = "Fit to Data",
                       colors = colPal,
                       #values = rescale(x = c(0, 0.01, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.6)),
                       breaks = seq(-1, 1, 0.5),
                       colours = colPal,
                       limits = c(-1, 1),
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
        axis.title = element_text(size = 12),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1))


##### N = 16 #####
# Fit surface
spec.loess <- loess(relativeLarge ~ sigma * threshSlope, data = improve06, degree = 2, span = 0.1)
spec.fit <- expand.grid(list(sigma = seq(0, max(improve06$sigma), (max(improve06$sigma) - min(improve06$sigma)) / 1000), 
                             threshSlope = seq(1, max(improve06$threshSlope), (max(improve06$threshSlope) - min(improve06$threshSlope)) / 1000)))
z <- predict(spec.loess, newdata = spec.fit)
spec.fit$spec <- as.numeric(z)

# Graph 
gg_large <- ggplot() +
  geom_raster(data = improve06, 
              aes(x = sigma, 
                  y = threshSlope, 
                  fill = relativeLarge)) +
  stat_contour(data = spec.fit,
               aes(x = sigma,
                   y = threshSlope,
                   z = spec),
               size = 0.4,
               alpha = 1,
               colour = "black",
               breaks = c(-0.1,  0.1)) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(1, seq(10, 30, 10))) +
  scale_fill_gradientn(name = "Fit to Data",
                       colors = colPal,
                       #values = rescale(x = c(0, 0.01, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.6)),
                       breaks = seq(-1, 1, 0.5),
                       colours = colPal,
                       limits = c(-1, 1),
                       oob = squish) +
  xlab(expression(sigma)) +
  ylab(expression(eta)) +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.key.height = unit(0.6, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1))




##### Plot All Relative Plots #####
# output
# png("output/ParameterExploration/Plot/Delta06_SlopeandSize.png", width = 6, height = 2, units = "in", res = 800)
# multiplot(gg_relslope, gg_small, gg_large, cols = 3)
# dev.off()

png("output/ParameterExploration/Plot/Delta06_SlopeandSizeAB.png", width = 4, height = 2, units = "in", res = 800)
multiplot(gg_relslope, gg_small, cols = 2)
dev.off()

ggsave(plot = gg_large,"output/ParameterExploration/Plot/Delta06_SlopeandSizeCwithLegend.png", width = 2, height = 2, units = "in", dpi = 800)

##### Total Fit - Delta 06 #####
# Try custom color
myPalette <- colorRampPalette((brewer.pal(9, "BuPu")))
colPal <- c("#ffffff", myPalette(9))

# Graph 
gg_fit06 <- ggplot(improve06, aes(x = sigma, y = threshSlope, fill = fitRMSE)) +
  geom_raster() +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(1, seq(10, 30, 10))) +
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
  theme(legend.position = "none", 
        legend.title = element_text(size = 7),
        legend.key.height = unit(0.8, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1))
gg_fit06

ggsave("output/ParameterExploration/Plot/Delta06_fitRMSE.png", width = 2, height = 2, units = "in", dpi = 800)


##### Total Fit - Delta 08 #####
# Graph 
gg_fit08 <- ggplot(improve08, aes(x = sigma, y = threshSlope, fill = fitRMSE)) +
  geom_raster() +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(1, seq(10, 30, 10))) +
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
  theme(legend.position = "none", 
        legend.title = element_text(size = 10),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1))
gg_fit08


ggsave("output/ParameterExploration/Plot/Delta08_fitRMSE.png", width = 2, height = 2, units = "in", dpi = 800)


##### Change in Fit #####
diff_improve <- improve08 %>% 
  arrange(sigma, threshSlope)
improve06 <- improve06 %>% 
  arrange(sigma, threshSlope)
diff_improve$fit <- diff_improve$fitRMSE - improve06$fitRMSE

gg_diff <- ggplot(diff_improve, aes(x = sigma, y = threshSlope, fill = fit)) +
  geom_raster() +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(1, seq(10, 30, 10))) +
  scale_fill_gradient2(name = "Change\nin Fit",
                       low = "#4d9221", 
                       mid = "white", 
                       high = "#c51b7d", 
                       midpoint = 0, 
                       limits = c(-0.5, 0.5), 
                       oob = squish) +
  xlab(expression(sigma)) +
  ylab(expression(eta)) +
  theme(legend.position = "none", 
        legend.title = element_text(size = 10),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "cm"),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        axis.ticks = element_line(size = 0.5),
        panel.border = element_rect(fill = NA, size = 1))
gg_diff

ggsave("output/ParameterExploration/Plot/Fixed_0806DifferenceOfFitRSME.png", width = 2, height = 2, units = "in", dpi = 800)
