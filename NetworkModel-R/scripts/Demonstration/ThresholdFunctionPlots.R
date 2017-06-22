
rm(list = ls())
source("scripts/__Util__MASTER.R")
library(ggplot2)

####################
# Demonstrating thresholds
####################

# Thresh eta = 2
stim <- seq(0, 30, 0.1)
thresh2 <- data.frame(stim = stim, prob =  (stim^2) / (stim^2 + 10^2), eta = "2")

# Thresh eta = 7
thresh5 <- data.frame(stim = stim, prob =  (stim^7) / (stim^7 + 10^7), eta = "7")

# Thresh eta = 10
thresh10 <- data.frame(stim = stim, prob =  (stim^20) / (stim^20 + 10^20), eta = "20")

# Merge
threshStyles <- rbind(thresh2, thresh5, thresh10)

# Set label
threshold <- paste(expression(theta[ij]))

# Plot
gg_thresh <- ggplot(data = threshStyles, aes(x = stim, y = prob, colour = eta)) +
  geom_vline(aes(xintercept = 10), linetype = "dotted", size = 0.4) +
  geom_line(size = 0.5) +
  theme_bw() +
  xlab(expression(s[j])) +
  ylab(expression(P[ij])) +
  scale_y_continuous(limit = c(0, 1), breaks = seq(0, 1, 0.5), expand = c(0.1, 0)) +
  scale_colour_manual(values = c("black", "#3c7caa", "#a0c4de"), 
                      labels = c(expression(paste(eta, " = 2")),
                                 expression(paste(eta, " = 5")),
                                 expression(paste(eta, " = 20")))) +
  annotate("text", x = 12, y = 0.0, label = threshold, parse = TRUE, size = 3) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, size = 1),
        legend.position = c(0.96, 0.30),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width= unit(0.4, "cm"),
        legend.margin =  margin(t = 0, r = 0, b = 0, l = -0.1, "cm"),
        legend.text = element_text(size = 6),
        legend.text.align = 0,
        axis.text.x = element_text(size = 8),
        axis.title = element_text(size = 10))

gg_thresh

ggsave(file = "output/OtherFigures/ThresholdSlopeExamples.png", width = 2, height = 2, units = "in", dpi = 800)

####################
# Gaussian Functions
####################
x <- seq(0,20, length = 1000)

# Different sigmas
sigma005 <- data.frame(x = seq(0,20, length = 1000), 
                      y = dnorm(x, mean = 10, sd = 0.5),
                      sigma = "sigma == 0.05")
sigma01 <- data.frame(x = seq(0,20, length = 1000), 
                      y = dnorm(x, mean = 10, sd = 1),
                      sigma = "sigma == 0.1")
sigma03 <- data.frame(x = seq(0,20, length = 1000), 
                      y = dnorm(x, mean = 10, sd = 3),
                      sigma = "sigma == 0.3")


# Plot
sigmas <- rbind(sigma01, sigma03, sigma005)
sigmas$sigma <- factor(sigmas$sigma, levels = c("sigma == 0.05", "sigma == 0.1", "sigma == 0.3"))

gg_sig <- ggplot(data = sigmas, aes(x = x, y = y, group = sigma)) +
  geom_line(colour = "#74a9cf") +
  geom_polygon(aes(x, y),  fill = "#74a9cf") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.85)) +
  xlab(expression(theta[j])) +
  ylab("Frequency") +
  theme(axis.text.x = element_text(size = 8),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 8, face = "italic"),
        strip.background = element_rect(fill = NA, colour = NA)) +
  facet_grid(. ~ sigma, 
             labeller = label_parsed,
             scales = "free")
  
gg_sig

ggsave(file = "output/OtherFigures/SigmaDistributionExamples.png", width = 4, height = 2, units = "in", dpi = 800)


# Plot - type 2

gg_sig <- ggplot(data = sigmas, aes(x = x, y = y, colour = sigma, fill = sigma)) +
  geom_line() +
  # geom_polygon(alpha = 0.25) +
  theme_classic() +
  xlab(expression(theta[j])) +
  ylab("Frequency") +
  theme(axis.text.x = element_text(size = 8),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 8, face = "italic"),
        strip.background = element_rect(fill = NA, colour = NA)) 

gg_sig
