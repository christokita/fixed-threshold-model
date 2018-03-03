################################################################################
#
# Compare distributions of model vs experiment for specilization
#
################################################################################
rm(list = ls())
source("scripts/__Util__MASTER.R")
source("scripts/3A_PrepPlotExperimentData.R")
load("output/__RData/ProbSocialInhibit__Sum_Delta06Sigma00Phi001c02d02_connectP04Bias2.Rdata")
load("output/__RData/ProbSocial__Sum_Delta06Sigma00Phi001c015d015_connectP04Bias1.Rdata")
load("output/__RData/Fixed_10_Sigma03.Rdata")

####################
# Task Rank Correlation
####################
# Unlist
taskCorrTot <- do.call("rbind", groups_taskCorr)
taskCorrTot <- taskCorrTot %>% 
  mutate(TaskMean = (Task1 + Task2) / 2)

# Manipulate and bind with Yuko data
taskCorrTot <- taskCorrTot %>% 
  mutate(Source = "Model") %>% 
  select(n, TaskMean, Source) %>% 
  rbind(yukoCorr) %>% 
  mutate(Source = as.factor(Source))


####################
# try fitting kernels
####################
# Split
modData <- taskCorrTot %>% 
  filter(Source == "Model")
expData <- taskCorrTot %>% 
  filter(Source == "Experiment")

# Fit kernels
ns <- unique(taskCorrTot$n)

exp.likelihood <- lapply(ns, function(x) {
  # Filter to size of interest
  data <- modData %>% 
    filter(n == x)
  obs <- expData %>% 
    filter(n == x)
  # Get density
  dens <- density(data$TaskMean)
  # Make density function
  dens.func <- approxfun(dens, rule = 2)
  # Estimate proability of observed points via integration
  probObserved <- lapply(1:nrow(obs), function(i) {
    area <- integrate(dens.func, 
                      lower = obs$TaskMean[i] - dens$bw,
                      upper = obs$TaskMean[i] + dens$bw)
    return(area$value)
  })
  # Calcualte total likelihood
  probObserved <- prod(unlist(probObserved))
  return(probObserved)
})
exp.likelihood <- unlist(exp.likelihood)
exp.likelihood <- prod(exp.likelihood)
exp.likelihood <- log(exp.likelihood, base = 10)


####################
# plot by group size
####################
# Palette
compPalette <- c("indianred2", "black")

# density
gg_specFit <- ggplot(data = taskCorrTot) +
  geom_density(aes(x = TaskMean, fill = Source), 
               alpha = 0.5,
               color = NA) +
  theme_classic() +
  scale_fill_manual(values = compPalette) +
  facet_grid(. ~ n) +
  xlab("Specialization")
gg_specFit

multiplot(gg_specFit1, gg_specFit2, gg_specFit, cols = 1)

# violin
gg_violin <- ggplot(data = taskCorrTot, aes(x = as.factor(n), y = TaskMean, fill = Source)) +
  geom_violin( position = position_dodge(width = 0.75),
               alpha = 0.5,
               color = NA) +
  theme_classic() +
  scale_fill_manual(values = compPalette) +
  xlab("Group Size")
gg_violin
