################################################################################
#
# Model incorporating both thresholds and network dynamics
#
################################################################################
rm(list = ls())

source("scripts/__Util__MASTER.R")
source("scripts/3A_PrepPlotExperimentData.R")
load("/Users/ChrisTokita/Documents/Research/Tarnita Lab/Evolution of DOL/Fixed_Delta06Sigma01Eta7LargerGroups100reps.Rdata")

taskSteps <- do.call('rbind', groups_taskStep)
taskSteps <- do.call('rbind', taskSteps)


corrSum1 <- Corrs1 %>% 
  group_by(n, Set) %>% 
  summarise(MeanSpec = mean(Specialization, na.rm = TRUE))


corrSum <- Corrs %>% 
  group_by(n, Set) %>% 
  summarise(MeanSpec = mean(Specialization, na.rm = TRUE))

gg_corrs <- ggplot(data = corrSum, aes(x = as.factor(n), y = MeanSpec)) +
  geom_hline(data = Corrs, 
             aes(yintercept = 0),
             colour = "grey30",
             alpha = 0.5) +
  geom_point() +
  theme_classic() +
  ylab("Specialization") +
  xlab("Group Size") +
  ggtitle(paste("100,000 Timesteps,\nSigma =", sigma[1])) +
  scale_y_continuous(limits = c(-0.5, 1))
gg_corrs

gg_corrs1 <- ggplot(data = corrSum1, aes(x = as.factor(n), y = MeanSpec)) +
  geom_hline(data = Corrs, 
             aes(yintercept = 0),
             colour = "grey30",
             alpha = 0.5) +
  geom_point() +
  theme_classic() +
  ylab("Specialization") +
  xlab("Group Size") +
  ggtitle(paste("10,000 Timesteps,\nSigma =", sigma[1])) +
  scale_y_continuous(limits = c(-0.5, 1))
gg_corrs1


corrTot <- corrSum %>% 
  group_by(n) %>% 　
  summarise(Spec = mean(MeanSpec),
            SpecSE = sd(MeanSpec)/sqrt(5))

corrTot1 <- corrSum1 %>% 
  group_by(n) %>% 　
  summarise(Spec = mean(MeanSpec),
            SpecSE = sd(MeanSpec)/sqrt(5))


