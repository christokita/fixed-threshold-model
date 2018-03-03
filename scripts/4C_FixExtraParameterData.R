################################################################################
#
# Fix parameter space data from extra runs
#
################################################################################
# In the runs from the extra parameter space exploration, 
# 0.35 does not equal 0.35 when typed (some sort of representation error).
# Therefore, I need to remove these values from the extra runs since it is already present
# in the original parameter exploration run. 

rm(list = ls())
source("scripts/__Util__MASTER.R")

####################
# Delta 08
####################
# load extra run 1
load("output/ParameterExploration/Rdata/FixedDelta08_SigmaSlopeExplorationEXTRA.Rdata")
improve1 <- improve %>% 
  mutate(relativePercInc = (PercIncrease - 1.220554) / 1.220554,
         relativeSlope   = (SlopeIncrease - 0.02322321) / 0.02322321, 
         relativeLarge   = (SpecLarge - 0.5915000) / 0.5915000,
         relativeSmall   = (SpecSmall - 0.2663750) / 0.2663750,
         Increase        = SlopeIncrease * 14) %>% 
  mutate(fit = (abs(relativeLarge) + abs(relativeSmall) + abs(relativeSlope)) / 3)

point35 <- improve1$sigma[26] # This indexing only works on the original set of data (see email)
improve1$sigma[improve1$sigma == point35] <- 0.35
improve <- improve1
save(improve, file = "output/ParameterExploration/Rdata/FixedDelta08_SigmaSlopeExplorationEXTRA.Rdata")
rm(improve, improve1)


