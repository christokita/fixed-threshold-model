################################################################################
#
# Playing with Yuko's specialization data
#
################################################################################
rm(list = ls())

source("scripts/__Util__MASTER.R")
source("scripts/3A_PrepPlotExperimentData.R")
library(R.matlab)


yukoCorr <- readMat("data/GSSumStats.mat")
