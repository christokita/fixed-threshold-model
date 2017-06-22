##################################################
#
# Threshold Probability of Performance 
#
##################################################


####################
# Seed task thresholds
####################
seedThresholds <- function(n, m, ThresholdMeans = NULL, ThresholdSDs = NULL) {
  # Loop through tasks and sample thresholds from normal dist
  threshMat <- lapply(1:length(ThresholdMeans), function(i) {
    threshList <- rtnorm(n = n, 
                         mean = ThresholdMeans[i], 
                         sd = ThresholdSDs[i], 
                         lower = 0)
    return(threshList)
  })
  threshMat <- do.call("cbind", threshMat)
  # Fix names
  colnames(threshMat) <- paste0("Thresh", 1:length(ThresholdMeans))
  rownames(threshMat) <- paste0("v-", 1:n)
  # Return
  return(threshMat)
}


####################
# Seed Exhaustion thresholds
####################
seedExhaustThresh <- function(n, ExhaustMean, ExhaustSD) {
  # Loop through individuals
  exhaustVector <- rtnorm(n = n, 
                          mean = ExhaustMean, 
                          sd = ExhaustSD, 
                          lower = 0)
  # Turn to matrix and return
  exhaustMat <- matrix(exhaustVector)
  colnames(exhaustMat) <- "ExhaustThresh"
  rownames(exhaustMat) <- paste0("v-", 1:n)
  # Return
  return(exhaustMat)
}


####################
# Threshold function
####################
threshProb <- function(s, phi, nSlope) {
  T_vi <- (s^nSlope) / (s^nSlope + phi^nSlope)
}


####################
# Output Threshold Demands
####################
calcThresholdProbMat <- function(TimeStep, ThresholdMatrix, StimulusMatrix, nSlope) {
  # select proper stimulus for this time step
  stimulusThisStep <- StimulusMatrix[TimeStep, ]
  # calculate threshold probabilities for one individual
  thresholdP <- lapply(1:nrow(ThresholdMatrix), function(i) {
    # select row for individual in threshold matrix
    indThresh <- ThresholdMatrix[i, ]
    # create task vector to be output and bound
    taskThresh <- rep(NA, length(indThresh))
    # loop through each task within individual
    for (j in 1:length(taskThresh)) {
      taskThresh[j] <- threshProb(s = stimulusThisStep[j], phi = indThresh[j], nSlope = nSlope)
    }
    return(taskThresh)
  })
  # bind
  thresholdP <- do.call("rbind", thresholdP)
  # # normalize NOTE: DISCUSS THIS ASSUMPTION!!!!
  # thresholdP <- thresholdP / rowSums(thresholdP)
  # fix names and return
  colnames(thresholdP) <- paste0("ThreshProb", 1:ncol(thresholdP))
  rownames(thresholdP) <- paste0("v-", 1:nrow(thresholdP))
  return(thresholdP)
}

####################
# Exhaustion Threshold Function
####################
calcExhaustDemand <- function(ExhaustStim, ExhaustThreshVector, nSlope) {
  # Create output matrix
  exhaustUpdate <- matrix(rep(NA, length(ExhaustStim)), ncol = 1)
  colnames(exhaustUpdate) <- "ExhaustProb"
  rownames(exhaustUpdate) <- paste0("v-", 1:length(ExhaustStim))
  # Loop through individuals
  for (i in 1:length(exhaustUpdate)) {
    exhaustUpdate[i] <- threshProb(s = ExhaustStim[i], phi = ExhaustThreshVector[i], nSlope = nSlope)
  }
  # Return
  return(exhaustUpdate)
}

####################
# Self-reinforcement of Threshold
####################
adjustThresholds <- function(ThresholdMatrix, X_sub_g, phi, phiMult, lowerThresh, upperThresh) {
  for (i in 1:nrow(X_sub_g)) {
    for (j in 1:ncol(X_sub_g)) {
      adjust <- ((1 - X_sub_g[i, j]) * phi - X_sub_g[i, j] * phi * phiMult)
      ThresholdMatrix[i, j] <- ThresholdMatrix[i, j] + adjust
      if (ThresholdMatrix[i, j] < lowerThresh) {
        ThresholdMatrix[i, j] <- lowerThresh
      } else if (ThresholdMatrix[i, j] > upperThresh) {
        ThresholdMatrix[i, j] <- upperThresh
      }
    }
  }
  return(ThresholdMatrix)
}

####################
# Self-reinforcement of Threshold
####################
adjustThresholdsSocial <- function(ThresholdMatrix, X_sub_g, phi, phiMult, SocialNetwork, lowerThresh, upperThresh) {
  # Calculate "sum" of task states/probs of neighbors
  NeighborSums <- SocialNetwork %*% X_sub_g
  # Loop through individuals
  for (i in 1:nrow(X_sub_g)) {
    for (j in 1:ncol(X_sub_g)) {
      adjust <- (((1 - X_sub_g[i, j]) * phi) - (X_sub_g[i, j] * phi * (phiMult + c * NeighborSums[i, j])))
      ThresholdMatrix[i, j] <- ThresholdMatrix[i, j] + adjust
      if (ThresholdMatrix[i, j] < lowerThresh) {
        ThresholdMatrix[i, j] <- lowerThresh
      } else if (ThresholdMatrix[i, j] > upperThresh) {
        ThresholdMatrix[i, j] <- upperThresh
      }
    }
  }
  return(ThresholdMatrix)
}
