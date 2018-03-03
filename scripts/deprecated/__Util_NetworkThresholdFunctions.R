#
# Functions for the Network-Threshold Model
#
rm(list = ls())

# Required libraries
require(igraph)
require(msm)
source("scripts/__Util_ForceAtlas2.R")

####################
# Social Network Functions
####################
# Generate a social network
seedSocialNetwork <- function(n, outDegree, networkPower) {
  # Scale free network
  g <- barabasi.game(n = n, m = outDegree, power = networkPower, directed = FALSE)
  # Plot preview of network
  layout <- layout.forceatlas2(g, 
                               iterations = 1000, 
                               k = 200, 
                               plotstep = 0)
  plot(g, 
       layout = layout, 
       vertex.label = NA,
       vertex.size = 7)
  title(main = "This is a preview of the social network",
        cex.main = 0.75)
  # Return
  return(g)     
}

# Probabilistically update the social network 
updateNetwork <- function(AdjacencyMat, P_g, q) {
  # Loop through each individual in the adjacency matrix
  for(i in 1:nrow(AdjacencyMat)) {
    # Sample whether to break connection
    breakConnect <- sample(x = c(0, 1), 
                           size = 1, 
                           prob = c(1 - q, q))
    if (breakConnect == 1) {
      # Grab row
      row <- AdjacencyMat[i, ]
      row <- row[-i] #drop node so that it doesn't self connect
      # Identify possible connections
      possibleConnect <- which(row == 0)
      # Break existant connection
      existantConnect <- which(row == 1)
      connectToBreak <- sample(x = existantConnect, size = 1)
      AdjacencyMat[i, names(connectToBreak)] <- 0 #symmetric matrix
      AdjacencyMat[names(connectToBreak), i] <- 0 #symmetric matrix
      # Calculate similarity and repsective probability
      similarities <- lapply(possibleConnect, function(x) {
        p <- P_g[i, ]
        q <- P_g[x, ]
        jsd <- calculateJSD(p = p, q = q)
        sim <- 1 - jsd
        return(sim)
      })
      similarities <- unlist(similarities)
      similarities <- similarities / sum(similarities)
      # Select and make new connection
      newConnect <- sample(x = similarities, size = 1, prob = similarities)
      AdjacencyMat[i, names(newConnect)] <- 1 #symmetric matrix
      AdjacencyMat[names(newConnect), i] <- 1 #symmetric matrix
    }
  }
  return(AdjacencyMat)
}

# Jensen-Shannon Divergence
calculateJSD <- function(p, q) {
  m <- (p + q) / 2
  jsd <- 0.5 * (sum(p * log2(p / m))) + 0.5 * (sum(q * log2(q / m)))
  return(jsd)
}

####################
# Task State Functions
####################
# Choose task probabilistically 
updateTaskPerformance <- function(P_sub_g) {
  # Create task state matrix
  taskState <- matrix(data = rep(0, length(P_sub_g)), ncol = ncol(P_sub_g))
  # Create possible task space
  tasks <- seq(1:ncol(P_sub_g))
  # Choose tasks
  for(i in 1:nrow(P_sub_g)) {
    probs <- P_sub_g[i, ]
    taskIndex <- sample(x = tasks, size = 1, prob = probs)
    taskState[i, taskIndex] <- 1
  }
  # Return
  colnames(taskState) <- paste0("Task", 1:ncol(P_sub_g))
  rownames(taskState) <- paste0("v-", 1:nrow(P_sub_g))
  return(taskState) 
}


####################
# Global Stimulus Functions 
####################
# Seed Stimuls
seedStimuls <- function(InitialSVector, RateVector, gens) {
  # Calculate number of blank spots to make
  repLength <- (length(InitialSVector) + length(RateVector)) * gens #intiial row does not count as gen
  # Build matrix
  stim <- matrix(data = c(InitialSVector, RateVector, rep(NA, repLength)),
                 byrow = TRUE, 
                 nrow = (gens + 1))
  # Fix Names
  colnames(stim) <- c(paste0(rep("s", length(InitialSVector)), 1:length(InitialSVector)),
                   paste0(rep("delta", length(RateVector)), 1:length(InitialSVector)))
  rownames(stim) <- paste0("Gen", 0:gens)
  # Return
  return(stim)
}

# Stimulus Level
globalStimUpdate <- function(stimulus, delta, alpha, Ni, n) {
  # Calculate
  s <- stimulus + delta - ( (alpha * Ni) / n )
  # If negative, make zero
  if(s < 0) {
    s <- 0
  }
  return(s)
}

####################
# Threshold Probability of Performance 
####################
# Seed task thresholds
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

# Threshold function
threshProb <- function(s, phi, nSlope) {
  T_vi <- (s^nSlope) / (s^nSlope + phi^nSlope)
}

# Output Threshold Probabilities
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
  # normalize NOTE: DISCUSS THIS ASSUMPTION!!!!
  thresholdP <- thresholdP / rowSums(thresholdP)
  # fix names and return
  colnames(thresholdP) <- paste0("ThreshProb", 1:ncol(thresholdP))
  rownames(thresholdP) <- paste0("v-", 1:nrow(thresholdP))
  return(thresholdP)
}


####################
# Social Information Probability
####################
# Calculate probabilty 
calcSocialProbMat <- function(SocialNetwork, UseThreshold, ThresholdMatrix, X_sub_g) {
  # Calculate "sum" of task states/probs of neighbors
  NeighborSums <- SocialNetwork %*% X_sub_g
  # Calcuate divisor
  if(UseThreshold == TRUE) {
    # Calculate total degree plus threshold (used to divide neighbor sums)
    divisor <- rowSums(SocialNetwork) + ThresholdMatrix
  } else {
    divisor <- rowSums(SocialNetwork)
  }
  # Calcualte task probability based on social neighbors
  L <- NeighborSums / divisor
  # Normalize (NOTE: check this assumption!!!)
  L <- L / rowSums(L)
  # Return
  colnames(L) <- paste0("SocProb", 1:ncol(L))
  rownames(L) <- paste0("v-", 1:nrow(L))
  return(L)
}

####################
# Total performance probability functions
####################
# Set initial probabilities
initiateProbMatrix <- function(n, m) {
  rowValues <- rep(1/m, m)
  p_g <- matrix(data = rep(rowValues, n), byrow = TRUE, ncol = m)
  rownames(p_g) <- paste0("v-", 1:n)
  colnames(p_g) <- paste0("Task", 1:m)
  return(p_g)
}
