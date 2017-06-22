##################################################
#
# Social Information Probability
#
##################################################


####################
# Calculate probabilty 
####################
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
  L <-  NeighborSums / divisor
  # Fix unconnected nodes
  L[is.na(L)] <- 0
  # Return
  colnames(L) <- paste0("SocProb", 1:ncol(L))
  rownames(L) <- paste0("v-", 1:nrow(L))
  return(L)
}