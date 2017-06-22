##################################################
#
# Social Network Functions
#
##################################################


####################
# Generate a social network
####################
seedSocialNetwork <- function(n, outDegree, networkPower, scaleFree) {
  # Scale free network
  if (scaleFree == TRUE) {
    g <- barabasi.game(n = n, m = outDegree, power = networkPower, directed = FALSE)
  } else {
    g <- random.graph.game(n = n, p.or.m = outDegree * n, type = "gnm")
  }

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

####################
# Probabilistically update the social network based on state matrix
####################
updateNetwork <- function(AdjacencyMat, StateMat, p, q) {
  # Loop through each individual in the adjacency matrix
  for(i in 1:nrow(AdjacencyMat)) {
    # Sample whether to break connection
    breakConnect <- sample(x = c("nobreak", "break"),
                           size = 1,
                           prob = c(1 - p, p))
    if (breakConnect == "break") {
      # Grab correct state matrix
      stateRow <- StateMat[i, ]
      # Grab column of which task is being performed by individual
      task <- which(stateRow == 1)
      stateCol <- StateMat[ ,task]
      stateCol <- stateCol[-i] #drop node so that it doesn't self connect
      # Grab adjacency row
      adjRow <- AdjacencyMat[i, ]
      adjRow <- adjRow[-i] #drop node so that it doesn't self connect
      # Break existant connection
      existantConnect <- which(adjRow == 1)
      connectToBreak <- sample(x = existantConnect, size = 1)
      AdjacencyMat[i, names(connectToBreak)] <- 0 #symmetric matrix
      AdjacencyMat[names(connectToBreak), i] <- 0 #symmetric matrix
      # Identify possible connections
      possibleConnect <- which(adjRow == 0)
      # Determine if connection should be with individual of the same state
      sameState <- sample(x = c("different", "same"),
                          size = 1,
                          prob = c(1 - q, q))
      # Form new connection
      if(sameState == "same") {
        # Grab individuals that match state
        matchState <- which(stateCol == 1)
        # Get individuals that match state and are unconnected
        matchState <- matchState[matchState %in% possibleConnect]
        # Sample and connect
        newConnect <- sample(x = matchState, size = 1)
        AdjacencyMat[i, names(newConnect)] <- 1 #symmetric matrix
        AdjacencyMat[names(newConnect), i] <- 1 #symmetric matrix
      } else {
        # Grab individuals that match state
        diffState <- which(stateCol == 0)
        # Get individuals that match state and are unconnected
        diffState <- diffState[diffState %in% possibleConnect]
        # Sample and connect
        newConnect <- sample(x = diffState, size = 1)
        AdjacencyMat[i, names(newConnect)] <- 1 #symmetric matrix
        AdjacencyMat[names(newConnect), i] <- 1 #symmetric matrix
      }
    }
  }
}


####################
# Probabilistically update the social network based on probability matrix
####################
updateNetwork <- function(AdjacencyMat, P_g, p) {
  # Loop through each individual in the adjacency matrix
  for(i in 1:nrow(AdjacencyMat)) {
    # Sample whether to break connection
    breakConnect <- sample(x = c("nobreak", "break"),
                           size = 1,
                           prob = c(1 - p, p))
    if (breakConnect == "break") {
      # Grab row
      row <- AdjacencyMat[i, ]
      row <- row[-i] #drop node so that it doesn't self connect
      # Break existant connection
      existantConnect <- which(row == 1)
      connectToBreak <- sample(x = existantConnect, size = 1)
      AdjacencyMat[i, names(connectToBreak)] <- 0 #symmetric matrix
      AdjacencyMat[names(connectToBreak), i] <- 0 #symmetric matrix
      # Identify possible connections
      possibleConnect <- which(row == 0)
      # Calculate similarity and repsective probability
      similarities <- lapply(possibleConnect, function(x) {
        ind1 <- P_g[i, ]
        ind2 <- P_g[x, ]
        jsd <- calculateJSD(ind1 = ind1, ind2 = ind2)
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


####################
# Jensen-Shannon Divergence
####################
calculateJSD <- function(ind1, ind2) {
  m <- (ind1 + ind2) / 2
  jsd <- 0.5 * (sum(ind1 * log2(ind1 / m))) + 0.5 * (sum(ind2 * log2(ind2 / m)))
  return(jsd)
}