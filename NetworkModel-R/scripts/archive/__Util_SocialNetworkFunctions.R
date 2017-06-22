##################################################
#
# Social Network Functions
#
##################################################


####################
# Generate a social network
####################
seedSocialNetwork <- function(n, m, outDegree, networkPower, type, edgeSD) {
  # Scale free network
  if (type == "ScaleFree") {
    g <- barabasi.game(n = n, power = networkPower, m = outDegree, directed = FALSE)
    E(g)$weight <- rep(1, length(E(g)))
  } else if (type == "Random") {
    g <- random.graph.game(n = n, p.or.m = outDegree * n, type = "gnm")
    E(g)$weight <-  E(g)$weight <- rtnorm(n = length(E(g)), mean = 0.5, sd = edgeSD, lower = 0.01, upper = 0.99)
  } else if (type == "Complete") {
    g <- graph.full(n = n)
    E(g)$weight <- rtnorm(n = length(E(g)), mean = 0.5, sd = edgeSD, lower = 0.01, upper = 0.99)
  } else if(type == "Regular") {
    if (n < outDegree + 1) {
      g <- sample_k_regular(n, k = (n %% (outDegree + 1)) - 1, directed = FALSE, multiple = FALSE)
    } else {
    g <- sample_k_regular(n, k = outDegree, directed = FALSE, multiple = FALSE)
    }
    E(g)$weight <-  E(g)$weight <- rtnorm(n = length(E(g)), mean = 0.5, sd = edgeSD, lower = 0.01, upper = 0.99)
  }
  # # Plot preview of network
  # if (n > 10) {
  #   layout <- layout.forceatlas2(g, 
  #                                iterations = 1000, 
  #                                k = 200, 
  #                                plotstep = 0)
  # } else {
  #   layout <- layout.reingold.tilford(g)
  # }
  # plot(g, 
  #      layout = layout, 
  #      vertex.label = NA,
  #      vertex.size = 7)
  # title(main = "This is a preview of the social network",
  #       cex.main = 0.75)
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
      existentConnect <- which(adjRow == 1)
      connectToBreak <- sample(x = as.list(existentConnect), size = 1)
      AdjacencyMat[i, names(connectToBreak)] <- 0 #symmetric matrix
      AdjacencyMat[names(connectToBreak), i] <- 0 #symmetric matrix
      # Identify possible connections
      possibleConnect <- which(adjRow == 0)
      # Determine if connection should be with individual of the same state
      sameState <- sample(x = c("different", "same"),
                          size = 1,
                          prob = c(1 - q, q))
      # Grab individuals that match state
      matchState <- which(stateCol == 1)
      # Get individuals that match state and are unconnected
      matchState <- matchState[matchState %in% possibleConnect]
      # Form new connection
      if(sameState == "same" & length(matchState) > 0) { #make sure there are unconnected matches
        # Sample and connect
        newConnect <- sample(x = as.list(matchState), size = 1)
        AdjacencyMat[i, names(newConnect)] <- 1 #symmetric matrix
        AdjacencyMat[names(newConnect), i] <- 1 #symmetric matrix
      } else {
        # Grab individuals that match state
        diffState <- which(stateCol == 0)
        # Get individuals that match state and are unconnected
        diffState <- diffState[diffState %in% possibleConnect]
        # Sample and connect
        newConnect <- sample(x =  as.list(diffState), size = 1)
        AdjacencyMat[i, names(newConnect)] <- 1 #symmetric matrix
        AdjacencyMat[names(newConnect), i] <- 1 #symmetric matrix
      }
      # Check for and fix unconnected nodes
      unattached <- which(rowSums(AdjacencyMat) == 0)
      if (length(unattached) > 0) {
        AdjacencyMat <- reattachNodes(AdjacencyMat = AdjacencyMat, 
                                      StateMat = StateMat, 
                                      q = q)
      }
    }
  }
  return(AdjacencyMat)
}

####################
# Safeguard against unconnected nodes
####################
reattachNodes <- function(AdjacencyMat, StateMat, q) {
  # Find unconnected nodes
  unattached <- which(rowSums(AdjacencyMat) == 0)
  # reattach
  for (node in unattached) {
    # Grab correct state matrix
    stateRow <- StateMat[node, ]
    # Grab column of which task is being performed by individual
    task <- which(stateRow == 1)
    stateCol <- StateMat[ ,task]
    stateCol <- stateCol[-node] #drop node so that it doesn't self connect
    # Grab adjacency row
    adjRow <- AdjacencyMat[node, ]
    adjRow <- adjRow[-node] #drop node so that it doesn't self connect
    # Identify possible connections
    possibleConnect <- which(adjRow == 0)
    # Determine if connection should be with individual of the same state
    sameState <- sample(x = c("different", "same"),
                        size = 1,
                        prob = c(1 - q, q))
    # Grab individuals that match state
    matchState <- which(stateCol == 1)
    # Get individuals that match state and are unconnected
    matchState <- matchState[matchState %in% possibleConnect]
    # Form new connection
    if(sameState == "same" & length(matchState) > 0) { #make sure there are unconnected matches
      # Sample and connect
      newConnect <- sample(x = as.list(matchState), size = 1)
      AdjacencyMat[node, names(newConnect)] <- 1 #symmetric matrix
      AdjacencyMat[names(newConnect), node] <- 1 #symmetric matrix
    } else {
      # Grab individuals that match state
      diffState <- which(stateCol == 0)
      # Get individuals that match state and are unconnected
      diffState <- diffState[diffState %in% possibleConnect]
      # Sample and connect
      newConnect <- sample(x = as.list(diffState), size = 1)
      AdjacencyMat[node, names(newConnect)] <- 1 #symmetric matrix
      AdjacencyMat[names(newConnect), node] <- 1 #symmetric matrix
    }
  }
  return(AdjacencyMat)
}


####################
# Form connections probabilistically
####################
temporalNetwork <- function(X_sub_g, p, bias) {
  dimension <- nrow(X_sub_g)
  g_adj <- matrix(data = rep(NA, dimension*dimension), ncol = dimension)
  # Loop through row individuals
  for (i in 1:dimension) {
    # get task performance of individual
    task <- which(X_sub_g[i, ] == 1)
    # loop through column individuals
    # If inactive, all connections equal prob
    if (length(task) == 0) {
      for (j in i:dimension) {
        con <- sample(x = c(0, 1), size = 1, prob = c(1 - p, p))
        g_adj[i, j] <- con
        g_adj[j, i] <- con
      }
    }
    # If active, biased towards individuals in same state
    else {
      for (j in 1:dimension) {
        same <- which(X_sub_g[ , task] == 1)
        # if same, biased
        if (j %in% same) {
          con <- sample(x = c(0, 1), size = 1, prob = c(1 - bias * p, bias * p))
          g_adj[i, j] <- con
          g_adj[j, i] <- con
        } else {
          con <- sample(x = c(0, 1), size = 1, prob = c(1 - p, p))
          g_adj[i, j] <- con
          g_adj[j, i] <- con
        }
      }
    }
  }
  # bind and name columns
  diag(g_adj) <- 0 #remove self-connections
  colnames(g_adj) <- paste0("v-", 1:dimension)
  rownames(g_adj) <- paste0("v-", 1:dimension)
  return(g_adj)
}

