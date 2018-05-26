##################################################
#
# Qunatifying Division of Labor Functions
#
##################################################

####################
# Mutual Entropy DOL Measure
####################
# From Gorelick, Bertram, Killeen, & Fewell (2004)
mutualEntropy <- function(TotalStateMat) {
  # Normalize matrix
  # normMat <- TotalStateMat / rowSums(TotalStateMat)
  normMat <- TotalStateMat / sum(TotalStateMat) #checking this after noticing possible error
  # Total Individuals
  n <- nrow(normMat)
  m <- ncol(normMat)
  total <- sum(normMat)
  # Shannon's entropy of individuals H(X)
  H_x <- apply(normMat, MARGIN = 1, function(ind) {
    p_x <- sum(ind)
    h_x <- p_x * log(p_x)
  })
  # Shannon's entropy of tasks H(Y)
  H_y <- apply(normMat, MARGIN = 2, function(task) {
    p_y <- sum(task)
    h_y <- p_y * log(p_y)
  })
  # Mutual entropy I(X,Y)
  I_xy <- lapply(1:n, function(ind) {
    # Loop through tasks for each individual
    mutualEntr <- rep(NA, m)
    for (task in 1:m) {
      # joint probability p(x,y)
      p_xy <- normMat[ind, task]
      # calculate log portion
      p_x <- sum(normMat[ind, ])
      p_y <- sum(normMat[ , task])
      logVal <- log(p_xy / (p_x * p_y))
      # If entry has zero probability, set total value to zero (instead of NA/-Inf)
      entry <- p_xy * logVal
      if (is.na(entry)) {
        entry <- 0 # setting to zero because if p_x or p_y is 0, then you will get Inf/NA, but times 0 is 0 (approx)
      }
      # enter into list
      mutualEntr[task] <- entry
    }
    mutualEntr <- sum(mutualEntr)
    return(mutualEntr)
  })
  # Sum values 
  H_x <- -sum(H_x)
  H_y <- -sum(H_y)
  I_xy <- sum(unlist(I_xy))
  # Calcualte symmetrid division of labor D(x,y)
  D_sym <- I_xy / sqrt(H_x * H_y)
  D_yx <- I_xy / H_x #names mixed up
  D_xy <- I_xy / H_y #names mixed up
  # Dataframe
  D <- data.frame(Dsym = D_sym, Dyx = D_yx, Dxy = D_xy)
  # Return 
  return(D)
}

####################
# Calcualte task rank
####################
calculateTaskRank <- function(TaskStepMat) {
  # Loop through columns
  for (column in 1:ncol(TaskStepMat)) {
    TaskStepMat[ , column] <- dense_rank(TaskStepMat[ , column])
  }
  # Return
  return(TaskStepMat)
}


