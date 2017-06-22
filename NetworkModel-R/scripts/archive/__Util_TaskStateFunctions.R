##################################################
#
# Task performance/state functions
#
##################################################


####################
# Choose task probabilistically 
####################
updateTaskPerformance <- function(P_sub_g, TaskMat, UpdateTime) {
  # Choose tasks
  for(i in 1:nrow(P_sub_g)) {
    # Sample for wehther to update tasks
    updateProb <- 1 / UpdateTime
    update <- sample(c("Update", "NoUpdate"), size = 1, prob = c(updateProb, 1 - updateProb))
    # Update task
    if (update == "Update") {
      # Create possible task space
      tasks <- seq(1:ncol(P_sub_g))
      # Zero out row
      TaskMat[i, ] <- 0
      # Get probabilities
      probs <- P_sub_g[i, ]
      # Add option for inactivity
      tasks <- c(tasks, 0)
      probs <- c(probs, round(1 - sum(probs))) #have to round because it gives small negative number instead of zero
      # Update task state
      taskIndex <- sample(x = tasks, size = 1, prob = probs)
      if (taskIndex != 0) { #if equals 0, then rest (row zeroed out)
        TaskMat[i, taskIndex] <- 1
      }
    }
  }
  # Return
  colnames(TaskMat) <- paste0("Task", 1:ncol(P_sub_g))
  rownames(TaskMat) <- paste0("v-", 1:nrow(P_sub_g))
  return(TaskMat) 
}

####################
# Set initial probabilities of performance
####################
initiateProbMatrix <- function(n, m) {
  rowValues <- rep(1/m, m)
  p_g <- matrix(data = rep(rowValues, n), byrow = TRUE, ncol = m)
  rownames(p_g) <- paste0("v-", 1:n)
  colnames(p_g) <- paste0("Task", 1:m)
  return(p_g)
}


####################
# Update task demand
####################
updateProbMatrix <- function(Lmatrix, Tmatrix, epsilon, Ematrix) {
  # Calculate demand
  D_g <- epsilon * Lmatrix + (1 - epsilon) * Tmatrix
  # Divisor
  divisor <- rowSums(D_g) + Ematrix
  # Calculate probability
  P_g <- D_g / as.vector(divisor)
  # Fix NA probs from dividing by zero (no demand, inactive)
  P_g[which(is.na(P_g))] <- 0
  # Return
  colnames(P_g) <- paste0("Task", 1:ncol(P_g))
  rownames(P_g) <- paste0("v-", 1:nrow(P_g))
  return(P_g)
}

