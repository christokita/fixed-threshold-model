##################################################
#
# Task performance/state functions
#
##################################################


####################
# Choose task probabilistically 
####################
updateTaskPerformance_og <- function(P_sub_g, TaskMat, QuitProb) {
      # Create possible task space
      tasks <- seq(1:ncol(P_sub_g))
      # Loop through individuals
      for(row in 1:nrow(TaskMat)) {
        # Inactive workers randomly sample one stimulus
        if (sum(TaskMat[row, ]) == 0) {
          # Sample task probability
          tasks_order <- sample(x = tasks, size = length(tasks), replace = FALSE)
          # Loop through tasks and go with first one that results in activity
          for (task in tasks_order) {
            prob <- P_sub_g[row, task]
            activity <- sample(x = c(0, 1), size = 1, prob = c(1 - prob, prob))
            if (activity == 1) {
              TaskMat[row, task] <- activity
              break
            }
          }
        } else { #active workers quit with certain probability
          quitNow <- sample(x = c("yes", "no"), size = 1, prob = c(QuitProb, (1 - QuitProb)))
          if (quitNow == "yes") {
            TaskMat[row, ] <- 0
          }
        }
      }
  # Return
  colnames(TaskMat) <- paste0("Task", 1:ncol(P_sub_g))
  rownames(TaskMat) <- paste0("v-", 1:nrow(P_sub_g))
  return(TaskMat) 
}

####################
# Choose task probabilistically 
####################
updateTaskPerformance_og_exhaust <- function(P_sub_g, TaskMat, ExhaustStim, ExhaustThreshVector) {
  # Create possible task space
  tasks <- seq(1:ncol(P_sub_g))
  # Loop through individuals
  for(row in 1:nrow(P_sub_g)) {
    # Inactive workers randomly sample one stimulus
    if (sum(TaskMat[row, ]) == 0) {
      # Sample task probability
      tasks_order <- sample(x = tasks, size = length(tasks))
      # Loop through tasks and go with first one that results in activity
      for (task in tasks_order) {
        prob <- P_sub_g[row, task]
        activity <- sample(x = c(0, 1), size = 1, prob = c(1 - prob, prob))
        if (activity == 1) {
          TaskMat[row, task] <- activity
          break
        }
      }
    } else { #active workers quit with certain probability
      exhaustProb <- calcExhaustDemand(ExhaustStim = ExhaustStim[i], 
                                       ExhaustThreshVector = ExhaustThreshVector[i], 
                                       nSlope = threshSlope)
      quitNow <- sample(x = c("yes", "no"), size = 1, prob = c(exhaustProb, (1 - exhaustProb)))
      if (quitNow == "yes") {
        TaskMat[row, ] <- 0
      }
    }
  }
  # Return
  colnames(TaskMat) <- paste0("Task", 1:ncol(P_sub_g))
  rownames(TaskMat) <- paste0("v-", 1:nrow(P_sub_g))
  return(TaskMat) 
}


####################
# Update task demand
####################
updateProbMatrix_og <- function(Lmatrix, Tmatrix, epsilon) {
  # Calculate demand
  P_g <- epsilon * Lmatrix + (1 - epsilon) * Tmatrix
  # Return
  colnames(P_g) <- paste0("Task", 1:ncol(P_g))
  rownames(P_g) <- paste0("v-", 1:nrow(P_g))
  return(P_g)
}

