##################################################
#
# Task performance/state functions
#
##################################################

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
# Choose task probabilistically 
####################
updateTaskPerformance <- function(P_sub_g, TaskMat, QuitProb) {
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
# Choose task with most demand (conceptual)
####################
updateTaskPerformance_Determ <- function(P_sub_g, TaskMat, QuitProb, TimeStep, StimulusMatrix) {
  # Create possible task space
  tasks <- seq(1:ncol(P_sub_g))
  # Get relevant stimulus levels
  stim_levels <- StimulusMatrix[TimeStep, 1:2]
  # Loop through individuals
  for(row in 1:nrow(TaskMat)) {
    # Inactive workers randomly sample one stimulus
    if (sum(TaskMat[row, ]) == 0) {
      # Sample task probability
      if (P_sub_g[row, 1] == P_sub_g[row, 2]) {
        tasks_order <- order(stim_levels, decreasing = T)
      } else {
        tasks_order <- order(P_sub_g[row, ], decreasing = T)
      }
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
# Choose task with most demand and have fixed work time (conceptual)
####################
updateTaskPerformance_DetermTimed <- function(P_sub_g, TaskMat, QuitProb, TimeStep, StimulusMatrix, TaskMemory, QuitRate) {
  # Create possible task space
  tasks <- seq(1:ncol(P_sub_g))
  # Get relevant stimulus levels
  stim_levels <- StimulusMatrix[TimeStep, 1:2]
  # Loop through individuals
  for(row in 1:nrow(TaskMat)) {
    # Inactive workers randomly sample one stimulus
    if (sum(TaskMat[row, ]) == 0) {
      # Sample task probability
      if (P_sub_g[row, 1] == P_sub_g[row, 2]) {
        tasks_order <- order(stim_levels, decreasing = T)
      } else {
        tasks_order <- order(P_sub_g[row, ], decreasing = T)
      }
      # Loop through tasks and go with first one that results in activity
      for (task in tasks_order) {
        prob <- P_sub_g[row, task]
        activity <- sample(x = c(0, 1), size = 1, prob = c(1 - prob, prob))
        if (activity == 1) {
          TaskMat[row, task] <- activity
          break
        }
      }
    } else { #active workers quit with fixed 
      if (TaskMemory[row] >= QuitRate) {
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
# Tasks have fixed work time (conceptual)
####################
updateTaskPerformance_Timed <- function(P_sub_g, TaskMat, QuitProb, TimeStep, StimulusMatrix, TaskMemory, QuitRate) {
  # Create possible task space
  tasks <- seq(1:ncol(P_sub_g))
  # Get relevant stimulus levels
  stim_levels <- StimulusMatrix[TimeStep, 1:2]
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
    } else { #active workers quit with fixed 
      if (TaskMemory[row] >= QuitRate) {
        TaskMat[row, ] <- 0
      }
    }
  }
  # Return
  colnames(TaskMat) <- paste0("Task", 1:ncol(P_sub_g))
  rownames(TaskMat) <- paste0("v-", 1:nrow(P_sub_g))
  return(TaskMat) 
}







