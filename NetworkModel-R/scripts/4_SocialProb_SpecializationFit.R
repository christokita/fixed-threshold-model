################################################################################
#
# Model incorporating both thresholds and network dynamics
#
################################################################################

rm(list = ls())
source("scripts/__Util__MASTER.R")

####################
# Set global variables
####################
# Initial paramters: Free to change
# Base parameters
Ns             <- c(2, 16) #vector of number of individuals to simulate
m              <- 2 #number of tasks
gens           <- 10000 #number of generations to run simulation 
corrStep       <- 200 #number of time steps for calculation of correlation  !!Change!!
reps           <- 100 #number of replications per simulation (for ensemble)

# Threshold Parameters
ThreshM        <- c(10, 10) #population threshold means 
ThreshSD       <- ThreshM * 0 #population threshold standard deviations
InitialStim    <- c(0, 0) #intital vector of stimuli
StimRates      <- c(0.6, 0.6) #vector of stimuli increase rates  
threshSlopes   <- c(1, 2, 5, 10, 15, 20, 25, 30) #exponent parameter for threshold curve shape  
nSlope         <- 2
alpha          <- m #efficiency of task performance
quitP          <- 0.2 #probability of quitting task once active
phi            <- 0.01 #learning/forgetting of threshold
gamma          <- 0.01   #bias towards learning of task
cs             <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.5, 0.75, 1) #social reinforcer of threshold learning
d              <- NA #social preventer of threshold forgetting
Averaging      <- FALSE #should social interactions be done in absolute or averaged terms?
lowerThresh    <- 0
upperThresh    <- 100

# Social Network Parameters
p              <- 0.4 #probability of interacting with other individuals
q              <- 2 #probability of interacting with individual in same state relative to others




####################
# Run simulation multiple times
####################
# prep mega list
improveSpec <- list()


# Loop through n slopes
for (k in 1:length(threshSlopes)) {
  
  # Set threshSlope 
  threshSlope <- threshSlopes[k]
  
  # Prep meta-lists for collection of group size simulations
  gammaDiff <- data.frame(PercIncrease = NULL, SlopeIncrease = NULL, SpecLarge = NULL, SpecSmall = NULL, sigma = NULL, threshSlope = NULL)
  
  # Loop through phis
  for (z in 1:length(cs)) {
    
    c <- cs[z]
    d <- c
    
    # Prep meta-lists for collection of group size simulations
    groups_taskCorr  <- list()
    groups_runStims  <- list()
    
    # Loop through group sizes
    for (i in 1:length(Ns)) {
      # Set group size
      n <- Ns[i]
      
      # Prep lists for collection of simulation outputs
      ens_taskCorr  <- list()
      ens_stimDiff  <- list()
      
      # Run Simulations
      for (sim in 1:reps) {

        ####################
        # Seed structures and intial matrices
        ####################
        
        # Set initial probability matrix (P_g)
        P_g <- initiateProbMatrix(n = n, m = m)
        
        # Seed task (external) stimuli
        stimMat <- seedStimuls(InitialSVector = InitialStim, 
                               RateVector = StimRates, 
                               gens = gens)

        # Seed internal thresholds
        threshMat <- seedThresholds(n = n, 
                                    m = m, 
                                    ThresholdMeans = ThreshM, 
                                    ThresholdSDs = ThreshSD)
        
        # Start task performance
        X_g <- matrix(data = rep(0, length(P_g)), ncol = ncol(P_g))
        
        # Create cumulative task performance matrix
        X_tot <- X_g
        
        # Prep correlation step matrix
        X_prev <- matrix(data = rep(0, n * m), ncol = m)
        X_prevTot <- matrix(data = rep(0, n * m), ncol = m)
        taskCorr <- list()
        
        ####################
        # Simulate
        ####################
        # Run simulation
        for (t in 1:gens) {
          # Update stimuli
          for (j in 1:(ncol(stimMat)/2)) {
            # update stim
            stimMat[t + 1, j] <- globalStimUpdate(stimulus = stimMat[t, j],
                                                  delta = stimMat[t, j + m], 
                                                  alpha = alpha, 
                                                  Ni = sum(X_g[ , j]), 
                                                  n = n)
            # shift down delta (rate increases)
            stimMat[t + 1, j + m] <- stimMat[t, j + m]
          }
          # Update social network
          g_adj <- temporalNetwork(X_sub_g = X_g,
                                   p = p,
                                   bias = q)
          # Calculate task demand based on global stimuli
          P_g <- calcThresholdProbMat(TimeStep = t + 1, # first row is generation 0
                                      ThresholdMatrix = threshMat, 
                                      StimulusMatrix = stimMat, 
                                      nSlope = threshSlope)
          # Update task performance
          X_g <- updateTaskPerformance(P_sub_g    = P_g,
                                       TaskMat    = X_g,
                                       QuitProb   = quitP)
          
          # Update threshold (reinforcement/forgetting)
          threshMat <- adjustThresholdsSocialInhibition(ThresholdMatrix = threshMat, 
                                                        X_sub_g = X_g, 
                                                        phi = phi, 
                                                        gamma = gamma, 
                                                        c = c, 
                                                        d = d, 
                                                        SocialNetwork = g_adj, 
                                                        lowerThresh = lowerThresh, 
                                                        upperThresh = upperThresh, 
                                                        Average = Averaging)
          
          # Update total task performance profile
          X_tot <- X_tot + X_g
          
          # Create time step for correlation
          if (t %% corrStep == 0) {
            # Get tasks performance in correlation step
            X_step <- X_tot - X_prevTot
            # Calculate rank correlation if it is not the first step
            if(sum(X_prev) != 0) {
              # Normalize
              stepNorm <- X_step / rowSums(X_step)
              prevNorm <- X_prev / rowSums(X_prev)
              # Calculate ranks
              step_ranks <- calculateTaskRank(TaskStepMat = X_step)
              prev_ranks <- calculateTaskRank(TaskStepMat = X_prev)
              # Calculate Correlation
              rankCorr <- cor(prev_ranks, step_ranks, method = "spearman")
              # Put in list
              taskCorr[[(t / corrStep) - 1]] <- diag(rankCorr)
              names(taskCorr)[(t / corrStep) - 1] <- paste0("Gen", t)
            }
            # Update previous step total matrix
            X_prevTot <- X_tot
            # Update previous step total matrix
            X_prev <- X_step
          }
        }
        # Calculate total difference in stim over averaged window at beginning and end
        stimMat <- as.data.frame(stimMat)
        stimDiff <- data.frame(task1start = mean(stimMat$s1[101:601]), #avg over 500 window after first 100 acclimation 
                               task2start = mean(stimMat$s2[101:601]),
                               task1end   = mean(stimMat$s1[(nrow(stimMat) - 500):nrow(stimMat)]), #avg over last 500 steps
                               task2end   = mean(stimMat$s2[(nrow(stimMat) - 500):nrow(stimMat)]))
        stimDiff <- stimDiff %>% 
          summarise(Task1Diff = task1end - task1start,
                    Task2Diff = task2end - task2start)
        
        # Add total task distributions, entropy values, and graphs to lists
        ens_taskCorr[[sim]]  <- taskCorr
        ens_stimDiff[[sim]] <- stimDiff
      }
      
      # Calculate mean correlation for each run
      runCorrs <- lapply(ens_taskCorr, function(x) {
        # Unlist
        runs <- do.call("rbind", x)
        # Calculate mean
        runMean <- matrix(data = rep(NA, m), ncol =  m)
        for (column in 1:m) {
          runMean[ , column] <- mean(runs[ , column], na.rm = TRUE)
        }
        colnames(runMean) <- colnames(runs)
        return(runMean)
      })
      runCorrs <- do.call("rbind", runCorrs)
      runCorrs <- transform(runCorrs, n = n)
      
      # Calculate mean stim difference
      runStims <- do.call("rbind", ens_stimDiff)
      runStims <- runStims %>% 
        mutate(n = n)
      
      # Add to list of lists
      groups_taskCorr[[i]]  <- runCorrs
      groups_runStims[[i]]  <- runStims
       
      # Print simulation completed
      print(paste0("DONE: N = ", n, ", c = d = ", c, ", N-Slope = ", threshSlope))
      
    }
    # Unlist correlations
    taskCorrTot <- do.call("rbind", groups_taskCorr)
    if ("X1" %in% names(taskCorrTot) | "X2" %in% names(taskCorrTot)) {
      names(taskCorrTot) <- c("Task1", "Task2", "n")
    }
    taskCorrTot[is.na(taskCorrTot)] <- 0 #fix NAs
    taskCorrTot <- taskCorrTot %>% 
      mutate(TaskMean = (Task1 + Task2) / 2)
    # Calculate mean
    taskCorrMeans <- taskCorrTot %>% 
      group_by(n) %>% 
      summarise(SpecMean = mean(TaskMean, na.rm = TRUE))
    # Unlist stim differences
    taskStimDiff <- do.call("rbind", groups_runStims)
    taskStimDiff <- taskStimDiff %>% 
      group_by(n) %>% 
      summarise(Task1Diff = mean(Task1Diff),
                Task2Diff = mean(Task2Diff))
    # Calculate improvement in specialization between group size 2 and 16
    CorrMeanDiff <- data.frame(PercIncrease = NA, 
                               SlopeIncrease = NA, 
                               SpecSmall = NA, 
                               SpecLarge = NA, 
                               Task1DiffSmall = taskStimDiff$Task1Diff[taskStimDiff$n == 2],
                               Task2DiffSmall = taskStimDiff$Task2Diff[taskStimDiff$n == 2],
                               Task1DiffLarge = taskStimDiff$Task1Diff[taskStimDiff$n == 16],
                               Task2DiffLarge = taskStimDiff$Task2Diff[taskStimDiff$n == 16],
                               gamma = gamma, 
                               phi = phi, 
                               threshSlope = threshSlope)
    n2 <- taskCorrMeans$SpecMean[taskCorrMeans$n == 2]
    n16 <- taskCorrMeans$SpecMean[taskCorrMeans$n == 16]
    CorrMeanDiff$PercIncrease <- (n16 - n2) / abs(n2)
    CorrMeanDiff$SlopeIncrease <- (n16 - n2) / (16 - 2)
    CorrMeanDiff$SpecLarge <- n16
    CorrMeanDiff$SpecSmall <- n2
    # Bind to list within sigma value
    gammaDiff <- rbind(gammaDiff, CorrMeanDiff)
  }
  
  # add results to list
  improveSpec[[k]] <- gammaDiff
  
}

####################
# Prep and Plot
####################
# Unlist
improve <- do.call("rbind", improveSpec)
