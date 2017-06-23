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
Ns             <- c(2, 4, 6, 8, 12, 16, 32, 100) #vector of number of individuals to simulate
m              <- 2 #number of tasks
gens           <- 10000 #number of generations to run simulation 
corrStep       <- 200 #number of time steps for calculation of correlation 
reps           <- 20 #number of replications per simulation (for ensemble) !!Change!!

# Threshold Parameters
ThreshM        <- c(10, 10) #population threshold means 
ThreshSD       <- ThreshM * 0.1 #population threshold standard deviations !!Change!!
InitialStim    <- c(0, 0) #intital vector of stimuli
StimRates      <- c(0.6, 0.6) #vector of stimuli increase rates  
threshSlope    <- 7 #exponent parameter for threshold curve shape  
alpha          <- m #efficiency of task performance
quitP          <- 0.2 #probability of quitting task once active

# Social Network Parameters
p              <- 0 #probability of interacting with individual in other states
q              <- 1 #probability of interacting with individual in same state relative to others



####################
# Run simulation multiple times
####################
# Prep meta-lists for collection of group size simulations
groups_taskDist  <- list()
groups_taskCorr  <- list()
groups_taskStep  <- list()
groups_taskTally <- list()
groups_stim      <- list()
groups_entropy   <- list()

# Loop through group sizes
for (i in 1:length(Ns)) {
  # Set group size
  n <- Ns[i]
  
  # Prep lists for collection of simulation outputs
  ens_taskDist  <- list()
  ens_taskCorr  <- list()
  ens_taskStep  <- list()
  ens_taskTally <- list()
  ens_entropy   <- list()
  ens_stim      <- list()
  
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
    taskCorr <- data.frame(Task1 = NULL, Task2 = NULL, timestep = NULL, n = NULL, replicate = NULL)
    taskStep <- list()
    taskTally <- list()
    
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
      # g_adj <- temporalNetwork(X_sub_g = X_g,
      #                          p = p, 
      #                          bias = q)
      # Calculate task demand based on global stimuli
      P_g <- calcThresholdProbMat(TimeStep = t + 1, # first row is generation 0
                                  ThresholdMatrix = threshMat, 
                                  StimulusMatrix = stimMat, 
                                  nSlope = threshSlope)
      # Update task performance
      X_g <- updateTaskPerformance(P_sub_g    = P_g,
                                   TaskMat    = X_g,
                                   QuitProb   = quitP)
      
      # Capture current task performance tally
      tally <- matrix(c(t, colSums(X_g)), ncol = ncol(X_g) + 1)
      colnames(tally) <- c("t", colnames(X_g))
      tally <- transform(tally, Inactive = n - sum(X_g), n = n, replicate = sim)
      taskTally[[t]] <- tally
      
      # Update total task performance profile
      X_tot <- X_tot + X_g
      
      # Create time step for correlation
      if (t %% corrStep == 0) {
        # Get tasks performance in correlation step
        X_step <- X_tot - X_prevTot
        # Add to ensemble list of task steps
        taskStep[[t / corrStep]] <- X_step
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
          corr <- diag(rankCorr)
          corr <- as.data.frame(corr)
          corr <- t(corr)
          corr$timestep <- t
          corr$n <- n
          corr$replicate <- sim
          names(corr)[1:2] <- c("Task1", "Task2")
          taskCorr <- rbind(taskCorr, corr)
        }
        # Update previous step total matrix
        X_prevTot <- X_tot
        # Update previous step total matrix
        X_prev <- X_step
      }
    }
    
    # Calculate Entropy
    entropy <- mutualEntropy(TotalStateMat = X_tot)
    entropy <- transform(entropy, n = n, replicate = sim)
    
    # Calculate total task distribution
    # totalTaskDist <- X_tot / rowSums(X_tot)
    totalTaskDist <- X_tot / gens
    totalTaskDist <- transform(totalTaskDist, Inactive = gens - rowSums(X_tot), n = n, replicate = sim)
    
    # Create tasktally table
    taskTally <- do.call("rbind", taskTally)
    
    # Create tasktally table
    stimMat <- transform(stimMat, n = n, replicate = sim)
    
    # Add total task distributions, entropy values, and graphs to lists
    ens_taskDist[[sim]]  <- totalTaskDist
    ens_entropy[[sim]]   <- entropy
    ens_taskCorr[[sim]]  <- taskCorr
    ens_taskTally[[sim]] <- taskTally
    ens_taskStep[[sim]]  <- taskStep
    ens_stim[[sim]]      <- stimMat
    
    # Print simulation completed
    print(paste0("DONE: N = ", n, ", Simulation ", sim))
  }
  
  # Unlist for corr over time
  runCorrs <- do.call("rbind", ens_taskCorr)
  
  # Add to list of lists
  groups_taskDist[[i]]  <- ens_taskDist
  groups_taskCorr[[i]]  <- runCorrs
  groups_taskStep[[i]]  <- ens_taskStep
  groups_taskTally[[i]] <- ens_taskTally
  groups_stim[[i]]      <- ens_stim
  groups_entropy[[i]]   <- ens_entropy
  
}

# trim out correlations for group size 1
if(1 %in% Ns) {
  groups_taskCorr <- groups_taskCorr[-1]
}


# Plot - Corr over time
Corrs <- do.call("rbind", groups_taskCorr)
Corrs <- Corrs %>% 
  mutate(Set = paste(n, replicate, sep = "-")) %>% 
  rowwise() %>% 
  mutate(Specialization = mean(c(Task1, Task2), na.rm = TRUE))
sigma <- ThreshSD / ThreshM

gg_corrtime <- ggplot(data = Corrs, 
                      aes(x = timestep, y = Specialization, group = Set, col = as.factor(n))) +
  geom_hline(data = Corrs, 
             aes(yintercept = 0),
             colour = "grey30",
             alpha = 0.5) +
  geom_line(alpha = 0.6) +
  theme_classic() +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  # scale_colour_manual(values = c("#F00924", "#4C0E78")) +
  xlab("Timestep") +
  facet_grid(replicate ~ n) +
  ggtitle(paste("Fixed Thresholds, Sigma =", sigma[1])) +
  theme(legend.position = "none",
        axis.text = element_text(size = 6))
gg_corrtime


# Plot - Stim over time
stims <- do.call("rbind", groups_stim)
stims <- do.call("rbind", stims)
stims <- stims %>% 
  mutate(Set = paste(n, replicate, sep = "-")) %>% 
  group_by(Set) %>% 
  mutate(timestep = 1:length(Set))

gg_stimtime <- ggplot(data = stims, 
                      aes(x = timestep)) +
  geom_hline(data = Corrs, 
             aes(yintercept = 0),
             colour = "grey30",
             alpha = 0.5) +
  geom_line(aes(y = s1),
            alpha = 0.6) +
  theme_classic() +
  # scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  scale_colour_manual(values = c("#F00924", "#4C0E78")) +
  xlab("Timestep") +
  facet_grid(replicate ~ n) +
  ggtitle(paste("Fixed Thresholds, Sigma =", sigma[1])) +
  theme(legend.position = "none",
        axis.text = element_text(size = 6))
gg_stimtime




# ggsave(filename = paste0("output/OtherFigures/CorrTime_Sigma", sigma[1], "100k_Fixed.png"), width = 5, height = 5, units = "in", dpi = 300)

# Evaluate mean specialization
Corrs1 <- Corrs
load("output/OtherFigures/Sigma01-100ktime.Rdata")

corrSum1 <- Corrs1 %>% 
  group_by(n, Set) %>% 
  summarise(MeanSpec = mean(Specialization, na.rm = TRUE))


corrSum <- Corrs %>% 
  group_by(n, Set) %>% 
  summarise(MeanSpec = mean(Specialization, na.rm = TRUE))

gg_corrs <- ggplot(data = corrSum, aes(x = as.factor(n), y = MeanSpec)) +
  geom_hline(data = Corrs, 
             aes(yintercept = 0),
             colour = "grey30",
             alpha = 0.5) +
  geom_point() +
  theme_classic() +
  ylab("Specialization") +
  xlab("Group Size") +
  ggtitle(paste("100,000 Timesteps,\nSigma =", sigma[1])) +
  scale_y_continuous(limits = c(-0.5, 1))
gg_corrs

gg_corrs1 <- ggplot(data = corrSum1, aes(x = as.factor(n), y = MeanSpec)) +
  geom_hline(data = Corrs, 
             aes(yintercept = 0),
             colour = "grey30",
             alpha = 0.5) +
  geom_point() +
  theme_classic() +
  ylab("Specialization") +
  xlab("Group Size") +
  ggtitle(paste("10,000 Timesteps,\nSigma =", sigma[1])) +
  scale_y_continuous(limits = c(-0.5, 1))
gg_corrs1


corrTot <- corrSum %>% 
  group_by(n) %>% 　
  summarise(Spec = mean(MeanSpec),
            SpecSE = sd(MeanSpec)/sqrt(5))

corrTot1 <- corrSum1 %>% 
  group_by(n) %>% 　
  summarise(Spec = mean(MeanSpec),
            SpecSE = sd(MeanSpec)/sqrt(5))


