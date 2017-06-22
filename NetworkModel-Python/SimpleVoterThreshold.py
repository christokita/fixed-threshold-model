from numpy import *
from scipy import *
from igraph import *

# Seed state space
def seedState(N):
    space = array([0, 1])
    state = random.choice(space, N, replace = True)
    return state

# Seed thresholds
def seedThresholds(N, Thresh1, Thresh2, ThreshSD):
    # Buld empty matrix
    out <- matrix()
    
