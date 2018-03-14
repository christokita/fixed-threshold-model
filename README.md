# Individual-based model: Fixed response thresholds

Theoretical simulation scripts and data for: 

Ulrich Y, Saragosti J, Tokita CK, Tarnita CE, Kronauer DJC. (In Revision). Fitness Benefits and Emergent Division of Labor at the Onset of Group-Living.


This directory has three main files:
* **data**: contains relevant experimental data produced by Yuko Ulrich and the Kronauer lab.
* **scripts**: contains all scripts for simulation, analysis, and plotting of the theoretical model
* **output**: contains all subsequent derived data from simulations as well as any graphs produced from analysis of the simulation data. 

The scripts folder has the following structure:
* Any script starting with **\_\_Util__** contains relevant utility functions used throughout the other scripts. These are broken down by general functionality. **__Util__MASTER.R** is the script sourced in all other scripts that imports all the other utility functions. 
* **1A** and **1B** contain the general simulation scripts that can run a single parameter combination. 
* **2** allows one to plot some of the results of the simulations from 1A or 1B
* **3** preps the experimental data from the "data" folder for use in comparison with the model
* **4** are simulation scripts that allow us to sweep a broad parameter space of threshold shape and threshold variation. 
* **5** contains analyses of runs (usually from 1A/B) that allow us to examine the effect of group size and specialization on fitness proxies
* **6** generates the figures used in the main text of the paper
* **other_scripts** contains a variety of other scripts that were useful at one point or another, typically to check certain facets of the model or data that were not central to the main text. 

