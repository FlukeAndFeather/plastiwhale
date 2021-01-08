# MAKEFILE AKA src/MAKE.R
library(lubridate)
library(oce)
library(pracma)
library(R.matlab)
library(tidyverse)
library(rorqual.morpho)

# 1. Collect data using collect_lunges
if (!all(file.exists("data/output/deployments.RDS", "data/output/lunges.RDS"))) { #want to test if it exists in the environment 
  source("src/collect_lunges.R")
} else {
  deployments <- readRDS("data/output/deployments.RDS") 
  lunges <- readRDS("data/output/lunges.RDS")
}
#variable that makes buckets isnt in environment fix me
# 2. Diel cycle calculations
if (!all(file.exists("data/output/diel_deployments.RDS", "data/output/diel_lunges.RDS"))) { 
  source("src/dielperiod.R")
} else {
  diel_deployments <- readRDS("data/output/diel_deployments.RDS")            
  diel_lunges <- readRDS("data/output/diel_lunges.RDS")
}


# 3. Feeding metric calculations
if (!all(file.exists("data/output/diel_deployments_pivot.RDS", "data/output/lunge_counts.RDS", "data/output/lunge_rates.RDS"))) { 
  source("src/depth_buckets.R")
} else {
  lunge_counts <- readRDS("data/output/lunge_counts.RDS")            
  diel_deployments_pivot <- readRDS("data/output/diel_deployments_pivot.RDS") 
  lunge_rates <- readRDS("data/output/lunge_rates.RDS") 
}

# 4. Collect morphology data
if (!file.exists("data/output/morphology.RDS")) {  
  source("src/collect_morphology.R")
} else {
  morphology <- readRDS("data/output/morphology.RDS") 
}

# 5. Collect prey data
if (!file.exists("data/output/prey.RDS")) {  
  source("src/collect_prey.R")
} else {
  prey <- readRDS("data/output/prey.RDS") 
}

# 6. Plastic from water (Choy)



