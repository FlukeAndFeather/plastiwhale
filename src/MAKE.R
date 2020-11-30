# MAKEFILE AKA src/MAKE.R

# 1. Collect data using collect_lunges

# If [RDS files] don't exist:
source("src/collect_lunges.R")
# Else:
deployments <- readRDS("data/output/deployments.RDS")
lunges <- readRDS("data/output/lunges.RDS")
# same for lunges

# 2. Diel cycle calculations
source("src/dielperiod.R")
# If RDS files exist ...
# Else ...

# 3. Feeding metric calculations
source("src/depth_buckets.R")
# ...