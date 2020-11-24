# MAKEFILE AKA src/MAKE.R

# 1. Collect data
# If [RDS files] don't exist:
source("src/collect_lunges.R")
# Else:
deployments <- readRDS("data/output/deployments.RDS")
# same for lunges

# 2. Diel cycle calculations
# If RDS files exist ...
# Else ...

# 3. Feeding metric calculations
# ...