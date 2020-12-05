#Collect Morphology

#---- Set Up ----
library(rorqual.morpho)

# ---- Process Data ----
morphology <- read_csv("data/raw/ENP_whale_lengths.csv") %>% 
  mutate(engulfment_kg = rorq_engulf(species_code, length_m))

# ---- Export Data ----
saveRDS(deployments, "data/output/morphology.RDS")
