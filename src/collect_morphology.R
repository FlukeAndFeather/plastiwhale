#Collect Morphology

#---- Set Up ----
library(rorqual.morpho)

# ---- Process Data ----
morphology <- read_csv("data/raw/ENP_whale_lengths.csv") %>% 
  mutate(engulfment_kg = rorq_engulf(species_code, length_m),
         engulfment_m3 = engulfment_kg / 1000)

# ---- Export Data ----
saveRDS(morphology, "data/output/morphology.RDS")
