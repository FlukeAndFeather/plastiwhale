#Collect Morphology

#---- Set Up ----
library(rorqual.morpho)

# ---- Process Data ----

#these numbers are m3
engulfment_allo_data <- tribble(
  ~species_code, ~slopeMW, ~interceptMW,
  "mn" , 3.24603, -2.15150,
  "bp", 3.54883, -2.85446,
  "bw", 3.667115, -3.024580)


morphology <- read_csv("data/raw/ENP_whale_lengths.csv") %>% 
  #mutate(engulfment_kg = rorq_engulf(species_code, length_m), #these are bad
   #      engulfment_m3 = engulfment_kg / 1000) %>%  #these are bad
  left_join(engulfment_allo_data, by = "species_code") %>% 
  mutate(engulf_m3_skr = length_m^slopeMW * 10^interceptMW)
  

# ---- Export Data ----
saveRDS(morphology, "data/output/morphology.RDS")


#
morphology %>% 
  group_by(species_code) %>% 
  summarise(med_kg = median(engulfment_kg),
            med_m3 = median(engulfment_m3),
            skr_m3 = median(engulf_m3_skr))
  
