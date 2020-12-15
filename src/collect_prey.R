#collect_prey
#need diff raw data file for fish and well concat

krill <- read_csv("data/raw/KrillData_Scaling_paper.csv") %>% 
  filter(region == "Temperate") %>% 
  transmute(SpeciesCode = Species, 
            biomass_logmean_kgm3 = log(Biomass), 
            biomass_logsd_kgm3 = log(`Biomass sd`),
            catch_alpha = NA,
            catch_beta = NA) %>% 
  mutate(prey_type = "krill") %>% 
  

# Catch percentage is beta distributed
mu <- 0.375
var <- 0.243^2
# Replace 7.8 with the geometric mean of fish density, replace 1 with geometric sd of fish density
fish <- tibble(SpeciesCode = "mn", 
               biomass_logmean_kgm3 = log(7.8),
               biomass_logsd_kgm3 = log(1), prey_type = "fish",
               catch_alpha = ((1 - mu) / var - 1 / mu) * mu ^ 2,
               catch_beta = catch_alpha * (1 / (mu - 1)))

prey <- rbind(krill, fish)

# Save to RDS
saveRDS(prey, "data/output/prey.RDS")
