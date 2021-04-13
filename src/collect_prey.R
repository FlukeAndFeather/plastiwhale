#collect_prey
#need diff raw data file for fish and well concat

krill <- read_csv("data/raw/KrillData_Scaling_paper.csv") %>% 
  filter(region == "Temperate") %>% 
  transmute(species_code = Species, 
            biomass_logmean_kgm3 = log(Biomass), 
            biomass_logsd_kgm3 = log(`Biomass sd`),
            catch_alpha = NA,
            catch_beta = NA) %>% 
  mutate(prey_type = "krill")
  
#Anchovy weights
#these numbers are taken from Sakagawa and Kimura
anch_weight <- c(25, 27, 22.063, 10.823, 11.208, 11.902, 20.8)
#to be used in total biomass distribution in simulation 

# Catch percentage is beta distributed
mu <- 0.375
var <- 0.243^2
# Replace 7.8 with the geometric mean of fish density, replace 1 with geometric sd of fish density
fish <- tibble(species_code = "mn", 
               biomass_logmean_kgm3 = log(7.8),
               biomass_logsd_kgm3 = log(1), 
               prey_type = "fish",
               catch_alpha = ((1 - mu) / var - 1 / mu) * mu ^ 2,
               catch_beta = catch_alpha * (1 / (mu - 1)))

prey <- rbind(krill, fish)
# 7.8 kg per cubic meter of anchovy in water 
# 

# Save to RDS
saveRDS(prey, "data/output/prey.RDS")

# 2000 kg of anchovy, each weighs 25 g, how many individuals 

# use whale food paper


#frequency of occurrence plastic in krill
# krill_plastic_conc_lo <- 0.06 #from desforges, 1 particle per every 17 
# krill_plastic_conc_med <- 0.25 #arbitrary midline 
# krill_plastic_conc_hi <- 0.5 #from sun 
# 
# #frequency of occurrence plastic in fish
# fish_plastic_conc_lo <- 0.02 #from hipfner
# fish_plastic_conc_med <- 0.3 #from rochman
# fish_plastic_conc_hi <- 0.77 #from tanaka



