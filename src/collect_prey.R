#collect_prey
#need diff raw data file for fish and well concat
#install.packages("psych")


krill <- read_csv("data/raw/KrillData_Scaling_paper.csv") %>% 
  filter(region == "Temperate") %>% 
  transmute(SpeciesCode = Species, 
            biomass_logmean_kgm3 = log(Biomass), 
            biomass_logsd_kgm3 = log(`Biomass sd`),
            catch_alpha = NA,
            catch_beta = NA) %>% 
  mutate(prey_type = "krill")
  
#Anchovy weights
#these numbers are taken from Sakagawa and Kimura
anch_weight <- c(15, 27, 22.063, 10.823, 11.208, 11.902, 20.8)

# Catch percentage is beta distributed
mu <- 0.375
var <- 0.243^2
# Replace 7.8 with the geometric mean of fish density, replace 1 with geometric sd of fish density
fish <- tibble(SpeciesCode = "mn", 
               biomass_logmean_kgm3 = mean(log(anch_weight)),
               biomass_logsd_kgm3 = sd(log(anch_weight)), 
               prey_type = "fish",
               catch_alpha = ((1 - mu) / var - 1 / mu) * mu ^ 2,
               catch_beta = catch_alpha * (1 / (mu - 1)))

prey <- rbind(krill, fish)

# Save to RDS
saveRDS(prey, "data/output/prey.RDS")




