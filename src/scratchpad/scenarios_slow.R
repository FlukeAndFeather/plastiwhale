#Simulations
#All are 0.75 retention and 0.06 freq of occurence for krill, 0.02 freq of occurence for fish

#Blue whale, krill
n_sim <- 1000
sim_sp <- "bw"
sim_prey <- "krill"


lunges_and_length <- simulate_feeding(n = n_sim, species = sim_sp, prey = sim_prey, empirical_rates = lunge_rates) %>% 
  left_join(simulate_morpho(n = n_sim, species = sim_sp), by = "simulation_id")

total_plastic <- simulate_h2o_plastic(feeding_simulation = lunges_and_length, retention =  0.50) %>%  #can be .25, .5, .75
  left_join(simulate_prey_plastic(feeding_simulation = lunges_and_length, "krill", prey_plastic_conc = 0.06), by = "simulation_id") %>% 
  select(-slopeMW, - interceptMW, -catch_alpha, -catch_beta, -catch_percentage, -data_source)

blue_krill <- total_plastic %>% 
  group_by(simulation_id) %>% 
  summarise(total_daily_lunges = sum(daily_lunges),
            total_biomass = sum(total_biomass, na.rm = TRUE),
            total_retained = first(retained_plastic),
            total_prey_plastic = sum(plastic_prey, na.rm = TRUE))
  
  summary_blue <- blue_krill %>% 
  summarise(iqr_dailylunges = quantile(total_daily_lunges, 
                                 probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
            iqr_biomass = quantile(total_biomass, 
                              probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
            iqr_retained = quantile(total_retained, 
                              probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 
            iqr_preyplastic = quantile(total_prey_plastic, 
                              probs = c(0.25, 0.5, 0.75), na.rm = TRUE)) %>% 
    mutate(species = "bw_krill")



#Fin whale, krill
n_sim <- 1000
sim_sp <- "bp"
sim_prey <- "krill"


lunges_and_length <- simulate_feeding(n = n_sim, species = sim_sp, prey = sim_prey, empirical_rates = lunge_rates) %>% 
  left_join(simulate_morpho(n = n_sim, species = sim_sp), by = "simulation_id")

total_plastic <- simulate_h2o_plastic(feeding_simulation = lunges_and_length, retention =  0.5) %>%  #can be .25, .5, .75
  left_join(simulate_prey_plastic(feeding_simulation = lunges_and_length, "krill", prey_plastic_conc = 0.06), by = "simulation_id") %>% 
  select(-slopeMW, - interceptMW, -catch_alpha, -catch_beta, -catch_percentage)


fin_krill <- total_plastic %>% 
  group_by(simulation_id) %>% 
  summarise(total_daily_lunges = sum(daily_lunges),
            total_biomass = sum(total_biomass, na.rm = TRUE),
            total_retained = first(retained_plastic),
            total_prey_plastic = sum(plastic_prey, na.rm = TRUE))

summary_fin <- fin_krill %>% 
  summarise(iqr_dailylunges = quantile(total_daily_lunges, 
                                       probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
            iqr_biomass = quantile(total_biomass, 
                                   probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
            iqr_retained = quantile(total_retained, 
                                    probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 
            iqr_preyplastic = quantile(total_prey_plastic, 
                                       probs = c(0.25, 0.5, 0.75), na.rm = TRUE)) %>% 
  mutate(species = "bp_krill")


#Humpback whale, krill
n_sim <- 1000
sim_sp <- "mn"
sim_prey <- "krill"


lunges_and_length <- simulate_feeding(n = n_sim, species = sim_sp, prey = sim_prey, empirical_rates = lunge_rates) %>% 
  left_join(simulate_morpho(n = n_sim, species = sim_sp), by = "simulation_id")

total_plastic <- simulate_h2o_plastic(feeding_simulation = lunges_and_length, retention =  0.5) %>%  #can be .25, .5, .75
  left_join(simulate_prey_plastic(feeding_simulation = lunges_and_length, "krill", prey_plastic_conc = 0.06), by = "simulation_id") %>% 
  select(-slopeMW, - interceptMW, -catch_alpha, -catch_beta, -catch_percentage)


humpback_krill <- total_plastic %>% 
  group_by(simulation_id) %>% 
  summarise(total_daily_lunges = sum(daily_lunges),
            total_biomass = sum(total_biomass, na.rm = TRUE),
            total_retained = first(retained_plastic),
            total_prey_plastic = sum(plastic_prey, na.rm = TRUE))

summary_humpback <- humpback_krill %>% 
  summarise(iqr_dailylunges = quantile(total_daily_lunges, 
                                       probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
            iqr_biomass = quantile(total_biomass, 
                                   probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
            iqr_retained = quantile(total_retained, 
                                    probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 
            iqr_preyplastic = quantile(total_prey_plastic, 
                                       probs = c(0.25, 0.5, 0.75), na.rm = TRUE)) %>% 
  mutate(species = "mn_krill")

#Humpback whale, fish 
n_sim <- 1000
sim_sp <- "mn"
sim_prey <- "fish"


lunges_and_length <- simulate_feeding(n = n_sim, species = sim_sp, prey = sim_prey, empirical_rates = lunge_rates) %>% 
  left_join(simulate_morpho(n = n_sim, species = sim_sp), by = "simulation_id")

total_plastic <- simulate_h2o_plastic(feeding_simulation = lunges_and_length, retention =  0.5) %>%  #can be .25, .5, .75
  left_join(simulate_prey_plastic(feeding_simulation = lunges_and_length, "fish", prey_plastic_conc = 0.02), by = "simulation_id") %>% 
  select(-slopeMW, - interceptMW, -catch_alpha, -catch_beta, -catch_percentage)

humpback_fish <- total_plastic %>% 
  group_by(simulation_id) %>% 
  summarise(total_daily_lunges = sum(daily_lunges),
            total_biomass = sum(total_biomass, na.rm = TRUE),
            total_retained = first(retained_plastic),
            total_prey_plastic = sum(plastic_prey, na.rm = TRUE))

summary_humpback_fish <- humpback_fish %>% 
  summarise(iqr_dailylunges = quantile(total_daily_lunges, 
                                       probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
            iqr_biomass = quantile(total_biomass, 
                                   probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
            iqr_retained = quantile(total_retained, 
                                    probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 
            iqr_preyplastic = quantile(total_prey_plastic, 
                                       probs = c(0.25, 0.5, 0.75), na.rm = TRUE)) %>% 
  mutate(species = "mn_fish")


all_whale_iqr <- rbind(summary_blue, summary_fin, summary_humpback, summary_humpback_fish)
write.csv(all_whale_iqr, "all_whale_iqr.csv")

