#Simulations
#---- Set Up ----
#MAKE.R must be run first 

set.seed(172021)

#diel period durations for simulation
dielperiod_durs <- tribble(
  ~dielperiod, ~hours,
  "day",       14,
  "twilight",  2,
  "night",     8
) %>% 
  mutate(dielperiod = factor(dielperiod, levels = c("day", "twilight", "night")))


# ---- Utility Functions ----

#Uses lunge_rates, species+prey combo to provide the simulated number of lunges per depth bucket
simulate_feeding <- function(n, species, prey, empirical_rates) {
  empirical_rates %>% 
    filter(species_code == species, prey_type == prey, period_hours > 0) %>% # filter lunge_rates (the empirical)
    group_by(dielperiod, depth_bucket) %>%   # group by period and depth bucket
    sample_n(n, replace = TRUE) %>%  #sample more than once if there are less than n
    mutate(simulation_id = row_number()) %>% #number the samples 
    ungroup() %>% 
    left_join(dielperiod_durs, by = "dielperiod") %>% #add the sample 'day' hours
    group_by(simulation_id, depth_bucket) %>% #doesnt change appearance, just gets it ready 
    summarise(daily_lunges = sum(lunge_rate * hours)) 
}


#simulates body size 
simulate_morpho <- function(n, species) {
  morphology %>% 
    filter(species_code == species) %>% 
    sample_n(n, replace = TRUE) %>% 
    mutate(simulation_id = row_number())
}

#Simulates the getting of plastic from the water, including retention 

simulate_h2o_plastic <- function(feeding_simulation, retention) {  
  feeding_simulation %>%
    mutate(plastic_conc = rplastic(depth_bucket, bounds = c(0, 20)), 
           total_h2o_plastic = daily_lunges * engulf_m3_skr * plastic_conc) %>% 
    group_by(simulation_id) %>% 
    summarize(retained_plastic = sum(all_plastic) * retention)
} #per day per animal 

#Simulate 
rplastic <- function(n, depth_bucket)
  
  
  
# Simulates the getting of plastic from the prey, pp/kg
  simulate_prey_plastic <- function(feeding_simulation, prey_type, prey_plastic_conc) { # as above, but prey and rlnorm instead of rpois
    feeding_simulation %>%
      group_by_at(vars(-depth_bucket,- daily_lunges)) %>% 
      summarize(daily_lunges = sum(daily_lunges)) %>%
      ungroup() %>% 
      left_join(filter(prey, prey_type == {{ prey_type }}), by = c(species_code = "SpeciesCode")) %>% 
      mutate(biomass_density = pmap_dbl(list(daily_lunges, biomass_logmean_kgm3, biomass_logsd_kgm3), 
                                        ~ mean(rlnorm(n = ..1, meanlog = ..2, sdlog = ..3))),
             catch_percentage = ifelse(prey_type == "krill",
                                       1,
                                       pmap_dbl(list(daily_lunges, catch_alpha, catch_beta),
                                                ~ mean(rbeta(n == ..1, alpha = ..1, beta = ..2)))),
             total_biomass = biomass_density * engulf_m3_skr * daily_lunges * catch_percentage,
             plastic_prey = (total_biomass/0.00008)  * prey_plastic_conc) #weight of indiv krill wet
  }



# ---- Process Data ---- 

# Need to provide n_sim, sim_sp, and sim_prey 
n_sim <- 100
sim_sp <- "bw"
sim_prey <- "krill"


#Provides a simulated whale and a simulated body length for that whale 
lunges_and_length <- simulate_feeding(n = n_sim, species = sim_sp, prey = sim_prey, empirical_rates = lunge_rates) %>% 
  left_join(simulate_morpho(n = n_sim, species = sim_sp), by = "simulation_id")

#Combines plastic from water and plastic from prey. Use for the scenarios by changing parameters 
total_plastic <- simulate_h2o_plastic(feeding_simulation = lunges_and_length, retention =  0.75) %>%  #can be .25, .5, .75
  left_join(simulate_prey_plastic(feeding_simulation = lunges_and_length, "krill", prey_plastic_conc = 0.05), by = "simulation_id") %>% 
  select(-slopeMW, - interceptMW, -catch_alpha, -catch_beta, -catch_percentage)
#change prey and prey_plastic_conc

    