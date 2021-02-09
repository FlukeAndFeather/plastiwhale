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
simulate_feeding <- function(n, species_code, prey, empirical_rates) {
  stopifnot(
    n %% 1 == 0,
    n > 0,
    length(species_code) == 1,
    length(prey) == 1,
    is.data.frame(empirical_rates),
    species_code %in% empirical_rates$species_code,
    prey %in% empirical_rates$prey_type
  )
  
  valid_rates <- empirical_rates %>% 
    filter(species_code == !!species_code,
           prey_type == !!prey, 
           period_hours > 0)
  
  stopifnot(nrow(valid_rates) > 0)
  
  valid_rates %>% 
    group_by(dielperiod, depth_bucket) %>%   # group by period and depth bucket
    sample_n(n, replace = TRUE) %>%  #sample more than once if there are less than n. problem with sample_n, missing an arguement?
    mutate(simulation_id = row_number()) %>% #number the samples 
    ungroup() %>% 
    left_join(dielperiod_durs, by = "dielperiod") %>% #add the sample 'day' hours
    group_by(simulation_id, dielperiod, deployID, lunge_rate, period_hours, depth_bucket) %>% #doesnt change appearance, just gets it ready 
    summarise(daily_lunges = sum(lunge_rate * hours),
              .groups = "drop") 
}


#simulates body size 
simulate_morpho <- function(n, species_code) {
  morphology %>% 
    filter(species_code == species_code) %>% 
    sample_n(n, replace = TRUE) %>% 
    mutate(simulation_id = row_number())
}

#Simulates the getting of plastic from the water, including retention 

simulate_h2o_plastic <- function(feeding_simulation, retention) {  
  feeding_simulation %>%
    mutate(water_plastic_conc = rplastic(depth_bucket, bounds = c(0, 20)), 
           total_h2o_plastic = daily_lunges * engulf_m3_skr * water_plastic_conc) %>% 
    group_by(simulation_id) %>% 
    summarize(retained_plastic = sum(total_h2o_plastic) * retention)
  
} #per day per animal 


# Simulates the getting of plastic from the prey, pp/kg
simulate_prey_plastic <- function(feeding_simulation, prey_type, prey_plastic_conc, indiv_prey_kg) { # as above, but prey and rlnorm instead of rpois
  feeding_simulation %>%
    group_by_at(vars(-depth_bucket, -daily_lunges)) %>% 
    summarize(daily_lunges = sum(daily_lunges)) %>%
    ungroup() %>% 
    left_join(filter(prey, prey_type == !!prey_type), by = "species_code") %>% 
    mutate(biomass_density = pmap_dbl(list(daily_lunges, biomass_logmean_kgm3, biomass_logsd_kgm3), 
                                      ~ mean(rlnorm(n = ..1, meanlog = ..2, sdlog = ..3))),
           catch_percentage = ifelse(prey_type == !!prey_type,
                                     1,
                                     pmap_dbl(list(daily_lunges, catch_alpha, catch_beta),
                                              ~ mean(rbeta(n == ..1, alpha = ..1, beta = ..2)))),
           total_biomass = biomass_density * engulf_m3_skr * daily_lunges * catch_percentage,
           prey_individuals = total_biomass/indiv_prey_kg, #the literal num of indiv
           plastic_prey = prey_individuals  * prey_plastic_conc) #the num of indiv times pieces of plastic per indiv, laving jus tpieces of plastic
}

# species_code - prey_size_kg are columns in scenarios data.frame
# n_sim - max_effort must be supplied to pmap separately
run_simulation <- function(species_code, retention, prey, plastic_conc, 
                           plastic_level, prey_size_kg, n_sim, lunge_rates, 
                           max_effort) { 
  #Provides a simulated whale and a simulated body length for that whale  
  simulated_whales <- simulate_feeding(n = n_sim, 
                                       species_code = species_code, #scenarios$species_code
                                       prey = prey,       #scenarios$prey
                                       empirical_rates = lunge_rates) %>% 
    left_join(simulate_morpho(n = n_sim, 
                              species_code = species_code),   #scenarios$species_code
              by = "simulation_id") %>% 
    left_join(max_effort, by = "species_code") %>% 
    filter(daily_lunges < lunge_95)
  
  #Combines plastic from water and plastic from prey. Use for the scenarios by changing parameters 
  simulated_plastic <- simulate_h2o_plastic(feeding_simulation = simulated_whales,
                                            retention = retention) %>%  #scenarios$retention
    left_join(simulate_prey_plastic(feeding_simulation = simulated_whales,
                                    prey,                    #scenarios$prey
                                    prey_plastic_conc = plastic_conc,   #scenarios$plastic_conc 
                                    indiv_prey_kg = prey_size_kg),
              by = "simulation_id") %>% 
    select(-slopeMW, - interceptMW, -catch_alpha, -catch_beta, -catch_percentage)
  
  simulated_plastic
}


#Results of the Simulation
results <- pmap_dfr(scenarios, run_simulation,
                    n_sim = 1000,
                    lunge_rates = lunge_rates,
                    max_effort = max_effort)

saveRDS(results, "data/output/results.RDS")





