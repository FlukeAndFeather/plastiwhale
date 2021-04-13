#  SIMULATION CHANGE ATTEMPT

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
    mutate(plastic_conc = rplastic(depth_bucket, bounds = c(0, 20)), 
           total_h2o_plastic = daily_lunges * engulf_m3_skr * plastic_conc) %>% 
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



# ---- Simulation Variables ---- 
prey_plastic_conc <- tribble(
  ~prey,   ~plastic_level, ~plastic_conc,
  "krill", "lo",           0.06,
  "krill", "med",          0.25,
  "krill", "hi",           0.5,
  "fish",  "lo",           0.02,
  "fish",  "med",          0.3,
  "fish",  "hi",           0.77
) %>% 
  mutate(plastic_level = factor(plastic_level, levels = c("lo", "med", "hi")))

scenarios <- expand_grid(
  species_code = c("bw", "bp", "mn"),
  retention = c(0.25, 0.5, 0.75),
  prey = c("krill", "fish"),
  plastic_level = factor(c("lo", "med", "hi"), levels = c("lo", "med", "hi"))
) %>% 
  left_join(prey_plastic_conc, by = c("prey", "plastic_level")) %>% 
  filter(prey == "krill" | species_code == "mn")




#Provides a simulated whale and a simulated body length for that whale  
lunges_and_length <- simulate_feeding(n = n_sim, 
                                      species_code = scenarios$species_code,
                                      prey = scenarios$prey,
                                      empirical_rates = lunge_rates) %>% 
  left_join(simulate_morpho(n = n_sim, 
                            species_code = scenarios$species_code), 
            by = "simulation_id") %>% 
  left_join(max_effort, by = "species_code") %>% 
  filter(daily_lunges < lunge_95)

#Combines plastic from water and plastic from prey. Use for the scenarios by changing parameters 
total_plastic <- simulate_h2o_plastic(feeding_simulation = lunges_and_length,
                                      retention = scenarios$retention),
  left_join(simulate_prey_plastic(feeding_simulation = lunges_and_length,
                                  prey = scenarios$prey,
                                  prey_plastic_conc = scenarios$plastic_conc,  
                                  indiv_prey_kg = indiv_prey),
            by = "simulation_id") %>% 
  select(-slopeMW, - interceptMW, -catch_alpha, -catch_beta, -catch_percentage)



#doesnt make sense to me to have the two functions and then commbine them into a function?
#how does it get the info from scenario? 



simulate_plastic_consumption <- function(species_code, retention, prey, plastic_conc) { 
  simulate_feeding(n, species_code, prey, empirical_rates) %>% 
    left_join(simulate_morpho(n, species_code)) %>% 
    by = "simulation_id" %>% 
      
    
}


simulate_plastic_consumption <- function(species_code, retention, prey, plastic_conc) { 

  
  
}




