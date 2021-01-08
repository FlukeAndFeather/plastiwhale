# simulating daily feeding rates

#i moved it outside the function so it would be easier to change over time if needed
#diel period durations for simulation
dielperiod_durs <- tribble(
  ~dielperiod, ~hours,
  "day",       14,
  "twilight",  2,
  "night",     8
) %>% 
  mutate(dielperiod = factor(dielperiod, levels = c("day", "twilight", "night")))

#plastic distribution in water for simulation (based on Choy and Kashiwabara)
#plastic distribution in water for simulation (based on Choy and Kashiwabara)
plastic_dis <- tribble(
  ~depth_bucket,        ~lambda,
  "Surface (<5m)",       1,
  "Shallow (5-50m)",    3,
  "Moderate (50-150m)",  10,
  "Deep (>150m)",         6
  
) %>% 
  mutate(depth_bucket = factor(depth_bucket, levels = c("Surface (<5m)", "Shallow (5-50m)", "Moderate (50-150m)", "Deep (>150m)")))



#function takes in lunge_rates, species+prey combo and gives the simulated nuber of lunges per depth bucket
simulate_feeding <- function(n, species, prey, empirical_rates) {
  empirical_rates %>% 
    filter(SpeciesCode == species, prey_type == prey, period_hours > 0) %>% # filter lunge_rates (the empirical)
    group_by(dielperiod, depth_bucket) %>%   # group by period and depth bucket
    sample_n(n, replace = TRUE) %>%  #sample more than once if there are less than n
    mutate(simulation_id = row_number()) %>% #number the samples 
    ungroup() %>% 
    left_join(dielperiod_durs, by = "dielperiod") %>% #add the sample 'day' hours
    group_by(simulation_id,depth_bucket) %>% #doesnt change appearance, just gets it ready 
    summarise(daily_lunges = sum(lunge_rate * hours)) 
}


#simulate body size 
simulate_morpho <- function(n, species) {
  morphology %>% 
    filter(species_code == species) %>% 
    sample_n(n, replace = TRUE) %>% 
    mutate(simulation_id = row_number())
}

n_sim <- 10
sim_sp <- "bp"

#provides a simulated whale and a simulated body length for that whale 
lunges_and_length <- simulate_feeding(n = n_sim, species = sim_sp, prey = "krill", empirical_rates = lunge_rates) %>% 
  left_join(simulate_morpho(n = n_sim, species = sim_sp), by = "simulation_id")


simulate_h2o_plastic <- function(feeding_simulation, retention) { #does all things to calculate plastic from water. can set retention  
  feeding_simulation %>%
    left_join(plastic_dis, by = "depth_bucket") %>% 
    mutate(plastic_conc = rpois(n(), lambda),
           all_plastic = daily_lunges*(engulfment_kg/1000)*plastic_conc) %>% 
    group_by(simulation_id) %>% 
    summarize(retained_plastic = sum(all_plastic) * retention)
}


#plastic in prey doesn't vary by depth
# where prey_plastic_conc is in pp/kg
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
           total_biomass = biomass_density * engulfment_m3 * daily_lunges * catch_percentage,
           plastic_prey = total_biomass * prey_plastic_conc)
}


total_plastic <- simulate_h2o_plastic(feeding_simulation = lunges_and_length, 0.75) %>%  #using 0.75 for example 
  left_join(simulate_prey_plastic(feeding_simulation = lunges_and_length, "krill", prey_plastic_conc = 1), by = "simulation_id")


#scenario.r - specify scens and run each 
#3 reten
## 3 krill plastic 


#flextable 




#Pconc
#one coulmn that's depth bucket 
# return simulated rates#surface is a mean of 1 particle per m3 sd (check laurens)
#shallow is 3
#mod 10 
# deep is 6 
# group by depth_bucket and sim_id 

tribble(
  ~species, ~prey_type, ~depth_bucket, ~daily_lunges, ~simulation_id,
  "mn",     "fish",     "Surface",     25[l1],        1,
  "mn",     "fish",     "Shallow",     75[l2],        1,
  "mn",     "fish",     "Moderate",    ...,           1,
  "mn",     "fish",     "Deep",        ...,           1,
  ... (n_simulations x n_buckets) rows total
  "mn",     "fish",     "Deep",        ...,           n
)

#l1 = sampled(day, surface) x day_hours + sampled(twilight, surface) x twilight_hours + sampled(night, surface) x night_hours, line 25



