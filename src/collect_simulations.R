#Simulations

#---- Functions ----

#----Simulated Daily Feeding Rates 

#Represents average day diel periods in peak feeding season
dielperiod_durs <- tribble(
  ~dielperiod, ~hours,
  "day",       14,
  "twilight",  2,
  "night",     8
) %>% 
  mutate(dielperiod = factor(dielperiod, levels = c("day", "twilight", "night")))

#Function uses species, prey type and empirical lunges rates to  
simulate_feeding <- function(n, species, prey, empirical_rates) {
  empirical_rates %>% 
    filter(SpeciesCode == species, prey_type == prey, period_hours > 0) %>% #  lunge_rates are the empirical
    group_by(dielperiod,depth_bucket) %>%   
    sample_n(n, replace = TRUE) %>%  #sample more than once if there are less than n
    mutate(simulation_id = row_number()) %>% #number the samples 
    ungroup() %>% 
    left_join(dielperiod_durs, by = "dielperiod") %>% #add the sample 'day' hours
    group_by(simulation_id,depth_bucket) %>% 
    summarise(daily_lunges = sum(lunge_rate * hours)) 
}


#----Simulated morphology
#specifically length and engulfment capacity 
simulate_morpho <- function(n, species) {
  morphology %>% 
    filter(species_code == species) %>% 
    sample_n(n, replace = TRUE) %>% 
    mutate(simulation_id = row_number())
}
