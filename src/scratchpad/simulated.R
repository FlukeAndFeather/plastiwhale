# simulating daily feeding rates

#i moved it outside the function so it would be easier to change over time if needed
dielperiod_durs <- tribble(
  ~dielperiod, ~hours,
  "day",       14,
  "twilight",  2,
  "night",     8
) %>% 
  mutate(dielperiod = factor(dielperiod, levels = c("day", "twilight", "night")))


#function takes in lunge_rates, species+prey combo 
simulate_feeding <- function(n, species, prey, empirical_rates) {
  empirical_rates %>% 
    filter(SpeciesCode == species, prey_type == prey, period_hours > 0) %>% # filter lunge_rates (the empirical)
    group_by(dielperiod,depth_bucket) %>%   # group by period and depth bucket
    sample_n(n, replace = TRUE) %>%  #sample more than once if there are less than n
    mutate(simulation_id = row_number()) %>% #number the samples 
    ungroup() %>% 
    left_join(dielperiod_durs, by = "dielperiod") %>% #add the sample 'day' hours
    group_by(simulation_id,depth_bucket) %>% #doesnt change appearance, just gets it ready 
    summarise(daily_lunges = sum(lunge_rate * hours)) 
}


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


#simulate body size 
simulate_morpho <- function(n, species) {
  morphology %>% 
    filter(species_code == species) %>% 
    sample_n(n, replace = TRUE) %>% 
    mutate(simulation_id = row_number())
}

n_sim <- 100
sim_sp <- "bw"
simulate_feeding(n = n_sim, species = sim_sp, prey = "krill", empirical_rates = lunge_rates) %>% 
  left_join(simulate_morpho(n = n_sim, species = sim_sp), by = "simulation_id") %>% 
  View()


plastic in water:
  # return simulated rates#surface is a mean of 1 particle per m3 sd (check laurens)
  #shallow is 3
  #mod 10 
  # deep is 6 
  # group by depth_bucket and sim_id 
  
  
  
  rlnorm
re set bins in histro ram or rlnorm 
bins of .5 and .1 kilograms, 0 to 5 on the x
