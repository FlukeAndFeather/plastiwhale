# Simulation Results
set.seed(172021)

species_weight <- tribble(
  ~species_code, ~massSlope, ~massIntercept,
  "bw",           3.3346,     0.3129,
  "bp",           2.595,      1.28388,
  "mn",           2.3373,     1.8535)

all_scenarios <- tibble(
  scenario_name = c("lo", "med", "hi"), 
  scenario = list(scenario_lo, scenario_med, scenario_hi)
  ) %>% 
  mutate(scenario_results = map(scenario, ~ pmap_dfr(.x, run_simulation,
                                                     n_sim = 1000,
                                                     lunge_rates = lunge_rates,
                                                     max_effort = max_effort))) %>% 
  unnest(scenario_results)


all_scenarios <- all_scenarios %>% 
  mutate(latin_name = substr(species_code, start = 1, stop = 2),
         latin_name = case_when(
           latin_name == "bw" ~ "B. musculus",
           latin_name == "bp" ~ "B. physalus",
           latin_name == "mn" ~ "M. novaeangliae")) %>%
  left_join(species_weight, by = "species_code") %>%
  mutate(mass_skr = length_m^massSlope * 10^massIntercept)


## MOVE ME TO FIGURE_FUNCTIONS.R
all_scenarios %>% 
  filter(scenario_name == "med") %>% 
  ggplot(aes(x = paste(species_code, prey_type),
             y = total_plastic,
             color = scenario_name)) +
  geom_boxplot() +
  theme_minimal()
## END MOVE


# saveRDS(all_scenarios, "data/output/all_scenarios.RDS")


#To plot the scenarios, go to figure_functions.R


# Old Stuff #### 
# results_lo <- pmap_dfr(scenario_lo, run_simulation,
#                         n_sim = 1000,
#                         lunge_rates = lunge_rates,
#                         max_effort = max_effort) %>% 
#   mutate(latin_name = substr(species_code, start = 1, stop = 2),
#          latin_name = case_when(
#            latin_name == "bw" ~ "B. musculus",
#            latin_name == "bp" ~ "B. physalus",
#            latin_name == "mn" ~ "M. novaeangliae")) %>%
#   left_join(species_weight, by = "species_code") %>%
#   mutate(mass_skr = length_m^massSlope * 10^massIntercept)
# # 
# 
# results_med <- pmap_dfr(scenario_med, run_simulation,
#                     n_sim = 1000,
#                     lunge_rates = lunge_rates,
#                     max_effort = max_effort)  %>% 
#   mutate(latin_name = substr(species_code, start = 1, stop = 2),
#          latin_name = case_when(
#            latin_name == "bw" ~ "B. musculus",
#            latin_name == "bp" ~ "B. physalus",
#            latin_name == "mn" ~ "M. novaeangliae")) %>% 
#   left_join(species_weight, by = "species_code") %>%
#   mutate(mass_skr = length_m^massSlope * 10^massIntercept)
# 
# results_hi <- pmap_dfr(scenario_hi, run_simulation,
#                         n_sim = 1000,
#                         lunge_rates = lunge_rates,
#                         max_effort = max_effort) %>% 
#   mutate(latin_name = substr(species_code, start = 1, stop = 2),
#          latin_name = case_when(
#            latin_name == "bw" ~ "B. musculus",
#            latin_name == "bp" ~ "B. physalus",
#            latin_name == "mn" ~ "M. novaeangliae")) %>% 
#   left_join(species_weight, by = "species_code") %>%
#   mutate(mass_skr = length_m^massSlope * 10^massIntercept)
# 
# 
# #Results of the Total Simulation
# total_results <- pmap_dfr(scenarios, run_simulation,
#                      n_sim = 1000,
#                      lunge_rates = lunge_rates,
#                      max_effort = max_effort) %>% 
#   left_join(species_weight, by = "species_code") %>%
#   mutate(mass_skr = length_m^massSlope * 10^massIntercept)
#  
