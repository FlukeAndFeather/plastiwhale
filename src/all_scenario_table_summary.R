#Summaries 
#go to figure functions
#Split into scenarios

set.seed(172021)
#HIGH SCENARIO
hi_scenario_summaries <- all_scenarios %>%
  filter(scenario_name == "hi") %>% #can change or turn off 
  mutate(total_plastic_weight_kg = ((total_plastic * 0.00443)/1000), #weight of pieces 
         water_plastic_weight_kg = ((retained_plastic * 0.00443)/1000), #weight of pieces (water)
         prey_plastic_weight_kg = ((plastic_prey * 0.00443)/1000), #weight of pieces (prey)
         mass_specific_total_plastic = total_plastic/mass_skr, #mass (of whale)- specific
         mass_specific_retained_plastic = retained_plastic/mass_skr, #mass (of whale)- specific (water)
         mass_specific_plastic_prey = plastic_prey/mass_skr, #mass (of whale)- specific (krill)
         massbymass_total_plastic = (total_plastic_weight_kg/ mass_skr), #mass by mass
         massbymass_water_weight = (water_plastic_weight_kg/ mass_skr), #mass by mass
         massbymass_prey_weight = (prey_plastic_weight_kg/mass_skr), #mass by mass
         ) %>% # here you calculate total weight of plastic, mass-specific plastic count, mass-specific plastic mass
  group_by(species_code, prey_type) %>% 
  summarize(across(c(daily_lunges, 
                     retained_plastic, plastic_prey, total_plastic, #plastic pieces
                     water_plastic_weight_kg, prey_plastic_weight_kg, total_plastic_weight_kg, #plastic pieces mass 
                     mass_specific_retained_plastic, mass_specific_plastic_prey, mass_specific_total_plastic, #mass-specific
                     massbymass_water_weight, massbymass_prey_weight, massbymass_total_plastic, #mass by mass
                     ), # Then add those variables here
                   list(med = median, p25 = ~quantile(.x, 0.25), p75 = ~quantile(.x, 0.75))),
            .groups = "drop")

hi <- write.csv(hi_scenario_summaries,"hi_scenario_summaries.csv")


#MEDIUM SCENARIO
set.seed(172021)
med_scenario_summaries <- all_scenarios %>%
  filter(scenario_name == "med") %>% #can change or turn off 
  mutate(total_plastic_weight_kg = ((total_plastic * 0.00443)/1000), #weight of pieces 
         water_plastic_weight_kg = ((retained_plastic * 0.00443)/1000), #weight of pieces (water)
         prey_plastic_weight_kg = ((plastic_prey * 0.00443)/1000), #weight of pieces (prey)
         mass_specific_total_plastic = total_plastic/mass_skr, #mass (of whale)- specific
         mass_specific_retained_plastic = retained_plastic/mass_skr, #mass (of whale)- specific (water)
         mass_specific_plastic_prey = plastic_prey/mass_skr, #mass (of whale)- specific (prey)
         massbymass_total_plastic = (total_plastic_weight_kg/ mass_skr), #mass by mass
         massbymass_water_weight = (water_plastic_weight_kg/ mass_skr), #mass by mass
         massbymass_prey_weight = (prey_plastic_weight_kg/mass_skr), #mass by mass
  ) %>% # here you calculate total weight of plastic, mass-specific plastic count, mass-specific plastic mass
  group_by(species_code, prey_type) %>% 
  summarize(across(c(daily_lunges, 
                     retained_plastic, plastic_prey, total_plastic, #plastic pieces
                     water_plastic_weight_kg, prey_plastic_weight_kg, total_plastic_weight_kg, #plastic pieces mass 
                     mass_specific_retained_plastic, mass_specific_plastic_prey, mass_specific_total_plastic, #mass-specific
                     massbymass_water_weight, massbymass_prey_weight, massbymass_total_plastic, #mass by mass
  ), # Then add those variables here
  list(med = median, p25 = ~quantile(.x, 0.25), p75 = ~quantile(.x, 0.75))),
  .groups = "drop")

med <- write.csv(med_scenario_summaries,"med_scenario_summaries.csv")


# #answer for savoca 
# foo <- all_scenarios %>%
#   filter(scenario_name == "med") %>%
#   filter(total_plastic > 0, total_biomass >0) %>% 
#   mutate(particlesbykg = (total_plastic/total_biomass),
#          ) %>% 
#   group_by(species_code, prey_type) %>% 
#   summarize(across(c(daily_lunges, 
#                      particlesbykg,
#                      ),
#             list(med = median, p25 = ~quantile(.x, 0.25), p75 = ~quantile(.x, 0.75))), 
# .groups = "drop")
# 
# 


#LOW SCENARIO
set.seed(172021)
lo_scenario_summaries <- all_scenarios %>%
  filter(scenario_name == "lo") %>% #can change or turn off 
  mutate(total_plastic_weight_kg = ((total_plastic * 0.00443)/1000), #weight of pieces 
         water_plastic_weight_kg = ((retained_plastic * 0.00443)/1000), #weight of pieces (water)
         prey_plastic_weight_kg = ((plastic_prey * 0.00443)/1000), #weight of pieces (prey)
         mass_specific_total_plastic = total_plastic/mass_skr, #mass (of whale)- specific
         mass_specific_retained_plastic = retained_plastic/mass_skr, #mass (of whale)- specific (water)
         mass_specific_plastic_prey = plastic_prey/mass_skr, #mass (of whale)- specific (krill)
         massbymass_total_plastic = (total_plastic_weight_kg/ mass_skr), #mass by mass
         massbymass_water_weight = (water_plastic_weight_kg/ mass_skr), #mass by mass
         massbymass_prey_weight = (prey_plastic_weight_kg/mass_skr), #mass by mass
  ) %>% # here you calculate total weight of plastic, mass-specific plastic count, mass-specific plastic mass
  group_by(species_code, prey_type) %>% 
  summarize(across(c(daily_lunges, 
                     retained_plastic, plastic_prey, total_plastic, #plastic pieces
                     water_plastic_weight_kg, prey_plastic_weight_kg, total_plastic_weight_kg, #plastic pieces mass 
                     mass_specific_retained_plastic, mass_specific_plastic_prey, mass_specific_total_plastic, #mass-specific
                     massbymass_water_weight, massbymass_prey_weight, massbymass_total_plastic, #mass by mass
  ), # Then add those variables here
  list(med = median, p25 = ~quantile(.x, 0.25), p75 = ~quantile(.x, 0.75))),
  .groups = "drop")

lo <- write.csv(lo_scenario_summaries,"lo_scenario_summaries.csv")

