#Summaries 
#go to figure functions
set.seed(172021)

# Summary Tables ----
#Function to get median, 25th, 75th, for PIECES of plastic 

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


write.csv(scenario_summaries,"scenario_summaries.csv")

med <- write.csv(scenario_summaries,"med_scenario_summaries.csv")

lo <- write.csv(scenario_summaries,"lo_scenario_summaries.csv")
