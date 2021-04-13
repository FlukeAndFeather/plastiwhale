#Summaries 
#go to figure functions
set.seed(172021)

# Summary Tables ----
#Function to get median, 25th, 75th, for PIECES of plastic 

scenario_summaries <- all_scenarios %>%
  mutate() %>% # here you calculate total weight of plastic, mass-specific plastic count, mass-specific plastic mass
  group_by(species_code, prey_type) %>% 
  summarize(across(c(daily_lunges, retained_plastic, plastic_prey, total_plastic), # Then add those variables here
                   list(med = median, p25 = ~quantile(.x, 0.25), p75 = ~quantile(.x, 0.75))),
            .groups = "drop")



pieces_summary  <- function(results){
  summary <- results %>% 
  group_by(species_code, prey_type) %>% 
  summarise(median_lunges = median(daily_lunges),
            lunges_0.25 = quantile(daily_lunges, 0.25),
            lunge_0.75 = quantile(daily_lunges, 0.75),
            median_retained = median(retained_plastic),
            retained_0.25 = quantile(retained_plastic, 0.25),
            retained_0.75 = quantile(retained_plastic, 0.75), 
            median_prey_plastic = median(plastic_prey),
            prey_0.25 = quantile(plastic_prey, 0.25),
            prey_0.75 = quantile(plastic_prey, 0.75),
            median_total = median(total_plastic),
            total_0.25 = quantile(total_plastic, 0.25),
            total_0.75 = quantile(total_plastic, 0.75),
            .groups = "drop")
}

med_pieces <- pieces_summary(results_med)

write.csv(med_pieces,"med_pieces_summary.csv")

#Function to find median, 25th, 75th, for MASS of plastic 
#Calculating Masses, big sheet. next function gives means and iqrs
plastic_weight_data <- function(results){
  plastic_weight <- results %>%
    left_join(whale_latin_names, by = "species_code") %>% 
    left_join(species_weight, by = "species_code") %>% 
    mutate(total_plastic_weight_kg = ((total_plastic * 0.00443)/1000),
           water_plastic_weight_kg = ((retained_plastic * 0.00443)/1000),
           prey_plastic_weight_kg = ((plastic_prey * 0.00443)/1000),
           total_plastic_spp_weight = total_plastic/mass_skr,
           retained_plastic_spp_weight = retained_plastic/mass_skr,
           plastic_prey_spp_weight = plastic_prey/mass_skr,
           total_plastic_spp_weight = (((total_plastic * 0.00443)/1000)/ mass_skr),
           water_weight_spp_weight = (((retained_plastic * 0.00443)/1000)/ mass_skr),
           prey_weight_spp_weight = (((plastic_prey * 0.00443)/1000)/mass_skr))%>% 
    filter(total_plastic > 0, 
           total_plastic_weight_kg > 0, 
           water_plastic_weight_kg > 0 , 
           prey_plastic_weight_kg > 0,
           total_plastic_spp_weight > 0, 
           retained_plastic_spp_weight > 0, 
           plastic_prey_spp_weight > 0)
}

med_weight <- plastic_weight_data(results_med)


#Summarizing Masses - needs plastic_weight_data to be run first for the different scenarios
#this provides the summary statistics 
mass_summary  <- function(weights){
  summary <- weights %>% 
    group_by(species_code, prey_type) %>% 
    summarise(median_tot_plas_kg = median(total_plastic_weight_kg),
              tot_plas_kg_0.25 = quantile(total_plastic_weight_kg, 0.25),
              tot_plas_kg_0.75 = quantile(total_plastic_weight_kg, 0.75),
              median_water_plas_kg = median(water_plastic_weight_kg),
              water_plas_kg_0.25 = quantile(water_plastic_weight_kg, 0.25),
              water_plas_kg_0.75 = quantile(water_plastic_weight_kg, 0.75), 
              median_prey_plas_kg = median(prey_plastic_weight_kg),
              prey_plas_kg_0.25 = quantile(prey_plastic_weight_kg, 0.25),
              prey_plas_kg_0.75 = quantile(prey_plastic_weight_kg, 0.75),
              median_tot_plas_spp_kg = median(total_plastic_spp_weight),
              tot_plas_spp_kg_0.25 = quantile(total_plastic_spp_weight, 0.25),
              tot_plas_spp_kg_0.75 = quantile(total_plastic_spp_weight, 0.75),
              median_water_plas_spp_kg = median(retained_plastic_spp_weight),
              water_plas_spp_kg_0.25 = quantile(retained_plastic_spp_weight, 0.25),
              water_plas_spp_kg_0.75 = quantile(retained_plastic_spp_weight, 0.75),
              median_prey_plas_spp_kg = median(plastic_prey_spp_weight),
              prey_plas_spp_kg_0.25 = quantile(plastic_prey_spp_weight, 0.25),
              prey_plas_spp_kg_0.75 = quantile(plastic_prey_spp_weight, 0.75),
              median_total_plas_kg_spp_kg = median(total_plastic_spp_weight),
              total_plas_kg_spp_kg_0.25 = quantile(total_plastic_spp_weight, 0.25),
              total_plas_kg_spp_kg_0.75 = quantile(total_plastic_spp_weight, 0.75),
              median_water_kg_spp_kg = median(water_weight_spp_weight),
              water_kg_spp_kg_0.25 = quantile(water_weight_spp_weight, 0.25),
              water_kg_spp_kg_0.75 = quantile(water_weight_spp_weight, 0.75),
              median_prey_kg_spp_kg = median(prey_weight_spp_weight),
              prey_kg_spp_kg_0.25 = quantile(prey_weight_spp_weight, 0.25),
              prey_kg_spp_kg_0.75 = quantile(prey_weight_spp_weight, 0.75),
              .groups = "drop")
}

med_mass_summary <- mass_summary(med_weight)

write.csv(med_mass_summary,"med_mass_summary.csv")
