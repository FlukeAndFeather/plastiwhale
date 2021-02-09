#4. Plastic Retention from Prey ----
#numbers seem really high
#just some ideas
plastic_from_prey <- ggplot(results, aes(species_code, plastic_prey,  fill = prey_type)) +
  geom_bar(stat = "identity")

plastic_from_prey


bw_only <- results %>% 
  filter(species_code == "bw") %>%
  drop_na(plastic_prey)

  
ggplot(bw_only, aes(x = species_code, y = plastic_prey)) +
  geom_point() +
  theme_classic()

#using the raincloud
