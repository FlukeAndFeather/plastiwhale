#Scaling Graph
#Total plastic by mass

whale_latin_names <- tribble(
  ~species,          ~prey,   ~order, ~species_code,
  "B. musculus",     "krill", 4,      "bw",
  "B. physalus",     "krill", 3,      "bp",
  "M. novaeangliae", "fish",  1,      "mn",
  "M. novaeangliae", "krill", 2,      "mn"
) %>% 
  # Create a new column with the proper species/prey format
  mutate(tick_labels = fct_reorder(
    str_glue("italic(\"{species}\") ~ \"({prey})\""),
    order
  ))

pal <- c("mn" = "gray30", 
         "bp" = "chocolate3",
         "bw" = "dodgerblue2")




#mass specific vs tl
Scaling_data <- med_weight %>% 
  group_by(prey_type, species_code) %>% 
  summarize(mean_total_plastic_spp_weight = mean(total_plastic_spp_weight),
            sd_total_plastic_spp_weight = sd(total_plastic_spp_weight),
            low_total_plastic_spp_weight = quantile(total_plastic_spp_weight, 0.25),
            high_total_plastic_spp_weight = quantile(total_plastic_spp_weight, 0.75)) 


scaling_fig <- ggplot(med_weight, aes(y = log(total_plastic_spp_weight), x = log(length_m),
                                      color = paste(species_code, prey_type))) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "log Total Length (m)") +
  labs(y = "log Mass-Specific Plastic Pieces") +
  theme_classic(base_size = 10) +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "right",
        legend.text = element_text(face = "italic")) +
  labs(color = "Species Code, Prey Type")  + 
  scale_color_discrete(name = "Species Code, Prey Type", 
                       labels = c("B. physalus, krill", "B. musculus, krill",
                                             "M. novaeangliae, fish", "M. novaeangliae, krill")) +
scale_color_manual(values=c("gray30", "chocolate3", "dodgerblue2", "darkgreen"))
scaling_fig

#LM Models 
linear_model_spp <- function(species_code, prey_type, weight_scenario_data) {
    lm(log(total_plastic_spp_weight) ~ log(length_m):species_code + species_code + prey_type, data = weight_scenario_data)
}

plastic_scaling <- results_med %>% 
  filter(total_plastic > 0) %>% 
  nest(plastic = -c(species_code, prey_type)) %>% 
  mutate(model = map(plastic, ~ lm(log10(total_plastic) ~ log10(length_m), 
                                   data = .x)),
         intercept = map_dbl(model, ~ coef(.x)[1]),
         slope = map_dbl(model, ~ coef(.x)[2]))

linear_model_spp("bp", "krill", med_weight)
linear_model_spp("bw", "krill", med_weight)
linear_model_spp("mn", "krill", med_weight)
linear_model_spp("mn", "fish", med_weight)

 
across_spp_lm <- lm(log(total_plastic_spp_weight) ~ log(length_m), data = med_weight)
across_spp_lm

bp_lm_data <- med_weight %>% 
  filter(species_code == "bp")
bp_lm <- lm(log(total_plastic_spp_weight) ~ log(length_m), data = bp_lm_data)
summary(bp_lm)

bw_lm_data <- med_weight %>% 
  filter(species_code == "bw")
bw_lm <- lm(log(total_plastic_spp_weight) ~ log(length_m), data = bw_lm_data)
summary(bw_lm)

mn_fish_lm_data <- med_weight %>% 
  filter(species_code == "mn",
         prey_type == "fish")
mn_fish_lm <- lm(log(total_plastic_spp_weight) ~ log(length_m), data = mn_fish_lm_data)
summary(mn_fish_lm)

mn_krill_lm_data <- med_weight %>% 
  filter(species_code == "mn",
         prey_type == "krill")
mn_krill_lm <- lm(log(total_plastic_spp_weight) ~ log(length_m), data = mn_krill_lm_data)
summary(mn_krill_lm)

