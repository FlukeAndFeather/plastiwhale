#Scaling Figure
#Requires stats.R to run 

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


#Needs statistics in order to run 
scaling_all <- med_scenario %>%
  select(5, 6, 7, 8, 9, 19, 23) %>% 
  filter(prey_type == "krill",
         total_plastic > 10000)




scaling_engulf_mass <- scaling_all %>% 
  ggplot() + 
  geom_point(aes(x = log10(mass_skr), y = log10(engulf_m3_skr),
                                              color = species_code, alpha = 1/10)) +
  geom_point(aes(x = log10(mass_skr), y = log10(daily_lunges),
                                     color = species_code, alpha = 1/10), shape = 2) +
  # geom_point(aes(x = log10(mass_skr), y = log10(total_plastic), 
  #                                    color = species_code, alpha = 1/10)) +
  stat_smooth(aes(x = log10(mass_skr), y = log10(engulf_m3_skr)), color = "black", method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(intercept = coef(feeding_model)[1],
              slope = coef(feeding_model)[2],
              color = "blue") +
  #stat_smooth(aes(x = log10(mass_skr), y = log10(total_plastic)), color = "red", method = "lm", formula = y ~ x, se = FALSE) +
  scale_color_manual(values= pal, 
                      name="Species",
                      #breaks=c("ctrl", "trt1", "trt2"),
                      labels=c("B. physalus", "B. musculus", "M. novaeangliae"))+
  scale_y_continuous("log Engulfment Capacity", 
    sec.axis = sec_axis(~ . * 1, name = "log Daily Lunges"))+
  labs(y =  bquote(''),
       x = "log Mass")+
  theme_minimal()
  
scaling_engulf_mass



scaling_plastic <- scaling_all %>% 
  ggplot() + 
  geom_point(aes(x = log10(mass_skr), y = log10(total_plastic), 
                 color = species_code, alpha = 1/10,), shape = 15) +
  #stat_smooth(aes(x = log10(mass_skr), y = log10(engulf_m3_skr)), color = "black", method = "lm", formula = y ~ x, se = FALSE) +
  #stat_smooth(aes(x = log10(mass_skr), y = log10(daily_lunges)), color = "blue", method = "lm", formula = y ~ x, se = FALSE) +
  stat_smooth(aes(x = log10(mass_skr), y = log10(total_plastic)), color = "red", method = "lm", formula = y ~ x, se = FALSE) +
  scale_color_manual(values= pal, 
                     name="Species",
                     #breaks=c("ctrl", "trt1", "trt2"),
                     labels=c("B. physalus", "B. musculus", "M. novaeangliae"))+
  labs(y =  bquote('log Total Plastic Ingested'),
       x = "log Mass")+
  theme_minimal()

scaling_plastic



combo_plastic <- ggarrange(scaling_plastic, 
                           scaling_fig,
                         labels = c("A", "B"), 
                         ncol=1, nrow = 2)
combo_plastic

