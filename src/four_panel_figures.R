#Figures combined by Scenario

#MED
med_scenario<- ggarrange(total_plastic_med, 
                        plastic_weight_med,
                        pieces_by_species_weight_med,
                        mass_specific_med, 
                        labels = c("A", "B", "C", "D"),
                        ncol = 2, nrow = 2)
med_scenario

annotate_figure(med_scenario,
                bottom = text_grob("Species and Prey type \n Medium Scenario", 
                                   size = 12))


#High
hi_scenario<- ggarrange(total_plastic_hi, 
                         plastic_weight_hi,
                         pieces_by_species_weight_hi,
                         mass_specific_hi, 
                         labels = c("A", "B", "C", "D"),
                         ncol = 2, nrow = 2)
hi_scenario

annotate_figure(hi_scenario,
                bottom = text_grob("Species and Prey type \n High Scenario", 
                                   size = 12))


#Low
lo_scenario<- ggarrange(total_plastic_lo, 
                        plastic_weight_lo,
                        pieces_by_species_weight_lo,
                        mass_specific_lo, 
                        labels = c("A", "B", "C", "D"),
                        ncol = 2, nrow = 2)
lo_scenario

annotate_figure(lo_scenario,
                bottom = text_grob("Species and Prey type \n Low Scenario", 
                                   size = 12))

