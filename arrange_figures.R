#Arrange Figures ----

med_panel <- ggarrange(total_plastic_med,
                       plastic_weight_med,
                       pieces_by_species_weight_med,
                       mass_specific_med, 
                       ncol = 2, nrow = 2, 
                       labels = c("A", "B", "C", "D"), label.x = 0.07)

annotate_figure(med_panel,  bottom = text_grob("Species and prey type"))
med_panel

ggarrange(med_panel) %>%
  ggexport(filename = "med_panel.pdf")