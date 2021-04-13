#3. Plastic Retention from Water ----

#Raincloud Builder -----
raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 13),
  axis.title.y = element_text(size = 13),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


"%||%" <- function(a, b) {
  if (!is.null(a))
    a
  else
    b
}

geom_flat_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "dodge",
           trim = TRUE,
           scale = "area",
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(trim = trim,
                    scale = scale,
                    ...)
    )
  }

GeomFlatViolin <-
  ggproto(
    "GeomFlatViolin",
    Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)
      
      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
        dplyr::group_by(.data = ., group) %>%
        dplyr::mutate(
          .data = .,
          ymin = min(y),
          ymax = max(y),
          xmin = x,
          xmax = x + width / 2
        )
    },
    
    draw_group = function(data, panel_scales, coord)
    {
      # Find the points for the line to go all the way around
      data <- base::transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
      
      # Make sure it's sorted properly to draw the outline
      newdata <-
        base::rbind(
          dplyr::arrange(.data = base::transform(data, x = xminv), y),
          dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
        )
      
      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1,])
      
      ggplot2:::ggname("geom_flat_violin",
                       GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },
    
    draw_key = draw_key_polygon,
    
    default_aes = ggplot2::aes(
      weight = 1,
      colour = "grey20",
      fill = "white",
      size = 0.5,
      alpha = NA,
      linetype = "solid"
    ),
    
    required_aes = c("x", "y")
  )



# Summary Stats  ----
# lb <- function(x) mean(x) - sd(x) #lowerbound
# 
# ub <- function(x) mean(x) + sd(x) #upperbound
# 
# sum.stats <- ddply(results, ~species_code,  summarise, 
#                    mean = mean(retained_plastic), median = median(retained_plastic),
#                    lower = lb(retained_plastic), upper = ub(retained_plastic))
# sum.stats
# 
# summary_results <- results %>% 
#   group_by(species_code, prey_type, dielperiod) %>% 
#   summarise(lunge_25 = quantile(daily_lunges, 0.25, na.omit = TRUE),
#             lunge_med = median(daily_lunges, na.omit = TRUE),
#             lunge_75 = quantile(daily_lunges, 0.75, na.omit = TRUE))


# Plotting -----

#Panel 1: Total  Plastic from both Water and Prey (called total_plastic) -----

pal <- c("mn" = "gray30", 
         "bp" = "chocolate3",
         "bw" = "dodgerblue2")

total_plastic_pieces <- results %>% 
  mutate(species_prey = paste(species_code, prey_type),
         species_prey = fct_reorder(species_prey, retained_plastic)) %>% 
  group_by(species_prey) %>% 
  filter(retained_plastic <= quantile(retained_plastic, 0.95),
         total_plastic > 0) %>% 
  ggplot() + 
  geom_flat_violin(aes(x = species_prey, y = total_plastic, color = species_code, fill = species_code), 
                   position = position_nudge(x = 0.35, y = 0), alpha = 0.4) +
  # geom_point(aes(x = species_prey, y = retained_plastic), color = "blue", 
  #            position = position_jitter(width = 0.2), size = .5, alpha = 0.1) +
  geom_boxplot(aes(x = species_prey, y = retained_plastic), color = "blue", 
                width = .2, position = position_nudge(x = 0.15, y = 0), guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  geom_boxplot(aes(x = species_prey, y = plastic_prey), color = "red", 
               width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
# geom_point(aes(x = species_prey, y = plastic_prey), color = "red", 
#              position = position_jitter(width = 0.2), size = .5, alpha = 0.1) +
#   geom_boxplot((aes(x = species_prey, y = total_plastic, color = species_code)),
#                width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  #expand_limits(x = 5.25) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_y_log10(labels = scales::comma, 
                limits = c(100, 500000000),
                breaks = c(100, 10000, 100000, 1000000, 10000000, 100000000)) +
  # annotation_logticks(sides = "b") +
  # scale_color_brewer(palette = "Spectral") +
  # scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() 
  # raincloud_theme
total_plastic_pieces
#blue is plastic from water, red is plastic from prey 
#Number of particles per day (num particles day -1)- b quote


#Panel 2: Weight of Total Plastic  -----

# mean mass of microplastic ~ 0.00443 g so we convert to kg below 

total_plastic_pieces_weight <- results %>% 
  mutate(species_prey = paste(species_code, prey_type),
         species_prey = fct_reorder(species_prey, retained_plastic),
         total_plastic_weight_kg = ((total_plastic * 0.00443)/1000),
         water_plastic_weight_kg = ((retained_plastic * 0.00443)/1000),
         prey_plastic_weight_kg = ((plastic_prey * 0.00443)/1000)) %>% 
  group_by(species_prey) %>% 
  filter(total_plastic > 0, 
         total_plastic_weight_kg > 0, 
         water_plastic_weight_kg > 0 , 
         prey_plastic_weight_kg > 0) %>% 
  #not removing the 95th quantile because the spread isnt that wide 
  ggplot() + 
  geom_flat_violin(aes(x = species_prey, y = total_plastic_weight_kg, color = species_code, fill = species_code), 
                   position = position_nudge(x = 0.3, y = 0), alpha = 0.4) +
  # geom_point(aes(x = species_prey, y = water_plastic_weight_kg), color = "blue", 
  #            position = position_jitter(width = 0.2), size = .5, alpha = 0.1) +
  # geom_point(aes(x = species_prey, y = prey_plastic_weight_kg), color = "red", 
  #            position = position_jitter(width = 0.2), size = .5, alpha = 0.1) +
  geom_boxplot((aes(x = species_prey, y = total_plastic_weight_kg, color = species_code)),
               width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  #expand_limits(x = 5.25) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_y_log10(labels = scales::comma, #no decimal or zero after the 1 
                limits = c(0.0001, 2000), #max space of graph
                breaks = c(0.001, 0.01, 1, 10, 100, 1000)) + #where the ticks are
  annotation_logticks(sides = "l") + 
  # scale_color_brewer(palette = "Spectral") +
  # scale_fill_brewer(palette = "Spectral") +
  #coord_flip() +
  theme_bw() 
# raincloud_theme

total_plastic_pieces_weight
#boxplot rather than dots, logticks, change axis titles, latin names 



#Panel 3: Total Plastic Particles By Species Weight  -----

#not totally sure of the point of this one 


total_plastic_pieces_by_species_weight <- results %>% 
  mutate(species_prey = paste(species_code, prey_type))%>% 
  left_join(species_weight, by = "species_code") %>% 
  mutate(total_plastic_spp_weight = total_plastic/mass_skr,
         retained_plastic_spp_weight = retained_plastic/mass_skr,
         plastic_prey_spp_weight = plastic_prey/mass_skr) %>% 
  group_by(species_prey) %>% 
  filter(total_plastic_spp_weight > 0, 
         retained_plastic_spp_weight > 0, 
         plastic_prey_spp_weight > 0) %>% 
  #including the water and prey breakdown as it may vary cuz of engulfment cap
  #no 95th cuz spread isnt that wide 
  ggplot() + 
  geom_flat_violin(aes(x = species_prey, y = total_plastic_spp_weight, color = species_code, fill = species_code), 
                   position = position_nudge(x = 0.3, y = 0), alpha = 0.4) +
  geom_point(aes(x = species_prey, y = retained_plastic_spp_weight), color = "blue", 
             position = position_jitter(width = 0.2), size = .5, alpha = 0.1) +
  geom_point(aes(x = species_prey, y = plastic_prey_spp_weight), color = "red", 
             position = position_jitter(width = 0.2), size = .5, alpha = 0.1) +
  geom_boxplot((aes(x = species_prey, y = total_plastic_spp_weight, color = species_code)),
               width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  #expand_limits(x = 5.25) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_y_log10(labels = scales::comma, 
                limits = c(0.0001, 20000), #max space of graph
                breaks = c(1, 10, 100, 1000)) + #where the ticks are
  labs(y = "Mass Specific Pieces of Plastic Per KG of Animal") +
  #annotation_logticks(sides = "l") + GET ME TO WORK 
  # scale_color_brewer(palette = "Spectral") +
  # scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() 
# raincloud_theme

total_plastic_pieces_by_species_weight


#Panel 4: Weight of Plastic by Weight of Species ----
# mean mass of microplastic ~ 0.00443 g so we convert to kg below 
#this is ugly. maybe less ugly if not log?

plastic_weight_spp_weight <- results %>% 
  left_join(species_weight, by = "species_code") %>% 
  mutate(species_prey = paste(species_code, prey_type),
         species_prey = fct_reorder(species_prey, retained_plastic),
         total_plastic_spp_weight = (((total_plastic * 0.00443)/1000)/ mass_skr),
         water_weight_spp_weight = (((retained_plastic * 0.00443)/1000)/ mass_skr),
         prey_weight_spp_weight = (((plastic_prey * 0.00443)/1000)/mass_skr)) %>% 
  group_by(species_prey) %>% 
  filter(total_plastic > 0, 
         total_plastic_spp_weight > 0, 
         water_weight_spp_weight > 0 , 
         prey_weight_spp_weight > 0) %>% 
  #not removing the 95th quantile because the spread isnt that wide 
  ggplot() + 
  geom_flat_violin(aes(x = species_prey, y = total_plastic_spp_weight, color = species_code, fill = species_code), 
                   position = position_nudge(x = 0.3, y = 0), alpha = 0.4) +
  geom_point(aes(x = species_prey, y = water_weight_spp_weight), color = "blue", 
             position = position_jitter(width = 0.2), size = .5, alpha = 0.1) +
  geom_point(aes(x = species_prey, y = prey_weight_spp_weight), color = "red", 
             position = position_jitter(width = 0.2), size = .5, alpha = 0.1) +
  geom_boxplot((aes(x = species_prey, y = total_plastic_spp_weight, color = species_code)),
               width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  #expand_limits(x = 5.25) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_y_log10(labels = scales::comma, 
                limits = c(0.000000001, 1), #max space of graph
                breaks = c(0.000000001, 0.0000001, 0.001, 1)) + #where the ticks are
  #annotation_logticks(sides = "l") + GET ME TO WORK 
  # scale_color_brewer(palette = "Spectral") +
  # scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() 
# raincloud_theme

plastic_weight_spp_weight



# Total Plot: ----
ggarrange(total_plastic_pieces,
          total_plastic_pieces_weight,
          total_plastic_pieces_by_species_weight,
          plastic_weight_spp_weight, 
          ncol = 2, nrow = 2)
#add labels, log ticks, no dots boxplots instead 



