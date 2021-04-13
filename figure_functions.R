#Functions to make Plots ----

#Needed to make plots -----

whale_latin_names <- tribble(
  ~species,          ~prey,   ~order, ~species_code,
  "B. musculus",     "krill", 2,      "bw",
  "B. physalus",     "krill", 1,      "bp",
  "M. novaeangliae", "fish",  3,      "mn",
  "M. novaeangliae", "krill", 4,      "mn"
) %>% 
  # Create a new column with the proper species/prey format
  mutate(tick_labels = fct_reorder(
    str_glue("italic(\"{species}\") ~ \"({prey})\""),
    order
  ))

pal <- c("mn" = "gray30", 
         "bp" = "chocolate3",
         "bw" = "dodgerblue2")


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






#Total Plastic Figure ----
total_plastic_figure <- function(results){
  figure <- results %>%
    left_join(whale_latin_names, by = "species_code") %>% 
    filter(retained_plastic <= quantile(retained_plastic, 0.95),
           total_plastic > 0) %>%
    ggplot() + 
    geom_flat_violin(aes(x = paste(species_code, prey_type), y = total_plastic, 
                     color = species_code, fill = species_code), 
                     position = position_nudge(x = 0.35, y = 0), alpha = 0.4) +
    geom_boxplot(aes(x = paste(species_code, prey_type), y = retained_plastic), color = "blue", 
                 width = .2, position = position_nudge(x = 0.15, y = 0), guides = FALSE, outlier.shape = NA, alpha = 0.5) +
    geom_boxplot(aes(x = paste(species_code, prey_type), y = plastic_prey), color = "red", 
                 width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    guides(fill = FALSE) + 
    guides(color = FALSE) +
    scale_y_log10(labels = scales::comma) +
    annotation_logticks(sides = "l") +
    #labs(y = " ", x = " ") +
    scale_x_discrete(labels = parse(text = levels(whale_latin_names$tick_labels))) +
    scale_y_log10("Total Plastic (num particles day ^-1)",
                        breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", label_math(10^.x))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank())  
}

total_plastic_med <- total_plastic_figure(all_scenarios)
total_plastic_med

total_plastic_hi <- total_plastic_figure(results_hi)
total_plastic_hi

total_plastic_lo <- total_plastic_figure(results_lo)
total_plastic_lo
 




#Weight of Total Plastic ----
plastic_weight_figure <- function(results){
  figure <- results %>%
    left_join(whale_latin_names, by = "species_code") %>%  
    mutate(total_plastic_weight_kg = ((total_plastic * 0.00443)/1000),
           water_plastic_weight_kg = ((retained_plastic * 0.00443)/1000),
           prey_plastic_weight_kg = ((plastic_prey * 0.00443)/1000)) %>% 
    filter(total_plastic > 0, 
           total_plastic_weight_kg > 0, 
           water_plastic_weight_kg > 0 , 
           prey_plastic_weight_kg > 0) %>% 
    ggplot() + 
    geom_flat_violin(aes(x = paste(species_code, prey_type), y = total_plastic_weight_kg, color = species_code, fill = species_code), 
                     position = position_nudge(x = 0.3, y = 0), alpha = 0.4) +
    geom_boxplot(aes(x = paste(species_code, prey_type), y = water_plastic_weight_kg), color = "blue", 
                 width = .2, position = position_nudge(x = 0.15, y = 0), 
                 guides = FALSE, outlier.shape = NA, alpha = 0.5) +
    geom_boxplot(aes(x = paste(species_code, prey_type), y = prey_plastic_weight_kg), color = "red", 
                 width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    guides(fill = FALSE) +
    guides(color = FALSE) +
    scale_y_log10(labels = function(x) ifelse(x == 0, "0", x),
                  limits = c(0.0001, 1000), #max space of graph
                  breaks = c(0.001, 0.01, 0, 1, 10, 100, 1000)) + #where the ticks are
    annotation_logticks(sides = "l") + 
    labs(y = "Total plastic weight (kg)", x = " ")+
    scale_x_discrete(labels = parse(text = levels(whale_latin_names$tick_labels))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank())
}

plastic_weight_med <- plastic_weight_figure(results_med)
plastic_weight_med

plastic_weight_lo <- plastic_weight_figure(results_lo)
plastic_weight_lo

plastic_weight_hi <- plastic_weight_figure(results_hi)
plastic_weight_hi


 



#Total Plastic Particles By Species Weight  -----
total_pieces_by_species_weight_figure <- function(results) {
  figure <- results %>%
    left_join(whale_latin_names, by = "species_code") %>% 
    #left_join(species_weight, by = "species_code") %>% 
    mutate(total_plastic_spp_weight = total_plastic/mass_skr,
           retained_plastic_spp_weight = retained_plastic/mass_skr,
           plastic_prey_spp_weight = plastic_prey/mass_skr) %>% 
    filter(total_plastic_spp_weight > 0, 
           retained_plastic_spp_weight > 0, 
           plastic_prey_spp_weight > 0) %>% 
    ggplot() + 
    geom_flat_violin(aes(x = paste(species_code, prey_type), y = total_plastic_spp_weight, color = species_code, fill = species_code), 
                     position = position_nudge(x = 0.3, y = 0), alpha = 0.4) +
    geom_boxplot(aes(x = paste(species_code, prey_type), y = retained_plastic_spp_weight), color = "blue", 
                 width = .2, position = position_nudge(x = 0.15, y = 0), 
                 guides = FALSE, outlier.shape = NA, alpha = 0.5) +
    geom_boxplot(aes(x = paste(species_code, prey_type), y = plastic_prey_spp_weight), color = "red", 
                 width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    guides(fill = FALSE) +
    guides(color = FALSE) +
    scale_y_log10(labels = function(x) ifelse(x == 0, "0", x), 
                  limits = c(0.01, 2000), #max space of graph
                  breaks = c(0.1, 1, 10, 100, 1000)) + #where the ticks are
    annotation_logticks(sides = "l") + 
    labs(y = bquote('Mass specific total plastic'~(particles~kg^-1)), x = " ") +
    scale_x_discrete(labels = parse(text = levels(whale_latin_names$tick_labels))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank())
}

pieces_by_species_weight_med <- total_pieces_by_species_weight_figure(results_med)
pieces_by_species_weight_med 

pieces_by_species_weight_hi <- total_pieces_by_species_weight_figure(results_hi)
pieces_by_species_weight_hi

pieces_by_species_weight_lo <- total_pieces_by_species_weight_figure(results_lo)
pieces_by_species_weight_lo 


#Weight of Plastic by Weight of Species: Mass Specific ----
mass_specific <- function(results){
  figure <- results %>% 
    left_join(whale_latin_names, by = "species_code") %>% 
    #left_join(species_weight, by = "species_code") %>% 
    mutate(total_plastic_spp_weight = (((total_plastic * 0.00443)/1000)/ mass_skr),
           water_weight_spp_weight = (((retained_plastic * 0.00443)/1000)/ mass_skr),
           prey_weight_spp_weight = (((plastic_prey * 0.00443)/1000)/mass_skr)) %>% 
    filter(total_plastic > 0, 
           total_plastic_spp_weight > 0, 
           water_weight_spp_weight > 0 , 
           prey_weight_spp_weight > 0) %>% 
    ggplot() + 
    geom_flat_violin(aes(x = paste(species_code, prey_type), y = total_plastic_spp_weight, 
                     color = species_code, fill = species_code), 
                     position = position_nudge(x = 0.3, y = 0), alpha = 0.4) +
    geom_boxplot(aes(x = paste(species_code, prey_type), y = water_weight_spp_weight),
                 color = "blue", 
                 width = .2, position = position_nudge(x = 0.15, y = 0), 
                 guides = FALSE, outlier.shape = NA, alpha = 0.5) +
    geom_boxplot(aes(x = paste(species_code, prey_type), y = prey_weight_spp_weight), 
                 color = "red", 
                 width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    guides(fill = FALSE) +
    guides(color = FALSE) +
    scale_y_log10(labels = function(x) ifelse(x == 0, "0", x), 
                  limits = c(0.0000001, 0.005), #max space of graph
                  breaks = c(0.000000001, 0.0000001, 0.001, 1)) + #where the ticks are
    annotation_logticks(sides = "l") +
    labs(y = "Mass of plastic (kg)/ mass of whale (kg)", x = " ") +
      scale_x_discrete(labels = parse(text = levels(whale_latin_names$tick_labels))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank())
} 
  
mass_specific_med <- mass_specific(results_med)
mass_specific_med

mass_specific_hi <- mass_specific(results_hi)
mass_specific_hi

mass_specific_lo <- mass_specific(results_lo)
mass_specific_lo






