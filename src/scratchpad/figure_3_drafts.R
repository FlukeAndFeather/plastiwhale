#3. Plastic Retention from Water ----
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

#Function Need for Raincloud ----
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


# Summary Stats and Plot ----
lb <- function(x) mean(x) - sd(x) #lowerbound

ub <- function(x) mean(x) + sd(x) #upperbound

sum.stats <- ddply(results, ~species_code,  summarise, 
                   mean = mean(retained_plastic), median = median(retained_plastic),
                   lower = lb(retained_plastic), upper = ub(retained_plastic))
sum.stats

summary_results <- results %>% 
  group_by(species_code, prey_type, dielperiod) %>% 
  summarise(lunge_25 = quantile(daily_lunges, 0.25, na.omit = TRUE),
            lunge_med = median(daily_lunges, na.omit = TRUE),
            lunge_75 = quantile(daily_lunges, 0.75, na.omit = TRUE))


#need to figure out how to separate by prey AND species

# results <- results %>% 
#   drop_na(plastic_prey)

plastic_retained <- results %>% 
  mutate(species_prey = paste(species_code, prey_type),
         species_prey = fct_reorder(species_prey, retained_plastic)) %>% 
  group_by(species_prey) %>% 
  filter(retained_plastic <= quantile(retained_plastic, 0.95)) %>% 
  ggplot(aes(y = retained_plastic, x = species_prey, fill = species_prey)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = retained_plastic, color = species_prey), position = position_jitter(width = .35), size = .5, alpha = 0.8) +
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.5) +
  #expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() +
  raincloud_theme

plastic_retained


