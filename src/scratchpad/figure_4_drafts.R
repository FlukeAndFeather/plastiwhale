#4. Plastic Retention from Prey Raincloud ----

#using the raincloud
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


lb <- function(x) mean(x) - sd(x) #lowerbound

ub <- function(x) mean(x) + sd(x) #upperbound

sum.stats <- ddply(results, ~species_code,  summarise, 
                   mean = mean(plastic_prey), median = median(plastic_prey),
                   lower = lb(plastic_prey), upper = ub(plastic_prey))
sum.stats

#why so flat 

plastic_from_prey <- results %>% 
  mutate(species_prey = paste(species_code, prey_type),
         species_prey = fct_reorder(species_prey, plastic_prey)) %>% 
  group_by(species_prey) %>% 
  filter(plastic_prey <= quantile(plastic_prey, 0.95)) %>% 
  ggplot(aes(y = plastic_prey, x = species_prey, fill = species_prey)) +
  geom_flat_violin(position = position_nudge(x = 0.1, y = 0), alpha = .8) +
  geom_point(aes(y = plastic_prey, color = species_prey), 
             position = position_jitter(width = 0.05), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() +
  raincloud_theme

plastic_from_prey




# Plastic Retention from Prey Density ----- GROSS


plastic_prey_density <- ggplot(results, aes(retained_plastic, color = species_code)) +
  geom_density()
plastic_prey_density



# 