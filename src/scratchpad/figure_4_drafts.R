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
#need to figure out how to seperate by prey AND species

plastic_from_prey <- ggplot(data = results, aes(y = plastic_prey, x = species_code, fill = species_code)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = plastic_prey, color = species_code), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  ylim(0, 10000000) +
  coord_flip() +
  theme_bw() +
  raincloud_theme

plastic_from_prey
