#FIGURES
#
# pal <- c("B. bonaerensis" = "firebrick3",
#          "B. borealis" = "goldenrod2", 
#          "B. brydei" = "darkorchid3",
#          "M. novaeangliae" = "gray30", 
#          "B. physalus" = "chocolate3",
#          "B. musculus" = "dodgerblue2")

pal <- c("mn" = "gray30", 
         "bp" = "chocolate3",
         "bw" = "dodgerblue2")

#1. The Map Figure ----


  

#2. 5 Panel Depth of Plastic in Water ----
lunge_depth_species <- ggplot(lunges, aes(lunge_depth)) +
  #geom_histogram(binwidth = 5) +
  geom_density(aes(color = species_code, fill = species_code), alpha = 0.2) +
  facet_wrap(vars(species_code, prey_type), nrow = 1, scales = "free_x") +
  coord_flip(xlim = c(400, 0)) +
  scale_x_reverse() +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(x = "Depth (m)",
       y = "Lunge Count") +
  theme_minimal() +
  theme(legend.position = "none") 

lunge_depth_species   


Choy_Kashi_graph <- Choy_Kashi %>% #smooth me and add laurens data 
  # filter(data_source == "observed values") %>% 
  # mutate(DEPTH_m = -DEPTH_m) %>% 
  ggplot(aes(x = med_50, y = depth_bucket)) +
  geom_ribbon(aes(xmin= lo_05, xmax = hi_95), alpha= 0.25) +
  geom_path(size = 1.5) +
  labs(x =  bquote('Plastic Concentration'~(particles/m^3)),
       y = "") +
  ylim(400, 0) +
  theme_minimal()

figure2 <- cowplot::plot_grid(lunge_depth_species, Choy_graph, 
                              align = "h", axis = "bt", rel_widths = c(1, 0.5))
figure2
#maybe make the color get darker as deeper? 
#need to make only 1 y axis, depth (m) 

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


lb <- function(x) mean(x) - sd(x) #lowerbound

ub <- function(x) mean(x) + sd(x) #upperbound

sum.stats <- ddply(results, ~species_code,  summarise, 
                   mean = mean(retained_plastic), median = median(retained_plastic),
                   lower = lb(retained_plastic), upper = ub(retained_plastic))
head(sum.stats)
#need to figure out how to seperate by prey AND species

plastic_retained <- ggplot(data = results, aes(y = retained_plastic, x = species_code, fill = species_code)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = retained_plastic, color = species_code), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() +
  raincloud_theme

plastic_retained




#4. Plastic Retention from Prey ----

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
head(sum.stats)
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
  coord_flip() +
  theme_bw() +
  raincloud_theme

plastic_from_prey







#Lunge Depth Bucket by Species 
ggplot(diel_lunges, aes(depth_bucket, species_code, fill = depth_bucket)) +
  geom_col(position = "stack") +
  facet_wrap(vars(species_code, prey_type), scales = "free")


#Lunge Count by Depth by Species
bw_lunges <- lunge_rates %>% 
  filter(species_code == "bw")

ggplot(bw_lunges, aes(n_lunges, depth_bucket)) +
  geom_line()


ggplot(lunge_rates, aes(depth_bucket, color = species_code)) +
  #geom_density() +
  geom_freqpoly() + # my preference is for this one
  #geom_histogram() +
  facet_wrap(.~species_code, scales = "free") + 
  coord_flip() +
  scale_x_reverse() +
  #scale_color_manual(values = pal) +
  labs(x = "Feeding Depth (m)") +
  labs(y = "Lunge Counts") +
  theme_classic(base_size = 18) +
  theme(legend.position = "none",
        strip.text = element_text(face = "italic"))
feeding_depth_by_sp


  
  



  
  
  
  