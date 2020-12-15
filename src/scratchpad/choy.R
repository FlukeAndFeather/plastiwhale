
Choy <- read_csv("data/raw/Choy_etal_2019_Figure_1b_rawdata.csv")
Choy %>% 
  filter(data_source == "loess model") %>% 
  ggplot(aes(x = med_50, y = DEPTH_m)) +
  geom_ribbon(aes(xmin= lo_05, xmax = hi_95), alpha= 0.25) +
  geom_path(size = 1.5) +
  ylim(-400, 0) +
  theme_classic()

Choy %>% 
  mutate(depth_bucket = cut(-DEPTH_m, 
                            breaks = c(-Inf, 25, 100, 250, Inf),
                            labels = c("Surface (<25m)", "Shallow (25-100m)", "Moderate (100-250m)", "Deep (>250m)"))) %>% 
  group_by(data_source, depth_bucket) %>% 
  summarize_at(vars(lo_05, med_50, hi_95),
               list(mean)) %>% 
  View()


#flip low and high 
#option 2!! 
#model better generalizes to the real world so matt thinks we should use that
#empirical would be best if we really had a whale foraging 
#data that is spatially coincident with where we know whales are feeding in this place and in the water column
#run for both to see what we like best 
#ask elliott- which is better emp or model  
#use #2 and try with both sources 