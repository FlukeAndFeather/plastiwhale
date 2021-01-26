library(pracma)

Choy <- read_csv("data/raw/Choy_etal_2019_Figure_1b_rawdata.csv") %>% 
  rename(lo_05 = hi_95,
         hi_95 = lo_05)
Choy %>% 
  filter(data_source == "observed values") %>% 
  ggplot(aes(x = med_50, y = DEPTH_m)) +
  geom_ribbon(aes(xmin= lo_05, xmax = hi_95), alpha= 0.25) +
  geom_path(size = 1.5) +
  ylim(-400, 0) +
  theme_classic()

Choy_cut <- Choy %>% 
  mutate(depth_bucket = cut(-DEPTH_m, 
                            breaks = c(-Inf, 5, 50, 150, Inf),
                            labels = c("Surface (<5m)", "Shallow (5-50m)", "Moderate (50-150m)", "Deep (>150m)"))) %>% 
  filter(DEPTH_m > -450) %>% # pick a maximum lunge depth
  group_by(data_source, depth_bucket) %>% 
  summarize_at(vars(lo_05, med_50, hi_95),
               list(mean)) %>% 
  ungroup() %>% 
  filter(data_source == "observed values")


#bounds are plastic pieces 
# Plastic in water sampling 



#option 2!! 
#model better generalizes to the real world so matt thinks we should use that
#empirical would be best if we really had a whale foraging 
#data that is spatially coincident with where we know whales are feeding in this place and in the water column
#run for both to see what we like best 
#ask elliott- which is better emp or model  
#use #2 and try with both sources 



#could we try to refit with a gamma as the new model. this would condense the peak.  
#justified because peak may be due to oversamplng/under at different depths 
#is there a reason for more plastic at top? different kinds? 
#loess has author influence, make own? fnd answer about particle difference  



# Sample function. Draw samples from arbitrary distribution when all you know is 
# 5%, 50%, and 95% quantiles

sample_05_50_95 <- function(n, lo_05, med_50, hi_95, bounds, ...){
  x <- c(0.0, 0.05, 0.5, 0.95, 1.0)
  y <- c(bounds[1], lo_05, med_50, hi_95, bounds[2])
  spinterp(x,y,xp = runif(n))
}

predictions <- Choy_cut %>% 
  mutate(plastic_conc = pmap(., sample_05_50_95, n =1000, bounds = c(0, 20))) %>% 
  unchop(plastic_conc)
ggplot(predictions, aes(plastic_conc)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  facet_wrap(~depth_bucket) +
  theme_minimal()
