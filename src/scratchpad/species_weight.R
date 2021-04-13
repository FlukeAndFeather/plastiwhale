# Total Plastic By Median Weight
# This used to be critical for the mass specific graphs. Now I don't think it is
#because I moved the species weight table to scenarios divided and it calculates based on the indiviual total length

#medianLength is from the average of the whale lengths used in this study
length_study <- morphology %>% 
  group_by(species_code) %>% 
  mutate(mean(length_m))


#where did matt get the other intercepts from?
species_weight <- tribble(
  ~species_code, ~massSlope, ~massIntercept,
  "bw",           3.3346,     0.3129,
  "bp",           2.595,      1.28388,
  "mn",           2.3373,     1.8535)


#make as table for supplement 
#include iqr  
#morphology table for supp 

species_weight <- species_weight %>% 
  mutate(mass_skr = medianLength^massSlope * 10^massIntercept)

species_weight_v1 <- tribble(
  ~species_code, ~massSlope, ~massIntercept, ~medianLength,
  "bw",           3.3346,     0.3129,         22.35075,
  "bp",           2.595,      1.28388,        18.87241,
  "mn",           2.3373,     1.8535,         11.52221)