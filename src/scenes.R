#---- Scenario Set Up ----
#includes all the params we'd like to vary 

prey_plastic_conc <- tribble(
  ~prey,   ~plastic_level, ~plastic_conc,
  "krill", "lo",           0.06,
  "krill", "med",          0.25,
  "krill", "hi",           0.5,
  "fish",  "lo",           0.02,
  "fish",  "med",          0.3,
  "fish",  "hi",           0.77
) %>% 
  mutate(plastic_level = factor(plastic_level, levels = c("lo", "med", "hi")))

prey_size <- tribble(
  ~prey,   ~prey_size_kg,
  "krill", 0.00008,
  "fish",  0.015
)

scenarios <- expand_grid(
  species_code = c("bw", "bp", "mn"),
  retention = c(0.25, 0.5, 0.75),
  prey = c("krill", "fish"),
  plastic_level = factor(c("lo", "med", "hi"), levels = c("lo", "med", "hi"))
) %>% 
  left_join(prey_size, by = "prey") %>% 
  left_join(prey_plastic_conc, by = c("prey", "plastic_level")) %>% 
  filter(prey == "krill" | species_code == "mn")


                
                  