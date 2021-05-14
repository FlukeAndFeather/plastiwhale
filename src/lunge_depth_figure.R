#Choy_Kashi Lunge Figure

pal <- c("mn" = "gray30", 
         "bp" = "chocolate3",
         "bw" = "dodgerblue2")

Choy <- read_csv("data/raw/Choy_etal_2019_Figure_1b_rawdata.csv") %>% 
  rename(lo_05 = hi_95,
         hi_95 = lo_05) %>% 
  filter(data_source == "observed values",
         DEPTH_m > -500.0) %>% 
  mutate(depth_m = DEPTH_m) %>% 
  select(-DEPTH_m)


Kashi <- tribble(
  ~depth_m, ~lo_05, ~med_50,    ~hi_95,     ~data_source,
  0,        0,     0.4333333,  	4.471667,   "observed values"
)

Choy_Kashi_data <- rbind(Choy, Kashi) %>% 
  arrange(desc(depth_m))

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


#Panel Depth of Plastic in Water ----
lunge_depth_species <- lunges %>% 
  mutate(lunge_depth = ifelse(lunge_depth < 0, 0, lunge_depth)) %>% 
  left_join(whale_latin_names, by = "species_code") %>% 
  ggplot(aes(lunge_depth)) +
  #geom_histogram(binwidth = 5) +
  geom_density(aes(color = species_code, fill = species_code), alpha = 0.2) +
  facet_wrap(vars(species, prey_type), nrow = 1, scales = "free_x") +
  coord_flip(xlim = c(400, 0)) +
  scale_x_reverse() +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(x = "Depth (m)",
       y = "Lunge Proportion") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))  
lunge_depth_species   

 
Choy_Kashi_graph <- Choy_Kashi_data %>% 
  ggplot(aes(x = med_50, y = depth_m)) +
  geom_ribbon(aes(xmin= lo_05, xmax = hi_95), alpha= 0.25) +
  geom_path(size = 1.5) +
  labs(x =  bquote('Plastic Concentration'~(particles/m^3)),
       y = "") +
 # scale_y_reverse() +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 13))
Choy_Kashi_graph

figure2 <- cowplot::plot_grid(lunge_depth_species, Choy_Kashi_graph, 
                              align = "h", axis = "bt", rel_widths = c(1, 0.5))
figure2


savePlot(filename = "figure2",    # Name of the file to be saved
         type = c("tiff", "eps", "pdf"),
         device = dev.cur(), # Number of the device to be saved
         restoreConsole = TRUE)


tiff("my_plot", compression = "zip")

#Summary
foo_depth <- lunges %>% 
  mutate(lunge_depth = ifelse(lunge_depth < 0, 0, lunge_depth)) %>% 
  left_join(whale_latin_names, by = "species_code") %>% 
  group_by(species_code, prey_type) %>% 
  count(depth_bucket)

species_depth_lunges <- write.csv(foo_depth,"species_depth_lunges.csv")
