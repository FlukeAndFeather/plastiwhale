#Tables for Paper

tag_hours <- deployments %>% 
  group_by(species_code, tag_Type, prey_type) %>% 
  summarise(tag_hours = mean(tag_off - tag_on),
            min_hours = min(tag_off - tag_on),
            max_hours = max(tag_off - tag_on))
  
