

#Humpback effort
dielperiod_durs <- tribble(
  ~dielperiod, ~hours,
  "day",       14,
  "twilight",  2,
  "night",     8
) %>% 
  mutate(dielperiod = factor(dielperiod, levels = c("day", "twilight", "night")))


cycle_humpbacks <- lunge_rates %>% 
  filter(species_code == "mn") %>% 
  group_by(deployID, prey_type) %>% 
  summarise(total_hours = sum(period_hours)/4,
            n_lunges = sum(n_lunges),
            .groups = "drop") %>% 
  mutate(lunge_rate = n_lunges/total_hours) 

ggplot(cycle_humpbacks, aes(total_hours, lunge_rate)) +
  geom_point() +
  facet_wrap(vars(prey_type))

hump_lunge_rate <- ggplot(cycle_humpbacks, aes(prey_type, lunge_rate)) + 
  geom_boxplot()

hump_lunge_rate <- ggplot(cycle_humpbacks, aes(prey_type, lunge_duration_ratio)) + 
  geom_boxplot()
#when I exclude the longer duration tags, the difference between the fish/kril
#is less. with the 24 hour limit, the data are much closer together  
# 

hump_daily_rates <- cycle_humpbacks %>% 
  left_join(dielperiod_durs, by = "dielperiod") %>%
  group_by(dielperiod, deployID, lunge_rate, period_hours, depth_bucket, prey_type) %>% 
  summarise(daily_lunges = sum(lunge_rate * hours))

hump_daily_lunges <- ggplot(hump_daily_rates, aes(prey_type, daily_lunges)) + 
  geom_boxplot()


summary_humpback_lunges <- cycle_humpbacks %>% 
  group_by(prey_type) %>% 
  summarise(mean_lunge_rate = mean(lunge_rate),
            med_lunge_rate = median(lunge_rate),
            n = n())