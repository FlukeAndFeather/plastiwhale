# For every ID, what is the total number of lunges, the total duration, and what is the lunge rate?
#For Empirical Data
names(lunges_and_length)[names(lunges_and_length) == 'simulation_id'] <- 'ID'
names(lunge_rates)[names(lunge_rates) == 'deployID'] <- 'ID'

empirical_dist <- lunge_rates %>% 
  group_by(ID = deployID) %>% 
  filter(species == "B. musculus") %>% 
  summarise(duration = sum(period_hours) / 4,
            hourly_lunges = sum(n_lunges)/ duration,
            daily_lunges = hourly_lunges * 24) %>% 
  mutate(source = "empirical")
  
emp_ecdf <- ggplot(emipirical_dist, aes(lunge_rate)) + stat_ecdf()


simulated_dist <- lunges_and_length %>%
  group_by(ID) %>% 
  summarize(daily_lunges = sum(daily_lunges),
            hourly_lunges = daily_lunges / 24,
            duration = NA) %>% 
  mutate(source = "simulated")

sim_ecdf <- ggplot(simulated_dist, aes(lunge_rate)) + stat_ecdf()

rbind(empirical_dist, simulated_dist) %>% 
  ggplot(aes(hourly_lunges, color = source)) + 
  stat_ecdf()
empirical_dist %>% 
  ggplot(aes(duration, daily_lunges)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_vline(xintercept = 24)


#theres an issue here. daily_lunges is the lunge rate times the number of hours in the fake day
#in the empiricial, it just the lunnges times the period hours. how do these align?

combined_dist <- rbind(emipirical_dist, simulated_dist)

ggplot(combined_dist, aes(lunge_rate, color = source)) + stat_ecdf() 
#looks bad, but axis are just so different 

