
# ---- Process Data ----

lunge_counts <- diel_lunges %>% 
  mutate(deployID = factor(deployID)) %>% 
  group_by(deployID, dielperiod, depth_bucket) %>% 
  summarise(n_lunges = n()) %>% 
  ungroup() %>% 
  complete(deployID, dielperiod, depth_bucket, fill = list(n_lunges = 0)) %>% 
  mutate(deployID = as.character(deployID))

diel_deployments_pivot <- diel_deployments %>% 
  rename_at(vars(ends_with("_hours")), ~ str_replace(.x, "_hours", "")) %>%  #removes the letters in quotes so that we are comparing the same words
  pivot_longer(cols = c(day, twilight, night), names_to = "dielperiod", values_to = "period_hours") %>% #new names, values already in the columns
  select(deployID, dielperiod, period_hours) %>% 
  mutate(dielperiod = factor(dielperiod, levels = c("day", "twilight", "night")))

#rates per time of day AND per depth bin 
lunge_rates <- lunge_counts %>% 
  left_join(diel_deployments_pivot, by = c("deployID", "dielperiod")) %>% 
  mutate(lunge_rate = ifelse(period_hours == 0, 0, n_lunges/period_hours))

# ----Show Data----

ggplot(diel_deployments_pivot, aes(deployID, period_hours, fill = dielperiod)) +
  geom_col(position = "stack")

