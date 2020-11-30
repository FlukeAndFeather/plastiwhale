
deployments <- readRDS("data/output/deployments.RDS")
#repeat for the lunges
#collect/group by lunges table to get per deployment
#group by id, summarize to get count of lunges in each diel period 
# retain the lunge depths 
# lunge rate by day period and depth (bucket?) and by species and by prey -> give rate 
# cat lunges by depth bucket 
# cut 




x <- rnorm(150, 175, 75)
x[x < 0] <- 0
hist(x)

# How many in bins <25m, 25-100m, 100-200m, 200m+
cut(x, breaks = c(-Inf, 25, 100, 200, Inf))
tibble(lunge_depth = x, 
       depth_bucket = cut(x, 
                          breaks = c(-Inf, 25, 100, 250, Inf),
                          labels = c("Surface (<25m)", "Shallow (25-100m)", "Moderate (100-250m)", "Deep (>250m)"))) %>% 
  View()

#ok so this needs to be made into a funtion, and then this function can be put into the collect_lunges script
# the diel stuff needs to be moved into its own script

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
  
ggplot(diel_deployments_pivot, aes(deployID, period_hours, fill = dielperiod)) +
  geom_col(position = "stack")

