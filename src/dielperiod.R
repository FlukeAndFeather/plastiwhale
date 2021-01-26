#---- Set Up ----
library(oce)


# ---- Utility Functions ----
dielperiod_hours <- function(tag_on, tag_off, longitude, latitude, delta_t = 60) {
  time <- seq(tag_on, tag_off, by = paste(delta_t, "secs"))
  altitude <- sunAngle(time, longitude, latitude)$altitude 
  list(day_hours = delta_t * sum(altitude > 0) / 3600,
       night_hours = delta_t * sum(altitude < -18) / 3600,
       twilight_hours = delta_t * sum(altitude >= -18 & altitude <= 0) / 3600)
} 



# ---- Process Data ----
diel_deployments <- deployments %>% 
  #adding the hours that each deployment is in each diel cycle (day/night/twi hours)
  mutate(dielperiod_hours = pmap(list(tag_on, tag_off, longitude, latitude), dielperiod_hours)) %>% 
  unnest_wider(dielperiod_hours)


diel_lunges <- lunges %>% 
  #adding in sun angle (altitude) based on at long and time
  mutate(
    sun_angle = sunAngle(lunge_time, longitude = longitude, latitude = latitude)$altitude,
    dielperiod = factor(
      case_when(
        sun_angle > 0 ~ "day",
        sun_angle < -18 ~ "night",
        TRUE ~ "twilight" #otherwise
      ),
      labels = c("day", "twilight", "night"),
      levels = c("day", "twilight", "night")
    )
  ) 


# ---- Export Data ----
saveRDS(diel_deployments, "data/output/diel_deployments.RDS")
saveRDS(diel_lunges, "data/output/diel_lunges.RDS")
