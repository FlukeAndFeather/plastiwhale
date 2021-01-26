
deploy_list<- read_csv("data/raw/alldata_CAwhales.csv") %>% 
  # find prh and lunge .mat
  slice(15) %>% 
  mutate(lungepath = map_chr(lunge_Name, findlungemat, lunge_dir = alldata_path),
         prhpath = map_chr(prh_Name, findprhmat, prh_dir = alldata_path)) %>%
  drop_na(lungepath, prhpath) %>% 
  # true false of whether the prh or lunge file has the lunges 
  mutate(haslungeDepth = map_lgl(lungepath, lungehasp)) %>% 
  # finding tag_on_off times from prh
  mutate(tag_on_off = map(prhpath, get_tagon_tagoff)) %>% 
  unnest_wider(tag_on_off)


delta_t <- 60  #timestep
t <- seq(deploy_list$tag_on[1], deploy_list$tag_off[1], by = paste(delta_t, "secs"))
a <- sunAngle(t, longitude = deploy_list$longitude, latitude = deploy_list$latitude)$altitude 

plot(t, a, type = "l")
abline(h = 0, lty = 2)
abline(h = -18, lty = 3)
day <- delta_t * sum(a > 0)

list(day = ...,
     twilight = ...,
     night = ...)

dielcycle_hours <- function(tag_on, tag_off, longitude, latitude, delta_t = 60) {
  time <- seq(tag_on, tag_off, by = paste(delta_t, "secs"))
  altitude <- sunAngle(time, longitude, latitude)$altitude 
  list(day_hours = delta_t * sum(altitude > 0) / 3600,
       night_hours = delta_t * sum(altitude < -18) / 3600,
       twilight_hours = delta_t * sum(altitude >= -18 & altitude <= 0) / 3600)
}

# how to use pmap
deploy_list %>% 
  mutate(diel_cycle = pmap(list(tag_on, tag_off, longitude, latitude), dayperiod_hours)) %>% 
  unnest_wider(dayperiods) %>% 
  View()


#Need to summarize but actually need two tables, one with each lunge as a line and one with each deployment as a line 


