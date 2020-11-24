#---- Set Up ----
library(R.matlab)
library(lubridate)
library(tidyverse)

# data information
alldata_path <- "C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 3- Plastic/Plastic Risk Assessment/alldata"

# ---- Utility Functions ----
#function takes path to alldata and name of file within that folder (lunge files)
findlungemat <- function(lunge_filename, lunge_dir) { #defining placeholders lunge_dir represents what ever i tell it 
  if (is.na(lunge_filename)){return(NA)}
  dir(lunge_dir, pattern = lunge_filename, full.name= TRUE)
  
}

#function takes path to alldata and name of file within that folder (prh files)
findprhmat <- function(prh_filename, prh_dir) { #defining placeholders lunge_dir represents what ever i tell it 
  if (is.na(prh_filename)){return(NA)}
  result <- dir(prh_dir, pattern = prh_filename, full.name= TRUE)
  if (length(result) == 0) {return(NA)}
  result
}

lungehasp <- function(checklungefile) {   #where the mat files is and to be able to read the mat file
  lunge_mat<- readMat(checklungefile)
  'LungeDepth' %in% names(lunge_mat)  #is the character string LungeP in the list of names in lunge_mat
 } 

#finds lungei and depth
extractlungedata<- function(lungepath, prhpath) { 
  find_lunge_mat <- readMat(lungepath)
  LungeI <- find_lunge_mat$LungeI
  LungeDepth <- find_lunge_mat$LungeDepth
  LungeDN <- find_lunge_mat$LungeDN
  
  # If lunge mat doesn't have LungeDepth *or* LungeDN, have to open the PRH
  if(is.null(LungeDepth) || is.null(LungeDN)) {
    prh <- readMat(prhpath)
    lunge_depth <- prh$p[LungeI]
    lunge_time <- matlab_to_posix_CA(prh$DN[LungeI])
  } else {
    lunge_depth <- as.vector(LungeDepth)
    lunge_time <- matlab_to_posix_CA(as.vector(LungeDN))
  }
  
  # Return list of both depth and time
  list(lunge_depth = lunge_depth, lunge_time = lunge_time)
}


# converts matlab date number into date time that is readable 
matlab_to_posix = function(x, timez = "UTC") {
  as.POSIXct((x - 719529) * 86400, origin = "1970-01-01", tz = "UTC") %>% 
    force_tz(timez)
}
matlab_to_posix_CA <- function(x) {
  matlab_to_posix(x, timez = "US/Pacific")
}

# get tag on and tag off times from prh (go into one column that is split into two)
get_tagon_tagoff <- function(prhpath) {
  find_tag_on <- readMat(prhpath)
  tag_on <- matlab_to_posix_CA(find_tag_on$DN[first(which(find_tag_on$tagon == 1))])
  tag_off <- matlab_to_posix_CA(find_tag_on$DN[last(which(find_tag_on$tagon == 1))])
  list(tag_on = tag_on, tag_off = tag_off)
}

dielperiod_hours <- function(tag_on, tag_off, longitude, latitude, delta_t = 60) {
  time <- seq(tag_on, tag_off, by = paste(delta_t, "secs"))
  altitude <- sunAngle(time, longitude, latitude)$altitude 
  list(day_hours = delta_t * sum(altitude > 0) / 3600,
       night_hours = delta_t * sum(altitude < -18) / 3600,
       twilight_hours = delta_t * sum(altitude >= -18 & altitude <= 0) / 3600)
}

# ---- Process Data ----
deployments <- read_csv("data/raw/alldata_CAwhales.csv") %>% 
  # find prh and lunge .mat
  slice(15:17) %>% 
  mutate(lungepath = map_chr(lunge_Name, findlungemat, lunge_dir = alldata_path),
         prhpath = map_chr(prh_Name, findprhmat, prh_dir = alldata_path)) %>%
  drop_na(lungepath, prhpath) %>% 
  # true false of whether the prh or lunge file has the lunges 
  mutate(haslungeDepth = map_lgl(lungepath, lungehasp)) %>% 
  # finding tag_on_off times from prh
  mutate(tag_on_off = map(prhpath, get_tagon_tagoff)) %>% 
  unnest_wider(tag_on_off) %>% 
  #adding the hours that each deployment is in each diel cycle (day/night/twi hours)
  mutate(dielperiod_hours = pmap(list(tag_on, tag_off, longitude, latitude), dielperiod_hours)) %>% 
  unnest_wider(dielperiod_hours) %>% 
  #adding species names
  mutate(SpeciesCode = substr(deployID, start = 1, stop = 2),
         Species = case_when(
           SpeciesCode == "Bm" ~ "B. musculus",
           SpeciesCode == "Bp" ~ "B. physalus",
           SpeciesCode == "mn" ~ "M. novaeangliae",
           SpeciesCode == "bw" ~ "B. musculus",
           SpeciesCode == "bp" ~ "B. physalus")) 

lunges <- deployments %>% 
  # unnest lunges (make each lunge and time it's own line)
  mutate(lunge_data = map2(lungepath, prhpath, extractlungedata)) %>% 
  unnest_wider(lunge_data) %>% 
  unchop(cols = c(lunge_depth, lunge_time)) %>% 
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
saveRDS(deployments, "data/output/deployments.RDS")
saveRDS(lunges, "data/output/lunges.RDS")

