# tag on off
library(R.matlab)
library(tidyverse)
library(oce)
blue <- readMat("C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 3- Plastic/Plastic Risk Assessment/alldata/bw170815-42 10Hzprh.mat")
blue$tagon
blue$DN


last(which(blue$tagon == 1))
blue$DN[first(which(blue$tagon == 1))]
blue$DN[last(which(blue$tagon == 1))]


matlab_to_posix = function(x, timez = "UTC") {
  as.POSIXct((x - 719529) * 86400, origin = "1970-01-01", tz = timez)
}

matlab_to_posix(blue$DN[first(which(blue$tagon == 1))])

matlab_to_posix(test$DN[last(which(test$tagon == 1))])

# Write a function
# Input: PRH path
# Output: a list of tag on and tag off
#deploy list and prh not the indiv lunges 

get_tagon_tagoff <- function(prhpath) {
  find_tag_on <- readMat(prhpath)
  tag_on <- matlab_to_posix(find_tag_on$DN[first(which(find_tag_on$tagon == 1))])
  tag_off <- matlab_to_posix(find_tag_on$DN[last(which(find_tag_on$tagon == 1))])
  list(tag_on = tag_on, tag_off = tag_off)
}



map_POSIXct <- function (.x,.f) {
  map(.x, .f) %>% 
    reduce(c) #combines elements 
}

alldata_path <- "C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 3- Plastic/Plastic Risk Assessment/alldata"

deploy_list<- read_csv("data/raw/alldata_CAwhales.csv") %>% 
  slice(1) %>% 
  # find prh and lunge .mat
  mutate(lungepath = map_chr(lunge_Name, findlungemat, lunge_dir = alldata_path),
         prhpath = map_chr(prh_Name, findprhmat, prh_dir = alldata_path)) %>%
  drop_na(lungepath, prhpath) %>% 
  # true false of whether the prh or lunge file has the lunges 
  mutate(haslungeDepth = map_lgl(lungepath, lungehasp)) %>% 
  mutate(tagon = map(prhpath, get_tagon_tagoff))

#----
  


#oce attempt

#sunAngle(t, longitude = 0, latitude = 0, useRefraction = FALSE)
# trying with what we would us
#need to get DN from prhpath to replace t
#use lat longs as they are 

dayperiod <- sunAngle(prhpath$DN, deploy_list$longitude, deploy_list$latitude, useRefraction = FALSE)


function that will take a vector of times and lat long to tell me if it's day nught or twi 
returns a vector 



