#---- Set Up ----
library(R.matlab)
library(foreign)
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
  LungeDepth <- find_lunge_mat$LungeDepth
  LungeI <- find_lunge_mat$LungeI
  if (!is.null(LungeDepth)) { 
    as.vector(LungeDepth)}
  else {
    prh <- readMat(prhpath)
    prh$p[LungeI] #change to return a list of prh$p and prh$LungeI
  }
}

# converts matlab date number into date time that is readable 
matlab_to_posix = function(x, timez = "UTC") {
  as.POSIXct((x - 719529) * 86400, origin = "1970-01-01", tz = timez)
}

# get tag on and tag off times from prh (go into one column that is split into two)
get_tagon_tagoff <- function(prhpath) {
  find_tag_on <- readMat(prhpath)
  tag_on <- matlab_to_posix(find_tag_on$DN[first(which(find_tag_on$tagon == 1))])
  tag_off <- matlab_to_posix(find_tag_on$DN[last(which(find_tag_on$tagon == 1))])
  list(tag_on = tag_on, tag_off = tag_off)
}

# ---- Process Data ----
deploy_list<- read_csv("data/raw/alldata_CAwhales.csv") %>% 
  # find prh and lunge .mat
  mutate(lungepath = map_chr(lunge_Name, findlungemat, lunge_dir = alldata_path),
         prhpath = map_chr(prh_Name, findprhmat, prh_dir = alldata_path)) %>%
  drop_na(lungepath, prhpath) %>% 
  # true false of whether the prh or lunge file has the lunges 
  mutate(haslungeDepth = map_lgl(lungepath, lungehasp)) %>% 
  # finding tag_on_off times from prh
  mutate(tag_on_off = map(prhpath, get_tagon_tagoff)) %>% 
  unnest_wider(tag_on_off) %>% 
  # unnest lunges (make each lunge it's own line)
  mutate(LungeDepths = map2(lungepath,prhpath, extractlungedata)) %>% 
  unnest_longer(LungeDepths) %>% 
  #adding in species names
  mutate(SpeciesCode = substr(deployID, start = 1, stop = 2),
         Species = case_when(
           SpeciesCode == "Bm" ~ "B. musculus",
           SpeciesCode == "Bp" ~ "B. physalus",
           SpeciesCode == "mn" ~ "M. novaeangliae",
           SpeciesCode == "bw" ~ "B. musculus",
           SpeciesCode == "bp" ~ "B. physalus")) 

# ---- Export Data ----
saveRDS(deploy_list, "data/output/deploy_list.RDS")

