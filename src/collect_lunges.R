#---- Set Up ----
library(R.matlab)
library(foreign)
library(tidyverse)

# data information
alldata_path <- "C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 3- Plastic/Plastic Risk Assessment/alldata"

# ---- Utility Functions ----
#function takes path to alldata and name of file within that folder (lunge files)
findlungemat <- function(lunge_filename, lunge_dir){ #defining placeholders lunge_dir represents what ever i tell it 
  if (is.na(lunge_filename)){return(NA)}
  dir(lunge_dir, pattern = lunge_filename, full.name= TRUE)
  
}
#function takes path to alldata and name of file within that folder (prh files)
findprhmat <- function(prh_filename, prh_dir){ #defining placeholders lunge_dir represents what ever i tell it 
  if (is.na(prh_filename)){return(NA)}
  result <- dir(prh_dir, pattern = prh_filename, full.name= TRUE)
  if (length(result) == 0) {return(NA)}
  result
}

lungehasp <- function(checklungefile){  #where the mat files is and to be able to read the mat file
  lunge_mat<- readMat(checklungefile)
  'LungeDepth' %in% names(lunge_mat)  #is the character string LungeP in the list of names in lunge_mat
}

#finds lungei and depth
extractlungedata<- function(lungepath, prhpath){
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

# ---- Process Data ----
deploy_list<- read_csv("data/raw/alldata_CAwhales.csv") %>% 
  # find prh and lunge .mat
  mutate(lungepath = map_chr(lunge_Name, findlungemat, lunge_dir = alldata_path),
         prhpath = map_chr(prh_Name, findprhmat, prh_dir = alldata_path)) %>%
  drop_na(lungepath, prhpath) %>% 
  # true false of whether the prh or lunge file has the lunges 
  mutate(haslungeDepth = map_lgl(lungepath, lungehasp)) %>% 
  # unnest lunges (make each lunge it's own line)
  sample_n(3) %>% 
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

