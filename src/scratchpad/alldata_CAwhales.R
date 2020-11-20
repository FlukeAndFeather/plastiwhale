#Libraries
library(R.matlab)
library(foreign)
library(tidyverse)
#install.packages('janitor')
#library(janitor)


deploy_list<- read_csv("alldata_CAwhales.csv")


#head(deploy_list)
# go through the lunge files and determine whether they have depth (p)
# check lunge file for DEPTH, if no, check PRH for DEPTH, if no, leave NAN

#list.files(pattern = "")

# parameter vs arguement. parameter is blueprint, arguement is the actual house
#function that takes whale id and returns the computer path to lunge mat 

#parameters are: route path, _lunges, prh, whaleid 
#where are the files 
# 

#function takes path to alldata and name of file within that folder

#vectorized function 
#dir has a parameter has pattern. expects a regular expression but we need to match exactly 
#second parameter, fixed, set to TRUE 
#dir(#path to folder#, )
#write function for one, apply to entire list . mapping function onto vector 

#step 1: funciton for single input- put the below into a function 

deploy_list$lunge_Name[1]
lunge_filename <- deploy_list$lunge_Name[1]
alldata_path <- "C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 3- Plastic/Plastic Risk Assessment/alldata"
whaleID <- deploy_list$deployID[1]

dir(alldata_path, pattern = lunge_filename, full.name= TRUE)


findlungemat <- function(lunge_filename, lunge_dir){ #defining placeholders lunge_dir represents what ever i tell it 
  if (is.na(lunge_filename)){return(NA)}
  dir(lunge_dir, pattern = lunge_filename, full.name= TRUE)
  
}

prh_filename <- deploy_list$prh_Name[1]
alldata_path <- "C:/Users/Shirel/Documents/Goldbogen Lab/Thesis/Chapter 3- Plastic/Plastic Risk Assessment/alldata"
whaleID <- deploy_list$deployID[1]

dir(alldata_path, pattern = prh_filename, full.name= TRUE)

findprhmat <- function(prh_filename, prh_dir){ #defining placeholders lunge_dir represents what ever i tell it 
  if (is.na(prh_filename)){return(NA)}
  result <- dir(prh_dir, pattern = prh_filename, full.name= TRUE)
  if (length(result) == 0) {return(NA)}
  result
}

map_chr(deploy_list$lunge_Name, findlungemat, lunge_dir = alldata_path) #left is parameter, right is variable
map_chr(deploy_list$prh_Name, findprhmat, prh_dir = alldata_path) #left is parameter, right is variable. vector of type character 



deploy_list_path<-deploy_list %>% 
  mutate(lungepath = map_chr(deploy_list$lunge_Name, findlungemat, lunge_dir = alldata_path),
         prhpath = map_chr(deploy_list$prh_Name, findprhmat, prh_dir = alldata_path)) %>%
  drop_na()


lungehasp <- function(checklungefile){  #where the mat files is and to be able to read the mat file
  lunge_mat<- readMat(checklungefile)
  'LungeDepth' %in% names(lunge_mat)  #is the character string LungeP in the list of names in lunge_mat
}

map_lgl(deploy_list_path$lungepath, lungehasp)      #lgl is short for logical

deploy_list_path<-deploy_list_path %>% 
  mutate(haslungeDepth = map_lgl(deploy_list_path$lungepath, lungehasp)) %>% 
  mutate(SpeciesCode = substr(deployID, start = 1, stop = 2),
         Species = case_when(
           SpeciesCode == "Bm" ~ "B. musculus",
           SpeciesCode == "Bp" ~ "B. physalus",
           SpeciesCode == "mn" ~ "M. novaeangliae",
           SpeciesCode == "bw" ~ "B. musculus",
           SpeciesCode == "bp" ~ "B. physalus"))






## input lunge file name and 'alldata_path'
## do this for the prh path - make new function, add call to mutate 
#now i have the paths, how to write function to look up lunge p variable

#lungehasp #only needs path to lunge file _lunge.mat. return TRUE OR FALSE 

#use readMat - will return contents as list 
#test for existence 
#check for type - or huristic so if p is <0, use stop to get you to check on something  
#1 open mat file 2 check for existence of variable 3 check for type 4 return TRUE OR FALSE


# now make it one row per lunge, even if it doesnt have a lunge depth 
# write a function that takes as input the patht ot the lunge file, the path to the prh and then this has lungeDepth
# return a vector of lunge depths 
#if the lunge file has lunge depth   
#lunge1= lungemat $lungei then open prh, prhmat$lungei *indexing problem 
#prhmat$p 



# data frame is a list where all the elements are the same length, making a table (spreadsheet style)
#each column of data frame is an entry in the list 

#lunge2<-readMat(deploy_list_path$lungepath[2])
#whale_ID_lunge<-deploy_list_path$lunge_Name[2]
#lunge_depth<- data.frame(lunge2$LungeDepth, lunge2$LungeI) #make into function

#lunge_depth <- lunge_depth %>% 
# mutate(whaleID =whale_ID)

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

#map2(deploy_list_path$lungepath,deploy_list_path$prhpath, extractlungedata) included in foo function 


#list column, arbitarty length vector

foo<- deploy_list_path %>% 
  mutate(LungeDepths = map2(deploy_list_path$lungepath,deploy_list_path$prhpath, extractlungedata)) %>% 
  unnest_longer(LungeDepths) %>% 
  mutate(SpeciesCode = substr(deployID, start = 1, stop = 2),
         Species = case_when(
           SpeciesCode == "Bm" ~ "B. musculus",
           SpeciesCode == "Bp" ~ "B. physalus",
           SpeciesCode == "mn" ~ "M. novaeangliae",
           SpeciesCode == "bw" ~ "B. musculus",
           SpeciesCode == "bp" ~ "B. physalus"))



#plot these babies

ggplot(foo, aes(LungeDepths, fill= Species))+geom_histogram()+
  coord_flip() +
  scale_x_reverse()



ggplot(foo, aes(LungeDepths, color= Species)) + geom_freqpoly()+
  coord_flip() +
  scale_x_reverse()




#### Simulation ####








## split humpbacks into krill vs fish 


#branching example
# s <- function(x) {
#   if (x < 5) {'cat'} 
#     else {'dog'
#     }
#   }
# 
# 
# 
# 
# #given path to lunge and path to prh and whether or not lunge file has the depth (bouillion value) to the branching function (find lunge depth
# extractlungedata<- function(lungedepths){
#   find_lunge_mat <- readMat(lungedepths)
#   lunge_depth<- data.frame(find_lunge_mat$LungeDepth, find_lunge_mat$LungeI)
#   
# }
# 
# map_dfr(deploy_list_path$lungepath, extractlungedata) #left is parameter, right is variable. vector of type character 
# 
# 
# 
# 
# if ('LungeDepth' == TRUE) { 
# 
# if has 'lunge depth' then call function for lunge mat
# else call function with prh and lunge
# # look at lungehas p
# #list column, unesting 
# 
# 
# prh1 <- readMat(deploy_list_path$prhpath[1])
# whale_ID_prh <- deploy_list_path$prh_Name[1]
# prh_depth <- data.frame(prh1$p) #there is no index for depth, so how could I line up lunges with it?
# prh_depth <- prh_depth %>% 
#   mutate(whaleID =whale_ID)
# 
# 
# 
# find_lungeI<- readMat(deploy_list_path$lungepath[1])
# LungeI<- find_lungeI$LungeI
# find_prh<- readMat(deploy_list_path$prhpath[1])
# depth <-find_prh$p
# LungeDepth <- depth(LungeI)
# 
# 
# 
# #get lungei for lunge file 1
# then open prh for 1 
# read prh file into prhmat #index into a vector 
# get prhmat$p
# 
# whale_ID_prh <- deploy_list_path$prh_Name[1]
#  i = 3391
# 
#  
#  plot(-prh1$p[(i-50):(i+50)], type = 'l') #sanity check 













