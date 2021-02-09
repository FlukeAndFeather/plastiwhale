#Parameter values 
sp_bw <- "bw"
sp_bp <- "bp"
sp_mn <- "mn"

retention1 <- 0.25
retention2 <- 0.5
retention3 <- 0.75

prey_k <- "krill"
prey_f <- "fish"

#frequency of occurrence plastic in krill
krill_plastic_conc_lo <- 0.06 #from desforges
krill_plastic_conc_med <- 0.25 #arbitrary midline 
krill_plastic_conc_hi <- 0.5 #from sun 

#frequency of occurrence plastic in fish
fish_plastic_conc_lo <- 0.02 #from hipfner
fish_plastic_conc_med <- 0.3 #from rochman
fish_plastic_conc_hi <- 0.77 #from tanaka





#this requires a bunch of functions that are in different .R scritps 

simulate <- function(retention1, prey_k, krill_plastic_conc_lo #...) {
                     # step 1: simulate feeding and # step 2: simulate morphology
                     # step 3: simulate plastic from h2o and # step 4: simulate plastic from prey
                     # step 5: shake and bake (combine results, concatenate input parameters)
                     
                     
                     simulate <- function(retention, prey_k, krill_plastic_conc_lo, fish_plastic_conc_lo) {
                       # step 1: simulate feeding and # step 2: simulate morphology
                       lunges_and_length <- simulate_feeding(n = n_sim, species = sp_bw, prey = prey_k, empirical_rates = lunge_rates) %>% 
                         left_join(simulate_morpho(n = n_sim, species = sp_bw), by = "simulation_id")
                       # step 3: simulate plastic from h2o and # step 4: simulate plastic from prey
                       total_plastic <- simulate_h2o_plastic(feeding_simulation = lunges_and_length, retention =  retention3) %>%  #can be .25, .5, .75
                         left_join(simulate_prey_plastic(feeding_simulation = lunges_and_length, prey_k, prey_plastic_conc = krill_plastic_conc_med), by = "simulation_id") #change prey and prey_plastic_conc
                       # step 5: shake and bake (combine results, concatenate input parameters)
                       total_simulation <- merge(lunges_and_length, total_plastic, by = "simulation_id")
                     }
                     
                     
                     
                     
                     
                     #need to have new line for fish  
                     # distribution for anchovy weights and krill weights
                     # to figure out how many anchovy in total biomass consumed we used this range from this paper, we uniformly selected
                     # a weight per simulation 
                     # rather than divide by indiv, have the plastic be per total biomass 
                     
                     
                     # 15000 kg krill consumed, how many indiv krill bodies is that? 
                     # 15 000 / mass of a single krill = total number of indiv within that total biomass 
                     # .0008 is median
                     # total bimass/mass of sinle = indiv in total biomass 
                     # 5% of those animals we predict will have plastic in them, or 30 or 77 
                     #                      
                     