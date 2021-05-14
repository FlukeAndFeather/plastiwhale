library(lme4)
library(MCMCglmm)
library(lmerTest)


med_scenario <- all_scenarios %>%
  filter(scenario_name == "med",
         total_plastic >0) %>% 
  mutate(mass_specific_total_plastic = total_plastic/mass_skr)

plasticmass_lm <- lm(log10(total_plastic) ~ log10(mass_skr), data = med_scenario)
plasticmass_lm
prey_type_plasticmass_lm <- lm(log10(total_plastic) ~ log10(mass_skr), data = filter(med_scenario, prey_type == "krill"))
prey_type_plasticmass_lm

ggplot(med_scenario, aes(log10(mass_skr), log10(total_plastic))) +
  geom_point(aes(color = species_code)) +
  geom_smooth(method = "lm") +
  theme(legend.position = "none")
  

engulfmentvsmass <- lmer(log10(engulf_m3_skr) ~ log10(mass_skr) + (1|species_code), 
                         data = filter(med_scenario, prey_type == "krill"))
# Fixed Effects:
#   (Intercept)  log10(mass_skr)  
# -4.443            1.326 
summary(engulfmentvsmass)
confint(engulfmentvsmass)

feedratevsmass <- lmer(log10(daily_lunges) ~ log10(mass_skr) + (1|species_code), 
                       data = filter(med_scenario, prey_type == "krill"))
# Fixed Effects:
#   (Intercept)  log10(mass_skr)  
# 1.8806           0.0813 
summary(feedratevsmass)
confint(feedratevsmass)

plasticvsmass_pt <- lmer(log10(total_plastic) ~ log10(mass_skr) + (1|species_code), 
                         data = filter(med_scenario, prey_type == "krill"))
plasticvsmass_pt
# 
# Fixed Effects:
#   (Intercept)  log10(mass_skr)  
# 0.3345           1.3964  



prey_type_plasticmass_lmer <- lmer(log10(total_plastic) ~ log10(mass_skr) + (1|species_code), 
               data = filter(med_scenario, prey_type == "krill"))
summary(prey_type_plasticmass_lmer) # like fig. 5, controls for prey type, tests for effect of scale on plastic ingestion 
#does plastic ingestion scale hyperallometrically 


plasticmasslmer <- lmer(log10(total_plastic) ~ log10(mass_skr) + (1|species_code),
                            data = med_scenario)
summary(plasticmasslmer)
#even controlling for body mass, larger animals eat more plastic 
#because length > 1, we test with a glmm, we use a bayesian framework, use mcmcglmm to look at 10000 of the slope 
#dist of possible slopes fromm mcmc shows that it's not importantly different than 1 
# in relation to fish, krill has a positive effect on mass spec total plastic - what you're eating matters more than how big you are, within or between species
# very confident that eating krill leads to higher plastic loads. How many pieces of plastic need to be in fish for a switch?


MCMCglmm_plasticscaling <- MCMCglmm(log10(total_plastic) ~ log10(mass_skr),
                           random = ~ species_code,
                           data = med_scenario, 
                           family = "gaussian",
                           nitt = 11000, thin = 1, burnin = 1000,
                           pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                           verbose = TRUE)
summary(MCMCglmm_plasticscaling)
#not neg allo, but isometric or slightly allometric, if theyre eating the same prey  



MCMCglmm_plasticscaling_preytype <- MCMCglmm(log10(total_plastic) ~ log10(mass_skr),
                                    random = ~ species_code,
                                    data = filter(med_scenario, prey_type == "krill"), 
                                    family = "gaussian",
                                    nitt = 11000, thin = 1, burnin = 1000,
                                    pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                                    verbose = TRUE)
summary(MCMCglmm_plasticscaling_preytype)




#testing for effect of mass on plastic 
MCMCglmm_humpbacks <- MCMCglmm(log10(total_plastic) ~ log10(mass_skr),
                               random = ~ prey_type,
                               data = filter(med_scenario, species_code == "mn"), 
                               family = "gaussian",
                               nitt = 11000, thin = 1, burnin = 1000,
                               pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                               verbose = TRUE)
summary(MCMCglmm_humpbacks)



## Engulfment capacity and feeding rates
install.packages("lmodel2")
library("lmodel2")

lmodel_engulf <- lmodel2(log10(engulf_m3_skr) ~ log10(mass_skr),   data = filter(med_scenario, prey_type == "krill"), "relative", "relative", 99)
lmodel_engulf

lmodel_feeding <- lmodel2(log10(daily_lunges) ~ log10(mass_skr),  data = med_scenario, "interval", "relative", 99)
lmodel_feeding

lmodel_plastic <- lmodel2(log10(total_plastic) ~ log10(mass_skr),  data = med_scenario, "relative", "relative", 99)
lmodel_plastic



# Comparative Models between SMA and OLS #####
#engulfment vs mass
# lmodel_engulf <- lmodel2(log10(engulf_m3_skr) ~ log10(mass_skr),   data = filter(med_scenario, prey_type == "krill"), "relative", "relative", 99)
# lmodel_engulf
# 
# #feeding rate vs mass
# lmodel_feeding <- lmodel2(log10(daily_lunges) ~ log10(mass_skr),  data = med_scenario, "interval", "relative", 99)
# lmodel_feeding
# #Use OLS, considering prey
# 
# #plastic vs mass
# lmodel_plastic <- lmodel2(log10(total_plastic) ~ log10(mass_skr),   data = filter(med_scenario, prey_type == "krill"), "relative", "relative", 99)
# lmodel_plastic
# #When accounting for prey, OLS provides isometry 


### GLMM Attempt ###

# Packages and Libraries ----
library(readr)
library(tidyverse)
library(car)
library(MASS)
library(glmm)
library(lme4)
install.packages("glmm")


# Input Data ----
GLMM <- med_scenario %>% 
  mutate(TL_z = as.numeric(scale(length_m)),
         prey_type_z = as.numeric(scale(prey_type)),
         mass_z = as.numeric(scale(mass_skr)))


#Plot to determine distribution
hist(log10(GLMM$Lunge_Count))
hist(log10(GLMM$TL))
hist(log10(GLMM$Mean_Depth))


whale2 <- glmer(total_plastic ~ mass_skr + #winner winner chicken dinner
                  prey_type + 
                  (1|species_code), 
                data = med_scenario, family = "poisson")
summary(whale2)


# includes speciescode as a variable. this isn't as useful as including TL. they are redundant if both included. 
#this model is slightly less good than whale2
# whale3 <- glmer(Lunge_Count ~ Mean_Depth_z + 
#                   SpeciesCode + 
#                   Dive_Length_z + 
#                   (1| ID), 
#                 data = GLMM, family = "poisson")

AIC(whale2, whale3)

hist(resid(whale2))
plot(GLMM$TL,resid(whale2))
plot(GLMM$Mean_Depth, resid(whale2))
plot(GLMM$Dive_Length, resid(whale2))
plot(GLMM$SpeciesCode, resid(whale2)) #plot resid of model against species code if dispersed the affect of species does not explain any more variation in the data




### GLMM Attempt ###
#Input Data ----
med_scen_glmm <- med_scenario %>% 
  mutate(mass_z = as.numeric(scale(mass_skr)))


#Plot to determine distribution
hist(log10(med_scenario$mass_skr))
hist(log10(med_scenario$engulf_m3_skr))
hist(log10(med_scenario$prey_type))


glmer3 <- glmer(total_plastic ~ mass_z + 
                  prey_type +
                  (1|species_code), 
                data = med_scen_glmm, family = "gaussian")
summary(glmer3) #all whales

glmer4 <- glmer(total_plastic ~ mass_z + 
                  (1|prey_type),
                data = filter(med_scen_glmm, species_code == "mn"), family = "gaussian")
summary(glmer4) #humpbacks only



# includes speciescode as a variable. this isn't as useful as including TL. they are redundant if both included. 
#this model is slightly less good than whale2
# whale3 <- glmer(Lunge_Count ~ Mean_Depth_z + 
#                   SpeciesCode + 
#                   Dive_Length_z + 
#                   (1| ID), 
#                 data = GLMM, family = "poisson")

AIC(whale2, whale3)

hist(resid(whale2))
plot(GLMM$TL,resid(whale2))
plot(GLMM$Mean_Depth, resid(whale2))
plot(GLMM$Dive_Length, resid(whale2))
plot(GLMM$SpeciesCode, resid(whale2)) #plot resid of model against species code if dispersed the affect of species does not explain any more variation in the data



############

med_scenario <- all_scenarios %>%
  filter(scenario_name == "med",
         total_plastic >0) %>% 
  mutate(mass_specific_total_plastic = total_plastic/mass_skr)


engulfmentvlength_glmm <- MCMCglmm(log10(engulf_m3_skr) ~ log10(length_m),
                                  random = ~ species_code,
                                  data = filter(med_scenario, prey_type == "krill"), 
                                  family = "gaussian",
                                  nitt = 11000, thin = 1, burnin = 1000,
                                  pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                                  verbose = TRUE)
summary(engulfmentvlength_glmm)
# 3.411    3.405    3.418


feedingvlength_glmm <- MCMCglmm(log10(daily_lunges) ~ log10(length_m),
                               random = ~ species_code,
                               data = filter(med_scenario, prey_type == "krill"), 
                               family = "gaussian",
                               nitt = 11000, thin = 1, burnin = 1000,
                               pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                               verbose = TRUE)
summary(feedingvlength_glmm)
# 0.1864  -0.1335   0.5118
# CHEKC FEEDING RATE 1/4

MCMCglmm_plasticscaling_preytype <- MCMCglmm(log10(total_plastic) ~ log10(length_m),
                                             random = ~ species_code,
                                             data = filter(med_scenario, prey_type == "krill"), 
                                             family = "gaussian",
                                             nitt = 11000, thin = 1, burnin = 1000,
                                             pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                                             verbose = TRUE)
summary(MCMCglmm_plasticscaling_preytype)
# 3.610    3.281    3.929




#### Max's crap ####
just_krill <- filter(med_scenario, prey_type == "krill")
# robust total plastic line
totalplastic_rlm <- MASS::rlm(log10(total_plastic) ~ log10(mass_skr), 
                              just_krill)

p <- ggplot(just_krill, aes(log10(mass_skr), log10(total_plastic))) +
  geom_point(aes(color = species_code), alpha = 0) +
  geom_hex() +
  geom_abline(intercept = coef(totalplastic_rlm)[1], 
              slope = coef(totalplastic_rlm)[2],
              color = "red",
              size = 1.5) +
  scale_color_manual(values = pal, guide = FALSE) +
  scale_fill_viridis_c() +
  labs(x = expression(log10 * "[" * mass ~~ (kg) * "]"),
       y = "log10[daily plastic ingestion (pp day^-1)]") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
ggExtra::ggMarginal(p, groupColour = TRUE)

#Allometry figure
feedingrate_rlm <- MASS::rlm(log10(daily_lunges) ~ log10(mass_skr), 
                             just_krill)
max_mass <- log10(max(just_krill$mass_skr))

allometry <- tibble(variable = c("Engulfment", 
                                 "Plastic", 
                                 "Isometry", 
                                 "Feeding rate"),
                    slope = c(1.326, 
                              coef(plasticvsmass_pt)$species_code[1, 2], 
                              1, 
                              coef(feedingrate_rlm)[2])) %>% 
  mutate(ylim = slope * max_mass)
ggplot(allometry) +
  geom_abline(aes(slope = slope, intercept = 0, color = variable)) +
  coord_cartesian(xlim = c(0, max_mass), ylim = range(allometry$ylim))
