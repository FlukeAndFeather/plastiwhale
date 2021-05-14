#Stats for Scaling 
#Used for scaling questions in particular 


med_scenario <- all_scenarios %>%
  filter(scenario_name == "med",
         total_plastic >0) %>% 
  mutate(mass_specific_total_plastic = total_plastic/mass_skr)


engulfmentvsmass_glmm <- MCMCglmm(log10(engulf_m3_skr) ~ log10(mass_skr),
                                             random = ~ species_code,
                                             data = filter(med_scenario, prey_type == "krill"), 
                                             family = "gaussian",
                                             nitt = 11000, thin = 1, burnin = 1000,
                                             pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                                             verbose = TRUE)
summary(engulfmentvsmass_glmm)
# 1.326    1.321    1.330


feedingvsmass_glmm <- MCMCglmm(log10(daily_lunges) ~ log10(mass_skr),
                                  random = ~ species_code,
                                  data = filter(med_scenario, prey_type == "krill"), 
                                  family = "gaussian",
                                  nitt = 11000, thin = 1, burnin = 1000,
                                  pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                                  verbose = TRUE)
summary(feedingvsmass_glmm)
# 0.08174 -0.04655  0.20637
# CHEKC FEEDING RATE 1/4

MCMCglmm_plasticscaling_preytype <- MCMCglmm(log10(total_plastic) ~ log10(mass_skr),
                                             random = ~ species_code,
                                             data = filter(med_scenario, prey_type == "krill"), 
                                             family = "gaussian",
                                             nitt = 11000, thin = 1, burnin = 1000,
                                             pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                                             verbose = TRUE)
summary(MCMCglmm_plasticscaling_preytype)
# 1.3959   1.2697   1.5265


feeding_model <- MASS::rlm(log10(daily_lunges) ~ log10(mass_skr), 
                           filter(med_scenario, prey_type == "krill"), 
                           method = "M")
summary(feeding_model)
confint(feeding_model)

ggplot(med_scenario, aes(log10(mass_skr), log10(daily_lunges))) +
  geom_point(aes(color = species_code)) +
  geom_abline(intercept = coef(feeding_model)[1],
              slope = coef(feeding_model)[2],
              color = "blue",
              linetype = "dashed") +
  geom_smooth(method = "lm",
              linetype = "dotted") +
  theme(legend.position = "none")


#how does prey effect daily lunge rate
#how are the lunge rets between prey type humps different 
MCMCglmm_humpbacks <- MCMCglmm(daily_lunges ~ prey_type,
                               random = ~ mass_skr,
                               data = filter(med_scenario, species_code == "mn"), 
                               family = "gaussian",
                               nitt = 11000, thin = 1, burnin = 1000,
                               pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                               verbose = TRUE)
summary(MCMCglmm_humpbacks)
#fish is the first line, the intercept line
#krill feeder is 111.7+193.6 
#mcmc p value is <.001 




