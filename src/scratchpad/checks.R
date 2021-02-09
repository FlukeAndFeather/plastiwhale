#Poke around scripts


diel_lunges %>% 
  group_by(dielperiod, deployID) %>% 
  summarise(n()) %>% 
  View()
#! check for the rates! 