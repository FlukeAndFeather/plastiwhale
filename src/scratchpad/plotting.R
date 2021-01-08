#Plotting

#1) Depth frequency for lunges

ggplot(lunges, aes(Species, lunge_depth))+
  geom_point(aes(color = depth_bucket))

#2) Daily Lunges per species per depth bucket
ggplot(lunges_and_length, aes(depth_bucket, daily_lunges))+
  geom_point()


#5) plastic per day from water
ggplot(total_plastic, aes(simulation_id, retained_plastic))+
  geom_point()

#5) plastic per day from prey
ggplot(total_plastic, aes(simulation_id, plastic_prey))+
  geom_point()
