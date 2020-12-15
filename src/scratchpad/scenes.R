#other param are species and prey type
#retentions
#pick vlaues for param, store in variables 
#

simulate <- function(retention, param2, ...) {
  # step 1: simulate feeding
  # step 2: simulate morphology
  # step 3: simulate plastic from h2o
  # step 4: simulate plastic from prey
  # step 5: shake and bake (combine results, concatenate input parameters)
  
}

# scenario table
scenarios <- expand_grid(
  retention = c(...),
  param2 = c(...),
  ...
) %>%
  pmap_dfr(., simulate)

