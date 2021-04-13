#Long deployment breakdown scratchpad 

## Attempt to split days at the deployment level

subset_deploy <- deployments %>% 
  slice(170:172)

endsOnOtherDay <- function(subset_deploy){
  as_date(subset_deploy$tag_on) != as_date(subset_deploy$tag_off)
}

split1rowInto2Days <- function(subset_deploy){
  df1 <- subset_deploy
  df2 <- subset_deploy
  df1$tag_off <- as_date(df1$tag_on) + days(1) - milliseconds(1)
  df2$tag_on <- as_date(df2$tag_on) + days(1)
  rbind(df1, df2)
}


splitDates <- function(subset_deploy){
  if (nrow(subset_deploy) > 1){
    return(subset_deploy %>%
             split(f = 1:nrow(subset_deploy)) %>%
             lapply(splitDates) %>%
             reduce(rbind))
  }
  
  if (subset_deploy %>% endsOnOtherDay()){
    return(subset_deploy %>%
             split1rowInto2Days() %>%
             splitDates())
  }
  
  subset_deploy
}


subset_deploy %>% 
  splitDates() %>%
  mutate(Duration = difftime(tag_off, tag_on, units = "hours")
         , onDay = as_date(tag_on)) %>%
  group_by(deployID, onDay) %>%
  summarise(Duration_perDay = sum(Duration))

