disp

mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(disp), n = n())

int %>%
  group_by(GEOID) %>%
  summarise(mean = mean("data...Var_col."))

Mean <- int %>% group_by(GEOID) %>% summarise(mean = mean("data...Var_col."))