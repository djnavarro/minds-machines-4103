# load packages 
library(tidyverse)
library(janeaustenr)

# source for nails data: Nails Magazine:
# http://files.nailsmag.com/Handouts/NABB2017-18stats-LR.pdf
# http://files.nailsmag.com/Handouts/NABB2016-17industrystats.pdf

# source for population data: US Census Bureau
# https://www.census.gov/data/datasets/2017/demo/popest/state-total.html

# source for AFL crowd sizes.
# I scraped the data from official sources in 2010 so I'm not sure
# where it came from exactly :-)

# source for Pride and Prejudice: Jane Austen
# ... obviously!


nail_technicians <- "us_nail_techs_per_capita.txt" %>%
  read_lines() %>%
  as.numeric()

state_population <-"us_state_populations.csv" %>%
  read_csv() %>%
  select(population) %>%
  unlist()
  
bimodal_pop <- c(
  rnorm(n = 3000, mean = 4, sd = 2),   # mode at 4
  rnorm(n = 7000, mean = 12, sd = 0.5) # mode at 12
) 
  
afl_crowd <- "afl.csv" %>%
  read_csv() %>%
  select(attendance) %>%
  mutate(attendance = attendance / 1000) %>%
  unlist()
  
pp_words <- prideprejudice %>%
  paste(collapse = " ") %>%
  str_remove_all("[:punct:]") %>% 
  str_remove_all("[:digit:]") %>%
  str_to_lower() %>%
  str_split(boundary("word")) %>%
  first()
pp_wordlen <- nchar(pp_words)

# the model
make_prediction <- function(population, statistic = mean) {
  
  # what range do we want to consider?
  v_min <- 0
  v_max <- max(population)
  n_pred <- 100
  
  # initialise
  observation <- seq(
    from = v_min, to = v_max,
    length.out = n_pred
  )
  prediction <- numeric(n_pred)
  
  # compute the predictions
  for(i in 1:n_pred) {
    possibilities <- population[population >= observation[i]]
    prediction[i] <- statistic(possibilities)
  }
  
  return(tibble(observation, prediction))
} 

# a plotting function
plot_predictions <- function(dataset) {
  pic <- ggplot(
    data = dataset, 
    mapping = aes(x = observation, y = prediction)) + 
    geom_line() + 
    geom_abline(intercept = 0, slope = 1, lty = 2) + 
    theme_bw() + 
    coord_equal()
  plot(pic)
}

# because I'm lazy...
run <- function(...) {
  out <- make_prediction(...) 
  plot_predictions(out)
  return(out)
}

# try it...

# state_pred <- run(state_population)
# nails_pred <- run(nail_technicians)
bimodal_pred <- run(bimodal_pop)
# afl_pred <- run(afl_crowd)
# pp_pred <- run(pp_wordlen)
