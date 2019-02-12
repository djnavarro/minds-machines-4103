# load packages
library(tidyverse)
library(here)
library(gganimate)


# prior
get_prior <- function(t, mu, sig) {
  pr <- dnorm(t, mean = mu, sd = sig) # normal probability
  pr <- pr / sum(pr)                  # make it sum to 1
  return(pr)
}

# likelihood
get_likelihood <- function(s, t, noise) {
  lik <- dnorm(s, mean = t, sd = noise) # normal probability
  return(lik) 
}

# normalisation function
normalise <- function(prob) {
  return( prob / sum(prob) )
} 

# posterior
get_posterior <- function(stimulus, target, noise, catmean, catsd) {
  
  # priors and likelihoods
  prior <- get_prior(target, catmean, catsd) 
  likelihood <- get_likelihood(stimulus, target, noise)
  
  # posterior
  posterior <- (prior * likelihood) %>% normalise()
  
  # organise everything into a tibble!
  out <- tibble(stimulus, target, prior, posterior, noise, catmean, catsd)
  
  return(out)
}


# a fancy-pants plotting function
plot_beliefs <- function(belief) {
  
  
  
  plot(pic)
}

# parameters 
target <- 1:100
category_mean <- 50
noise <- 10
category_sd <- 20

stimulus <- seq(2,100,2) %>% as.list()

beliefs <- stimulus %>% 
  map(function(stimulus) {
    get_posterior(stimulus, target, noise, category_mean, category_sd)
  }) %>%
  reduce(bind_rows)

belief_long <- beliefs %>% 
  gather(key = "type", value = "belief", prior, posterior)

belief_summaries <- beliefs %>% 
  group_by(stimulus) %>%
  summarise(
    prior = sum(prior * target),
    posterior = sum(posterior * target),
    stim = mean(stimulus)
  ) %>% 
  ungroup() %>%
  gather(key = "type", value = "location", prior, posterior, stim)

base_pic <- belief_long %>% 
  ggplot(aes(x = target, y = belief, colour = type)) + 
  geom_line(lwd = 1.5) + 
  geom_point(
    data = belief_summaries, 
    mapping = aes(x = location, y = 0, colour = type),
    size = 5
  ) 

#plot(base_pic + facet_wrap(~stimulus))

pic <- base_pic + 
  transition_time(time = stimulus) +
  ease_aes('linear')

# save animation
pic %>% animate(nframes = 100, length = 1.5)
anim_save(here::here("magnet", "move_stimulus.gif"))


