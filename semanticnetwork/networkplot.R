# the network visualisation tools
library(network)
library(GGally)
library(sna)

# our usual data wrangling tools
library(here)
library(tidyverse)
library(magrittr)

# the data are stored as a zipped tab-separated
# value file. we can read it directly without 
# manually unzipping...
swow <- here("semanticnetwork","swow.csv.zip") %>%
  read_tsv()

# function that takes a set of user-defined words
# and returns a subnetwork of related words and the
# connections between them
make_net <- function(words, edge_threshold) {
  
  # a word is deemed "nearby" to the listed words if
  # it appears as a response to one of the words (a 
  # forward association from the word) or elicts the
  # word as a response (backward association). we 
  # impose a threshold where these associations only
  # count if they occur often enough...
  near_words <- swow %>% 
    filter(cue %in% words | response %in% words) %>%
    filter(R1.Strength > edge_threshold) %$%
    union(cue, response)
  
  # now construct the complete semantic network among
  # all words within this set of "nearby" words, again
  # imposing threshold
  edges <- swow %>% 
    filter(cue %in% near_words & response %in% near_words) %>%
    filter(R1.Strength > edge_threshold) 
  
  # take the network data and recode this as a 
  # formal "network" data structure
  net <- edges %>%
    select(cue, response) %>%
    as.network(matrix.type = "edgelist") 
  
  # add vertex attribute indicating whether it was a 
  # seed word
  names <- network.vertex.names(net)
  kind = ifelse(names %in% words, "seed", "other")
  net %v% "kind" <- kind
  

  return(net)
}

# a function to draw a pretty pictue
show_net <- function(net, label_threshold) {

  # work out which nodes to label: specifically,
  # only those with sufficiently high degree-centrality
  # (i.e. lots of connections)
  labels <- network.vertex.names(net)
  centrality <- degree(net)
  labels <- labels[centrality > label_threshold]
  
  # construct a picture using the ggnet2 function
  pic <- net %>%
    ggnet2(
      alpha = .8,             # transparency of nodes
      size = "degree",        # size of note represents degree
      size.min = 1,           # smallest node size allowed
      color = "kind",
      palette = c("seed" = "gold", "other" = "lightblue"),
      edge.color = "grey85",  # colour of edges
      label = labels          # the labels
    )
  
  plot(pic) # draw it!
  
}


# simple wrapper function
visualise <- function(words, thresholds) {
  words %>%                                             # pipe the words...
    make_net(edge_threshold = thresholds["edge"]) %>%   # to make a network...
    show_net(label_threshold = thresholds["label"])     # ... and plot it
}


# for image save
wd <- 7
ht <- 7

# example:
visualise(
  words = c("cat", "dog", "horse"),
  thresholds = c("edge" = .05, "label" = 3)
)
ggsave(here("semanticnetwork","animals.png"), width=wd, height=ht)

# example:
visualise(
  words = c("one", "two", "three", "four", "five"),
  thresholds = c("edge" = .04, "label" = 3)
)
ggsave(here("semanticnetwork","numbers.png"), width=wd, height=ht)


# example:
visualise(
  words = c("skirt", "pants", "scarf"),
  thresholds = c("edge" = .02, "label" = 10)
)
ggsave(here("semanticnetwork","clothes.png"), width=wd, height=ht)


# example:
visualise(
  words = c("mother", "father", "daughter", "son"),
  thresholds = c("edge" = .03, "label" = 5)
)
ggsave(here("semanticnetwork","family.png"), width=wd, height=ht)


# example:
visualise(
  words = c("cup", "hail", "teacher"),
  thresholds = c("edge" = .012, "label" = 5)
)
ggsave(here("semanticnetwork","arbitrary.png"), width=wd, height=ht)


# example:
visualise(
  words = c("psychology", "statistics", "physics"),
  thresholds = c("edge" = .03, "label" = 6)
)
ggsave(here("semanticnetwork","science.png"), width=wd, height=ht)


# example:
visualise(
  words = c("tweet","twitter","blog"),
  thresholds = c("edge" = .01, "label" = 8)
)
ggsave(here("semanticnetwork","socialmedia.png"), width=wd, height=ht)


# example:
visualise(
  words = c("man","woman"),
  thresholds = c("edge" = .04, "label" = 4)
)
ggsave(here("semanticnetwork","gender.png"), width=wd, height=ht)



