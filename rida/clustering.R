# loading libraries + data 

library(tidyverse)
library(maps)
library(flexclust)
library(extrafont)

prescriptions <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/prescriptions.csv")


# manipulating data

prescriptions.clean <- aggregate(NumberClaims ~ State + Type, prescriptions, sum)
prescriptions.clust <- prescriptions.clean |> 
  pivot_wider(names_from = Type, values_from = NumberClaims) %>%
  rename(Brand = Brand, Generic = Generic)

# standardizing

prescriptions.std <- prescriptions.clust |> 
  mutate(
    std_brand = as.numeric(scale(Brand)),
    std_generic = as.numeric(scale(Generic))
  )

# computing the distance matrix

state_dist <- prescriptions.std |> 
  select(std_brand, std_generic) |> 
  dist()
state_dist_matrix <- state_dist |> 
  as.matrix()
rownames(state_dist_matrix) <- prescriptions.std$State
colnames(state_dist_matrix) <- prescriptions.std$State
long_dist_matrix <- state_dist_matrix |> 
  as_tibble() |> 
  mutate(state1 = rownames(state_dist_matrix)) |> 
  pivot_longer(!state1, names_to = "state2", values_to = "distance")

# k-means++

init_kmpp <- prescriptions.std |> 
  select(std_brand, std_generic) |> 
  kcca(k = 4, control = list(initcent = "kmeanspp"))
prescriptions.std |> 
  mutate(state_cluster = factor(init_kmpp@cluster)) |> 
  ggplot(aes(x = std_brand, y = std_generic, 
             color = state_cluster)) + 
  geom_point() +
  coord_fixed()