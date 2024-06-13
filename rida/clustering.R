# loading libraries + data 

library(tidyverse)
library(maps)
library(flexclust)
library(extrafont)

prescriptions <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/prescriptions.csv")

# initial data wrangling + standardizing variables

clean.prescriptions <- prescriptions |> 
  select(NumberClaims, NumberClaims65Older, State) |> 
  group_by(State)  |> 
  summarize(
    total.claims = sum(NumberClaims, na.rm = TRUE),
    total.claims65 = sum(NumberClaims65Older, na.rm = TRUE)) |> 
  mutate(percent65 = (total.claims65 / total.claims) * 100)


# initial eda + scaling

# scatterplot
prescriptions.clust |> 
  ggplot(aes(x = percent65, y = total.claims)) +
  geom_point(size = 4)

# standardizing
prescriptions.clust <- clean.prescriptions |> 
  mutate(
    std_pct_over65 = as.numeric(scale(percent65)),
    std_total_claims= as.numeric(scale(total.claims))
  )
prescriptions.clust |> 
  ggplot(aes(x =  std_pct_over65, y = std_total_claims)) +
  geom_point(size = 4) + 
  coord_fixed()

# computing the distance matrix

state_dist <- prescriptions.clust |> 
  select(std_pct_over65, std_total_claims) |> 
  dist()
state_dist_matrix <- state_dist |> 
  as.matrix()
rownames(state_dist_matrix) <- prescriptions.clust$State
colnames(state_dist_matrix) <- prescriptions.clust$State
long_dist_matrix <- state_dist_matrix |> 
  as_tibble() |> 
  mutate(state1 = rownames(state_dist_matrix)) |> 
  pivot_longer(!state1, names_to = "state2", values_to = "distance")

# complete linkage 

prescrip.complete <- state_dist |> 
  hclust(method = "complete") 
prescriptions.clust |> 
  mutate(cluster = cutree(prescrip.complete, k = 3)) |> 
  ggplot(aes(x = std_pct_over65, y = std_total_claims, color = cluster)) + 
  geom_point() 
prescrip.complete |> 
  ggdendrogram() + 
  labs(y = "Dissimilarities between clusters")

prescriptions_clust2 |> 
  ggplot(aes(x = total.cost)) + 
  geom_histogram()


# CLUSTERING ANALYSIS

prescriptions.clean <- aggregate(NumberClaims ~ State + Type, prescriptions, sum)

prescriptions.clust <- prescriptions.clean |> 
  pivot_wider(names_from = Type, values_from = NumberClaims) %>%
  rename(Brand = Brand, Generic = Generic)


# initial eda

prescriptions.clust |> 
  ggplot(aes(x = Brand, y = Generic)) +
  geom_point(size = 4)

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