# loading libraries + data 

library(tidyverse)
library(maps)
library(flexclust)
library(extrafont)
library(ggthemes)
theme_set(theme_light())
prescriptions <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/prescriptions.csv")


# manipulating data

prescriptions.clean <- aggregate(NumberClaims ~ State + OpioidFlag, prescriptions, sum)
prescriptions.clust <- prescriptions.clean |> 
  pivot_wider(names_from = OpioidFlag, values_from = NumberClaims)

# standardizing

prescriptions.std <- prescriptions.clust |> 
  mutate(
    std_opioid = as.numeric(scale(Opioid)),
    std_notopioid = as.numeric(scale(NotOpioid))
  )

# computing the distance matrix

state_dist <- prescriptions.std |> 
  select(std_opioid, std_notopioid) |> 
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
  select(std_opioid, std_notopioid) |> 
  kcca(k = 4, control = list(initcent = "kmeanspp"))
prescriptions.std |> 
  mutate(state_cluster = factor(init_kmpp@cluster)) |> 
  ggplot(aes(x = std_opioid, y = std_notopioid, 
             color = state_cluster)) + 
  geom_point() +
  coord_fixed()

# kmeans++ using flexclust

library(flexclust)
init_kmeanspp <- prescriptions.std |> 
  select(std_opioid, std_notopioid) |> 
  kcca(k = 4, control = list(initcent = "kmeanspp"))

prescriptions.std |>
  mutate(
    state_clusters = as.factor(init_kmeanspp@cluster)
  ) |>
  ggplot(aes(x = Opioid, y = NotOpioid,
             color = state_clusters)) +
  geom_point(size = 4) + 
  ggthemes::scale_color_colorblind() +
  labs(x = "Number of Opioid Prescription Claims", 
       y = "Number of Non-Opioid Prescription Claims ", 
       color = "State Clusters") +
  theme(panel.background = element_blank(),
        legend.position = "right", 
        text = element_text(family = "Times New Roman"))

# elbow plot

prescriptions_kmpp <- function(k) {
  
  kmeans_results <- prescriptions.std |>
    select(std_opioid, std_notopioid) |>
    kcca(k = k, control = list(initcent = "kmeanspp"))
  
  kmeans_out <- tibble(
    clusters = k,
    total_wss = sum(kmeans_results@clusinfo$size * 
                      kmeans_results@clusinfo$av_dist)
  )
  return(kmeans_out)
}

n_clusters_search <- 2:12
kmpp_search <- n_clusters_search |> 
  purrr::map(prescriptions_kmpp) |> 
  bind_rows()
kmpp_search |> 
  ggplot(aes(x = clusters, y = total_wss)) +
  geom_line() + 
  geom_point(size = 4) +
  scale_x_continuous(breaks = n_clusters_search)