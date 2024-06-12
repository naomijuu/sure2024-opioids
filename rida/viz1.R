# loading libraries + data 

library(tidyverse)
library(maps)
library(ggdendro)
prescriptions <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/prescriptions.csv")

# changing data format 

prescriptions.new <- prescriptions |> 
  select(State, OpioidFlag) |> 
  group_by(State, OpioidFlag) |> 
  summarize(num.claims = n()) |> 
  filter(OpioidFlag == "Opioid")

# map data

states.list <- map_data("state")
states.list$region <- state.abb[match(states.list$region, 
                                        tolower(state.name))]
states.map.data <- states.list %>%
  left_join(prescriptions.new, by = c("region" = "State"))

# plotting heatmap

ggplot(states.map.data) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = num.claims), color = "black") + 
  scale_fill_gradient2(low = "white", mid = "lightpink", 
                       high = "violetred3", midpoint = 3) +
  theme_void() +
  coord_map("polyconic") + 
  labs(
    title = "Opioid Prescriptions by State",
    fill = "Number of Opioid Claims"
  ) + 
  theme(legend.position = "bottom")

# clustering analysis

west <- c('AK', 'AZ', 'CA', 'CO', 'HI', 'ID', 'MT', 'NV', 'NM', 'OR', 'UT', 'WA', 'WY')
midwest <- c('IL', 'IN', 'IA', 'KS', 'MI', 'MN', 'MO', 'NE', 'ND', 'OH', 'SD', 'WI')
southwest <- c('AZ', 'NM', 'OK', 'TX')
southeast <- c('AL', 'AR', 'FL', 'GA', 'KY', 'LA', 'MS', 'NC', 'SC', 'TN', 'VA', 'WV')
northeast <- c('CT', 'MD', 'ME', 'MA', 'NH', 'NJ', 'NY', 'PA', 'RI', 'VT')

prescriptions$Region <- ifelse(prescriptions$State %in% northeast, 'Northeast',
                               ifelse(prescriptions$State %in% west, 'West',
                                      ifelse(prescriptions$State %in% midwest, 'Midwest',
                                             ifelse(prescriptions$State %in% southwest, 'Southwest',
                                                    ifelse(prescriptions$State %in% southeast, 'Southeast', 'Other')))))

agg_data2 <- aggregate(NumberClaims ~ Region + Type, prescriptions, sum)


# Unsupervised learning: hierarchical clustering

# loading libraries + data 

library(tidyverse)
library(seriation)
library(ggthemes)
library(ggdendro)
library(protoclust)

prescriptions.clust <- prescriptions |> 
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
prescriptions.clust <- prescriptions.clust |> 
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

# heatmap 

long_dist_matrix |> 
  ggplot(aes(x = state1, y = state2, fill = distance)) +
  geom_tile() + 
  scale_fill_gradient(low = "thistle", high = "darkblue") + 
  coord_fixed() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
# seriation 
state_dist_seriate <- seriate(state_dist)
state_order <- get_order(state_dist_seriate)
county_names_order <- prescriptions.clust$State[state_order]
long_dist_matrix |> 
  mutate(state1 = fct_relevel(state1, state_names_order),
         state2 = fct_relevel(state2, state_names_order)) |> 
  ggplot(aes(x = state1, y = state2, fill = distance)) +
  geom_tile() + 
  scale_fill_gradient(low = "thistle", high = "darkblue") + 
  coord_fixed() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

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

# k-means clustering 
prescriptions_clust2 <- prescriptions %>%
  select(State, TotalDrugCost, TotalDrugCost65Older) %>%
  group_by(State) %>%
  summarize(
    total.cost = sum(TotalDrugCost, na.rm = TRUE),
    total.cost65 = sum(TotalDrugCost65Older, na.rm = TRUE)
  )
pre_kmeans <- prescriptions_clust2 |>
  select(total.cost, total.cost65) |> 
  kmeans(centers = 3, nstart = 1, algorithm = "Lloyd")
prescriptions_clust2 |> 
  mutate(state_cluster = factor(pre_kmeans$cluster)) |> 
  ggplot(aes(x = total.cost, y = total.cost65, color = state_cluster)) + 
  geom_point() +
  coord_fixed()
# scaling variables
std_pre <- prescriptions_clust2 |> 
  mutate(std_tc = as.numeric(scale(total.cost)),
         std_tc65 = as.numeric(scale(total.cost65)))
std_pre_kmeans <- std_pre |>
  select(std_tc, std_tc65) |> 
  kmeans(centers = 4, nstart = 1, algorithm = "Lloyd")
std_pre |> 
  mutate(state_cluster = factor(pre_kmeans$cluster)) |> 
  ggplot(aes(x = std_tc, y = std_tc65, color = state_cluster)) + 
  geom_point() +
  coord_fixed()
# k-means++
init_kmpp <- std_pre |> 
  select(std_tc, std_tc65) |> 
  kcca(k = 4, control = list(initcent = "kmeanspp"))
std_pre |> 
  mutate(state_cluster = factor(init_kmpp@cluster)) |> 
  ggplot(aes(x = std_tc, y = std_tc65, 
             color = state_cluster)) + 
  geom_point() +
  coord_fixed()

# elbow plot
pre_kmpp <- function(k) {
  
  kmeans_results <- prescriptions_clust2 |>
    select(std_tc, std_tc65) |>
    kcca(k = k, control = list(initcent = "kmeanspp"))
  
  kmeans_out <- tibble(
    clusters = k,
    total_wss = sum(kmeans_results@clusinfo$size * 
                      kmeans_results@clusinfo$av_dist)
  )
  return(kmeans_out)
}
library(flexclust)
n_clusters_search <- 2:12
kmpp_search <- n_clusters_search |> 
  map(pre_kmpp) |> 
  rbind()
kmpp_search |> 
  ggplot(aes(x = clusters, y = total_wss)) +
  geom_line() + 
  geom_point(size = 4) +
  scale_x_continuous(breaks = n_clusters_search)