library(ggthemes)
library(tidyverse)
theme_set(theme_light())
library(dslabs)
glimpse(gapminder)
gapminder |> 
  ggplot(aes(x = gdp)) + 
  geom_histogram() 
clean_gapminder <- gapminder |>
  filter(year == 2011, !is.na(gdp)) |>
  mutate(log_gdp = log(gdp))
init_kmeans <- clean_gapminder |> 
  select(log_gdp, life_expectancy) |> 
  kmeans(algorithm = "Lloyd", centers = 4, nstart = 1)

clean_gapminder |>
  mutate(
    country_clusters = as.factor(init_kmeans$cluster) ) |>
  ggplot(aes(x = log_gdp, y = life_expectancy,
             color = country_clusters)) +
  geom_point(size = 4) + 
  ggthemes ::scale_color_colorblind() +
  theme(legend.position = "bottom") 

