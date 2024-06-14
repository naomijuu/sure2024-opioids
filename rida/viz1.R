# loading libraries + data 

library(tidyverse)
library(maps)
library(extrafont)
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
  scale_fill_gradient2(low = "white", mid = "thistle", 
                       high = "midnightblue", midpoint = 3) +
  theme_void() +
  coord_map("polyconic") + 
  labs(
    title = "Opioid Prescriptions Claims by State",
    fill = "Number of Opioid Prescription Claims"
  ) + 
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.1))