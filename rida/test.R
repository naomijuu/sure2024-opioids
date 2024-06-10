# loading libraries + data 

library(tidyverse)
prescriptions <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/prescriptions.csv")

# changing data format 

prescriptions.new <- prescriptions |> 
  select(State, OpioidFlag) |> 
  group_by(State, OpioidFlag) |> 
  summarize(num.claims = n())