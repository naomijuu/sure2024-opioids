library(tidyverse)
prescriptions <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/prescriptions.csv")



# Calculate average TotalDrugCost per SpecialtyCateg and Type
avg_cost <- prescriptions %>%
  group_by(SpecialtyCateg, Type) %>%
  summarize(AverageTotalDrugCost = mean(TotalDrugCost, na.rm = TRUE)) %>%
  ungroup()

# Faceted bar plot: Average TotalDrugCost per SpecialtyCateg, differentiated by Type
ggplot(avg_cost, aes(x = SpecialtyCateg, y = AverageTotalDrugCost, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Total Drug Cost per Specialty Category by Drug Type",
       x = "Specialty Category",
       y = "Average Total Drug Cost") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Type)

