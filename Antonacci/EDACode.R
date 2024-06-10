#Code for EDA project
install.packages("ggplot2")
library(ggplot2)

library(tidyverse)
library(readr)
prescriptions <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/prescriptions.csv")

View(prescriptions)


west <- c('AK', 'AZ', 'CA', 'CO', 'HI', 'ID', 'MT', 'NV', 'NM', 'OR', 'UT', 'WA', 'WY')
midwest <- c('IL', 'IN', 'IA', 'KS', 'MI', 'MN', 'MO', 'NE', 'ND', 'OH', 'SD', 'WI')
southwest <- c('AZ', 'NM', 'OK', 'TX')
southeast <- c('AL', 'AR', 'FL', 'GA', 'KY', 'LA', 'MS', 'NC', 'SC', 'TN', 'VA', 'WV')
northeast <- c('CT', 'ME', 'MA', 'NH', 'NJ', 'NY', 'PA', 'RI', 'VT')

prescriptions$Region <- ifelse(prescriptions$State %in% west, 'West',
                               ifelse(prescriptions$State %in% midwest, 'Midwest',
                                      ifelse(prescriptions$State %in% southwest, 'Southwest',
                                             ifelse(prescriptions$State %in% southeast, 'Southeast',
                                                    ifelse(prescriptions$State %in% northeast, 'Northeast', 'Other')))))

agg_data <- aggregate(NumberClaims ~ Region + Type, prescriptions, sum)

ggplot(agg_data, aes(fill = Type, y = NumberClaims, x = Region)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Number of Generic vs Brand Name Drug Claims in US Regions",
       x = "Region",
       y = "Number of Claims") +
  scale_fill_manual(values = c("Brand" = "thistle", "Generic" = "violetred3")) +
  theme_minimal()

