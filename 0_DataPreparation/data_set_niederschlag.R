read.csv("https://raw.githubusercontent.com/Tomson909/SalesForecastingBakery/main/0_DataPreparation/val_data.csv")

library(dplyr)

# creating a column "Niederchlag" and dividing the data into Niederschlag types
val_data <- val_data %>%
  mutate(Niederschlag = case_when(
    Wettercode >= 50 & Wettercode < 59 ~ "Sprühregen",
    Wettercode >= 60 & Wettercode < 69 ~ "Regen",
    Wettercode >= 70 & Wettercode <= 79 ~ "Schnee",
    Wettercode >= 80 & Wettercode <= 89 ~ "Schauer",
    Wettercode >= 90 & Wettercode <= 99 ~ "Gewitter",
    TRUE ~ NA_character_
  ))
