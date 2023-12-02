# Load relevant packages

library(styler)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)
library(DataExplorer)
library(caret)
#Get working directory correct
#setwd("C:/Users/sunpn1013/Desktop/Data Science Kurs/SalesForecastingBakery/0_DataPreparation")
setwd("/home/tomruge/Schreibtisch/UNI/Semester_7/Einführung_in_datascience_maschinelleslernen/SalesForecastingBakery/0_DataPreparation")


# import all necessary data
# Read "umsatzdaten_gekuerzt.csv" into a tibble called "sales_data"
umsatzdaten_gekuerzt <- read_csv("umsatzdaten_gekuerzt.csv", col_types = cols(Datum = col_date(format = "%Y-%m-%d"), Warengruppe = col_integer(), Umsatz = col_double()))
kiwo <- read_csv("kiwo.csv", col_types = cols(Datum = col_date(format = "%Y-%m-%d"), KielerWoche = col_integer()))
wetter <- read_csv("wetter.csv", col_types = cols(Datum = col_date(format = "%Y-%m-%d"), Bewoelkung = col_integer(), Temperatur = col_double(), Windgeschwindigkeit = col_integer(), Wettercode = col_integer()))
feiertage <- read_csv("Feiertage_Deutschland.csv", col_types = cols(Summary = col_character(), Description = col_character(), Datum = col_date(format = "%Y-%m-%d")))
ferien <- read_csv('ferien.csv', col_types = cols(Datum = col_date(format = "%Y-%m-%d"), IsFerien = col_logical()))


# datensätze zusammenführen
dataset <- umsatzdaten_gekuerzt %>%
  left_join(kiwo, by = "Datum") %>%
  left_join(wetter, by = "Datum") %>%
  left_join(feiertage, by = "Datum") %>%
  left_join(ferien, by = "Datum")

#create logical values for ferien and feiertage and Kielerwoche
dataset <- dataset %>%
  mutate(IsFerien = ifelse(IsFerien == TRUE, 1, 0),
         KielerWoche = ifelse(is.na(KielerWoche), 0, 1),
         IsFeiertag = ifelse(is.na(Summary), 0, 1))

# remove columns Summary, Description
dataset <- dataset %>%
  select(-c(Summary, Description))

# map numeric values to categorical values
dataset <- dataset %>%
  mutate(Produktname = case_when(Warengruppe == 1 ~ "Brot",
                                 Warengruppe == 2 ~ "Broetchen",
                                 Warengruppe == 3 ~ "Croissant",
                                 Warengruppe == 4 ~ "Konditorei",
                                 Warengruppe == 5 ~ "Kuchen",
                                 Warengruppe == 6 ~ "Saisonbrot"))

# add weather description
# creating a column "Niederchlag" and dividing the data into Niederschlag types, woher kommt dieses Wissen?
dataset <- dataset %>%
  mutate(Niederschlag = case_when(
    Wettercode >= 50 & Wettercode < 59 ~ "Sprühregen",
    Wettercode >= 60 & Wettercode < 69 ~ "Regen",
    Wettercode >= 70 & Wettercode <= 79 ~ "Schnee",
    Wettercode >= 80 & Wettercode <= 89 ~ "Schauer",
    Wettercode >= 90 & Wettercode <= 99 ~ "Gewitter",
    TRUE ~ NA_character_
  ))

# Define the proportions for training, validation, and test sets
train_prop <- 0.6
val_prop <- 0.2
test_prop <- 0.2

set.seed(123)

# Create an index for splitting the dataset
index_train <- createDataPartition(y = dataset$Umsatz, p = train_prop, list = FALSE)

# Split the dataset into training set
train_data <- dataset[index_train, ]

# Calculate the remaining proportions after the training set
remaining_prop <- 1 - train_prop
val_test_prop <- val_prop / remaining_prop

# Create a vector of indices for random splitting
indices <- sample(1:nrow(dataset), size = nrow(dataset))

# Calculate the number of rows for each set
num_train <- round(train_prop * nrow(dataset))
num_val <- round(val_prop * nrow(dataset))
num_test <- nrow(dataset) - num_train - num_val

# Split the dataset using the calculated indices
train_data <- dataset[indices[1:num_train], ]
validation_data <- dataset[indices[(num_train + 1):(num_train + num_val)], ]
test_data <- dataset[indices[(num_train + num_val + 1):(num_train + num_val + num_test)], ]

# Print the dimensions of the resulting sets
cat("Train set dimensions:", dim(train_data), "\n")
cat("Validation set dimensions:", dim(validation_data), "\n")
cat("Test set dimensions:", dim(test_data), "\n")


# Save the sets to CSV files
write_csv(train_data, "train_data.csv")
write_csv(validation_data, "validation_data.csv")
write_csv(test_data, "test_data.csv")

