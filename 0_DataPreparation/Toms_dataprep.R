# Load relevant packages

library(styler)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)
library(DataExplorer)

#Get working directory correct
#setwd("C:/Users/sunpn1013/Desktop/Data Science Kurs/SalesForecastingBakery/0_DataPreparation")
setwd("/home/tomruge/Schreibtisch/UNI/Semester_7/Einführung_in_datascience_maschinelleslernen/SalesForecastingBakery/0_DataPreparation")


# import all necessary data
# Read "umsatzdaten_gekuerzt.csv" into a tibble called "sales_data"
umsatzdaten_gekuerzt <- read_csv("umsatzdaten_gekuerzt.csv", col_types = cols(Datum = col_date(format = "%Y-%m-%d"), Warengruppe = col_integer(), Umsatz = col_double()))
kiwo <- read_csv("kiwo.csv", col_types = cols(Datum = col_date(format = "%Y-%m-%d"), KielerWoche = col_integer()))
wetter <- read_csv("wetter.csv", col_types = cols(Datum = col_date(format = "%Y-%m-%d"), Bewoelkung = col_integer(), Temperatur = col_double(), Windgeschwindigkeit = col_integer(), Wettercode = col_integer()))
feiertage <- read_csv("Feiertage_Deutschland.csv", col_types = cols(Summaryy = col_character(), Description = col_character(), Datum = col_date(format = "%Y-%m-%d")))

# ferien müssen noch hinzugefügt werden


# datensätze zusammenführen
dataset <- umsatzdaten_gekuerzt %>%
  full_join(kiwo, by = "Datum") %>%
  full_join(wetter, by = "Datum") %>%
  full_join(feiertage, by = "Datum")

print(dataset)



# Define the proportions for training, validation, and test sets
train_prop <- 0.6
val_prop <- 0.2
test_prop <- 0.2

# Calculate the number of rows for each set
num_rows <- nrow(dataset)
train_size <- round(train_prop * num_rows)
val_size <- round(val_prop * num_rows)

# Create indices for training, validation, and test sets
train_indices <- slice_sample(dataset, n = train_size, replace = FALSE)$Datum
remaining_data <- dataset %>% filter(!Datum %in% train_indices)
val_indices <- slice_sample(remaining_data, n = val_size, replace = FALSE)$Datum
test_indices <- setdiff(remaining_data$Datum, val_indices)

# Create the actual datasets
train_data <- dataset %>% filter(Datum %in% train_indices)
val_data <- dataset %>% filter(Datum %in% val_indices)
test_data <- dataset %>% filter(Datum %in% test_indices)


# save the datastes as csv files
write_csv(train_data, "train_data.csv")
write_csv(val_data, "val_data.csv")
write_csv(test_data, "test_data.csv")


