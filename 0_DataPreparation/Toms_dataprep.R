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
  mutate(Wettererscheinung = case_when(
    between(Wettercode, 0, 3) ~ "Bewölkungsentwicklung",
    between(Wettercode, 4, 8) ~ "Staub und Wirbel",
    between(Wettercode, 9, 19) ~ "Sturm und Tromben",
    between(Wettercode, 20, 29) ~ "Nebel und Sicht",
    between(Wettercode, 30, 35) ~ "Sandsturm",
    between(Wettercode, 36, 47) ~ "Niederschlag beendet",
    between(Wettercode, 48, 49) ~ "Nebel mit Reifbildung",
    between(Wettercode, 50, 59) ~ "Sprühregen",
    between(Wettercode, 60, 69) ~ "Regen",
    between(Wettercode, 70, 79) ~ "Schnee",
    between(Wettercode, 80, 99) ~ "Gewitter",
    TRUE ~ NA_character_
  ))

# also comfortably with IsNiederschlag
dataset <- dataset %>% mutate(IsNiederschlag = case_when(between(Wettercode, 50,99) ~ 1,
                                                         between(Wettercode, 0,49) ~ 0,))

#rename column
dataset <- dataset %>% rename(IsKielerWoche = KielerWoche)

# create train data and test data
train_date_1 <- as.Date("01.07.2013", format = "%d.%m.%Y")
train_date_2 <- as.Date("31.07.2017", format = "%d.%m.%Y")
test_date_1 <- as.Date("01.08.2017", format = "%d.%m.%Y")
test_date_2 <- as.Date("31.07.2018", format = "%d.%m.%Y")


train_data <- dataset %>%
  filter(Datum >= train_date_1 & Datum <= train_date_2)
test_data <- dataset %>%
  filter(Datum >= test_date_1 & Datum <= test_date_2)

# write data to csv
write_csv(train_data, "train_data.csv")
write_csv(test_data, "test_data.csv")

