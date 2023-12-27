###################################################
### Preparation of the Environment ####

# Clear environment
remove(list = ls())

# Create list with needed libraries
pkgs <- c("readr", "dplyr", "reticulate", "ggplot2", "Metrics", "lubridate", "tidyverse")

# Load each listed library and check if it is installed and install if necessary
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

###################################################
### Data Import ####

# Read relevant data
sales_data <- read_csv("train.csv")
test_data <- read_csv("test.csv")

# Trainings- und Testdaten in einen Datensaz
combined_data <- bind_rows(sales_data, test_data)

#Kiwo Daten
kiwo_data <- read.csv("kiwo.csv")
# Convert "Datum" column to Date format
kiwo_data$Datum <- as.Date(kiwo_data$Datum, format = "%Y-%m-%d")

#Ferien Daten
ferien_data <- read_csv("updated_ferien.csv")

#Wetter Daten
weather_data <- read_csv("wetter.csv")


# Creating Big Data Set.Assuming 'Datum' is the common key for all datasets
combined_data <- combined_data %>%
  left_join(kiwo_data, by = "Datum") %>%
  left_join(weather_data, by = "Datum") %>%
  left_join(ferien_data, by = "Datum")


# Create a vector of public holidays for Schleswig-Holstein (2013-2018), including Dec 24th and Dec 31st
schleswig_holstein_feiertage <- as.Date(c(
  "2013-01-01", "2013-03-29", "2013-04-01", "2013-05-01", "2013-05-09",
  "2013-05-20", "2013-10-03", "2013-12-25", "2013-12-26", "2013-12-24", "2013-12-31",
  "2014-01-01", "2014-04-18", "2014-04-21", "2014-05-01", "2014-05-29",
  "2014-06-09", "2014-10-03", "2014-12-25", "2014-12-26", "2014-12-24", "2014-12-31",
  "2015-01-01", "2015-04-03", "2015-04-06", "2015-05-01", "2015-05-14",
  "2015-05-25", "2015-10-03", "2015-12-25", "2015-12-26", "2015-12-24", "2015-12-31",
  "2016-01-01", "2016-03-25", "2016-03-28", "2016-05-01", "2016-05-05",
  "2016-05-16", "2016-10-03", "2016-12-25", "2016-12-26", "2016-12-24", "2016-12-31",
  "2017-01-01", "2017-04-14", "2017-04-17", "2017-05-01", "2017-05-25",
  "2017-06-05", "2017-10-03", "2017-12-25", "2017-12-26", "2017-12-24", "2017-12-31",
  "2018-01-01", "2018-03-30", "2018-04-02", "2018-05-01", "2018-05-10",
  "2018-05-21", "2018-10-03", "2018-12-25", "2018-12-26", "2018-12-24", "2018-12-31",
  "2019-01-01", "2019-04-19", "2019-04-22", "2019-05-01", "2019-05-30", "2019-06-10"
))


# # Add a holiday column to combinded_data
combined_data <- combined_data %>%
  mutate(IsFeiertag = ymd(Datum) %in% schleswig_holstein_feiertage)

#Create Variable for Weekdays
combined_data <- combined_data %>%
  mutate(
    Datum = as.Date(Datum),
    Wochentag = weekdays(Datum, abbreviate = FALSE)
  )


#Kieler Woche as Logical Vector
combined_data <- combined_data %>%
  mutate(KielerWoche = if_else(is.na(KielerWoche), FALSE, KielerWoche == 1))


# Entferne der Variable "Wettercode" aus dem Datensatz "combined_data" [hat sehr viele NA]
combined_data <- combined_data %>% select(-Wettercode)


###################################################
### Data Preparation for Tensor Flow ####

# Umwandlung der kategorialen Variablen in Faktoren
combined_data$Warengruppe <- as.factor(combined_data$Warengruppe)
combined_data$KielerWoche <- as.factor(combined_data$KielerWoche)
combined_data$IsFerien <- as.factor(combined_data$IsFerien)
combined_data$IsFeiertag <- as.factor(combined_data$IsFeiertag)
combined_data$Wochentag <- as.factor(combined_data$Wochentag)


#Erstellen der Dummy-Variablen und Hinzufügen der numerischen Variablen
features_matrix <- model.matrix(Umsatz ~ Warengruppe + KielerWoche + Temperatur + Windgeschwindigkeit + IsFerien + IsFeiertag + Wochentag, data=combined_data)

#Umwandlung in ein tibble
features <- as_tibble(features_matrix)

# Entfernt die erste Spalte, die den Intercept darstellt
features <- features[, -1]


# Hinzufügen label und Datum
prepared_data <- tibble(Datum = cleaned_data$Datum, Umsatz = combined_data$Umsatz, features)


###################################################
### Selection of Training, Validation and Test Data ####


# Set up date ranges for training, validation, and test data
training_start <- as.Date("2013-07-01")
training_end <- as.Date("2017-07-31")
validation_start <- as.Date("2017-08-01")
validation_end <- as.Date("2018-07-31")
test_start <- as.Date("2018-08-01")
test_end <- as.Date("2019-07-30")

# Create Training Data Set
train_data <- cleaned_data %>%
  filter(Datum >= training_start & Datum <= training_end)

# Create Validation Data Set
validation_data <- cleaned_data %>%
  filter(Datum >= validation_start & Datum <= validation_end)

# Create Test Data Set
test_data <- cleaned_data %>%
  filter(Datum >= test_start & Datum <= test_end)








