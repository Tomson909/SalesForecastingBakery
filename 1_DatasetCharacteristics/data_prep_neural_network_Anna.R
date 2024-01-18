###################################################
#### Preparation of the Environment #####

#Vorbreitungen und allgemeine Daten aus Data Preperation

## Load relevant packages
library(styler)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)
library(DataExplorer)
library(VIM)
library(DMwR2)

# Data Import and Creating Relevant Variables
# Read relevant data
sales_data <- read_csv("umsatzdaten_gekuerzt.csv")
weather_data <- read_csv("wetter.csv")
kiwo_data <- read.csv("kiwo.csv")
ferien_data <- read_csv("ferien.csv")
feiertage_data <- read.csv("Feiertage.txt")

# Convert "Datum" column to Date format
kiwo_data$Datum <- as.Date(kiwo_data$Datum, format = "%Y-%m-%d")
feiertage_data$Datum <- as.Date(feiertage_data$Datum,format = "%Y-%m-%d")

# Creating Big Data Set.Assuming 'Datum' is the common key for all datasets
combined_data <- sales_data %>%
  left_join(kiwo_data, by = "Datum") %>%
  left_join(weather_data, by = "Datum") %>%
  left_join(ferien_data, by = "Datum") %>%
  left_join(feiertage_data, by = "Datum")

#NA der Feiertage durch 0 ersetzen, da die Feiertag Datei nur die 1 enthält
combined_data$Feiertag <- ifelse(is.na(combined_data$Feiertag), 0, 1)


# Mapping of Warengruppe to product name. 1=Brot; 2=Broetchen; 3=Croissant;4=Konditorei;5=Kuchen;6=Saisonbrot.
combined_data <- combined_data %>%
  mutate(Produktname = case_when(Warengruppe == 1 ~ "Brot",
                                 Warengruppe == 2 ~ "Broetchen",
                                 Warengruppe == 3 ~ "Croissant",
                                 Warengruppe == 4 ~ "Konditorei",
                                 Warengruppe == 5 ~ "Kuchen",
                                 Warengruppe == 6 ~ "Saisonbrot"))

#Create Variable for Weekdays
combined_data <- combined_data %>%
  mutate(
    Datum = as.Date(Datum),
    Wochentag = weekdays(Datum, abbreviate = FALSE)
  )

# Create Regenvariable
# Funktion, um zu prüfen, ob ein Wettercode Regen darstellt
ist_regen <- function(code) {
  return(code >= 50 & code <= 69 | code >= 80 & code <= 82 | code >= 91 & code <= 92 | code %in% c(95, 97))
}

combined_data <- combined_data %>%
  mutate(IsRegen = sapply(Wettercode, ist_regen))

# Neue Kategorie "Wolkenlos" erstellen
combined_data <- combined_data %>%
  mutate(Wolkenlos = Bewoelkung >= 0 & Bewoelkung <= 2)


# Kieler Woche as Logical Vector
combined_data <- combined_data %>%
  mutate(KielerWoche = if_else(is.na(KielerWoche), FALSE, KielerWoche == 1))

# Create Trainingsdatensatz vom 01.07.2013 bis 31.07.2017
train_data <- combined_data %>%
  filter(Datum >= as.Date("2013-07-01") & Datum <= as.Date("2017-07-31"))

# Create Validierungsdatensatz vom 01.08.2017 bis 31.07.2018
validation_data <- combined_data %>%
  filter(Datum >= as.Date("2017-08-01") & Datum <= as.Date("2018-07-31"))
View(combined_data)

 ######### MISSING VALUES - Part KNN  #########
# Wettercode aus combined data in einen Datenrahmen umgewandelt
wettercode_df <- data.frame(Wettercode = combined_data$Wettercode)
view(wettercode_df)

# Wende k-NN-Imputation an
wettercode_imputed <- knnImputation(wettercode_df, k=2)

# Füge die imputierte Spalte wieder in den ursprünglichen Datensatz ein
combined_data$Wettercode <- wettercode_imputed$Wettercode

# Ausgabe des imputierten Datensatzes
print(combined_data)



######### ONE-HOT-ENCODING #########


#Erstellen der Dummy-Variablen und Hinzufügen der numerischen Variablen
features_matrix_train <- model.matrix(Umsatz ~ Warengruppe + KielerWoche + Temperatur + Windgeschwindigkeit + IsFerien + IsFeiertag + Wochentag, data=train_data_combined)

#Umwandlung in ein tibble
features_train <- as_tibble(features_matrix_train)

# Entfernt die erste Spalte, die den Intercept darstellt
#features_train <- features_train[, -1]


# Erstellen der Dummy-Variablen und Hinzufügen der numerischen Variablen für validation_data_combined
features_matrix_validation <- model.matrix(Umsatz ~ Warengruppe + KielerWoche + Temperatur + Windgeschwindigkeit + IsFerien + IsFeiertag + Wochentag, data=validation_data_combined)

# Umwandlung in ein tibble
features_validation <- as_tibble(features_matrix_validation)


# Entfernt die erste Spalte, die den Intercept darstellt
#features_validation <- features_test[, -1]


# Erstellen der Dummy-Variablen und Hinzufügen der numerischen Variablen für validation_data_combined
features_matrix_test <- model.matrix(~ Warengruppe + KielerWoche + Temperatur + Windgeschwindigkeit + IsFerien + IsFeiertag + Wochentag, data=test_data_combined)

# Umwandlung in ein tibble
features_test <- as_tibble(features_matrix_test)


# Erstellen des label_train Tibbles aus der Umsatz-Spalte von train_data_combined
label_train <- train_data_combined %>%
  select(Umsatz) %>%
  rename(label = Umsatz) %>%
  as_tibble()



# Erstellen des label_train Tibbles aus der Umsatz-Spalte von train_data_combined
label_validation <- validation_data_combined %>%
  select(Umsatz) %>%
  rename(label = Umsatz) %>%
  as_tibble()



# Check the dimensions of the dataframes
cat("Training features dimensions:", dim(features_train), "\n")
cat("Training labels dimensions:", dim(label_train), "\n")
cat("Validation labels dimensions:", dim(label_validation), "\n")
cat("Validation features dimensions:", dim(features_validation), "\n")
cat("Test features dimensions:", dim(features_test), "\n")


######### Export Data Sets for Tensor-Flow #########

# Exportieren von features_train als CSV
write.csv(features_train, "features_train.csv", row.names = FALSE)

# Exportieren von label_train als CSV
write.csv(label_train, "label_train.csv", row.names = FALSE)

# Exportieren von label_validation als CSV
write.csv(label_validation, "label_validation.csv", row.names = FALSE)

# Exportieren von features_validation als CSV
write.csv(features_validation, "features_validation.csv", row.names = FALSE)

# Exportieren von features_test als CSV
write.csv(features_test, "features_test.csv", row.names = FALSE)


#Exportieren von test_data_combined für die IDs
write.csv(test_data_combined, "test_data_IDs.csv", row.names = FALSE)


