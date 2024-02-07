###################################################
#### Preparation of the Environment #####

# Clear environment
remove(list = ls())

setwd("C:/Users/sunpn1013/Desktop/Neuer Ordner")

# Create list with needed libraries
pkgs <- c("readr", "dplyr", "reticulate", "ggplot2", "Metrics", "lubridate", "tidyverse", "VIM")

# Load each listed library and check if it is installed and install if necessary
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


######### Data Import #########

# Read relevant data
sales_data <- read_csv("train.csv")

test_data <- read_csv("test.csv")



######### Split Data Into Training and Validation Set #########

# Create Trainingsdatensatz vom 01.07.2013 bis 31.07.2017
train_data <- sales_data %>%
  filter(Datum >= as.Date("2013-07-01") & Datum <= as.Date("2017-07-31"))

# Create Validierungsdatensatz vom 01.08.2017 bis 31.07.2018
validation_data <- sales_data %>%
  filter(Datum >= as.Date("2017-08-01") & Datum <= as.Date("2018-07-31"))

summary(train_data)

summary(validation_data)


######### Add Feature Variables  #########

#Kiwo Daten
kiwo_data <- read.csv("kiwo.csv")
# Convert "Datum" column to Date format
kiwo_data$Datum <- as.Date(kiwo_data$Datum, format = "%Y-%m-%d")

#Ferien Daten
ferien_data <- read_csv("ferien.csv")

#Wetter Daten
weather_data <- read_csv("wetter.csv")



# Join fÃ¼r train_data
train_data_combined <- train_data %>%
  left_join(kiwo_data, by = "Datum") %>%
  left_join(weather_data, by = "Datum") %>%
  left_join(ferien_data, by = "Datum")

# Join fÃ¼r validation_data
validation_data_combined <- validation_data %>%
  left_join(kiwo_data, by = "Datum") %>%
  left_join(weather_data, by = "Datum") %>%
  left_join(ferien_data, by = "Datum")


# Join fÃ¼r test_data
test_data_combined <- test_data %>%
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



# Add a holiday column to train_data
train_data_combined <- train_data_combined %>%
  mutate(IsFeiertag = ymd(Datum) %in% schleswig_holstein_feiertage)

# Add a holiday column to validation_data
validation_data_combined <- validation_data_combined %>%
  mutate(IsFeiertag = ymd(Datum) %in% schleswig_holstein_feiertage)

# Add a holiday column to test_data
test_data_combined <- test_data_combined %>%
  mutate(IsFeiertag = ymd(Datum) %in% schleswig_holstein_feiertage)


#Create Variable for Weekdays to train_data
train_data_combined <- train_data_combined %>%
  mutate(
    Datum = as.Date(Datum),
    Wochentag = weekdays(Datum, abbreviate = FALSE)
  )

#Create Variable for Weekdays to validation_data
validation_data_combined <- validation_data_combined %>%
  mutate(
    Datum = as.Date(Datum),
    Wochentag = weekdays(Datum, abbreviate = FALSE)
  )

#Create Variable for Weekdays to test_data
test_data_combined <- test_data_combined %>%
  mutate(
    Datum = as.Date(Datum),
    Wochentag = weekdays(Datum, abbreviate = FALSE)
  )


#Kieler Woche as Logical Vector for train_data
train_data_combined <- train_data_combined %>%
  mutate(KielerWoche = if_else(is.na(KielerWoche), FALSE, KielerWoche == 1))

#Kieler Woche as Logical Vector for validation_data
validation_data_combined <- validation_data_combined  %>%
  mutate(KielerWoche = if_else(is.na(KielerWoche), FALSE, KielerWoche == 1))

#Kieler Woche as Logical Vector for test_data
test_data_combined <- test_data_combined  %>%
  mutate(KielerWoche = if_else(is.na(KielerWoche), FALSE, KielerWoche == 1))


# Create Regenvariable
# Funktion, um zu prüfen, ob ein Wettercode Regen darstellt
#ist_regen <- function(code) {
# return(code >= 50 & code <= 69 | code >= 80 & code <= 82 | code >= 91 & code <= 92 | code %in% c(95, 97))
#}

#Add to train_data
#train_data_combined <- train_data_combined %>%
# mutate(Regen = sapply(Wettercode, ist_regen))

#Add to validation_data
#validation_data_combined <- validation_data_combined %>%
#  mutate(Regen = sapply(Wettercode, ist_regen))

#Add to test_data


######### Abbildungen #########

#Zusammenhang Ferien und Umsatz
# Erstelle einen Boxplot
ggplot(train_data_combined, aes(x = IsFerien, y = Umsatz, fill = IsFerien)) +
  geom_boxplot() +
  labs(title = "Umsatz während Ferien und Nicht-Ferien", x = "IsFerien", y = "Umsatz") +
  theme_minimal()


######### MISSING VALUES - Part 1 #########

# Entferne der Variable "Wettercode" aus dem Datensatz "combined_data" [hat sehr viele NA]
train_data_combined <- train_data_combined %>% select(-Wettercode)


# Entferne der Variable "Wettercode" aus dem Datensatz "combined_data" [hat sehr viele NA]
validation_data_combined <- validation_data_combined %>% select(-Wettercode)

# Entferne der Variable "Wettercode" aus dem Datensatz "test_data" [hat sehr viele NA]
test_data_combined <- test_data_combined %>% select(-Wettercode)


# Untersuchen von Missing Values in jeder Spalte von train_data_combined
missing_values_train <- train_data_combined %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Anzeigen des Ergebnisses
print(missing_values_train)


# Untersuchen von Missing Values in jeder Spalte von validation_data_combined
missing_values_validation <- validation_data_combined %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Anzeigen des Ergebnisses
print(missing_values_validation)


# Untersuchen von Missing Values in jeder Spalte von test_data_combined
missing_values_test <- test_data_combined %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Anzeigen des Ergebnisses
print(missing_values_test)



# Entfernen aller Zeilen mit mindestens einem NA aus train_data
train_data_combined <- na.omit(train_data_combined)

# Entfernen aller Zeilen mit mindestens einem NA aus validation_data
validation_data_combined <- na.omit(validation_data_combined)

# Entfernen aller Zeilen mit mindestens einem NA aus test_data
test_data_combined <- na.omit(test_data_combined)


###################################################
### Data Preparation for Tensor Flow ####


# Umwandlung der kategorialen Variablen in Faktoren

# Train_Data

train_data_combined$Warengruppe <- as.factor(train_data_combined$Warengruppe)
train_data_combined$KielerWoche <- as.factor(train_data_combined$KielerWoche)
train_data_combined$IsFerien <- as.factor(train_data_combined$IsFerien)
train_data_combined$IsFeiertag <- as.factor(train_data_combined$IsFeiertag)
train_data_combined$Wochentag <- as.factor(train_data_combined$Wochentag)

#Validation_Data

validation_data_combined$Warengruppe <- as.factor(validation_data_combined$Warengruppe)
validation_data_combined$KielerWoche <- as.factor(validation_data_combined$KielerWoche)
validation_data_combined$IsFerien <- as.factor(validation_data_combined$IsFerien)
validation_data_combined$IsFeiertag <- as.factor(validation_data_combined$IsFeiertag)
validation_data_combined$Wochentag <- as.factor(validation_data_combined$Wochentag)


#Test_Data

test_data_combined$Warengruppe <- as.factor(test_data_combined$Warengruppe)
test_data_combined$KielerWoche <- as.factor(test_data_combined$KielerWoche)
test_data_combined$IsFerien <- as.factor(test_data_combined$IsFerien)
test_data_combined$IsFeiertag <- as.factor(test_data_combined$IsFeiertag)
test_data_combined$Wochentag <- as.factor(test_data_combined$Wochentag)

######### LINEARES MODEL  #########

#Modelgleichung ohne One-Hot

#Ohne Wettercode, da es Probleme mit den Stufen gibt

baseline_model <- lm(Umsatz ~ Warengruppe + KielerWoche + Temperatur
                     + Windgeschwindigkeit + IsFerien + IsFeiertag +
                       Wochentag, data = train_data_combined)

summary(baseline_model)


# Evaluation with validation

# Make predictions on the validation dataset
validation_predictions <- predict(baseline_model, newdata = validation_data_combined)

# MAE
mae <- mae(validation_data_combined$Umsatz, validation_predictions)

# MSE
mse <- mse(validation_data_combined$Umsatz, validation_predictions)

# RMSE
rmse <- sqrt(mse)

# R-squared
r_squared <- cor(validation_data_combined$Umsatz, validation_predictions)^2

# Calculate MAPE
mape <- mean(abs((validation_data_combined$Umsatz - validation_predictions) / validation_data_combined$Umsatz)) * 100


######### MISSING VALUES - Part 2 - Imputation #########

# Hier kÃ¶nnen wir die Imputationsverfahren ausprobieren. DafÃ¼r dÃ¼rfen wir vorher natÃ¼rlich keine NA rauswerfen


######### ONE-HOT-ENCODING #########


#Erstellen der Dummy-Variablen und HinzufÃ¼gen der numerischen Variablen
features_matrix_train <- model.matrix(Umsatz ~ Warengruppe + KielerWoche + Temperatur + Windgeschwindigkeit + IsFerien + IsFeiertag + Wochentag, data=train_data_combined)

#Umwandlung in ein tibble
features_train <- as_tibble(features_matrix_train)

# Entfernt die erste Spalte, die den Intercept darstellt
#features_train <- features_train[, -1]


# Erstellen der Dummy-Variablen und HinzufÃ¼gen der numerischen Variablen fÃ¼r validation_data_combined
features_matrix_validation <- model.matrix(Umsatz ~ Warengruppe + KielerWoche + Temperatur + Windgeschwindigkeit + IsFerien + IsFeiertag + Wochentag, data=validation_data_combined)

# Umwandlung in ein tibble
features_validation <- as_tibble(features_matrix_validation)


# Entfernt die erste Spalte, die den Intercept darstellt
#features_validation <- features_test[, -1]


# Erstellen der Dummy-Variablen und HinzufÃ¼gen der numerischen Variablen fÃ¼r validation_data_combined
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
write.csv(features_train, "features_train_1701.csv", row.names = FALSE)

# Exportieren von label_train als CSV
write.csv(label_train, "label_train_1701.csv", row.names = FALSE)

# Exportieren von label_validation als CSV
write.csv(label_validation, "label_validation_1701.csv", row.names = FALSE)

# Exportieren von features_validation als CSV
write.csv(features_validation, "features_validation_1701.csv", row.names = FALSE)

# Exportieren von features_test als CSV
write.csv(features_test, "features_test_1701.csv", row.names = FALSE)


#Exportieren von test_data_combined fÃ¼r die IDs
write.csv(test_data_combined, "test_data_IDs_1701.csv", row.names = FALSE)

