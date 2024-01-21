# Create list with needed libraries
pkgs <- c("purrr","DataExplorer","caret","Metrics","readr", "dplyr", "reticulate", "ggplot2", "Metrics", "lubridate", "tidyverse", "VIM","broom", "RColorBrewer")

# Load each listed library and check if it is installed and install if necessary
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

setwd("C:/Users/sunpn1013/Desktop/Neuer Ordner")

######### Data Import #########

# Read relevant data
sales_data <- read_csv("train.csv")
kiwo_data <- read.csv("kiwo.csv")
ferien_data <- read_csv("ferien.csv")
weather_data <- read_csv("wetter.csv")

# Convert "Datum" column to Date format in kiwo_data

kiwo_data$Datum <- as.Date(kiwo_data$Datum, format = "%Y-%m-%d")



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



######### Add Feature Variables  ######### 

# General processing for the whole sales_data
combined_data <- sales_data %>%
  left_join(kiwo_data, by = "Datum") %>%
  left_join(weather_data, by = "Datum") %>%
  left_join(ferien_data, by = "Datum") %>%
  mutate(
    IsFeiertag = ymd(Datum) %in% schleswig_holstein_feiertage,
    Wochentag = weekdays(Datum, abbreviate = FALSE),
    KielerWoche = if_else(is.na(KielerWoche), FALSE, KielerWoche == 1)
  )

# Mapping of Warengruppe to product name. 1=Brot; 2=Broetchen; 3=Croissant;4=Konditorei;5=Kuchen;6=Saisonbrot.
combined_data <- combined_data %>%
  mutate(Produktname = case_when(Warengruppe == 1 ~ "Brot",
                                 Warengruppe == 2 ~ "Broetchen",
                                 Warengruppe == 3 ~ "Croissant",
                                 Warengruppe == 4 ~ "Konditorei",
                                 Warengruppe == 5 ~ "Kuchen",
                                 Warengruppe == 6 ~ "Saisonbrot"))

# Sortiert den Datensatz nach dem Datum aufsteigend
combined_data <- combined_data %>% arrange(Datum)






######### Abbildungen  #########

ggplot(combined_data, aes(x = IsFerien, y = Umsatz, fill = IsFerien)) +
  geom_boxplot() +
  labs(title = "Umsatz während Ferien und Nicht-Ferien", x = "IsFerien", y = "Umsatz") +
  theme_minimal()


#Zusammenhang Regen und Umsatz
# Erstelle einen Boxplot
#ggplot(combined_data, aes(x = Regen, y = Umsatz, fill = IsRegen)) +
# geom_boxplot() +
# labs(title = "Umsatz beinflusst durch Regen?", x = "Regen", y = "Umsatz") +
#theme_minimal()


#Mittelwert von Ferientag vs. Nicht-Ferientag
Durchschnitt_Wochentag <- combined_data %>%
  group_by(Wochentag) %>%
  summarize(Durchschnittsumsatz = mean(Umsatz))

Konfidenz_Wochentag <- combined_data %>%
  group_by(Wochentag) %>%
  do(tidy(t.test(.$Umsatz)))

Zusammengefügte_Ergebnisse <- merge(Durchschnitt_Wochentag, Konfidenz_Wochentag, by= "Wochentag")



bar_colors <- c("#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe", "#008080", "#e6beff", "#9a6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075", "#808080", "#ffffff", "#000000")


# Plotten der Daten: Abhängigkeit der Variablen von Umsatz
ggplot(Zusammengefügte_Ergebnisse) +
  geom_bar(aes(x = Wochentag, y = Durchschnittsumsatz, fill = Wochentag), stat = "identity") +
  labs(title = "Mittlere Umsätze der Wochentage", x = "Ferien", y = "Umsatz") +
  theme_dark() +  # Ändere das Theme zu einem dunklen Hintergrund
  scale_fill_manual(values = bar_colors) +  # Verwende die definierte Farbpalette
  geom_errorbar(aes(x = Wochentag, ymin = conf.low, ymax = conf.high), size = 1.0, width = 0.5, colour = "white", alpha = 1) +
  theme(
    panel.background = element_rect(fill = "grey"),  # Hintergrundfarbe des Plots
    panel.grid.major = element_blank(),  # Kein Gitter
    panel.grid.minor = element_blank(),  # Kein Gitter
    axis.text = element_text(color = "black"),  # Textfarbe der Achsenbeschriftungen
    axis.title = element_text(color = "black"),  # Textfarbe der Achsentitel
    legend.text = element_text(color = "black"),  # Textfarbe der Legende
    legend.title = element_text(color = "black")  # Textfarbe des Legendentitels
  )


######### MISSING VALUES  #########

#Darstellung der Missing Values

create_report(combined_data)


# Untersuchen von Missing Values in jeder Spalte von train_data_combined
missing_values_combined <- combined_data %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Anzeigen des Ergebnisses
print(missing_values_combined)


###### KNN für Temperatur ########

# Funktion für KNN-basierte Imputation
impute_knn <- function(data, column_name) {
  # Neue Spalte für die imputierten Werte erstellen
  data[[paste0(column_name, "_Imputated")]] <- data[[column_name]]
  
  for (i in 1:nrow(data)) {
    if (is.na(data[[paste0(column_name, "_Imputated")]][i])) {
      # Bereiche für vorherige und nachfolgende Werte definieren
      range_start <- max(1, i - 5)
      range_end <- min(nrow(data), i + 5)
      
      # Werte vor und nach dem NA-Wert extrahieren
      values <- data[[paste0(column_name, "_Imputated")]][range_start:range_end]
      
      # NA-Werte aus diesen Werten ausschließen
      values <- values[!is.na(values)]
      
      # Wenn es ausreichend viele Nicht-NA-Werte gibt, ersetzen
      if (length(values) > 0) {
        data[[paste0(column_name, "_Imputated")]][i] <- mean(values, na.rm = TRUE)
      }
    }
  }
  return(data)
}

# Anwendung der Funktion auf den Datensatz combined_data
combined_data <- impute_knn(combined_data, "Temperatur")


# Untersuchen von Missing Values in jeder Spalte von train_data_combined
missing_values_combined <- combined_data %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Anzeigen des Ergebnisses
print(missing_values_combined)


##### Imputation Bewoelkung #####

impute_with_previous <- function(data, column_name) {
  # Neue Spalte für imputierte Werte erstellen
  data[[paste0(column_name, "_Imputated")]] <- data[[column_name]]
  
  # Iterieren über alle Zeilen, beginnend mit der zweiten Zeile
  for (i in 2:nrow(data)) {
    # Überprüfen, ob der aktuelle Wert NA ist
    if (is.na(data[[paste0(column_name, "_Imputated")]][i])) {
      # Ersetzen des NA-Wertes mit dem Wert der vorherigen Zeile
      data[[paste0(column_name, "_Imputated")]][i] <- data[[paste0(column_name, "_Imputated")]][i-1]
    }
  }
  
  return(data)
}

# Anwenden der Funktion auf Ihren Datensatz
combined_data <- impute_with_previous(combined_data, "Bewoelkung")

# Untersuchen von Missing Values in jeder Spalte von train_data_combined
missing_values_combined <- combined_data %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Anzeigen des Ergebnisses
print(missing_values_combined)


##### Imputation Windgeschwindigkeit #####

# Anwenden der Funktion auf die Spalte "Windgeschwindigkeit" im Datensatz
combined_data <- impute_with_previous(combined_data, "Windgeschwindigkeit")



##### Imputation Wettercode #####

# Applying the function to the "Wettercode" column in your dataset
combined_data <- impute_with_previous(combined_data, "Wettercode")

# Untersuchen von Missing Values in jeder Spalte von train_data_combined
missing_values_combined <- combined_data %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Anzeigen des Ergebnisses
print(missing_values_combined)

# Jetzt kann die Variable Regen erstellt werden

# Create Regenvariable
# Funktion, um zu prüfen, ob ein Wettercode Regen darstellt
funktion_regen <- function(code) {
return(code >= 50 & code <= 69 | code >= 80 & code <= 82 | code >= 91 & code <= 92 | code %in% c(95, 97))
}

combined_data <- combined_data %>%
 mutate(Regen = sapply(Wettercode_Imputated, funktion_regen))



#combined_data <- combined_data %>% mutate(Bewoelkung = replace_na(Bewoelkung, 0))

#combined_data <- combined_data %>%
# mutate(Windgeschwindigkeit = if_else(is.na(Windgeschwindigkeit), 0, Windgeschwindigkeit))

#combined_data <- combined_data %>%
# mutate(Temperatur = if_else(is.na(Temperatur), 0, Temperatur))

#combined_data <- combined_data %>%
# mutate(Wettercode = if_else(is.na(Wettercode), 0, Wettercode))


# Untersuchen von Missing Values in jeder Spalte von train_data_combined
missing_values_combined <- combined_data %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Anzeigen des Ergebnisses
print(missing_values_combined)



# Entferne der Variable "Wettercode" aus dem Datensatz "combined_data" [hat sehr viele NA]
#combined_data <- combined_data %>% select(-Wettercode)

# Entfernen aller Zeilen mit mindestens einem NA aus combined_data
#combined_data <- na.omit(combined_data)

# Untersuchen von Missing Values in jeder Spalte von train_data_combined
#missing_values_combined <- combined_data %>%
 # summarise(across(everything(), ~sum(is.na(.))))

# Anzeigen des Ergebnisses
#print(missing_values_combined)

######### Faktorisierung der Variablen  #########

combined_data <- combined_data %>%
  mutate(
    Warengruppe = as.factor(Warengruppe),
    KielerWoche = as.factor(KielerWoche),
    IsFerien = as.factor(IsFerien),
    IsFeiertag = as.factor(IsFeiertag),
    Wochentag = as.factor(Wochentag),
    Bewoelkung_Imputated = as.factor(Bewoelkung_Imputated),
  )

######### Split Data Into Training and Validation Set #########

# Create Trainingsdatensatz vom 01.07.2013 bis 31.12.2017
train_data <- combined_data %>%
  filter(Datum >= as.Date("2013-07-01") & Datum <= as.Date("2017-12-31"))


# Create Validierungsdatensatz vom 01.01.2018 bis 31.07.2018
validation_data <- combined_data %>%
  filter(Datum >= as.Date("2018-01-01") & Datum <= as.Date("2018-07-31"))


######### LINEARES MODEL  #########

#Modelgleichung ohne One-Hot

#Ohne Wettercode, da es Probleme mit den Stufen gibt

baseline_model <- lm(Umsatz ~ Warengruppe + KielerWoche  
                       + IsFerien + IsFeiertag + 
                       Wochentag + Temperatur_Imputated + Bewoelkung_Imputated + 
                       Windgeschwindigkeit_Imputated + Regen , data = train_data)

summary(baseline_model)


# Evaluation with validation

# Make predictions on the validation dataset
validation_predictions <- predict(baseline_model, newdata = validation_data)

# MAE
mae <- mae(validation_data$Umsatz, validation_predictions)

# MSE
mse <- mse(validation_data$Umsatz, validation_predictions)

# RMSE
rmse <- sqrt(mse)

# R-squared
r_squared <- cor(validation_data$Umsatz, validation_predictions)^2

# Calculate MAPE
mape <- mean(abs((validation_data$Umsatz - validation_predictions) / validation_data$Umsatz)) * 100


######### AUFBEREITUNG FUER TENSOR FLOW  #########

######### ONE-HOT-ENCODING #########


#Erstellen der Dummy-Variablen und HinzufÃ¼gen der numerischen Variablen
features_matrix_train <- model.matrix(~Warengruppe + KielerWoche  
                                      + IsFerien + IsFeiertag + 
                                        Wochentag + Temperatur_Imputated + Bewoelkung_Imputated + 
                                        Windgeschwindigkeit_Imputated + Regen, data=train_data)

#Umwandlung in ein tibble
features_train <- as_tibble(features_matrix_train)



#Erstellen der Dummy-Variablen und HinzufÃ¼gen der numerischen Variablen
features_matrix_validation <- model.matrix(~ Warengruppe + KielerWoche  
                                           + IsFerien + IsFeiertag + 
                                             Wochentag + Temperatur_Imputated + Bewoelkung_Imputated + 
                                             Windgeschwindigkeit_Imputated + Regen, data=validation_data)

#Umwandlung in ein tibble
features_validation <- as_tibble(features_matrix_validation)


#label für train_data
label_train <- train_data %>%
  select(Umsatz) %>%
  rename(label = Umsatz) %>%
  as_tibble()


#label für validation_data
label_validation <- validation_data %>%
  select(Umsatz) %>%
  rename(label = Umsatz) %>%
  as_tibble()



# Check the dimensions of the dataframes
cat("Training features dimensions:", dim(features_train), "\n")
cat("Training labels dimensions:", dim(label_train), "\n")
cat("Validation labels dimensions:", dim(label_validation), "\n")
cat("Validation features dimensions:", dim(features_validation), "\n")



######### Export Data Sets for Tensor-Flow #########

# Exportieren von features_train als CSV
write.csv(features_train, "features_train_regen.csv", row.names = FALSE)

# Exportieren von label_train als CSV
write.csv(label_train, "label_train_regen.csv", row.names = FALSE)

# Exportieren von label_validation als CSV
write.csv(label_validation, "label_validation_regen.csv", row.names = FALSE)

# Exportieren von features_validation als CSV
write.csv(features_validation, "features_validation_regen.csv", row.names = FALSE)


######################################## Data Prep for test_data #######################################

test_data <- read_csv("test.csv")

# General processing for the whole sales_data
test_data <- test_data %>%
  left_join(kiwo_data, by = "Datum") %>%
  left_join(weather_data, by = "Datum") %>%
  left_join(ferien_data, by = "Datum") %>%
  mutate(
    IsFeiertag = ymd(Datum) %in% schleswig_holstein_feiertage,
    Wochentag = weekdays(Datum, abbreviate = FALSE),
    KielerWoche = if_else(is.na(KielerWoche), FALSE, KielerWoche == 1)
  )

# Mapping of Warengruppe to product name. 1=Brot; 2=Broetchen; 3=Croissant;4=Konditorei;5=Kuchen;6=Saisonbrot.
test_data <- test_data %>%
  mutate(Produktname = case_when(Warengruppe == 1 ~ "Brot",
                                 Warengruppe == 2 ~ "Broetchen",
                                 Warengruppe == 3 ~ "Croissant",
                                 Warengruppe == 4 ~ "Konditorei",
                                 Warengruppe == 5 ~ "Kuchen",
                                 Warengruppe == 6 ~ "Saisonbrot"))


#Darstellung der Missing Values

###### KNN für Temperatur ########

# Funktion für KNN-basierte Imputation
impute_knn <- function(data, column_name) {
  # Neue Spalte für die imputierten Werte erstellen
  data[[paste0(column_name, "_Imputated")]] <- data[[column_name]]
  
  for (i in 1:nrow(data)) {
    if (is.na(data[[paste0(column_name, "_Imputated")]][i])) {
      # Bereiche für vorherige und nachfolgende Werte definieren
      range_start <- max(1, i - 5)
      range_end <- min(nrow(data), i + 5)
      
      # Werte vor und nach dem NA-Wert extrahieren
      values <- data[[paste0(column_name, "_Imputated")]][range_start:range_end]
      
      # NA-Werte aus diesen Werten ausschließen
      values <- values[!is.na(values)]
      
      # Wenn es ausreichend viele Nicht-NA-Werte gibt, ersetzen
      if (length(values) > 0) {
        data[[paste0(column_name, "_Imputated")]][i] <- mean(values, na.rm = TRUE)
      }
    }
  }
  return(data)
}

# Anwendung der Funktion auf den Datensatz combined_data
test_data <- impute_knn(test_data, "Temperatur")


##### Imputation Bewoelkung #####

impute_with_previous <- function(data, column_name) {
  # Neue Spalte für imputierte Werte erstellen
  data[[paste0(column_name, "_Imputated")]] <- data[[column_name]]
  
  # Iterieren über alle Zeilen, beginnend mit der zweiten Zeile
  for (i in 2:nrow(data)) {
    # Überprüfen, ob der aktuelle Wert NA ist
    if (is.na(data[[paste0(column_name, "_Imputated")]][i])) {
      # Ersetzen des NA-Wertes mit dem Wert der vorherigen Zeile
      data[[paste0(column_name, "_Imputated")]][i] <- data[[paste0(column_name, "_Imputated")]][i-1]
    }
  }
  
  return(data)
}

# Anwenden der Funktion auf Ihren Datensatz
test_data <- impute_with_previous(test_data, "Bewoelkung")

##### Imputation Windgeschwindigkeit #####

# Anwenden der Funktion auf die Spalte "Windgeschwindigkeit" im Datensatz
test_data <- impute_with_previous(test_data, "Windgeschwindigkeit")

# Anwenden der Funktion auf die Spalte "Wettercode" im Datensatz
test_data <- impute_with_previous(test_data, "Wettercode")

#Regen-Variable hinzufügen
test_data <- test_data %>%
  mutate(Regen = sapply(Wettercode_Imputated, funktion_regen))


######### Faktorisierung der Variablen  #########

test_data <- test_data %>%
  mutate(
    Warengruppe = as.factor(Warengruppe),
    KielerWoche = as.factor(KielerWoche),
    IsFerien = as.factor(IsFerien),
    IsFeiertag = as.factor(IsFeiertag),
    Wochentag = as.factor(Wochentag),
    Bewoelkung_Imputated = as.factor(Bewoelkung_Imputated)
  )


#Erstellen der Dummy-Variablen und HinzufÃ¼gen der numerischen Variablen
features_matrix_test <- model.matrix(~ Warengruppe + KielerWoche  
                                     + IsFerien + IsFeiertag + 
                                       Wochentag + Temperatur_Imputated + Bewoelkung_Imputated + 
                                       Windgeschwindigkeit_Imputated + Regen, data=test_data)


#Umwandlung in ein tibble
features_test <- as_tibble(features_matrix_test)



cat("Test features dimensions:", dim(features_test), "\n")

# Check the dimensions of the dataframes
cat("Training features dimensions:", dim(features_train), "\n")
cat("Training labels dimensions:", dim(label_train), "\n")
cat("Validation labels dimensions:", dim(label_validation), "\n")
cat("Validation features dimensions:", dim(features_validation), "\n")


#Exportieren von features_test als CSV
write.csv(features_test, "features_test_regen.csv", row.names = FALSE)


#Exportieren von test_data_combined fÃ¼r die IDs
write.csv(test_data, "test_data_IDs_regen.csv", row.names = FALSE)

getwd()


# Mape für einzelne Warengruppen

validation_predictions <- read_csv("validation_predictions.csv")

print(validation_predictions)

validation_data$Prediction <- validation_predictions$Prediction

print(validation_data)

summary(validation_data)

# Berechnung des MAPE
calculate_mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual) * 100, na.rm = TRUE)
}

# Anwendung des MAPE auf jedes Produkt
mape_per_product <- validation_data %>%
  group_by(Produktname) %>%
  summarise(MAPE = calculate_mape(Umsatz, Prediction))

print(mape_per_product)
