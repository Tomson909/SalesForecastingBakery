# Load relevant packages

library(styler)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)
library(DataExplorer)

#Get working directory correct
setwd("C:/Users/sunpn1013/Desktop/Data Science Kurs/SalesForecastingBakery/0_DataPreparation")

# Read "umsatzdaten_gekuerzt.csv" into a tibble called "sales_data"
sales_data <- read_csv("umsatzdaten_gekuerzt.csv")


# Add a holiday column to your sales_data
sales_data <- sales_data %>%
  mutate(IsHoliday = ymd(Datum) %in% schleswig_holstein_holidays)

# Read "wetter.csv" into a tibble called "weather_data"
weather_data <- read_csv("wetter.csv")

# Read "kiwo.csv" into a tibble called "kiwo_data"
kiwo_data <- read.csv("kiwo.csv")

# Convert "Datum" column to Date format
kiwo_data$Datum <- as.Date(kiwo_data$Datum, format = "%Y-%m-%d")
weather_data$Datum <- as.Date(weather_data$Datum, format = "%Y-%m-%d")

# Assuming 'Datum' is the common key for all datasets
combined_data <- sales_data %>%
  left_join(kiwo_data, by = "Datum") %>%
  left_join(weather_data, by = "Datum")

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


# Create a vector of public holidays for Schleswig-Holstein (2013-2018), including Dec 24th and Dec 31st
schleswig_holstein_holidays <- as.Date(c(
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
  "2018-05-21", "2018-10-03", "2018-12-25", "2018-12-26", "2018-12-24", "2018-12-31"
))

# # Add a holiday column to combinded_data
combined_data <- combined_data %>%
  mutate(IsHoliday = ymd(Datum) %in% schleswig_holstein_holidays)


# Read a .csv with schulferien

holiday_data <- read_csv(file_path, show_col_types = FALSE) %>%
  as_tibble()

#Rename Variables
holiday_data <- holiday_data %>%
  rename(Datum = Date, IsFerien = IsHoliday)

# Join the holiday_data to combined_data
combined_data <- combined_data %>%
  left_join(holiday_data, by = "Datum")

