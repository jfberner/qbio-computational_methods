# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Sara's Last class - Time Series
# First version 2022-07-22
# jfb
# --------------------------------------------------#

##### Libs #####
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
##### Time Series in R #####
covid <- read.csv("data/raw/covid19-dd7bc8e57412439098d9b25129ae6f35.csv")

head(covid)

# First checking the class
class(covid$date)

# Changing to date format
covid$date <- as_date(covid$date)
# Checking the class
class(covid$date)

# Now we can make numeric operations
range(covid$date)

# Plotting a time-series with ggplot2
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal()

# Oops. We have negative cases and will substitute the negative values per zero.
covid$new_confirmed[covid$new_confirmed < 0] <- 0
# and plot again

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  labs(x = "Date", y = "New cases")

# Easy Peasy
