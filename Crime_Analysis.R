
# Load Packages ------------------------------------------------------------

library(dplyr)

# Create object country ---------------------------------------------------

country <- c(rep("Nigeria", 3), "Cameroon",
  rep("Nigeria", 3), "Cameroon", 
  rep("Nigeria", 8), "Cameroon", 
  rep("Nigeria", 3))

length(country)
unique(country)

city <- c("Maiduguri", "Unknown", "Kano", "Bamenda",
  "Port Harcourt", "Potiskum", "Damaturu",
  "Unknown", "Jos", "Lagos", "Gombe", "Kaduna",
  "Logo District", "Konduga", "Warri", "Bama",
  "Buea", "Buni Yadi", "Damboa", "Gamboru"
  )

n_crime <- c(488, 225, 130, 87, 76, 65, 63, 55, 51,
  46, 42, 38, 37, 36, 35, 32, 31, 30, 30, 29)


# Data Types and Structures -----------------------------------------------

## Data Type
typeof(country)
typeof(city)
typeof(n_crime)

## Data Structure
str(country)
str(city)
str(n_crime)


# Total Number of Crime Committed ------------------------------------------

sum(n_crime)

crime_tbl <- data.frame(
  country,
  city,
  n_crime
)

crime_tbl$crime_rate <- ifelse(
  test = crime_tbl$n_crime >= 400,
  yes = "High",
  no = ifelse(
    test = between (crime_tbl$n_crime, 100, 399),
    yes = "Medium",
    no = "Low"
  )
)

## Second method
crime_tbl$crime_rate <- if_else(
  condition = crime_tbl$n_crime >= 400,
  true = "High",
  false = if_else(
    condition = between(crime_tbl$n_crime, 100, 399),
    true = "Medium",
    false = "Low"
  )
)

crime_tbl |> 
  summarise(
    .by = country,
    total_crime = sum(n_crime),
    average_number_of_crime = mean(n_crime)
  )


crime_tbl |> 
  summarise(
    .by = country,
    total_crime = sum(n_crime),
  ) |> 
  top_n(n = 1, wt = total_crime)