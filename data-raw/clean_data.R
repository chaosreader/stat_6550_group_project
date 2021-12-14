# Info about weather variables:
# https://climate.usu.edu/mchd/dashboard/overview/USUwx.php

library(anytime)
library(data.table)
library(lubridate)

# Read in all separate raw data files
july.bee <- data.table::fread("data-raw/R_4_5_18July2020.csv", sep = ",")
oct.bee <- data.table::fread("data-raw/R_4_5_03Oct2020.csv", sep = ",")
aug.bee <- data.table::fread("data-raw/R_4_5_24Aug2020.csv", sep = ",")


# Combined all files into a single data.table
combined.bee <- rbind(july.bee, aug.bee)
combined.bee <- rbind(combined.bee, oct.bee)


# Create different time measurements; prep for table merge
combined.bee[, DATE := floor_date(anytime(gsub(".*2020", "2020", VIDEO)), "15 minutes")]
combined.bee[, MONTH := lubridate::month(DATE)]
combined.bee[, DAY := day(DATE)]
combined.bee[, HOUR := hour(DATE)]
combined.bee[, MINUTE := minute(DATE)]
combined.bee <- combined.bee[order(DATE)]


# Only need some of the original columns
combined.bee <- combined.bee[, .(
  DATE, MONTH, DAY, HOUR, MINUTE, TOTAL_COUNT,
  UPWARD, DOWNWARD, LATERAL, WINDOW, OVERLAP
)]


# Import and and date info for weather data
weather <- data.table::fread("data-raw/hourly_weather.csv")
weather$date_time <- anytime(gsub(" UTC", " MDT", weather$date_time))


# Break down the date into distinct parts
weather[, DATE := round_date(date_time, "15 minutes")]
weather[, HOUR := hour(DATE)]
weather[, MONTH := month(DATE)]
weather[, DAY := day(DATE)]
weather[, MINUTE := minute(DATE)]


# Create a data.table for all 15 minute time intervals for an evenly-spaced ts.
first_date <- combined.bee[1]$DATE
last_date <- combined.bee[nrow(combined.bee)]$DATE
all_times <- data.table("DATE" = seq(
  from = first_date, to = last_date,
  by = "15 mins"
))


# Break down the date into distinct parts
all_times[, MONTH := month(DATE)]
all_times[, DAY := day(DATE)]
all_times[, MINUTE := minute(DATE)]
all_times[, HOUR := hour(DATE)]


# Set the keys in order to merge the three tables
setkey(combined.bee, MONTH, DAY, HOUR, MINUTE)
setkey(weather, MONTH, DAY, HOUR)
setkey(all_times, MONTH, DAY, HOUR, MINUTE)


# Merge all time intervals with weather data
all_times <- merge(all_times, weather[, .(
  TEMP = airt_avg, WIND = winds_avg,
  WET = wet_pct, PRECIP = precip,
  HOUR, DAY, MONTH, DATE
)],
all = TRUE
)


# Fix duplicate columns from the merge
all_times[, DATE := ifelse(!is.na(DATE.x), DATE.x, DATE.y)]
all_times$DATE <- anytime(all_times$DATE)
all_times[, MONTH := month(DATE)]
all_times[, DAY := day(DATE)]
all_times[, MINUTE := minute(DATE)]
all_times[, HOUR := hour(DATE)]
all_times[, DATE.x := NULL]
all_times[, DATE.y := NULL]


# Merge bee and time data
bee.data <- merge(combined.bee, all_times, all = TRUE)
bee.data[, DATE := ifelse(!is.na(DATE.y), DATE.y, DATE.x)]
bee.data$DATE <- anytime(bee.data$DATE)
bee.data[, DATE.x := NULL]
bee.data[, DATE.y := NULL]


# Select only desired columns for final results
bee.data <- bee.data[, .(
  DATE, MONTH, DAY, HOUR, MINUTE, TOTAL_COUNT,
  UPWARD, DOWNWARD, LATERAL, WINDOW, OVERLAP,
  TEMP, WIND, WET,
  PRECIP
)]


# Impute the missing TOTAL_COUNT with the median based on MONTH and DAY
bee.data[, IMP := ifelse(!is.na(TOTAL_COUNT), TOTAL_COUNT,
  median(TOTAL_COUNT, na.rm = TRUE)
),
by = c("MONTH", "DAY")
]


# Manual removal of duplucate date/time
bee.data <- bee.data[-c(1:8)]
bee.data <- bee.data[-c(6046, 6048)]

usethis::use_data(bee.data, overwrite = TRUE)
