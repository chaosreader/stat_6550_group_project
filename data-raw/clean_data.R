# Infor about weather variables:
# https://climate.usu.edu/mchd/dashboard/overview/USUwx.php

library(anytime)
library(data.table)
library(lubridate)


july.bee <- data.table::fread('data-raw/R_4_5_18July2020.csv', sep = ',')
oct.bee  <- data.table::fread('data-raw/R_4_5_03Oct2020.csv', sep = ',')
aug.bee  <- data.table::fread('data-raw/R_4_5_24Aug2020.csv', sep = ',')

combined.bee <- rbind(july.bee, aug.bee)
combined.bee <- rbind(combined.bee, oct.bee)


# Create different time measurements; prep for table merge
combined.bee[, DATE := anytime(gsub('.*2020', '2020', VIDEO))]
combined.bee[, MONTH := lubridate::month(DATE)]
combined.bee[, HOUR := hour(DATE)]
combined.bee[, DAY := day(DATE)]
combined.bee <- combined.bee[order(DATE)]

# Only need some of the original columns
combined.bee <- combined.bee[, .(DATE, MONTH, DAY, HOUR, TOTAL_COUNT, UPWARD,
                                 DOWNWARD, LATERAL, WINDOW, OVERLAP)]

# Import and and date info for weather data
weather  <- data.table::fread('data-raw/hourly_weather.csv')
weather[, HOUR := hour(date_time)]
weather[, MONTH := month(date_time)]
weather[, DAY := day(date_time)]

# Set the keys in order to merge the two tables
setkey(combined.bee, MONTH, DAY, HOUR)
setkey(weather, MONTH, DAY, HOUR)

bee.data <- combined.bee[weather[, .(TEMP = airt_avg, WIND = winds_avg,
                                     WET = wet_pct, PRECIP = precip,
                                     HOUR, DAY, MONTH)], nomatch = 0]

usethis::use_data(bee.data, overwrite = TRUE)

