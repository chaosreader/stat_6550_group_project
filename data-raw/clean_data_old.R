# Infor about weather variables:
# https://climate.usu.edu/mchd/dashboard/overview/USUwx.php

library(anytime)
library(data.table)
library(lubridate)


july.bee <- data.table::fread("data-raw/R_4_5_18July2020.csv", sep = ",")
oct.bee <- data.table::fread("data-raw/R_4_5_03Oct2020.csv", sep = ",")
aug.bee <- data.table::fread("data-raw/R_4_5_24Aug2020.csv", sep = ",")

combined.bee <- rbind(july.bee, aug.bee)
combined.bee <- rbind(combined.bee, oct.bee)


# Create different time measurements; prep for table merge
combined.bee[, DATE := floor_date(anytime(gsub(".*2020", "2020", VIDEO)), "15 minutes")]
# combined.bee[, DATE := anytime(gsub('.*2020', '2020', VIDEO))]
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
weather[, DATE := round_date(date_time, "15 minutes")]
weather[, HOUR := hour(DATE)]
weather[, MONTH := month(DATE)]
weather[, DAY := day(DATE)]
weather[, MINUTE := minute(DATE)]


# Add all_times date-times to the table
first_date <- combined.bee[1]$DATE
last_date <- combined.bee[nrow(combined.bee)]$DATE

all_times <- data.table("DATE" = seq(
  from = first_date, to = last_date,
  by = "15 mins"
))

all_times[, MONTH := month(DATE)]
all_times[, DAY := day(DATE)]
all_times[, MINUTE := minute(DATE)]
all_times[, HOUR := hour(DATE)]

# Set the keys in order to merge the two tables
setkey(combined.bee, MONTH, DAY, HOUR, MINUTE)
setkey(weather, MONTH, DAY, HOUR)
setkey(all_times, MONTH, DAY, HOUR, MINUTE)

bee.data <- combined.bee[weather[, .(
  TEMP = airt_avg, WIND = winds_avg,
  WET = wet_pct, PRECIP = precip,
  HOUR, DAY, MONTH
)], nomatch = 0]


bee.data <- bee.data[all_times, ]
bee.data[, DATE := round_date(i.DATE, "15 minutes")]
bee.data[, i.DATE := NULL]


bee.data <- bee.data[-c(6046, 6048)]
bee.data[, IMP := ifelse(!is.na(TOTAL_COUNT), TOTAL_COUNT,
  median(TOTAL_COUNT, na.rm = TRUE)
),
by = c("MONTH", "DAY")
]



usethis::use_data(bee.data, overwrite = TRUE)
