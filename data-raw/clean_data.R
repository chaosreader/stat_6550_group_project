
july.bee <- data.table::fread('data-raw/R_4_5_18July2020.csv', sep = ',')
oct.bee  <- data.table::fread('data-raw/R_4_5_03Oct2020.csv', sep = ',')
aug.bee  <- data.table::fread('data-raw/R_4_5_24Aug2020.csv', sep = ',')

weather  <- data.table::fread('data-raw/hourly_weather.csv')
combined.bee <- rbind(july.bee, aug.bee)
combined.bee <- rbind(combined.bee, oct.bee)


combined.bee[, DATE := anytime::anytime(gsub('.*2020', '2020', VIDEO))]
combined.bee[, MONTH := lubridate::month(DATE)]
combined.bee <- combined.bee[order(DATE)]



# usethis::use_data(bee.data, overwrite = TRUE)
