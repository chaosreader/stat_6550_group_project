library(data.table)
data <- bee.data

head(data, 20)

# data[, COUNT := .N, by = c('MONTH', 'DAY', 'HOUR', 'MINUTE')]
data[, COUNT := .N, by = DATE]
data[COUNT == 2]

data[, TIME_DIFF := DATE - shift(DATE)]


par(mfrow = c(3, 1))

for (month in 7:9) {
  plot(data[MONTH == month, DATE], data[MONTH == month, IMP])
}


par(mfrow = c(3, 1))


for (month in 7:9) {
  plot(data[MONTH == month, DATE], data[MONTH == month, PRECIP])
}

par(mfrow = c(3, 1))

for (month in 7:9) {
  plot(data[MONTH == month, DATE], data[MONTH == month, WET])
}


par(mfrow = c(3, 1))

for (month in 7:9) {
  plot(data[MONTH == month, DATE], data[MONTH == month, DATE])
}


# plot(data[MONTH == 10, IMP])

data[TOTAL_COUNT == 0, .(.N), by = HOUR] %>%
  .[order(N)]

plot(data$TIME_DIFF)
