data <- bee.data

head(data, 20)

# data[, COUNT := .N, by = c('MONTH', 'DAY', 'HOUR', 'MINUTE')]
data[, COUNT := .N, by = DATE]
data[COUNT == 2]

data[, TIME_DIFF := DATE - shift(DATE)]


par(mfrow = c(2, 1))
plot(data[MONTH == 10, TOTAL_COUNT])
plot(data[MONTH == 10, IMP])

