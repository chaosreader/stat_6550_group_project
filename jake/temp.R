data <- bee.data

x <- base_impute(data, impute_col = "TOTAL_COUNT", group_by = c('MONTH', 'DAY'))$IMPUTED_VALS

x <- diff(x, lag = 96)

ar <- sparse_ar(x)
acf(ar$residuals)


  data <- base_impute(bee.data[1:100], impute_col = "TOTAL_COUNT")
  x <- bee.data$IMPUTED_VALS
  model <- sparse_ma(x)
