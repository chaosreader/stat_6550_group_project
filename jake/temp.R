data <- base_impute(bee.data[1:1000], impute_col = "TOTAL_COUNT")
x <- data$IMPUTED_VALS
model <- sparse_arma(x)



ar <- sparse_ar(x)
acf(ar$residuals)


ma <- sparse_ma(x)
acf(ma$residuals)


arma <- sparse_arma(x)
acf(arma$residuals)

acf(x)
pacf(x)$acf
