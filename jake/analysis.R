library(xts)
data <- bee.data

original <- bee.data[!is.na(TOTAL_COUNT)]

par(mfrow = c(2, 1))
# Plot First
plot(ts(original$TOTAL_COUNT[1:140]), main = "Original Data (First 140)",
     ylab = 'Count', xlab = "Index")

# Plot Second
plot(ts(data$TOTAL_COUNT[1:200]), main = "Data Accounting for Missing Times",
     ylab = 'Count', xlab = "Index")

dev.off()

# Various types of impuration
zero_imp <- base_impute(data[1:200], type = 'zero', impute_col = 'TOTAL_COUNT')

median_imp <- base_impute(data[1:200], type = 'median', impute_col = 'TOTAL_COUNT',
                          group_by = c('MONTH', 'DAY'))

mean_imp <- base_impute(data[1:200], type = 'mean', impute_col = 'TOTAL_COUNT',
                        group_by = c('MONTH', 'DAY'))


par(mfrow = c(3, 1))


missing_idxs <- which(is.na(data$TOTAL_COUNT[1:200]))

# Plot Third
plot(ts(zero_imp$IMPUTED_VALS))
lines(missing_idxs, zero_imp$IMPUTED_VALS[missing_idxs], col = 'red',
      type = 'p', pch = 20)

plot(ts(mean_imp$IMPUTED_VALS))
lines(missing_idxs, mean_imp$IMPUTED_VALS[missing_idxs], col = 'red',
      type = 'p', pch = 20)

plot(ts(median_imp$IMPUTED_VALS))
lines(missing_idxs, median_imp$IMPUTED_VALS[missing_idxs], col = 'red',
      type = 'p', pch = 20)


data <- base_impute(data, impute_col = 'TOTAL_COUNT',
                    group_by = c('DAY', 'MONTH'))

data[HOUR < 7 & is.na(TOTAL_COUNT), IMPUTED_VALS := 0]


# Plot Fourth
dev.off()
plot(ts(data$IMPUTED_VALS[1:200]), ylab = 'Total Bee Count',
     main = "First 200 Observations with Imputed Values (Red)")
lines(missing_idxs, data$IMPUTED_VALS[missing_idxs], col = 'red',
      type = 'p', pch = 20)



train <- bee.data[1:9658]
test  <- bee.data[1:9659]
