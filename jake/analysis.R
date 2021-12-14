library(xts)
data <- bee.data
original <- bee.data[!is.na(TOTAL_COUNT)]


#------Plot Comparisons of Original Data and Data with all Time Intervals------#

par(mfrow = c(2, 1))


plot(ts(original$TOTAL_COUNT[1:140]), main = "Original Data (First 140)",
     ylab = 'Count',
     xlab = 'Time (15 Min. Intervals)')


plot(ts(data$TOTAL_COUNT[1:200]), main = "Data Accounting for Missing Times",
     ylab = 'Count',
     xlab = 'Time (15 Min. Intervals)')


dev.off()

#----------------------General Plots about the Data----------------------------#

hist(data[!is.na(TOTAL_COUNT), TOTAL_COUNT], breaks = seq(0, 13500, 500),
     main = 'Distribution of Bee Counts', xlab = 'Bee Count')







#-------------------Various Comparisons of Imputation--------------------------#

zero_imp <- base_impute(data[1:200], type = 'zero', impute_col = 'TOTAL_COUNT')

median_imp <- base_impute(data[1:200], type = 'median',
                          impute_col = 'TOTAL_COUNT',
                          group_by = c('MONTH', 'DAY'))

mean_imp <- base_impute(data[1:200], type = 'mean', impute_col = 'TOTAL_COUNT',
                        group_by = c('MONTH', 'DAY'))


#--------------------Plots Comparing the Imputations---------------------------#
missing_idxs <- which(is.na(data$TOTAL_COUNT[1:200]))

par(mfrow = c(3, 1))


plot(ts(zero_imp$IMPUTED_VALS), main = 'Zero Imputed Values',
     ylab = 'Total Bee Count', xlab = '15 Min. Time Intervals')
lines(missing_idxs, zero_imp$IMPUTED_VALS[missing_idxs], col = 'red',
      type = 'p', pch = 20)


plot(ts(mean_imp$IMPUTED_VALS), main = 'Mean Imputed Values',
     ylab = 'Total Bee Count', xlab = '15 Min. Time Intervals')
lines(missing_idxs, mean_imp$IMPUTED_VALS[missing_idxs], col = 'red',
      type = 'p', pch = 20)


plot(ts(median_imp$IMPUTED_VALS), main = 'Median Imputed Values',
     ylab = 'Total Bee Count', xlab = '15 Min. Time Intervals')
lines(missing_idxs, median_imp$IMPUTED_VALS[missing_idxs], col = 'red',
      type = 'p', pch = 20)


dev.off()

#------------------Final Data Imputation for Analysis--------------------------#

data <- base_impute(data, impute_col = 'TOTAL_COUNT',
                    group_by = c('DAY', 'MONTH'))

data[HOUR < 7 & is.na(TOTAL_COUNT), IMPUTED_VALS := 0]


plot(ts(data$IMPUTED_VALS[1:200]), ylab = 'Total Bee Count',
     main = "First 200 Observations with Imputed Values (Red)",
     xlab = 'Time (15 Min. Intervals)')
lines(missing_idxs, data$IMPUTED_VALS[missing_idxs], col = 'red',
      type = 'p', pch = 20)


# Plots involving Weather Correlation
wind_cor <- cor(data[!is.na(WIND), WIND], data[!is.na(WIND), IMPUTED_VALS],
                method = 'pearson')

plot(data[!is.na(WIND), WIND], data[!is.na(WIND), IMPUTED_VALS], pch = 19,
     main = paste('Bee Count vs. Wind Speed (Corr:', round(wind_cor, 3), ')'),
     xlab = 'Average Wind Speed (within the Hour) (m/s)',
     ylab = 'Total Bees')


temp_cor <- cor(data[!is.na(TEMP), TEMP], data[!is.na(TEMP), IMPUTED_VALS],
                method = 'pearson')

plot(data[!is.na(TEMP), TEMP], data[!is.na(TEMP), IMPUTED_VALS], pch = 19,
     main = paste('Bee Count vs. Temperature (Corr:', round(temp_cor, 3), ')'),
     xlab = 'Average Temperature (within the Hour) (F)',
     ylab = 'Total Bees')

# The training set is June - September
train <- data[1:9658]
train_count <- ts(train$IMPUTED_VALS)
train_count_96 <- diff(train_count, lag = 96)

# The test set is the first three days of October
test  <- data[9659:9894]
test_count <- ts(test$IMPUTED_VALS)
test_count_96 <- diff(test_count, lag = 96)


par(mfrow = c(2, 1))
plot(ts(train_count[1:600]), main = 'Original Scale',
     xlab = 'Time (15 Min. Intervals)',
     ylab = 'Bee Count')

plot(ts(train_count_96[1:600]), main = 'Lag 96 Difference',
     xlab = 'Time (15 Min. Intervals)',
     ylab = 'Differenced Bee Count')
#------------------------------------------------------------------------------#


# p-val: 1
sps_ma   <- sparse_ma(train_count_96)

# p-val: 0.9998
sps_ar   <- sparse_ar(train_count_96)

# p-val: 0.6397
sps_arma <- sparse_arma(train_count_96)

par(mfrow = c(2, 1))
# Fit the MA model and make future predictions
sps_ma_fit  <- ts(train_count_96 - sps_ma$residuals)
sps_ma_pred <- predict(sps_ma, n.ahead = 30, se.fit = TRUE)
sps_ma_lb   <- ts(sps_ma_pred$pred - 1.96*sps_ma_pred$se, start = 9693)
sps_ma_ub   <- ts(sps_ma_pred$pred + 1.96*sps_ma_pred$se, start = 9693)

plot_vals <- ts(rep(0, 191), start = 9532)
plot_vals[1:161] <- sps_ma_fit[9402:9562] + mean(train_count_96)
plot_vals[162:191] <- sps_ma_pred$pred + mean(train_count_96)

ts.plot(ts(train_count_96[9402:9562], start = 9532), plot_vals,
        sps_ma_lb, sps_ma_ub,
        gpars = list(col = c('black', 'red', 'blue', 'blue'),
                     main = 'Sarse MA(25) Predicted Values (Red) with 95% CI'))


# Fit the AR model and make future
sps_ar_fit  <- ts(train_count_96 - sps_ar$residuals)
sps_ar_pred <- predict(sps_ar, n.ahead = 30, se.fit = TRUE)
sps_ar_lb   <- ts(sps_ar_pred$pred - 1.96*sps_ar_pred$se, start = 9693)
sps_ar_ub   <- ts(sps_ar_pred$pred + 1.96*sps_ar_pred$se, start = 9693)

plot_vals_ar <- ts(rep(0, 191), start = 9532)
plot_vals_ar[1:161] <- sps_ar_fit[9402:9562] + mean(train_count_96)
plot_vals_ar[162:191] <- sps_ar_pred$pred + mean(train_count_96)

ts.plot(ts(train_count_96[9402:9562], start = 9532), plot_vals,
        sps_ar_lb, sps_ar_ub,
        gpars = list(col = c('black', 'red', 'blue', 'blue'),
                     main = 'Sarse AR(15) Predicted Values (Red) with 95% CI'))

dev.off()
# Plot MA on original scale
ma_orig <- diffinv(plot_vals, lag = 96, differences = 1,
                   xi = train_count[9402:9497])


ts.plot(ts(train_count[9402:9562], start = 9532), ma_orig,
        gpars = list(col = c('black', 'red'),
                     main = "MA(25) Model on Original Count Scale",
                     xlab = 'Time (15 Min. Intervals)'))


ts.plot(ts(train_count[9402:9562], start = 9532), ma_orig,
        gpars = list(col = c('black', 'red'),
                     main = "MA(25) Model on Original Count Scale (Tail End)",
                     xlab = 'Time (15 Min. Intervals)',
                     xlim = c(9630, 9680)))

# Calculate MSE
sum((sps_ma_pred$pred - test_count_96[1:30])^2) / 30
