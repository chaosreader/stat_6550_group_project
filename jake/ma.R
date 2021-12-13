library(xts)
library(lubridate)

n <- nrow(bee.data)

avg_temp <- xts(bee.data$TEMP, order.by = bee.data$DATE)
# avg_temp <- ts(bee.data$TEMP)

plot(avg_temp,
     main = 'Average Monthly Temperature (F)',
     ylab = 'Degrees F',
     xlab = 'Date')


count <- xts(bee.data$IMP, order.by = bee.data$DATE)
# count <- ts(bee.data$IMP)

plot(count,
     main = 'Count of Bees',
     ylab = 'Count',
     xlab = 'Date')


plot(count,
     main = 'Count of Bees',
     ylab = 'Count',
     xlab = 'Date',
     xlim = c(bee.data$DATE[1], bee.data$DATE[200]))

count_96 <- diff(count, lag = 96, na.pad = FALSE)

plot(count_96,
     main = 'Count of Bees',
     ylab = 'Count',
     xlab = 'Date',
     xlim = c(0, 1000))

# To determine the MA model
acf(count_96)
pacf(count_96)

# Fill MA(25) model
ma25 <- arima(count_96 - mean(count_96),
              order = c(0, 0, 25), include.mean = FALSE,
              method = 'CSS-ML')

acf(ma25$residuals, lag.max = 48,
    main = 'Residual ACF of MA(25) Model')

Box.test(ma25$residuals, lag = 36, type = 'Ljung-Box')



# Sparse MA Based off Coefs: 1 - 12, 14, 15, 20, 25
sps.ma25 <- arima(count_96 - mean(count_96), order = c(0, 0, 25), include.mean = FALSE,
                  fixed = c(rep(NA, 12), 0, NA, NA, 0, 0, 0, 0, NA,
                            rep(0, 4), NA),
                  transform.pars = FALSE, method = 'CSS-ML')

acf(sps.ma25$residuals, lag.max = 48,
    main = 'Residual ACF of Sparse MA(25) Model')

Box.test(sps.ma25$residuals, lag = 36, type = 'Ljung-Box')

# Make Fit and Predicted Values

sps.ma.fit  <- count_96 - xts(sps.ma25$residuals,
                              order.by = bee.data$DATE[97:nrow(bee.data)])


pred_dates <- bee.data$DATE[n] + seq(15 * 60, 15 * 60 * 25, 15 * 60)
sps.ma.pred <- predict(sps.ma25, n.ahead = 25, se.fit = TRUE)

# Creating 95% Confidence Interval bounds
sps.ma.lb <- xts(sps.ma.pred$pred - 1.96*sps.ma.pred$se, order.by = pred_dates)
sps.ma.ub <- xts(sps.ma.pred$pred + 1.96*sps.ma.pred$se, order.by = pred_dates)


order_by = append(bee.data$DATE[-(1:96)], pred_dates)

# Fitted and Predicted time series for the plot
preds.plot <- xts(rep(0, length(count_96) + 25),
                  order.by = order_by)

preds.plot[1:(length(sps.ma.fit))] <- sps.ma.fit + mean(count_96)

preds.plot[(length(sps.ma.fit) + 1):(length(sps.ma.fit) + 25)] <-
  sps.ma.pred$pred + mean(count_96)


# TODO: THE BELOW PLOTS DO NOT ACCOUNT FOR DATE (USING TS, NOT XTS)
# Plotting the predictions and fitted values
ts.plot(ts(count_96), ts(preds.plot), ts(sps.ma.lb), ts(sps.ma.ub),
        gpars = list(col = c('black', 'red', 'blue', 'blue')))


# Plotting the predictions and fitted values
ts.plot(ts(count_96), ts(preds.plot), ts(sps.ma.lb), ts(sps.ma.ub),
        gpars = list(col = c('black', 'red', 'blue', 'blue')),
        xlim = c(n - 96 - 200, n - 96  + 25))

# Plotting the predictions and fitted values
ts.plot(ts(count_96), ts(preds.plot), ts(sps.ma.lb), ts(sps.ma.ub),
        gpars = list(col = c('black', 'red', 'blue', 'blue')),
        xlim = c(0, 200))


# TRY PLOTTING XTS DIRECTLY

plot(as.zoo(count_96[9500:9798]), col = 'black')
lines(as.zoo(preds.plot[9500:9823]), col = 'red')
lines(as.zoo(sps.ma.lb), col = 'blue')
lines(as.zoo(sps.ma.ub), col = 'blue')

