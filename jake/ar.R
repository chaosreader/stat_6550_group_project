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
ar15 <- arima(count_96 - mean(count_96),
              order = c(15, 0, 0), include.mean = FALSE,
              method = 'CSS-ML')

acf(ar15$residuals, lag.max = 48,
    main = 'Residual ACF of AR(15) Model')

Box.test(ar15$residuals, lag = 36, type = 'Ljung-Box')


ar15.fit <- count_96 - xts(ar15$residuals,
                              order.by = bee.data$DATE[97:nrow(bee.data)])


pred_dates <- bee.data$DATE[n] + seq(15 * 60, 15 * 60 * 25, 15 * 60)
ar15.pred <- predict(ar15, n.ahead = 25, se.fit = TRUE)

# Creating 95% Confidence Interval bounds
ar15.lb <- xts(ar15.pred$pred - 1.96*ar15.pred$se, order.by = pred_dates)
ar15.ub <- xts(ar15.pred$pred + 1.96*ar15.pred$se, order.by = pred_dates)


order_by = append(bee.data$DATE[-(1:96)], pred_dates)

# Fitted and Predicted time series for the plot
preds.plot <- xts(rep(0, length(count_96) + 25),
                  order.by = order_by)

preds.plot[1:(length(ar15.fit))] <- ar15.fit + mean(count_96)

preds.plot[(length(ar15.fit) + 1):(length(ar15.fit) + 25)] <-
  ar15.pred$pred + mean(count_96)


# TRY PLOTTING XTS DIRECTLY

plot(as.zoo(count_96[9500:9798]), col = 'black')
lines(as.zoo(preds.plot[9500:9823]), col = 'red')
lines(as.zoo(ar15.lb), col = 'blue')
lines(as.zoo(ar15.ub), col = 'blue')

