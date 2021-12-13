#'Sparse ARMA Model with Auto-Selected Coefficients
#' @name sparse_arma
#' @param x A time series object
#' @param p_max The maximum number of coefficients for the AR part of the model
#' @param q_max The maximum number of coefficients for the MA part of the model
#' @param pct_ci What percentage of confidence interval to be used for
#'   coefficient consideration.
#' @param margin An additional margin to increase sparsity.
#' @examples
#'   data <- base_impute(bee.data[1:100], impute_col = "TOTAL_COUNT")
#'   x <- data$IMPUTED_VALS
#'   model <- sparse_arma(x)
#' @export
sparse_arma <- function(x, p_max = 10, q_max = 10, pct_ci = .95, margin = 0) {

  n <- length(x)
  d <- seq(1, n)
  se <- stats::qnorm(1 - (1 - pct_ci) / 2)

  criticals <-  se / sqrt(n - d)
  crit_p <- criticals[1:p_max]
  crit_q <- criticals[1:q_max]

  init_acf  <- stats::acf(x, lag.max = q_max, plot = FALSE)$acf[2:(q_max + 1)]
  init_pacf <- stats::pacf(x, lag.max = p_max, plot = FALSE)$acf[1:p_max]

  init_combined <- c(init_pacf, init_acf)

  coef_locs <- which(init_combined > c(crit_p, crit_q) * ( 1 + margin))

  print(coef_locs)

  fixed <- rep(0, p_max + q_max)
  fixed[coef_locs] <- NA

  print(fixed)

  model <- stats::arima(x - mean(x), order = c(p_max, 0, q_max),
                        include.mean = FALSE, fixed = fixed,
                        transform.pars = FALSE, method = 'CSS-ML')


  print(stats::Box.test(model$residuals, lag = 36, type = 'Ljung-Box'))

  return(model)

}
