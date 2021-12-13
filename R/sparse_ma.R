#'Sparse MA Model with Auto-Selected Coefficients
#' @name sparse_ma
#' @param x A time series object
#' @param q_max The maximum number of coefficients to be compute
#' @param pct_ci What percentage of confidence interval to be used for
#'   coefficient consideration.
#' @param margin An additional margin to increase sparsity.
#' @examples
#'   data <- base_impute(bee.data[1:100], impute_col = "TOTAL_COUNT")
#'   x <- data$IMPUTED_VALS
#'   model <- sparse_ma(x)
#' @export
sparse_ma <- function(x, q_max = 30, pct_ci = .95, margin = 0) {

  n <- length(x)
  d <- seq(1, n)
  se <- stats::qnorm(1 - (1 - pct_ci) / 2)

  criticals <-  se / sqrt(n - d)
  criticals <- criticals[1:q_max]


  init_acf  <- stats::acf(x, lag.max = q_max, plot = FALSE)$acf[2:(q_max + 1)]
  coef_locs <- which(init_acf > criticals * ( 1 + margin))

  fixed <- rep(0, q_max)
  fixed[coef_locs] <- NA

  model <- stats::arima(x - mean(x), order = c(0, 0, q_max),
                        include.mean = FALSE, fixed = fixed,
                        transform.pars = FALSE, method = 'CSS-ML')


  print(stats::Box.test(model$residuals, lag = 36, type = 'Ljung-Box'))

  return(model)

}
