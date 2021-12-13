#' Basic (naive) imputation for missing values in a data.table.
#' @name base_impute
#' @param dt A data.table with a column of with missing values.
#' @param type The type of imputation to be done; options are 'mean', 'median',
#'   and 'zero'.
#' @param impute_col The column on which to perform imputation.
#' @param group_by The columns to group the imputation method by.
#' @import data.table
#' @export
#' @examples
#'   base_impute(bee.data, 'median', 'TOTAL_COUNT', c('MONTH', 'DAY'))

base_impute <- function(dt, type = 'median', impute_col, group_by = '') {

  IMPUTED_VALS <- 'IMPUTED_VALS'

  if (type == 'mean') {
    dt[, IMPUTED_VALS := ifelse(!is.na(get(impute_col)), get(impute_col),
                                base::mean(get(impute_col), na.rm = TRUE)),
       by = group_by]
  } else if (type == 'median') {
    dt[, IMPUTED_VALS := ifelse(!is.na(get(impute_col)), get(impute_col),
                                stats::median(get(impute_col), na.rm = TRUE)),
       by = group_by]
  } else if (type == 'zero')  {
    dt[, IMPUTED_VALS := ifelse(!is.na(get(impute_col)), get(impute_col),
                                0),
       by = group_by]

  } else {
    print('Currently only mean, median, and 0 are supported. Returning NA')
    return(NA)
  }

  return(dt)
}
