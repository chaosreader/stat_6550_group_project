data <- bee.data


imp_data <- base_impute(data, 'median', 'TOTAL_COUNT', c('MONTH', 'DAY'))

View(imp_data)
