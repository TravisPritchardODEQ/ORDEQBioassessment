
# Metrics info ------------------------------------------------------------


metric_info <- openxlsx::read.xlsx('data-raw/MetricNames 2.xlsx',
                                   sheet = 'metadata_ODEQ.keep')


usethis::use_data(metric_info,
                  overwrite = TRUE)