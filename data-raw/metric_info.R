
# Metrics info ------------------------------------------------------------


metric_info <- openxlsx::read.xlsx('data-raw/ORDEQ.mets_FINAL_for.AWQMS.xlsx')


usethis::use_data(metric_info,
                  overwrite = TRUE)