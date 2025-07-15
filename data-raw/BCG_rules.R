## code to prepare `BCG_rules` dataset goes here

library(readxl)

df.rules <- read_excel(system.file("./extdata/Rules.xlsx"
                                   , package="BCGcalc")
                       , sheet="Rules")

df.checks <- read_excel(system.file("./extdata/MetricFlags.xlsx"
                                    , package="BCGcalc")
                        , sheet="Flags")

usethis::use_data(df.rules,df.checks , overwrite = TRUE)
