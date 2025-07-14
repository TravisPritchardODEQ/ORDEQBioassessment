
attribute_table_loc = 'https://github.com/leppott/BioMonTools_SupportFiles/raw/refs/heads/main/data/taxa_official/ORWA/old/ORWA_Attributes_20241121.csv'

attribute_table <- read.csv(attribute_table_loc)


usethis::use_data(attribute_table, overwrite = TRUE)
