## code to prepare `leppo_taxa_table` dataset goes here

taxonomy_leppo <- read.csv('https://github.com/leppott/BioMonTools_SupportFiles/raw/refs/heads/main/data/taxa_official/ORWA/old/ORWA_TaxaTranslator_20240619.csv')


usethis::use_data(taxonomy_leppo, overwrite = TRUE)
