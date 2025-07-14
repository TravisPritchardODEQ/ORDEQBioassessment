## code to prepare `DEQ_Taxonomy_Table` dataset goes here


DEQ_taxonomy_table_location = "C:/Users/tpritch/OneDrive - Oregon/R Projects/BioMonORDEQ/bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx"
DEQ_taxonomy_table <- readxl::read_excel(DEQ_taxonomy_table_location, col_types = "text")


usethis::use_data(DEQ_taxonomy_table, overwrite = TRUE)
