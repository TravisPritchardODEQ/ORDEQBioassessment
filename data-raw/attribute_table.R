library(tidyverse)
library(openxlsx)

rstudioapi::showDialog(title = "WHERE ARE YOU GETTING THE ATTRIBUTE TABLE???", message = "the 7/2/2026 version has been edited by Shannon. If using biomontools, make sure it is all correct")


# Get attribute table -----------------------------------------------------


## If updating from biomontools, use the below code ------------------------

# attribute_table_loc = 'https://github.com/leppott/BioMonTools_SupportFiles/raw/refs/heads/main/data/taxa_official/ORWA/old/ORWA_Attributes_20241121.csv'
# 
# attribute_table <- read.csv(attribute_table_loc)
# 


# If using a secondary source ---------------------------------------------

#We battle this stuff constantly because taxa are added incrementally and things change, 
#or people aren't completely accurate.  But it means I need to make more updates. 
#You good with me pulling it out into Excel, making changes, then submitting back to you?


attribute_table <- read.xlsx("data-raw/attributes_SLH edits_7.2.26.xlsx") |> 
  #mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~coalesce(.x, "")))


attribute_table <- attribute_table |>
  mutate(Class = ifelse(Class == 'INSECTA', 'Insecta', Class))


usethis::use_data(attribute_table, overwrite = TRUE)
