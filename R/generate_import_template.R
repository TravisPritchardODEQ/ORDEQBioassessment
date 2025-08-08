
#' generate_import_template
#' 
#' Generates AWQMS import templates based on data. USed for OE, MMI, and metrics
#' This will save one AWQMS import configuration for each organization
#' AS well as the input file with all metadata and blank values
#'
#' @param df data to load in
#' @param type type of index or metric. use "OE". "MMI", or "Metrics"
#' @param save_location Location where you want the data saved
#'
#' @returns nothing, writes excel files
#' @export
#'
#' 
generate_import_template <- function(df, type, save_location){



# Testing -----------------------------------------------------------------


# df <- metrics
# type <-  'Metrics'
# save_location <- 'C:/Users/tpritch/OneDrive - Oregon/R Projects/ORDEQBioassessment'


# OE scores ---------------------------------------------------------------

  

if (type == "OE") {
  config <- df |>
    dplyr::mutate(
      Project1 = dplyr::case_when(
        org_id == 'OREGONDEQ' &  Project1 == 'Total Maximum Daily Load Sampling' ~ "TMDL",
        org_id == 'OREGONDEQ' &  Project1 == 'Water Quality Response Monitoring' ~ "Water Quality Response",
        TRUE ~ Project1
      )
    ) |>
    dplyr::transmute(
      org_id,
      MLocID,
      `Index ID` = act_id,
      `O/E Ratio` = OoverE,
      `Bray Curtis Similarity Index` = BC,
      SampleStart_Date,
      Project1,
      `Index Commment` = "",
      `Index Qualifier Code` = "",
      `Data Quality Level` = ""
    ) |>
    tidyr::pivot_longer(
      cols = c(`O/E Ratio`, `Bray Curtis Similarity Index`),
      names_to = "Index Type ID",
      values_to = "Index Score"
    ) |>
    dplyr::relocate("Index Type ID", .after = MLocID) |>
    dplyr::relocate("Index Score", .after = `Index Type ID`) |>
    dplyr::mutate(
      `Index Score` = round(`Index Score`, 3),
      `Index ID` = dplyr::case_when(
        `Index Type ID` == 'O/E Ratio' ~ paste0(`Index ID`, ":OE"),
        `Index Type ID` == 'Bray Curtis Similarity Index' ~
          paste0(`Index ID`, ":BC")
      )
    ) |> 
    dplyr::filter(!is.na(`Index Score`))

  #get original actid
  #str_remove(config$`Index ID`, "\\:[^:]*$")
}


# MMI ---------------------------------------------------------------------


  
if (type == "MMI") {
    config <- df |>
      dplyr::mutate(
        Project1 = dplyr::case_when(
          org_id == 'OREGONDEQ' &  Project1 == 'Total Maximum Daily Load Sampling' ~ "TMDL",
          org_id == 'OREGONDEQ' &  Project1 == 'Water Quality Response Monitoring' ~ "Water Quality Response",
          TRUE ~ Project1
        )
      ) |>
      dplyr::transmute(
        org_id,
        MLocID,
        `Index ID` = SAMPLEID,
        MMI, 
        SampleStart_Date,
        Project1,
        `Index Commment` = QC_Comm,
        `Index Qualifier Code` = "",
        `Data Quality Level` = ""
      ) |>
      tidyr::pivot_longer(
        cols = c(MMI),
        names_to = "Index Type ID",
        values_to = "Index Score"
      ) |>
      dplyr::relocate("Index Type ID", .after = MLocID) |>
      dplyr::relocate("Index Score", .after = `Index Type ID`) |>
      dplyr::mutate(
        `Index Score` = round(`Index Score`, 3),
        `Index ID` = dplyr::case_when(
          `Index Type ID` == 'O/E Ratio' ~ paste0(`Index ID`, ":OE"),
          `Index Type ID` == 'Bray Curtis Similarity Index' ~ paste0(`Index ID`, ":BC"),
          `Index Type ID` == 'MMI' ~ paste0(`Index ID`, ":MMI"),
        )
      ) |> 
      dplyr::filter(!is.na(`Index Score`))
    
    #get original actid
    #str_remove(config$`Index ID`, "\\:[^:]*$")
}  
  
  
if( type == "Metrics")  {
  
  metrics_keep <- ORDEQBioassessment::metric_info |> 
    dplyr::filter(ODEQ.keep == "Y") |> 
    dplyr::select(ODEQ.keep, METRIC_NAME)
  
  config <- df |>
    dplyr::relocate(org_id,SAMPLEID,Activity_Type,Sample_Media,SampleStart_Date,
             Project1,MLocID, act_comments,  Sample_Method, Assemblage ) |> 
    tidyr::pivot_longer(
      cols = 18:length(colnames(df)),
      names_to = "MetricTypeID",
      values_to = "Activity Metric Score"
    ) |> 
    dplyr::mutate(`Activity Metric Score` = round(`Activity Metric Score`, 3)) |> 
    dplyr::left_join(metrics_keep, dplyr::join_by(MetricTypeID == METRIC_NAME)) |> 
    dplyr::filter(!is.na(ODEQ.keep)) |> 
    dplyr::rename(act_id = SAMPLEID) |> 
    dplyr::transmute(org_id,
                     act_id,
                     Activity_Type,
                     Sample_Media,
                     SampleStart_Date,
                     Project1,
                     MLocID,
                     MetricTypeID,
                     `Activity Metric Score`,
                     `Data Quality Level` = NA_character_,
                     act_comments,
                     CollectionMethod = Sample_Method,
                     Assemblage,
                     EquipmentID = 'D-Frame Net'	)
    
  
  
}
# Once metric and index QC process is created, update this to reflect new process
# For now, we are just using what was identified in model development phase


DQL <- sample_info_model |>
  dplyr::mutate(DQL = ifelse(qualifer != 0, 'C', "")) |>
  dplyr::select(act_id, DQL)

if(type %in% c('OE', 'MMI')){
config <- config |>
  dplyr::mutate(act_id = stringr::str_remove(`Index ID`, "\\:[^:]*$")) |>
  dplyr::left_join(DQL, by = dplyr::join_by(act_id)) |>
  dplyr::mutate(`Data Quality Level` = DQL) |>
  dplyr::select(-DQL, -act_id)

} else {
  
  config <- config |>
   #mutate(act_id = str_remove(`Index ID`, "\\:[^:]*$")) |>
    dplyr::left_join(DQL, by = dplyr::join_by(act_id)) |>
    dplyr::mutate(`Data Quality Level` = DQL) |>
    dplyr::select(-DQL)
}

config_list <- split(config, f = config$org_id)

write_template <- function(x){
  
  name <- x[[1,1]]
  
  openxlsx::write.xlsx(
    x,
    paste0(save_location,"/",Sys.Date(),"- ",type," scores- AWQMS Upload - ", name, ".xlsx")
  )
  
}

purrr::map(config_list,write_template )


openxlsx::write.xlsx(
  df,
  paste0(save_location,"/",Sys.Date(),"- ",type," scores- all scores and metadata.xlsx"
  )
)


}
