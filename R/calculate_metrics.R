#' calculate_metrics
#'
#' @param bug_data dataframe of bug counts
#'
#' @returns list of netrics and attributes
#' @export
#'



calculate_metrics <- function(bug_data) {
  #testing
  # bug_data <- bug_tax_data_filtered
  #  attribute_table_loc = 'https://github.com/leppott/BioMonTools_SupportFiles/raw/refs/heads/main/data/taxa_official/ORWA/ORWA_Attributes_20250610.csv'

  df <- bug_data

  attributes <- attribute_table |>
    dplyr::select(-Kingdom)

  df_bugs_taxa <- df |>
    dplyr::left_join(attributes, by = c('OTU_BCG_MariNW' = 'Taxon'))

  # Get NHD info ----------------------------------------------------------------------------------------------------

  bug_tax_nhd <- ORDEQBioassessment::get_NHD_info(df_bugs_taxa)

  # Stremcat --------------------------------------------------------------------------------------------------------

  # Get list of COMIDs and remove blanks
  comidID <- unique(df$COMID)

  comidID <- comidID[!is.na(comidID)]

  streamcat <- ORDEQBioassessment::get_streamcat(comids = comidID, type = "BCG")

  actids <- df |>
    dplyr::select(act_id, COMID, QC_Comm) |>
    unique()

  #Join steramcat data to the activity IDS
  actid_streamcat <- actids |>
    dplyr::left_join(streamcat, by = dplyr::join_by(COMID))
  #Produce a list of errors- used in development
  streamcat_errors <- actid_streamcat |>
    dplyr::filter(is.na(WSAREASQKM))
  
  streamcat_mloc_data <- actid_streamcat |>
    dplyr::filter(!is.na(WSAREASQKM)) |>
    dplyr::mutate(
      PRECIP8110 = dplyr::case_when(
        stringr::str_detect(QC_Comm, "Used closest COMID") ~ PRECIP8110CAT,
        TRUE ~ PRECIP8110WS
      ),
      ELEV = dplyr::case_when(
        stringr::str_detect(QC_Comm, "Used closest COMID") ~ ELEVCAT,
        TRUE ~ ELEVWS
      ),
      AREASQKM = dplyr::case_when(
        stringr::str_detect(QC_Comm, "Used closest COMID") ~ CATAREASQKM,
        TRUE ~ WSAREASQKM
      ),
      II = dplyr::case_when(
        stringr::str_detect(QC_Comm, "Used closest COMID") ~ ICI,
        TRUE ~ IWI
      ),
    ) |>
    dplyr::select(act_id, PRECIP8110, ELEV, AREASQKM)


  bug_tax_nhd_2 <- bug_tax_nhd |>
    dplyr::left_join(streamcat_mloc_data, by = 'act_id') |>
    dplyr::mutate(
      SITE_TYPE = dplyr::case_when(
        NHD_pSLOPE < 1 & ELEV < 750 ~ 'lograd-loelev',
        NHD_pSLOPE >= 1 & ELEV < 750 ~ 'higrad-loelev',
        NHD_pSLOPE >= 1 & ELEV >= 750 ~ 'higrad-hielev',
        NHD_pSLOPE < 1 & ELEV >= 750 ~ 'lograd-hielev'
      )
    )

  # limit data to only whats needed ---------------------------------------------------------------------------------

  BCG_Bug_data <- bug_tax_nhd_2 |>
    dplyr::transmute(
      SampleID = act_id,
      Area_mi2 = NA_integer_,
      SurfaceArea = NA_integer_,
      TaxaID = OTU_BCG_MariNW, #is this correct???
      N_Taxa = Result_Numeric,
      Index_Name = 'BCG_MariNW_Bugs500ct',
      INDEX_CLASS = SITE_TYPE,
      NonTarget,
      SITE_TYPE,
      Phylum,
      SubPhylum,
      Class,
      SubClass,
      Order,
      SuperFamily,
      Family,
      Tribe,
      Genus,
      SubGenus,
      Species,
      BCG_attr,
      FFG,
      Habit,
      Life_Cycle,
      Thermal_indicator,
      TolVal,
      INFRAORDER = NA_character_,
      HABITAT = Habitat,
      ELEVATION_ATTR = NA_character_,
      GRADIENT_ATTR = NA_character_,
      WSAREA_ATTR = NA_character_,
      HABSTRUCT = NA_character_,
      UFC = NA_integer_,
      Density_ft2. = NA_integer_,
      DENSITY_M2 = NA_integer_,
    )

#limit data to only whats needed ---------------------------------------------------------------------------------
bugs.excluded <- BioMonTools::markExcluded(
  BCG_Bug_data,
  TaxaLevels = c(
    "Kingdom",
    "Phylum",
    "SubPhylum",
    "Class",
    "SubClass",
    "Order",
    "SubOrder",
    "SuperFamily",
    "Family",
    "SubFamily",
    "Tribe",
    "GenusGroup",
    "Genus",
    "SubGenus",
    "SpeciesGroup",
    "SpeciesSubGroup",
    "SpeciesComplex",
    "Species"
  )
)

  # Extra columns to keep in results
  keep.cols <- c(
    "Area_mi2",
    "SurfaceArea",
    "DENSITY_M2",
    "Density_ft2.",
    "Site_Type"
  )
  # Run Function
  df.metrics <- BioMonTools::metric.values(
    bugs.excluded,
    "bugs",
    fun.cols2keep = keep.cols,
    boo.Shiny = TRUE
  )

  Metric_list <- list(
    Metrics = df.metrics,
    metric_taxa_attribute = bugs.excluded
  )

  return(Metric_list)
}
