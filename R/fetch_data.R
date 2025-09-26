#' fetch_bug_data
#'
#' @param startdate Startdate for datapull
#' @param enddate Enddate for datapull
#'
#' @returns dataframe of raw macroinvertebrate data joined to DEQ taxa translator
#' and Eric Leppo's taxonomy table from BioMonTools_SupportFiles
#' @export
#'



fetch_bug_data <- function(startdate = NULL,
                       enddate = NULL,
                       filter_existing = TRUE){
  
  
  


  
  ## Download raw bug data from AWQMS --------------------------------------------------------------------------------
  
  
  raw_bugs <- AWQMSdata::AWQMS_Raw_Macros() 
  
 
  # get list of index activity ID's for filtering ---------------------------
  
  
  if(filter_existing){
    cat(cli::col_cyan("Searching for existing data\n"))
    
    existing_indexes <- AWQMSdata::AWQMS_Bio_Indexes(Index_Name = c('O/E Ratio',  'MMI'))  
    
    existing_indexes_actids <- unique(stringr::str_remove(existing_indexes$Act_id, "\\:[^:]*$"))
    
    raw_bugs <- raw_bugs |> 
      dplyr::filter(!act_id %in% existing_indexes_actids)
    
    cat(cli::col_cyan("Found and removed data from existing indexes\n"))
    
  }
  
  
   
  ## Join Taxonomy table ---------------------------------------------------------------------------------------------
  
  #AWQMS taxa = Taxonomic_Name
  #Join DEQ taxonomy table by DEQ_Taxon to get Taxon
  #Match Taxon to leppo's taxa table by Taxon = Taxon_orig
  #Keep   OTU_RIV_24 from DEA taxa table and OTU_MTTI, OTU_BST from leppo taxa table
  
  #read in DEQ taxa table
  taxonomy <- ORDEQBioassessment::DEQ_taxonomy_table
  
  
  taxonomy.otu <- taxonomy |>
    dplyr::select(DEQ_Taxon = DEQ_TAXON, Taxon,Kingdom, OTU_RIV_24)
  
  taxonomy.otu$DEQ_Taxon <- as.character(taxonomy.otu$DEQ_Taxon)
  
  

  
  
  taxonomy_leppo_select <- ORDEQBioassessment::taxonomy_leppo |> 
    dplyr::select(Taxon_orig,OTU_MTTI, OTU_BSTI, OTU_MetricCalc, OTU_BCG_MariNW) |> 
    dplyr::rename(Taxon = Taxon_orig)
  
  
  
  
  raw.bugs_taxonomy <- raw_bugs |>
    dplyr::left_join(taxonomy.otu, by='DEQ_Taxon') |> 
    dplyr::left_join(taxonomy_leppo_select, by = dplyr::join_by(Taxon))
  
  
  
  
  return(raw.bugs_taxonomy)
  
}

