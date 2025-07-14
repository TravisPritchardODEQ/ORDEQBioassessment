#' random_subsample
#' 
#' `random_subsample` randomly subsamples the raw bugs dataframe down to a 
#' a sample size of 300. This is basically a wrapper around `BioMonTools::rarify()`
#' with some minimal data processing 
#'
#' @param df dataframe of raw bug counts
#' @param OTU_col Name of OTU column to be used
#' @param subsiz subsample size. Default is 300 
#' @param seed random seed for consistency 
#'
#' @returns dataframe of bugs randomized down to subsiz count
#' @export
#'



random_subsample <- function(df, OTU_col, subsiz = 300, seed){
  
  if(missing(seed)){
    stop("random_subsample() function needs a seed argument to continue. Use seed = 16412730 as default seed or NA for no seed.")
    
  }
  
  
  # df <- bug_tax_data_filtered
  
  raw.bugs_OTUs <- as.data.frame(df %>%
                                   dplyr::group_by(Sample=act_id, MLocID, OTU={{OTU_col}}) %>%
                                   dplyr::summarise(Count=sum(Result_Numeric)) %>%
                                   dplyr::filter(OTU != 'DNI') |> 
                                   dplyr::ungroup())
  

  
  b.rare <-BioMonTools::rarify(na.omit(raw.bugs_OTUs), 'Sample', 'Count', subsiz, mySeed = seed) 
  
}

