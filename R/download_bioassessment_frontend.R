
#' download_bioassessment_frontend
#' 
#' This function will download the the most up to date version of the
#' most up to date bioassessment frontend. A directory chooser will open to ask
#' about where to save. OPen this file up to complete the bioassessment 
#' calculations.
#'
#' @returns Downloads the most up to date version of the bioassessment frontend
#' @export
#'

download_bioassessment_frontend <- function(){


url <- 'https://github.com/TravisPritchardODEQ/ORDEQBioassessment/raw/refs/heads/main/frontend.R'

filename <- basename(url)


save_path  <- paste0(rstudioapi::selectDirectory(caption = "Select Save Location"), "/")



download.file(url = url, destfile = paste0(save_path, filename, sep = ""))

}