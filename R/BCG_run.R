#' BCG_run
#' 
#' Runs the BCG calculations. Mostly wrappers around the BCGcalc family of functions
#'
#' @param df_metric dataframe of metrics produced by `calculate_metrics()`
#'
#' @returns list containing Metric.Membership, Level.Membership,Levels and  Levels.Flags
#' @export
#'

BCG_run <- function(df_metric){
  
  
  # testing ---------------------------------------------------------------------------------------------------------
  #df_metric <- BCG_metrics
  
  
  # 1.B. Metric Membership
  

  # Run function
  df.Metric.Membership <- BCGcalc::BCG.Metric.Membership(df_metric, df.rules)
  # Show Results
  # View(df.Metric.Membership)
  # # Save Results
  # write.table(df.Metric.Membership, "Metric.Membership.Test.tsv"
  #             , row.names=FALSE, col.names=TRUE, sep="\t")
  
  # 1.C. Level Assignment
  # Run Function
  
  #Delete this after leppot fixes the repo bug
  #source('bugs analyses/All_together/metric_membership_fix.R')
  
  df.Level.Membership <- BCGcalc::BCG.Level.Membership(df.Metric.Membership, df.rules)
  
  # 1.D. Level Membership
  # Run Function
  df.Levels <- BCGcalc::BCG.Level.Assignment(df.Level.Membership)
  
  # 1.E. Flags


  
  
  # Run Function
  df.flags <- BioMonTools::qc.checks(df_metric, df.checks)
  # Change terminology; PASS/FAIL to NA/flag
  df.flags[,"FLAG"][df.flags[,"FLAG"]=="FAIL"] <- "flag"
  df.flags[, "FLAG"][df.flags[,"FLAG"]=="PASS"] <- NA
  # long to wide format
  df.flags.wide <- reshape2::dcast(df.flags, SAMPLEID ~ CHECKNAME, value.var="FLAG")
  # Calc number of "flag"s by row.
  df.flags.wide$NumFlags <- base::rowSums(df.flags.wide=="flag", na.rm=TRUE)
  # Rearrange columns
  NumCols <- ncol(df.flags.wide)
  df.flags.wide <- df.flags.wide[, c(1, NumCols, 2:(NumCols-1))]
  # Merge Levels and Flags
  df.Levels.Flags <- base::merge(df.Levels, df.flags.wide,by.x="SampleID", by.y="SAMPLEID", all.x=TRUE)
  
  BCG <- list(Metric.Membership = df.Metric.Membership,
              Level.Membership = df.Level.Membership,
              Levels  = df.Levels,
              Levels.Flags = df.Levels.Flags)
  
  return(BCG)
  
}