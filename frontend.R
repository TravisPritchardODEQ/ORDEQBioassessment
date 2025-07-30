#  _______   __                                                                                                         __     
# /       \ /  |                                                                                                       /  |    
# $$$$$$$  |$$/   ______   ______    _______  _______   ______    _______  _______  _____  ____    ______   _______   _$$ |_   
# $$ |__$$ |/  | /      \ /      \  /       |/       | /      \  /       |/       |/     \/    \  /      \ /       \ / $$   |  
# $$    $$< $$ |/$$$$$$  |$$$$$$  |/$$$$$$$//$$$$$$$/ /$$$$$$  |/$$$$$$$//$$$$$$$/ $$$$$$ $$$$  |/$$$$$$  |$$$$$$$  |$$$$$$/   
# $$$$$$$  |$$ |$$ |  $$ |/    $$ |$$      \$$      \ $$    $$ |$$      \$$      \ $$ | $$ | $$ |$$    $$ |$$ |  $$ |  $$ | __ 
# $$ |__$$ |$$ |$$ \__$$ /$$$$$$$ | $$$$$$  |$$$$$$  |$$$$$$$$/  $$$$$$  |$$$$$$  |$$ | $$ | $$ |$$$$$$$$/ $$ |  $$ |  $$ |/  |
# $$    $$/ $$ |$$    $$/$$    $$ |/     $$//     $$/ $$       |/     $$//     $$/ $$ | $$ | $$ |$$       |$$ |  $$ |  $$  $$/ 
# $$$$$$$/  $$/  $$$$$$/  $$$$$$$/ $$$$$$$/ $$$$$$$/   $$$$$$$/ $$$$$$$/ $$$$$$$/  $$/  $$/  $$/  $$$$$$$/ $$/   $$/    $$$$/ 
#
#
# Front end for running bioassessment score calculations
#
# Version 2.0
#
# . .... . . ..  . ... ..... . .. . . .   . ..  . .;.. .. . . . .     .  . . . . . .       . .  . ... 
# .... ...... ... .. .. . ..  . .... . ... .. ...:: . . . .  . . ... . . .  . . . . . .  .  .  . .  . 
# . .. . .  .. ... .. .. . ... . . .. . . . . ..;:...... .. . . . . . . . . . .   .    .  .  .    . .  
# .. ... .... . ... ... . . . . .  .. . . ... .+;..... .. . . . . . . . .      . . . . .. . . . . . . 
# ..... .. . .. . .. . ....... ....... . .  ..:+..... . .. . . . . .  .   . ...  .  . .  .       .  . 
# . . .. ..... ... ...... ......:++......... .+;........ .... . . . .. .. . .  . . . . . . . . . . ..
# .... ... . .. ..... ...:;:..++XXx:..... ...;x:.......... . . . . .. .  . . . .  . .  .  . .  .    . 
# .. ........ .... .........;x;+&$;::...:;XXx+.....:;:;+;++++;;;:::;;;;:. . . . . . . . . .   . . . . 
# ... ..............::::......+$X::::;$XXX+...:++x$$$+;.........:.:........  . . . . .  .   .     ...
# . ... . .........:+&&&&:...+$x:::+XX;::;;xXXXx;::::::::::::........... ..... . .. . .  ... .. .   . 
# .. ...........;XXX$&&&$$$:;$$+;;X&&$$$$X$XX+;;;;::::::.................... . .. .. .... . . . . . . 
# ........ ....x++$$&$&$$$$x&&$XX$&&$$$$$$X$$XX$$X+XX$Xx+X$$XxX$$$$$XXx+++;;.:..:::... . ... . .   ..  
# . .. . .....;++X&$$$$$$$$$$$$XX$$$XX$XX$$$XXX$$XxX$$$XXX$&$$&&&&&&&&&&&$&$$X$&&&$$$X+:. . . . . . . 
# ............:X+x$$$$&$$$$$&$$$X$&&$$$$$$$$$$$$$XxX$&$XX$$&$$&&&&&&&&&&&$&&$$$&&&&&&&&&&&X;::. . . . 
# .. ..........;$$X&&&&&$$$X&&&$$&&&&$$$$$$&$$&&&XXX$&$xx$&&$$&&&&&&&&&&&$&$$$$&&&&&$&&&&&$X;++++ . .
# .. .. .......:;;&&&&&$+;;+$$++X&&&$$$$$$XXxxXXx++xX+;++xX+++xXx++++;;;;;;::;x&&&&&&$$+:;:::... ....
# ...............:.::;;;;;:::+XX;;XXx++xX$XX++;;::::::::::::::::::::............::;+++xxXXX+:..... ..
# . .. ..........:..:.::....::XX$XX$&$XX&&$X$$$xX$&&X++;................................:.::::.. . ..
# ........ .....................:XXx;;;+x$$XX+:::;;::;:::x+:................................. ... ...
# . . ....... ...................:x+:::::;XX;:::...::::::;++:............ . .................   . . . 
# ........ ..... .................:x:::;xx::::::............:++:...... . .... .. . ... .. . . ... . . 
# . .... ........ . .............:x::++;:::::..................:;;;:..... . . .... . . .. . .   .  ..
# .. . ... . ...... . .... .. ...+;:+:::.......... . ... .. .........:::.. . . . . . .. . .. .. . . . 
# . .......... ... ..... .. ....:+:;:.............. . .. . .. .. .. .. . .... .   . .   .. .  . . . . 
# .. ... . . ... .. .  .........::........ .. . ........... . .... .. . .  . . .. .. ..  . . .    . .  
# ........ ...... .......  ......:...... .. .....  .. . . . .. .  . .. . . .. . .  . .  . .  .  .  .. 
# ... . ... .  ... . . ..... . .. .. . ... . .  ... . . .  . . .. .. .. . . . .. .    .. . . . . . ..
# . .... ...... ..... . .. .. . .. .. .. . .. ..  . .. . . . .. . . . . . . . . . ... . .   . . . . . 
#   


library(ORDEQBioassessment)
library(tidyverse)


bug_tax_data <- ORDEQBioassessment::fetch_bug_data()


bug_tax_data_filtered <- bug_tax_data |>
  #filter(Result_Status != 'Rejected') |> ### 12/2 - LAM removed this for now - will apply later ?
  filter(SampleStart_Date > "1997-01-01") %>% ### 12/2 - LAM modified this to 1997 -  SH used 97 and 98 data in the model builds
  filter(
    Sample_Method %in%
      c(
        'Benthic Kick - Riffle',
        'Benthic Kick - Targeted Riffle',
        'Benthic Kick - Transect',
        'Benthic Kick - Mixed'
      )
  ) %>% # LAM added mixed for USU transect data inclusion
  filter(Char_Name == 'Count') %>%
  mutate(SampleStart_Date = lubridate::ymd(SampleStart_Date)) |>
  mutate(month = format(SampleStart_Date, "%m")) %>%
  filter(
    month %in%
      '06' |
      month %in% '07' |
      month %in% '08' |
      month %in% '09' |
      month %in% '10'
  )


sample_info <- bug_tax_data_filtered |>
  select(
    org_id,
    Project1,
    Project2,
    MLocID,
    StationDes,
    MonLocType,
    act_id,
    act_comments,
    Activity_Type,
    SampleStart_Date,
    SampleStart_Time,
    Sample_Media,
    Sample_Method,
    Result_Status,
    Assemblage,
    EcoRegion3,
    EcoRegion4,
    EcoRegion2,
    HUC8_Name,
    HUC12_Name,
    Lat_DD,
    Long_DD,
    Reachcode,
    Measure,
    ELEV_Ft,
    GNIS_Name,
    Conf_Score,
    QC_Comm,
    COMID,
    AU_ID,
    ReferenceSite,
    Wade_Boat
  ) |>
  distinct()
### Check for missing comids ----------------------------------------------------------------------------------------

no_comid <- bug_tax_data_filtered |>
  filter(is.na(COMID)) |>
  select(MLocID, COMID) |>
  distinct()

if (nrow(no_comid) > 0) {
  warning("Missing COMIDs. Please address before continuing")
}

OE_results <- ORDEQBioassessment::OE_run(df_bugs = bug_tax_data_filtered)

OE_scores <- OE_results$OE_Scores

missing_streamcat <- OE_results$missing_streamcat


# MMI -------------------------------------------------------------------------------------------------------------

MMI_results <- ORDEQBioassessment::MMI_run(
  df_bugs = bug_tax_data_filtered,
  df_sample = sample_info
)

MMI_scores <- MMI_results$MMI_result

MMI_metrics <- MMI_results$MMI_metrics


## Calculate Metrics ----------------------------------------------------------------------------------------------

BCG_metric_list <- ORDEQBioassessment::calculate_metrics(bug_tax_data_filtered)

BCG_metrics <- BCG_metric_list$Metrics
BCG_Metric_taxa_attributes <- BCG_metric_list$metric_taxa_attribute


## Run BCG --------------------------------------------------------------------------------------------------------

BCG <- ORDEQBioassessment::BCG_run(BCG_metrics)

BCG_results <- BCG$Levels.Flags
BCG_Metric.Membership <- BCG$Metric.Membership
BCG_Level.Membership <- BCG$Level.Membership


BCG_sample <- sample_info |>
  left_join(BCG_results, by = c('act_id' = 'SampleID'))
