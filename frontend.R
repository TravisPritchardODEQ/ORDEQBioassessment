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
# Version 3.0
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
library(randomForest)


##############
###Change this location
#######################

#File folder that files will be saved to
file_save_locationx <- '//deqlead-lims/SERVERFOLDERS/AWQMS/BioMon/2025 models'


#################################################################
###DID YOU CONFIRM THE CORRECT LOCATION???????????????????
#### SEE ABOVE
##################################

bug_tax_data <- ORDEQBioassessment::fetch_bug_data(filter_existing = TRUE)


bug_tax_data_filtered <- bug_tax_data |>
  #filter(Result_Status != 'Rejected') |> ### 12/2 - LAM removed this for now - will apply later ?
  # filter(SampleStart_Date > "1997-01-01") %>% ### 12/2 - LAM modified this to 1997 -  SH used 97 and 98 data in the model builds
  # filter(
  #   Sample_Method %in%
  #     c(
  #       'Benthic Kick - Riffle',
  #       'Benthic Kick - Targeted Riffle',
  #       'Benthic Kick - Transect',
  #       'Benthic Kick - Mixed'
  #     )
  # ) %>% # LAM added mixed for USU transect data inclusion
  filter(Char_Name == 'Count') %>%
  mutate(SampleStart_Date = lubridate::ymd(SampleStart_Date)) |>
  mutate(month = format(SampleStart_Date, "%m")) #%>%
  # filter(
  #   month %in%
  #     '06' |
  #     month %in% '07' |
  #     month %in% '08' |
  #     month %in% '09' |
  #     month %in% '10'
  # )


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


# Generate AWQMS import templates
# Use "BioMon Indexes_v2" import configuration
ORDEQBioassessment::generate_import_template(OE_scores,
                                             type = "OE",
                                             save_location = file_save_locationx)

# MMI -------------------------------------------------------------------------------------------------------------

MMI_results <- ORDEQBioassessment::MMI_run(
  df_bugs = bug_tax_data_filtered,
  df_sample = sample_info
)

MMI_scores <- MMI_results$MMI_result

MMI_metrics <- MMI_results$MMI_metrics


#This section adds the ni_total to the scores and is used for DQL generation
MMI_ni_total <- OE_scores |> 
  transmute(SAMPLEID = act_id,
            ni_total = tot.abund_raw.bugs)

MMI_scores <- MMI_scores |> 
  left_join(MMI_ni_total)

# Generate AWQMS import templates
# Use "BioMon Indexes_v2" import configuration

ORDEQBioassessment::generate_import_template(MMI_scores,
                                             type = "MMI",
                                             save_location = file_save_locationx)


## Calculate Metrics ----------------------------------------------------------------------------------------------

metric_list <- ORDEQBioassessment::calculate_metrics(bug_tax_data_filtered)

metrics <- metric_list$Metrics |> 
  left_join(select(sample_info,org_id,act_id,Activity_Type,Sample_Media,SampleStart_Date,
                   Project1,MLocID, act_comments, Sample_Method, Assemblage, EcoRegion3), join_by(SAMPLEID == act_id))
Metric_taxa_attributes <- metric_list$metric_taxa_attribute

# Generate AWQMS import templates
# Use "BioMon Metrics V2" import configuration
ORDEQBioassessment::generate_import_template(metrics,
                                             type = "Metrics",
                                             save_location = file_save_locationx)


## Run BCG --------------------------------------------------------------------------------------------------------

BCG <- ORDEQBioassessment::BCG_run(BCG_metrics)

BCG_results <- BCG$Levels.Flags
BCG_Metric.Membership <- BCG$Metric.Membership
BCG_Level.Membership <- BCG$Level.Membership


BCG_sample <- sample_info |>
  left_join(BCG_results, by = c('act_id' = 'SampleID'))
