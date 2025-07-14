
load('C:/Users/tpritch/OneDrive - Oregon/R Projects/BioMonORDEQ/bugs analyses/MMI/_2024 model build/rfmod_pt_tv_intol.Rdata' )
load('C:/Users/tpritch/OneDrive - Oregon/R Projects/BioMonORDEQ/bugs analyses/MMI/_2024 model build/rfmod_nt_habitat_rheo.Rdata' )          
load('C:/Users/tpritch/OneDrive - Oregon/R Projects/BioMonORDEQ/bugs analyses/MMI/_2024 model build/rfmod_pt_ti_stenocold_cold_cool.Rdata' )          
load('C:/Users/tpritch/OneDrive - Oregon/R Projects/BioMonORDEQ/bugs analyses/MMI/_2024 model build/rfmod_pi_EPTNoHydro.Rdata' )


usethis::use_data(
  rfmod_nt_habitat_rheo,
  rfmod_pi_EPTNoHydro,
  rfmod_pt_ti_stenocold_cold_cool,
  rfmod_pt_tv_intol,
  overwrite = TRUE
)
