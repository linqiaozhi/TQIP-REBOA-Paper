library(arsenal)
library(haven)
library(tidyverse)
################################
# 1.1.1 Loading and merging data 
################################

puf_demo_2015      <- read_csv('data/PUF_AY_2015/CSV/PUF_DEMO.csv')
puf_vital_2015     <- read_csv('data/PUF_AY_2015/CSV/PUF_VITALS.csv') %>% filter(VSTYPE == "ED")
puf_vital_ems_2015     <- read_csv('data/PUF_AY_2015/CSV/PUF_VITALS.csv') %>% filter(VSTYPE == "EMS") %>% rename(EMSSBP=SBP) %>% select( INC_KEY, EMSSBP)
puf_ed_2015        <- read_csv('data/PUF_AY_2015/CSV/PUF_ED.csv') # ED info
puf_pm_2015        <- read_csv('data/PUF_AY_2015/CSV/PUF_PM.csv') %>% rename(INC_KEY=inc_key) # Process of care measures
puf_discharge_2015 <- read_csv('data/PUF_AY_2015/CSV/PUF_DISCHARGE.csv') # discharge and outcome info
puf_2015_           <- left_join(puf_demo_2015, puf_vital_2015 ) %>% left_join(., puf_vital_ems_2015) %>% left_join(. , puf_pm_2015) %>%  left_join(., puf_discharge_2015) %>% left_join(.,puf_ed_2015)
puf_complic_2015_      <- read_csv('data/PUF_AY_2015/CSV/PUF_COMPLIC.csv')



puf_demo_2016      <- read_csv('data/PUF_AY_2016/CSV/PUF_DEMO.csv')
puf_vital_2016     <- read_csv('data/PUF_AY_2016/CSV/PUF_VITALS.csv') %>% filter(VSTYPE == "ED")
puf_vital_ems_2016     <- read_csv('data/PUF_AY_2016/CSV/PUF_VITALS.csv') %>% filter(VSTYPE == "EMS") %>% rename(EMSSBP=SBP) %>% select( INC_KEY, EMSSBP)
puf_ed_2016        <- read_csv('data/PUF_AY_2016/CSV/PUF_ED.csv') # ED info
puf_pm_2016        <- read_csv('data/PUF_AY_2016/CSV/PUF_PM.csv') %>% rename(INC_KEY=inc_key) # Process of care measures
puf_discharge_2016 <- read_csv('data/PUF_AY_2016/CSV/PUF_DISCHARGE.csv') # discharge and outcome info
puf_2016_           <- left_join(puf_demo_2016, puf_vital_2016 ) %>% left_join(., puf_vital_ems_2016) %>% left_join(. , puf_pm_2016) %>%  left_join(., puf_discharge_2016) %>% left_join(.,puf_ed_2016)
puf_complic_2016_      <- read_csv('data/PUF_AY_2016/CSV/PUF_COMPLIC.csv')

# 2015 and 2016 are roughly the same format. We merge the 2015 with 2016, then merge it to 2017
puf_2016 <- bind_rows( list('2015'=puf_2015_,'2016'=puf_2016_), .id = 'year_2015_2016')
# In 2016 and earlier, the two types of NA (not applicable or not known) are encoded as -1 and -2. Here we convert them to NA
puf_2016 <- puf_2016 %>% mutate( 
                                        RACE = case_when (RACE1 == 'American Indian' | RACE1 == 'Asian' | RACE1 == 'Native Hawaiian or Other Pacific Islander' ~ 'Other Race', RACE1 == 'Not Applicable BIU 1' | RACE1 == 'Not Known/Not Recorded BIU 2'  ~ NA_character_,  T ~ RACE1),
                                        GENDER = case_when ( GENDER == 'Not Applicable BIU 1' | GENDER == 'Not Known/Not Recorded BIU 2'  ~ NA_character_,  T ~ GENDER),
                                        ETHNICITY  = case_when ( ETHNIC == 'Not Applicable BIU 1' | ETHNIC == 'Not Known/Not Recorded BIU 2'  ~ NA_character_,  T ~ ETHNIC),
                                        SUPPOXY  = case_when ( SUPPOXY == 'Not Applicable BIU 1' | SUPPOXY == 'Not Known/Not Recorded BIU 2'  ~ NA_character_,  T ~ SUPPOXY),
                                        SBP  = case_when ( SBP == -2 | SBP == -1  ~ NA_real_,  T ~ SBP),
                                        EMSSBP  = case_when ( EMSSBP == -2 | EMSSBP == -1  ~ NA_real_,  T ~ EMSSBP),
                                        PULSE  = case_when ( PULSE == -2 | PULSE == -1  ~ NA_real_,  T ~ PULSE),
                                        OXYSAT  = case_when ( OXYSAT == -2 | OXYSAT == -1  ~ NA_real_,  T ~ OXYSAT),
                                        RR  = case_when ( RR == -2 | RR == -1  ~ NA_real_,  T ~ RR),
                                        TEMP  = case_when ( TEMP == -2 | TEMP == -1  ~ NA_real_,  T ~ TEMP),
                                        GCSEYE  = case_when ( GCSEYE == -2 | GCSEYE == -1  ~ NA_real_,  T ~ GCSEYE),
                                        GCSVERB  = case_when ( GCSVERB == -2 | GCSVERB == -1  ~ NA_real_,  T ~ GCSVERB),
                                        GCSMOT  = case_when ( GCSMOT == -2 | GCSMOT == -1  ~ NA_real_,  T ~ GCSMOT),
                                        GCS_Q1  = case_when ( GCS_Q1 == 'Not Applicable BIU 1' | GCS_Q1 == 'Not Known/Not Recorded BIU 2'  ~ NA_character_,  T ~ GCS_Q1),
                                        RRAQ  = case_when ( RRAQ == 'Not Applicable BIU 1' | RRAQ == 'Not Known/Not Recorded BIU 2'  ~ NA_character_,  T ~ RRAQ),
                                        SIGNSOFLIFE  = case_when ( SIGNSOFLIFE == 'Not Known/Not Recorded BIU 2'  ~ NA_character_,  T ~ SIGNSOFLIFE),
                                        deceased = case_when ( HOSPDISP  ==  'Deceased/Expired'  |  EDDISP == 'Deceased/Expired' ~ 'Complication',  T ~ 'No complication' ),
                                        ed_deceased = case_when (   EDDISP == 'Deceased/Expired' ~ 'Complication',  T ~ 'No complication' ),
                                        TRANSFER  = case_when ( TRANSFER == 'Not Known/Not Recorded BIU 2'  ~ NA_character_,  T ~ TRANSFER),
                                    )

# Unfortunately, 2017 completely changed everything. Here we rename variables
# that map directly to the 2016 variables and make new variables that match the
# 2016 variables
puf_trauma_2017  <- read_sas('data/PUF_AY_2017/SAS/PUF_TRAUMA.sas7bdat')

puf_trauma <- puf_trauma_2017 %>% 
        rename( 
               INC_KEY  = inc_key,
               AGE      = AGEYEARS,
               LOWEST_SBP = LOWESTSBP,
               PULSE    = PULSERATE,
               RR       = RESPIRATORYRATE,
               OXYSAT   = PULSEOXIMETRY,
               TEMP     = TEMPERATURE,
               GCSVERB  = GCSVERBAL,
               GCSMOT   = GCSMOTOR,
               #EMSRESP  = EMSRESPONSEMINS,
               #EMSSCENE = EMSSCENEMINS,
               ) %>% 
        mutate(
           RACE    = case_when( ASIAN == 1 | AMERICANINDIAN == 1 | PACIFICISLANDER == 1 | RACEOTHER == 1 ~ 'Other Race',
                                  BLACK == 1 ~ 'Black or African American',
                                  WHITE == 1 ~ 'White',
                                  T ~NA_character_),
           GENDER    = case_when( SEX == 1 ~ 'Male',
                                  SEX == 2 ~ 'Female',
                                  T ~NA_character_),
           TRANSFER    = case_when( INTERFACILITYTRANSFER == 1 ~ 'Yes',
                                    INTERFACILITYTRANSFER == 2 ~ 'No',
                                  T ~NA_character_),
           TEACHINGSTATUS = case_when( TEACHINGSTATUS == 'community' ~ 'Community',
                                  TEACHINGSTATUS == 'nonteaching' ~ 'Non-Teaching',
                                  TEACHINGSTATUS == 'university' ~ 'University',
                                  T ~NA_character_),
           SUPPOXY = case_when( SUPPLEMENTALOXYGEN == 2    ~ 'Supplemental Oxygen',
                                  SUPPLEMENTALOXYGEN == 1 ~ 'No Supplemental Oxygen',
                                  T ~NA_character_),
           GCS_Q1 = case_when( GCSQ_EYEOBSTRUCTION == 1 ~ 'Obstruction to the Patient\'s Eye',
                                  GCSQ_SEDATEDPARALYZED == 1 ~ 'Patient chemically sedated or paralyzed',
                                  GCSQ_INTUBATED == 1 ~ 'Patient Intubated',
                                  GCSQ_VALID == 1 ~ 'Valid GCS:  Patient was not sedated, not intubated, and did not have obstruction to the eye',
                                  T ~NA_character_),
            RRAQ = case_when ( RESPIRATORYASSISTANCE == 1 ~ 'Unassisted Respiratory Rate',
                              RESPIRATORYASSISTANCE == 2 ~ 'Assisted Respiratory Rate',
                              T ~ NA_character_),
           SIGNSOFLIFE = case_when( DEATHINED == 1 ~ 'Arrived with NO signs of life',
                                  DEATHINED == 2 ~ 'Arrived with signs of life',
                                  T ~NA_character_),
           deceased  = case_when ( HOSPDISCHARGEDISPOSITION == 5 | EDDISCHARGEDISPOSITION == 5 ~ 'Complication', T ~ 'No complication' ),
           ed_deceased  = case_when (  EDDISCHARGEDISPOSITION == 5 ~ 'Complication', T ~ 'No complication' )
            ) 


puf_complic_2016 <- bind_rows( puf_complic_2015_,puf_complic_2016_)
# Complications
outcomes_ <- c('dvt', 'pe', 'stroke', 'mi', 'compartment_syndrome', 'unplanned_return_to_or', 'aki')
puf_complic_2016_wider <- puf_complic_2016 %>%  mutate( ones = 1) %>% pivot_wider( id_cols = INC_KEY, names_from= COMPLDES, values_from = ones)
puf_complic_2016_wider <- puf_complic_2016_wider %>% mutate( 
                                                            dvt                    = `Deep Vein Thrombosis (DVT)`,
                                                            pe                     = `Pulmonary embolism`,
                                                            stroke                 = `Stroke / CVA`,
                                                            mi                     = `Myocardial infarction`,
                                                            compartment_syndrome   = `Extremity compartment syndrome`,
                                                            unplanned_return_to_or = `Unplanned return to the OR`,
                                                            aki = `Acute kidney injury`,
                                                            )
puf_complic_2016_wider <- puf_complic_2016_wider %>% mutate_at( outcomes_, ~ replace_na(., 0) )  %>% select( INC_KEY,  all_of(outcomes_))
puf_2016 <- puf_2016 %>% left_join(puf_complic_2016_wider)
colnames(puf_2016)
table( puf_complic_2016_wider$dvt, useNA="ifany") # Thse patients that still have NA are the ones that did not have an entry in the table, so we don'tk now if they had complications or not


colnames(puf_trauma)
puf_trauma <- puf_trauma %>% rename( 
                      dvt = HC_DVTHROMBOSIS,
                      pe = HC_EMBOLISM,
                      stroke = HC_STROKECVA,
                      mi = HC_MI,
                      compartment_syndrome = HC_EXTREMITYCS,
                      unplanned_return_to_or = HC_RETURNOR,
                      aki = HC_KIDNEY
                      )
# Bind
cois_  <- c('AGE', 'GENDER', 'RACE',  'HEIGHT', 'WEIGHT', 'TEACHINGSTATUS', 'SBP','EMSSBP', 'PULSE', 'RR', 'OXYSAT', 'SUPPOXY', 'TEMP', 'GCSEYE', 'GCSVERB', 'GCSMOT', 'GCS_Q1','RRAQ',  'deceased','ed_deceased', 'SIGNSOFLIFE','TRANSFER',  outcomes_)
cois  <- c( cois_, 'splenic', 'kidney', 'liver')
puf <- bind_rows(list( '2016' = puf_2016[,c( 'INC_KEY','year_2015_2016', cois_)], '2017' = puf_trauma[,c( 'INC_KEY', cois_)] ), .id = 'year' ) 
puf$year[ puf$year_2015_2016 == '2015'] = '2015'

puf %>% group_by(year) %>% summarise( mean(deceased=='Complication'))

################################
# AIS Description tables 
################################

# Process the injury codes. First get the descriptions
# There are new lines in the ddescription field which screws things up. First remove those, then replace the carriage returns with new lines
# tr '\n' ' '< PUF_AISDES.csv > PUF_AISDES2.csv; tr '\r' '\n'< PUF_AISDES2.csv > PUF_AISDES3.csv
puf_aisdes_2016 <- read_csv('data/PUF_AY_2016/CSV/PUF_AISDES3.csv')
puf_aisdes_2016  <- puf_aisdes_2016 %>% rename(description = AISDESC) %>% select( PREDOT, description)
puf_aisdes_2017_<- read_sas('data/PUF_AY_2017/SAS/PUF_AISDIAGNOSIS_LOOKUP.sas7bdat')
puf_aisdes_2017  <- puf_aisdes_2017_ %>% rename(description = AISDESCRIPTION, PREDOT=AISPREDOT) %>% select( PREDOT, description)



puf_aisdes  <-  (bind_rows(puf_aisdes_2016, puf_aisdes_2017))
# Both use AIS 05. But the actual description text is slightly different. For example:
#puf_aisdes %>% arrange(PREDOT)  %>% print(n = 200)
#190 122407 Superior longitudinal (saggital) sinus, thrombosis; occlusion, anterior half of sinus
#191 122408 Sinus - Superior longitudinal (saggital) sinus - thrombosis; occlusion - posterior half of sinus
#So, we will just pick one of them.
puf_aisdes  <- puf_aisdes  %>% distinct( PREDOT, .keep_all=T)





################################
# AIS codes themselves 
################################
fct_case_when <- function(...) {
      args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
    levels <- levels[!is.na(levels)]
    factor(dplyr::case_when(...), levels=levels)
}

# Most of the 2015 entries use '05 version (2.5 million records) and some use the '98 (166k records). We need to translate the '98 to '05. 
# The mapping is many to one, though. That is, the '05 version is more detailed, so many '05 codes map to
# the same '98 code. We will just pick one of them, though.
puf_ais98cpode_2015 <- read_sas('data/PUF_AY_2015/SAS/PUF_AISPCODE.sas7bdat') 
to98xwalk <- read_sas('data/PUF_AY_2016/SAS/PUF_AIS05TO98_CROSSWALK.sas7bdat') 
puf_aiscpode_converted_2015  <- puf_ais98cpode_2015 %>%left_join(to98xwalk, by = c('PREDOT'='AIS98_PREDOT')) %>% select(INC_KEY,AIS05_PREDOT, AIS05_SEVERITY, PREDOT)
puf_aiscpode_converted_2015_  <- puf_aiscpode_converted_2015 %>% distinct( INC_KEY, PREDOT, .keep_all=T) %>%  select(-PREDOT) %>% rename(PREDOT = AIS05_PREDOT, SEVERITY = AIS05_SEVERITY)



# NOw, join the codes assigned to each incident with the descriptions. Then pivot that wide, so we have a separate variable for each description (that fits our grep, that is)
puf_aiscp05ode_2015 <- read_sas('data/PUF_AY_2015/SAS/PUF_AISP05CODE.sas7bdat') 
puf_aiscpode_2015  <- bind_rows(puf_aiscpode_converted_2015_, puf_aiscp05ode_2015)
puf_aiscpode_2016 <- read_sas('data/PUF_AY_2016/SAS/PUF_AISPCODE.sas7bdat') 
puf_aiscpode_2017 <- read_sas('data/PUF_AY_2017/SAS/PUF_AISDIAGNOSIS.sas7bdat') %>% rename(INC_KEY=inc_key, PREDOT=AISPREDOT) 
puf_aiscpode_long  <- bind_rows(list('2015'=puf_aiscpode_2015, '2016'=puf_aiscpode_2016, '2017'=puf_aiscpode_2017),.id = 'year') %>% filter(PREDOT != "")  %>% right_join(puf_aisdes) %>% select( year, INC_KEY, PREDOT, description) 
#saveRDS(puf_aiscpode_long, 'data/puf_aiscpode_long.RDS')

puf_aiscpode  <-  puf_aiscpode_long %>% select( -PREDOT) %>%  filter( grepl('Liver|Spleen|Kidney|Femur|Tibia|Fibula|Pelvic|Iliac|Vascular|vascular|Brachiocephalic|Subclavian|Vena|Aorta|vein|arteries|artery', description ))  %>% 
                                        mutate( description= str_replace_all(description, "[^[:alnum:]]+", "_"), ones=1)  %>% distinct_all()  %>% 
                                        pivot_wider( id_cols = c(year, INC_KEY), names_from = description, values_from = ones)
puf_aiscpode  <- puf_aiscpode %>% mutate( 
                                 splenic = fct_case_when (
                                                          !is.na(Spleen_laceration_hilar_disruption_producing_total_devascularization_tissue_loss_avulsion_massive_OIS_V_) | !is.na(Spleen_contusion_hematoma_subcapsular_50_percent_surface_area_or_expanding_ruptured_subcapsular_or_parenchymal_intraparenchymal_5cm_in_diameter_or_expanding_major_OIS_III_) ~ "Grade V",
                                                          !is.na(Spleen_laceration_involving_segmental_or_hilar_vessels_producing_major_devascularization_of_25_percent_of_spleen_but_no_hilar_injury_major_OIS_IV_) ~ "Grade IV",
                                                          !is.na(Spleen_laceration_no_hilar_or_segmental_parenchymal_disruption_or_destruction_3cm_parenchymal_depth_or_involving_trabecular_vessels_moderate_OIS_III_) | !is.na(Spleen_contusion_hematoma_subcapsular_50_percent_surface_area_or_expanding_ruptured_subcapsular_or_parenchymal_intraparenchymal_5cm_in_diameter_or_expanding_major_OIS_III_) ~ "Grade III",
                                                          !is.na(Spleen_laceration_simple_capsular_tear_3cm_parenchymal_depth_and_no_trabecular_vessel_involvement_minor_superficial_OIS_I_II_) | !is.na(Spleen_contusion_hematoma_subcapsular_50_percent_surface_area_intraparenchymal_5cm_in_diameter_minor_superficial_OIS_I_II_) ~ "Grade I-II",
                                                          !is.na(Spleen_laceration_NFS) | !is.na(Spleen_NFS) | !is.na(Spleen_rupture_NFS) ~ "Unspecified",
                                                          T  ~ "No injury"),
                                 kidney = fct_case_when (
                                                         !is.na(Kidney_laceration_hilum_avulsion_total_destruction_of_organ_and_its_vascular_system_OIS_V_) ~ "Grade V",
                                                         !is.na(Kidney_laceration_extending_through_renal_cortex_medulla_and_collecting_system_main_renal_vessel_injury_with_contained_hemorrhage_major_OIS_IV_) ~ "Grade IV",
                                                         !is.na(Kidney_laceration_1cm_parenchymal_depth_of_renal_cortex_no_collecting_system_rupture_or_urinary_extravasation_moderate_OIS_III_) | !is.na(Kidney_contusion_hematoma_subcapsular_50_percent_surface_area_or_expanding_major_large_OIS_III_) ~ "Grade III",
                                                         !is.na(Kidney_laceration_1cm_parenchymal_depth_of_renal_cortex_no_urinary_extravasation_minor_superficial_OIS_II_) | !is.na(Kidney_contusion_hematoma_subcapsular_nonexpanding_confined_to_renal_retroperitoneum_minor_superficial_OIS_I_II_) ~ "Grade I-II",
                                                         !is.na(Kidney_laceration_NFS) | !is.na(Kidney_contusion_hematoma_NFS) | !is.na(Kidney_rupture) | !is.na(Kidney_NFS) ~ "Unspecified",
                                                         T  ~ "No injury"),
                                 liver = fct_case_when (
                                                        !is.na(Liver_laceration_hepatic_avulsion_total_separation_of_all_vascular_attachments_OIS_VI_) ~ "Grade VI",
                                                        !is.na(Liver_laceration_parenchymal_disruption_of_75_percent_of_hepatic_lobe_or_3_Couinards_segments_within_a_single_lobe_or_involving_retrohepatic_vena_cava_central_hepatic_veins_massive_complex_OIS_V_) ~ "Grade V",
                                                        !is.na(Liver_laceration_parenchymal_disruption_75_percent_hepatic_lobe_multiple_lacerations_3cm_deep_burst_injury_major_OIS_IV_) ~ "Grade IV",
                                                        !is.na(Liver_laceration_3cm_parenchymal_depth_major_duct_involvement_blood_loss_20_percent_by_volume_moderate_OIS_III_) | !is.na(Liver_contusion_hematoma_subcapsular_50_percent_surface_area_or_expanding_ruptured_subcapsular_or_parenchymal_intraparenchymal_10cm_or_expanding_major_OIS_III_) ~ "Grade III",
                                                        !is.na(Liver_laceration_simple_capsular_tears_3cm_parenchymal_depth_10cm_long_blood_loss_20_percent_by_volume_minor_superficial_OIS_II_) | !is.na(Liver_contusion_hematoma_subcapsular_50_percent_surface_area_or_nonexpanding_intraparenchymal_10cm_in_diameter_minor_superficial_OIS_I_II_) ~ "Grade I-II",
                                                        !is.na(Liver_laceration_NFS) | !is.na(Liver_rupture) | !is.na(Liver_contusion_hematoma_NFS) | !is.na(Liver_NFS) ~ "Unspecified",
                                                        T  ~ "No injury"),
                                         femur_fracture = ifelse(rowSums(!is.na( select(., contains("Femur")) )) >0, 'Injury', 'No injury'),
                                         tibia_fracture = ifelse(rowSums(!is.na( select(., contains("Tibia")) )) >0, 'Injury', 'No injury'),
                                         fibula_fracture = ifelse(rowSums(!is.na( select(., contains("Fibula")) )) >0, 'Injury', 'No injury'),
                                         #num_great_vessels = rowSums(!is.na( select(., contains('vascular_injuries_in_thorax')| contains('Vena') | contains('Aorta') | contains("Subclavian") | contains("Brachiocephalic")) ) ) ,
                                         # great_vessels = ifelse( rowSums(!is.na( select(., contains('vascular_injuries_in_thorax')| contains('Vena Cava, superior') | contains('Aorta, thoracic') | contains("Subclavian") | contains("Brachiocephalic") | contains('Pulmonary')) ) ) >0 , 'Injury', 'No injury'),
                                         great_vessels = ifelse( rowSums(!is.na( select(., contains('Aorta') ) ) ) >0 , 'Injury', 'No injury'),
                                         pelvis = fct_case_when(  rowSums(!is.na( select(., contains("incomplete_disruption_of_posterior") ) )) >0  ~ 'Pelvic ring fracture, incomplete disruption of posterior arch' ,
                                                            rowSums(!is.na( select(., contains("complete_disruption_of_posterior") ) )) >0  ~ 'Pelvic ring fracture, complete disruption of posterior arch' ,
                                                            rowSums(!is.na( select(., contains("posterior_arch_intact") ) )) >0  ~ 'Pelvic ring fracture, posterior arch intact' ,
                                                            rowSums(!is.na( select(., contains("Pelvic_ring_fracture_NFS") ) )) >0  ~ 'Pelvic ring fracture, not further specified' ,
                                                                               T  ~ "No injury"
                                                          ),
                                         lower_extremity_vascular = fct_case_when (
                                                                               !is.na(Femoral_artery_laceration_perforation_puncture_major_rupture_transection_segmental_loss_blood_loss_20_percent_by_volume) |
                                                                                   !is.na(Femoral_vein_laceration_perforation_puncture_major_rupture_transection_segmental_loss_blood_loss_20_percent_by_volume) |
                                                                                   !is.na(Popliteal_vein_laceration_perforation_puncture_major_rupture_transection_segmental_loss_blood_loss_20_percent_by_volume) |
                                                                                   !is.na(Other_named_arteries_e_g_distal_to_knee_or_small_lower_limb_arteries_laceration_perforation_puncture_major_rupture_transection_segmental_loss_blood_loss_20_percent_by_volume) |
                                                                                   !is.na(Other_named_veins_e_g_distal_to_knee_or_small_lower_limb_veins_laceration_perforation_puncture_major_rupture_transection_segmental_loss_blood_loss_20_percent_by_volume) ~
                                                                                   "Laceration, perforation, puncture major >20% volume loss",
                                                                               !is.na(Femoral_artery_laceration_perforation_puncture_minor_superficial_incomplete_circumferential_involvement_blood_loss_20_percent_by_volume) |
                                                                                   !is.na(Femoral_vein_laceration_perforation_puncture_minor_superficial_incomplete_circumferential_involvement_blood_loss_20_percent_by_volume) |
                                                                                   !is.na(Popliteal_artery_laceration_perforation_puncture_minor_superficial_incomplete_circumferential_involvement_blood_loss_20_percent_by_volume) |
                                                                                   !is.na(Popliteal_vein_laceration_perforation_puncture_minor_superficial_incomplete_circumferential_involvement_blood_loss_20_percent_by_volume) |
                                                                                   !is.na(Other_named_arteries_e_g_distal_to_knee_or_small_lower_limb_arteries_laceration_perforation_puncture_minor_superficial_incomplete_circumferential_involvement_blood_loss_20_percent_by_volume) |
                                                                                   !is.na(Other_named_veins_e_g_distal_to_knee_or_small_lower_limb_veins_laceration_perforation_puncture_minor_superficial_incomplete_circumferential_involvement_blood_loss_20_percent_by_volume) ~ 
                                                                                   "Laceration, perforation, puncture minor <20% volume loss",
                                                                               !is.na(Femoral_artery_intimal_tear_no_disruption) |
                                                                                   !is.na(Popliteal_artery_intimal_tear_no_disruption) |
                                                                                   !is.na(Other_named_arteries_e_g_distal_to_knee_or_small_lower_limb_arteries_intimal_tear_no_disruption)  ~ 
                                                                                   "Intimal tear",
                                                                               !is.na(Femoral_artery_laceration_perforation_puncture_NFS) |
                                                                                   !is.na(Femoral_vein_laceration_perforation_puncture_NFS) |
                                                                                   !is.na(Popliteal_artery_laceration_perforation_puncture_NFS) |
                                                                                   !is.na(Popliteal_vein_laceration_perforation_puncture_NFS)|
                                                                                   !is.na(Other_named_arteries_e_g_distal_to_knee_or_small_lower_limb_arteries_laceration_perforation_puncture_NFS) |
                                                                                   !is.na(Other_named_veins_e_g_distal_to_knee_or_small_lower_limb_veins_laceration_perforation_puncture_NFS | 
                                                                               !is.na(vascular_injuries_in_the_lower_extremity_NFS) |
                                                                                   !is.na(Femoral_artery_NFS) |
                                                                                   !is.na(Femoral_vein_NFS) |
                                                                                   !is.na(Popliteal_artery_NFS) |
                                                                                   !is.na(Popliteal_vein_NFS)|
                                                                                   !is.na(Other_named_veins_e_g_distal_to_knee_or_small_lower_limb_veins_NFS) |
                                                                                   !is.na(Other_named_arteries_e_g_distal_to_knee_or_small_lower_limb_arteries_NFS)) ~
                                                                                   "NFS",
                                                                               T  ~ "No injury"),
                                         iliac_vascular = fct_case_when (
                                                                     rowSums(!is.na( select(., starts_with('Iliac') & contains('major_rupture') ))) > 0 ~ "Laceration, perforation, puncture major >20% volume loss",
                                                                     rowSums(!is.na( select(., starts_with('Iliac') & contains('minor_superficial_incomplete') ))) > 0 ~ "Laceration, perforation, puncture minor <20% volume loss",
                                                                     rowSums(!is.na( select(., starts_with('Iliac') & contains('puncture_NFS') ))) > 0 | !is.na(Iliac_Artery_common_internal_external_intimal_tear_no_disruption)  ~ "Intimal tear, Laceration, perforation, puncture NFS",
                                                                     rowSums(!is.na( select(., starts_with('Iliac') & contains('NFS') )) ) > 0 | !is.na(Iliac_Artery_bilateral_for_common_iliac_artery_only)  ~ "NFS",
                                                                     T  ~ "No injury"
                                                                     ),
                                         any_vascular = fct_case_when (
                                                                   rowSums(!is.na( select(.,  contains('major_rupture') ))) > 0 ~ "Laceration, perforation, puncture major >20% volume loss",
                                                                   rowSums(!is.na( select(.,  contains('minor_superficial_incomplete') ))) > 0 ~ "Laceration, perforation, puncture minor <20% volume loss",
                                                                   rowSums(!is.na( select(.,  contains('puncture_NFS') ))) > 0 ~ "Laceration, perforation, puncture NFS",
                                                                   rowSums(!is.na( select(.,  contains('intimal_tear') ))) > 0 ~ "intimal tear, no disruption",
                                                                   #rowSums(!is.na( select(.,  contains('NFS') ))) > 0  ~ "NFS",
                                                                   T  ~ "No injury"
                                         )
)

table( puf_aiscpode$great_vessels, useNA="ifany")

cois  <- c( cois_, 'splenic', 'kidney', 'liver', 'femur_fracture', 'tibia_fracture', 'great_vessels', 'pelvis', 'lower_extremity_vascular', 'iliac_vascular', 'any_vascular')
puf <- puf %>% left_join(puf_aiscpode[,c('year', 'INC_KEY', setdiff(cois,cois_))])
puf <- puf %>% mutate_at( setdiff(cois,cois_), ~ replace_na(., 'No injury') ) 
puf %>% group_by(year) %>% summarise( mean( tibia_fracture != 'No injury'))


# Check a few of the AIS '98 examples to be extra sure the conversion worked
puf %>% filter( INC_KEY == 150021699) %>% select(c(INC_KEY, splenic)) %>% print(width=Inf)
puf %>% filter( INC_KEY == 160740762) %>% select(c(INC_KEY, liver)) %>% print(width=Inf)




################################
# Generate the ISS codes  
################################

# The region code is only in the 2017 version, so we need to link it

puf_aisdes_joined <- puf_aisdes %>% left_join(puf_aisdes_2017_, by=c('PREDOT'='AISPREDOT')) %>% select(PREDOT, ISSREGION, AISSEVERITY) 
puf_aisdes_joined$ISSREGION[ puf_aisdes_joined$PREDOT == '816021']  <- 1
puf_aisdes_joined$AISSEVERITY[ puf_aisdes_joined$PREDOT == '816021']  <- 3
puf_aisdes_joined$ISSREGION[ puf_aisdes_joined$PREDOT == '243000']  <- 2
puf_aisdes_joined$AISSEVERITY[ puf_aisdes_joined$PREDOT == '243000']  <- 1
puf_aisdes_joined$ISSREGION[ puf_aisdes_joined$PREDOT == '250699']  <- 2
puf_aisdes_joined$AISSEVERITY[ puf_aisdes_joined$PREDOT == '250699']  <- 1
puf_aisdes_joined <- puf_aisdes_joined %>% mutate ( region =  case_when( ISSREGION == 0 ~ 'No Map',
                                 ISSREGION == 1 ~ 'Head_Neck',
                                 ISSREGION == 2 ~ 'Face',
                                 ISSREGION == 3 ~ 'Thorax',
                                 ISSREGION == 4 ~ 'Abdomen',
                                 ISSREGION == 5 ~ 'Extremities',
                                 ISSREGION == 6 ~ 'External'))
sum(is.na(puf_aisdes_joined))
aiscopode_long_des <- puf_aiscpode_long %>% left_join(puf_aisdes_joined) 
ais_max_regions <- aiscopode_long_des %>% filter( AISSEVERITY != 9) %>% group_by(year,INC_KEY, region ) %>%  summarize(max_region = max(AISSEVERITY)) 
puf_aiscpode_long[rowSums(is.na(puf_aiscpode_long))>0,]
#aiscopode_long_des[rowSums(is.na(aiscopode_long_des))>0,]
ais_max_regions[rowSums(is.na(ais_max_regions))>0,]
ais_max_regions <- ais_max_regions %>% drop_na()
ais_max_regions <- ais_max_regions %>% pivot_wider( id_cols = c(year, INC_KEY), names_from = region, values_from = max_region)
ais_max_regions <- ais_max_regions %>% replace(is.na(.), 0)
#ais_max_regions$any_sixes  <- rowSums( ais_max_regions %>% select(Head_Neck:External) == 6) >0
ais_max_regions$any_sixes  <- rowSums( ais_max_regions %>% select( Head_Neck, Face, Thorax, Abdomen, Extremities, External) == 6) >0
ais_max_regions  <- ais_max_regions %>% 
    mutate ( ISS = sum(sort ( c( Head_Neck, Face, Thorax, Abdomen, Extremities, External), decreasing = T)[1:3]^2))%>% 
    mutate( ISS = ifelse( any_sixes, 75, ISS))



puf <- puf %>% left_join(ais_max_regions)
iss_cois <- c( 'Head_Neck', 'Face', 'Thorax', 'Abdomen', 'Extremities', 'External', 'ISS')
cois <-unique( c(cois,iss_cois))
injury_cois <- setdiff(cois,cois_)

# The NAs in ISS are patients who did not have any recorded injuries. We will leave these as NA.
# We assume that a patient in TQIP has to have some kind of injury, so if there are no injuries recorded
# then that patient's injuries are "missing." Hence, they will be removed as missing values, which is appropriate.
puf %>% filter( is.na(ISS) ) %>% select( INC_KEY, Head_Neck, Face, Thorax, Abdomen, Extremities, External)
#puf <- puf  %>% replace_na(list(ISS=0))
#puf %>% filter(ISS==0) %>% select( INC_KEY, Head_Neck, Face, Thorax, Abdomen, Extremities, External)

################################
# Identify REBOA cases 
################################
# Load the descriptions. These just make it easier to debug code, they are not used in producing the dataset
PUF_ICD9PROCEDURE_LOOKUP_2016 <- read_csv('data/PUF_AY_2016/CSV/PUF_PCODEDES.csv') %>% rename (ICD9_PCODE = PCODE, ICD9_Description = PCODEDESCR)
PUF_ICDPROCEDURE_LOOKUP_2017  <- read_csv('data/PUF_AY_2017/CSV/PUF_ICDPROCEDURE_LOOKUP.csv') %>% rename(ICD10_PCODE = ICDPROCEDURECODE, ICD10_Description = ICDProcedureCode_Desc )

# 

reboa_icd10_code_list <- c('02LW3DJ', '04L03DJ', '04L03DZ', '04L03ZZ', '04L04DZ', '04L04ZZ')
                      #02LW3DJ    04L03DJ,   04L03DZ,              04L04DZ, and .

# Check how many REBOAs are in 2015
PUF_PCODE_2015  <- read_csv('data/PUF_AY_2015/CSV/PUF_PCODE.csv') %>% select(  INC_KEY, PCODE, ICD10_PCODE, DAYTOPROC, HOURTOPROC) %>% rename( ICD9_PCODE = PCODE)
PUF_PCODE_2016  <- read_csv('data/PUF_AY_2016/CSV/PUF_PCODE.csv') %>% select(  INC_KEY, PCODE, ICD10_PCODE, DAYTOPROC, HOURTOPROC) %>% rename( ICD9_PCODE = PCODE)
PUF_PCODE_2017  <- read_csv('data/PUF_AY_2017/CSV/PUF_ICDPROCEDURE.csv') %>% rename( INC_KEY = Inc_Key, ICD10_PCODE = ICDPROCEDURECODE , DAYTOPROC=ProcedureDays)  %>% mutate( HOURTOPROC = ProcedureMins /60)  %>% select( INC_KEY, ICD10_PCODE, DAYTOPROC, HOURTOPROC)

PUF_PCODES <- bind_rows(list('2015'=PUF_PCODE_2015, "2016"=PUF_PCODE_2016, "2017"=PUF_PCODE_2017),.id = 'year') %>%
                                                                        left_join(PUF_ICDPROCEDURE_LOOKUP_2017) %>%
                                                                        left_join(PUF_ICD9PROCEDURE_LOOKUP_2016) %>%
                                                                        filter(ICD10_PCODE %in% reboa_icd10_code_list | ICD9_PCODE == '39.78' ) 

                                                                    
# There are (very few) records where the number of days is not consistent with the number of hours
PUF_PCODES <- PUF_PCODES %>%  mutate( day_hour_mismatch = (DAYTOPROC -1)*24> HOURTOPROC , HOURTOPROC = ifelse( day_hour_mismatch , (DAYTOPROC-1)*24+HOURTOPROC , HOURTOPROC))


# If there is a REBOA ICD10 code, then use it. If there is not, use the ICD9 code.
PUF_PCODES <- PUF_PCODES %>% mutate( reboa_pcode = case_when ( !is.na(ICD10_PCODE) &ICD10_PCODE!= '-1' & ICD10_PCODE != '-2'  ~ ICD10_PCODE, !is.na(ICD9_PCODE) &ICD9_PCODE!= '-1' & ICD9_PCODE != '-2' ~ ICD9_PCODE , T ~ NA_character_ ) )
stopifnot ( sum(is.na( PUF_PCODES$reboa_pcode)) == 0)
puf_pcodes <- PUF_PCODES %>% group_by( year, INC_KEY) %>% summarize( reboa_pcode = first(reboa_pcode), reboa_hourto =  ifelse( sum(!is.na(HOURTOPROC)) >0, min(HOURTOPROC, na.rm = T), NA)  )

puf  <- puf %>% left_join(puf_pcodes[,c('year', 'INC_KEY', 'reboa_pcode', 'reboa_hourto')]) %>% mutate( reboa = ifelse (is.na(reboa_pcode) , 'No REBOA', 'REBOA'))



################################
# Amputation 
################################

PUF_PCODES_AMPUTATION <- bind_rows(list('2015'=PUF_PCODE_2015, "2016"=PUF_PCODE_2016, "2017"=PUF_PCODE_2017),.id = 'year') %>%
                                                                        left_join(PUF_ICDPROCEDURE_LOOKUP_2017) %>%
                                                                        left_join(PUF_ICD9PROCEDURE_LOOKUP_2016) 
# These are the amputation codes
PUF_ICDPROCEDURE_LOOKUP_2017  %>% filter(grepl('^0Y6[2-8]|^0Y6[CDFHJ]', ICD10_PCODE ))%>% print(n=Inf) 
PUF_ICD9PROCEDURE_LOOKUP_2016  %>% filter(grepl('^84.1[5-9]',ICD9_PCODE ))  %>% rename ( PCODE = ICD9_PCODE) %>% print(n = Inf)
#PUF_PCODES_AMPUTATION %>% filter(grepl('^0Y6[2-8]|^0Y6[CDFHJ]', ICD10_PCODE )  | grepl('^84.1[5-9]',ICD9_PCODE )) %>%   
# Which patients have had an amputation, based on ICD10 or ICD9 codes?
puf_pcodes_amputation <- PUF_PCODES_AMPUTATION %>% filter(grepl('^0Y6[2-8]|^0Y6[CDFHJ]', ICD10_PCODE )  | grepl('^84.1[5-9]',ICD9_PCODE )) %>%  group_by(year, INC_KEY) %>% summarize(days_to_amputation = min(DAYTOPROC)) %>% mutate( days_to_amputation = replace_na(days_to_amputation,-1 ))
#PUF_PCODES %>% filter(grepl('^0Y6[2-8]|^0Y6[CDFHJ]', ICD10_PCODE )  | grepl('^84.1[5-9]',PCODE )) %>%  filter(year == 2016 & ICD10_PCODE %in% c('-1', '-2') )

# What is the maximum severity lower extremity injury from AIS?
# AIS codes corresponding to lower extremity injury start with 8
le_injury <- aiscopode_long_des %>% filter(grepl('^8', PREDOT)) %>% group_by(year, INC_KEY) %>% summarize(  le_injury = max( AISSEVERITY),  le_injury_descriptions= paste0(description, collapse = " ||| ") )
puf <- puf %>% left_join(puf_pcodes_amputation) %>%left_join(le_injury)  %>% mutate(major_amputation_filtered = ifelse(  !is.na(days_to_amputation) & days_to_amputation > 1 & is.na(le_injury), 1, 0),  major_amputation = ifelse ( is.na(days_to_amputation) ,0,1 )  )
outcomes_ <- c(outcomes_, 'major_amputation' ,'major_amputation_filtered' )

puf <- puf %>% mutate_at( outcomes_, ~ifelse( . , 'Complication', 'No complication'))

cois  <- c(cois, 'days_to_amputation', 'le_injury', 'major_amputation_filtered', 'major_amputation')



################################
# Filtering and reporting number of patietns at each step 
################################
baseline_variables<-c( "AGE","GENDER","RACE","TEACHINGSTATUS",
                      "SBP","PULSE","RR","RRAQ",
                      "GCSEYE","GCSVERB","GCSMOT","GCS_Q1","ISS",'Head_Neck',
                      'Face', 'Thorax', 'Abdomen', 'Extremities', 'External',
                      'splenic', 'kidney', 'liver', 'femur_fracture',
                      'tibia_fracture',  'pelvis', 'lower_extremity_vascular',
                      'iliac_vascular', 'any_vascular')
puf <- puf %>% mutate( reboa_with_time = case_when( 
                                                    reboa == 'REBOA' & reboa_hourto <=1 ~ 'REBOA within 1 hours', 
                                                    reboa == 'REBOA' & reboa_hourto <=2 ~ 'REBOA within 2 hours', 
                                                    reboa == 'REBOA' ~ 'REBOA later than 2 hours', 
                                                    T ~ 'No REBOA' ))
colSums(is.na(puf[puf$reboa == 'REBOA',baseline_variables]))
puf %>% filter(ISS ==0 & reboa == 'REBOA') %>% print(width=Inf)
puf %>% filter( reboa == 'REBOA') %>% print(width=Inf)
puf$missing_baseline_variable  <- rowSums( is.na( puf[,baseline_variables]) ) > 0 
num.pts  <- function( A_tmp ) {
    cat(sprintf(' Total of %s patients, with %d who underwent REBOA\n', prettyNum(nrow(A_tmp), big.mark=','), sum(A_tmp$reboa == 'REBOA')))
    cat(sprintf(' No REBOA %s patients, REBOA %d\n', prettyNum(sum(A_tmp$reboa == 'No REBOA'), big.mark=','), sum(A_tmp$reboa == 'REBOA')))
}

# Supplemental Figure 1 Flow Diagram
# How many patietns are there before any exclusion?
num.pts(puf)
#puf %>% filter ( reboa == 'REBOA' & year == '2015')  %>% select(INC_KEY)

# First stage of exclusion:
# Get the number to be removed for each  exclusion criterion
A_exclusions <- puf %>% mutate ( 
                      age_exclusion = !AGE >= 18, 
                      transfer_exclusion = !TRANSFER  == 'No',
                      great_vessel_exclusion = !great_vessels  == 'No injury', 
                      signs_of_life_exclusion = SIGNSOFLIFE != 'Arrived with signs of life')
A_exclusions  %>% select(  reboa, age_exclusion:signs_of_life_exclusion) %>% group_by(reboa) %>% summarise_all(~sum(.==T, na.rm = T))
#A_missingness %>% filter( reboa == 'REBOA') %>% select(all_of(baseline_variables), age_exclusion:signs_of_life_exclusion) %>% colSums (., na.rm = T) %>% t %>% t

A_withmissing_withlatereboa <- puf %>% filter ( 
                      AGE >= 18  & 
                      TRANSFER  == 'No' & 
                      great_vessels  == 'No injury'& 
                      SIGNSOFLIFE == 'Arrived with signs of life' ,
                      #!missing_baseline_variable 
                      #reboa_with_time %in% c('REBOA within 1 hours','REBOA within 2 hours',  'No REBOA') & 
                        )
num.pts(A_withmissing_withlatereboa)
table( A_withmissing_withlatereboa$reboa, useNA="ifany")

# Second stage of exclusion:  remove the missing values
# How many have missing values?
A_withmissing_withlatereboa %>% group_by(reboa) %>% summarise(s=sum(missing_baseline_variable) )
A_withlatereboa <- A_withmissing_withlatereboa %>% filter ( 
                      !missing_baseline_variable 
                      #reboa_with_time %in% c('REBOA within 1 hours','REBOA within 2 hours',  'No REBOA') & 
                        )
num.pts(A_withlatereboa)
table( A_withlatereboa$reboa_with_time, useNA="ifany")

# Final stage of exclusion: time of rEBOA 
A <- A_withlatereboa %>% filter ( 
                      reboa_with_time %in% c('REBOA within 1 hours','REBOA within 2 hours',  'No REBOA') 
                        )
A %>% filter(reboa == 'REBOA' ) %>% count(year) 
num.pts(A)

################################
# REport characteristics of excluded patients
################################

nrow(A)
nrow(puf)

A_excluded = puf[!puf$INC_KEY %in%  A$INC_KEY,]
label_list = list (
                  "AGE"                     = 'Age',
                   "GENDER"                  = 'Gender',
                   "RACE"                    = 'Race',
                   "TEACHINGSTATUS"          = 'Teaching Status',
                   "SBP"                     = 'Systolic Blood Pressure',
                   "PULSE"                   = 'Pulse',
                   "RR"                      = 'Respiratory Rate',
                   "GCS"                      = 'Glasgow Coma Score',
                   "Head_Neck"               = 'Head and Neck',
                   "splenic"                 = 'Spleen',
                   "kidney"                  = 'Kidney',
                   "liver"                   = 'Liver',
                   "femur_fracture"          = 'Femur Fracture',
                   "tibia_fracture"          = 'Tibia Fracture',
                   "pelvis"                  = 'Pelvis Fracture',
                   "lower_extremity_vascular" = 'Lower Extremity Vascular',
                   "iliac_vascular"         = 'Iliac vessels',
                   "any_vascular"           = 'Any vascular'
                                  )

A_excluded  <- A_excluded %>% mutate( GCS = GCSEYE + GCSVERB + GCSMOT)
covariates_toprint <- c('AGE', 'GENDER', 'RACE',  'TEACHINGSTATUS', 'SBP', 'PULSE', 'RR','GCS',  'Head_Neck', 'Face', 'Thorax', 'Abdomen', 'Extremities', 'External', 'splenic', 'kidney', 'liver', 'femur_fracture', 'tibia_fracture',  'pelvis', 'lower_extremity_vascular', 'iliac_vascular', 'any_vascular', 'ISS')
labels(A_excluded) <- label_list

# Usng Arsenal
tblcontrol <- tableby.control( numeric.simplify = T, digits = 1,total = F,test = F, numeric.stats=c('Nmiss', 'meansd' ))
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
excluded_tbl <- tableby(reboa ~ ., data = A_excluded[, c('reboa',  covariates_toprint)], control = tblcontrol)
summary(excluded_tbl, text=T) %>% as.data.frame %>% write_csv('output/excluded_tbl.csv')

################################
# The missing data analysis 
################################

A_missing_indicators_withlatereboa  <- A_withmissing_withlatereboa  %>% mutate(RACE = ifelse( is.na(RACE), 'Missing', RACE),
                                                                               SBP_missing = is.na( SBP), SBP = ifelse( is.na(SBP), mean(SBP,na.rm = T), SBP),
                                                                               PULSE_missing = is.na( PULSE), PULSE = ifelse( is.na(PULSE), mean(PULSE,na.rm = T), PULSE),
                                                                               RR_missing = is.na( RR), RR = ifelse( is.na(RR), mean(RR,na.rm = T), RR),
                                                                               GCS_Q1 = ifelse( is.na(GCS_Q1), 'Missing', GCS_Q1),
                                                                               RRAQ = ifelse( is.na(RRAQ), 'Missing', RRAQ))
missing_indicators <- c('SBP_missing', 'PULSE_missing', 'RR_missing')
sapply( A_missing_indicators_withlatereboa, class)

A_missing_indicators_withlatereboa$missing_baseline_variable  <- rowSums( is.na( A_missing_indicators_withlatereboa[,baseline_variables]) ) > 0 
A_missing_indicators_withlatereboa <- A_missing_indicators_withlatereboa %>% filter ( 
                      !missing_baseline_variable  
                      #reboa_with_time %in% c('REBOA within 1 hours','REBOA within 2 hours',  'No REBOA') 
                        )
num.pts(A_missing_indicators_withlatereboa)
A_missing_indicators <- A_missing_indicators_withlatereboa %>% filter ( 
                      reboa_with_time %in% c('REBOA within 1 hours','REBOA within 2 hours',  'No REBOA') 
                        )
num.pts(A_missing_indicators)

saveRDS(object=A_missing_indicators[, c('INC_KEY', 'reboa','year', 'le_injury_descriptions', 'reboa_hourto', cois,missing_indicators, 'missing_baseline_variable' )], file = 'data/A_missing_indicators.rds')
################################
# Report number of REBOAS by time and year 
################################
# Supplemental Table: By times 
# Table 1A
supp_table <- A_withlatereboa %>% filter( reboa == 'REBOA') %>% mutate( reboa_time  = case_when( reboa_hourto < 1  ~ '<1 hour',
                                                                                                                   reboa_hourto < 2 ~ '1-2 hours',
                                                                                                                  reboa_hourto < 3 ~ '2-3 hours',
                                                                                                                  reboa_hourto >=3 ~ '>3 hours', 
                                                                                                                  T ~ 'Unknown time'))%>%
                                                 count( reboa_pcode, reboa_time) %>%
                                                complete(reboa_pcode,reboa_time, fill = list(n=0)) %>%  pivot_wider( id_cols = reboa_pcode, names_from = reboa_time, values_from = n)
supp_table  <- supp_table %>% left_join( PUF_ICDPROCEDURE_LOOKUP_2017[,c('ICD10_PCODE', 'ICD10_Description')], by = c( 'reboa_pcode' = 'ICD10_PCODE')) %>% rename (Description = ICD10_Description)
supp_table$Description[ supp_table$reboa_pcode == '39.78' ] = 'Endovascular implantation of branching or fenestrated graft(s) in aorta'
supp_table <- supp_table %>% unite('name', reboa_pcode,  Description, sep = ' - ')
supp_table  <- supp_table %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
supp_table %>% write_csv('output/supplemental_table_reboa_codes_time.csv')

# Table 1B
# Supplemental Table: REBOA codes by year, after filtering
supp_table <- A %>% filter( reboa == 'REBOA') %>%count(year, reboa_pcode) %>% complete(year, reboa_pcode, fill = list(n=0)) %>%  pivot_wider( id_cols = reboa_pcode, names_from = year, values_from = n)
supp_table  <- supp_table %>% left_join( PUF_ICDPROCEDURE_LOOKUP_2017[,c('ICD10_PCODE', 'ICD10_Description')], by = c( 'reboa_pcode' = 'ICD10_PCODE')) %>% rename (Description = ICD10_Description)
supp_table$Description[ supp_table$reboa_pcode == '39.78' ] = 'Endovascular implantation of branching or fenestrated graft(s) in aorta'
supp_table <- supp_table %>% unite('name', reboa_pcode,  Description, sep = ' - ')
supp_table  <- supp_table %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
supp_table %>% write_csv('output/supplemental_table_reboa_codes_year.csv')


################################
# Output dataset for propensity score code 
################################

saveRDS(object=A[, c('INC_KEY', 'reboa','year', 'le_injury_descriptions', 'reboa_hourto', cois, 'missing_baseline_variable' )], file = 'data/A_alldeceased.rds')


################################
# Look at some amputations 
################################
matched_injuries <- A %>% filter( reboa  == 'REBOA'& major_amputation == 'Complication') %>% select( AGE, SBP,  deceased, days_to_amputation, le_injury, le_injury_descriptions)
matched_injuries %>% write_tsv('output/reboa_amputations.tsv')


