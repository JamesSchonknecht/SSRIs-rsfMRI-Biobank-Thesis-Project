# This script groups participants into three groups based on ever having an anxiety disorder and SSRI use at imaging session 1
# An rds file called `all_groups_dataframe.rds` will be created containing subjects seperated into groups (make sure the saveRDS line at the bottom of this script is uncommented)

library(tidyverse)

cat("This script takes approximately 2-3 minutes to complete \n\n")
###############################################################################################################################################
# READING IN SAVED DATAFRAME FROM .rds FILE ()
###############################################################################################################################################
ukbb_filtered_full_data <- readRDS(file = "Data/ukbb_filtered_full_data.rds")

###############################################################################################################################################
# rs-fMRI DATA
###############################################################################################################################################

# Find participants who attended imaging sessions and had fMRI data collected (Participants who have a rfMRI NIFTI image available)
# Only participants with usable NIFTI data are included (indicated by a participant having a non NA value in the dimension 25 correlation matrix data-field)
# The first imaging session (2014+) and the repeat imaging session (2019+) are separated to enable separate analysis

# Determine the number of participants who had unusable NIFTI data (for reporting later)
unusable_NIFTI_participants <- 
  ukbb_filtered_full_data %>% 
  filter(!is.na(`20227-2.0`)) %>% # Participants who have rfMRI NIFTI data available to download
  filter(is.na(`25752-2.0`)) # Participants who have unusable NIFTI data (participants don't have a correlation matrix available if their NIFTI data was unusable)
cat("Number of participants removed due to having unusable NIFTI data:", nrow(unusable_NIFTI_participants), "\n")


# Filtering first imaging session participants who have usable NIFTI data
rsfmri_session_1 <- ukbb_filtered_full_data %>% 
  filter(!is.na(`20227-2.0`)) %>% # Participants who have rfMRI NIFTI data available to download
  filter(!(is.na(`25752-2.0`))) # Excludes participants who have unusable NIFTI data (participants don't have a correlation matrix available if their NIFTI data was unusable)

# Repeat imaging session participants
  # rsfmri_session_2 <- ukbb_filtered_full_data %>% 
  #   filter(if_any(starts_with("20227-3"), ~ !is.na(.)))

###############################################################################################################################################
# EXCLUDING CONFOUNDING CONDITIONS
###############################################################################################################################################
# Calls a function from the script "02_excluding_confound_diagnoses.R" which eliminates participants with one or more diagnoses which may confound results
# See that script for more detail on diagnoses excluded and data fields used
source("Code/R_code/excluding_confound_diagnoses.R")
ukbb_diagnoses_excluded_data <- eliminate_confound_diagnoses()


###############################################################################################################################################
# MENTAL HEALTH CONDITIONS
###############################################################################################################################################

# Below are the numerical codes corresponding to each anxiety disorder which was self-reported as diagnosed by a professional (Data-Field 20544)
# These were self-reported by participants in the online follow-up questionnaire
# Question asked: "Have you been diagnosed with one or more of the following mental health problems by a professional, even if you don't have it currently? (tick all that apply)
  # Social anxiety or social phobia: 1
  # Panic attacks: 6
  # Anxiety, nerves or generalized anxiety disorder: 15
  # Agoraphobia: 17
  # Any other phobia (e.g. disabling fear of heights or spiders): 5 (THIS WILL NOT BE INCLUDED AND THEREFORE IS NOT INCLUDED IN anxiety_disorder_codes)
anxiety_disorder_codes_self_reported_online <- c(1, 6, 15, 16)

# Below is the numerical code corresponding to a participant having reported having "anxiety/panic attacks" at any of the assessment center verbal interviews prior to and including the first imaging session 
# (Data-Field 20002)
anxiety_disorder_codes_self_reported_verbal <- 1287

# Below are the ICD-9 (International Classification of Diseases 9) diagnosis codes for anxiety disorders (Data-Field 41271)
# These codes were recorded across all of a subject's hospital admissions
  # Anxiety states: 3000 
    # Includes:
      # Anxiety state, unspecified, 
      # Panic disorder without agoraphobia, 
      # Generalized anxiety disorder, 
      # Other anxiety, dissociative, and somatoform disorders
anxiety_disorder_codes_ICD9 <- c("3000")

# Below are the ICD-10 (International Classification of Diseases 10) diagnosis codes for anxiety disorders (Data-Field 41270)
# These codes were recorded across all of a subject's hospital admissions
  # Agoraphobia: F400
  # Social phobias: F401
  # Panic disorder [episodic paroxysmal anxiety]: F410
  # Generalized anxiety disorder: F411
anxiety_disorder_codes_ICD10 <- c("F400", "F401", "F410", "F411")

# Calls a function from a separate script (GAD_field_extraction.R) which extracts a variable containing all eids of subjects meeting the-
# symptom-based criteria for GAD (generalized anxiety disorder) as defined by Davis et al.(https://doi.org/10.1002/mpr.1796)
# See the script called "GAD_field_extraction.R" for these criteria
source("Code/R_code/GAD_field_extraction.R")
case_eids_symptom_based_GAD <- 
  get_symptom_based_GAD_case_eids(ukbb_filtered_full_data, GAD_symptoms_data_fields)


# Filters participants who have rs-fmri data to find participants with AT LEAST one anxiety disorder diagnosis
# Uses five different data-fields (including symptom-based GAD cases from above) to find anxiety case subjects

# Online follow-up mental health questionnaire: self-reported anxiety
case_eids_20544 <-
  ukbb_diagnoses_excluded_data %>% 
    filter(if_any(starts_with("20544-"), ~ . %in% anxiety_disorder_codes_self_reported_online)) %>% 
    select("eid")

# Assessment center verbal interview: self-reported anxiety/panic attacks
case_eids_20002 <- 
  ukbb_diagnoses_excluded_data %>% 
  filter(if_any(starts_with(c("20002-0", "20002-1", "20002-2")), ~ . == anxiety_disorder_codes_self_reported_verbal)) %>% 
  select("eid")
  
# ICD9 hospital anxiety diagnoses
case_eids_41271 <-
  ukbb_diagnoses_excluded_data %>% 
  filter(if_any(starts_with("41271-"), ~ . %in% anxiety_disorder_codes_ICD9)) %>% 
  select("eid")

# ICD10 hospital anxiety diagnoses
case_eids_41270_dates_ignored <-
  ukbb_diagnoses_excluded_data %>% 
  filter(if_any(starts_with("41270-"), ~ . %in% anxiety_disorder_codes_ICD10)) %>% 
  filter(!(is.na(`53-2.0`)))

columns_41270 <- 
  ukbb_diagnoses_excluded_data %>% 
  filter(eid %in% case_eids_41270_dates_ignored$eid) %>% 
  select(starts_with(c("eid", "41270-", "41280-", "53-2.0"))) %>% 
  mutate(across(starts_with("eid"), as.integer)) %>%
  mutate(across(starts_with("41270-"), as.character)) %>% 
  mutate(across(starts_with("41280-"), as.Date)) %>% 
  mutate(across(starts_with("53-2.0"), as.Date))

code_col_names <- 
  ukbb_diagnoses_excluded_data %>% 
  select(starts_with("41270-")) %>% 
  names()

final_columns_41270 <- tibble(code_col_name = character(), eid = integer(), code = character(), diagnosis_date = Date(), assessment_centre_date = Date())
for (code_col_name in code_col_names) { # Loop through each column name starting with 41270
  date_col_name <- sub("70-", "80-", code_col_name)
  
  index <- 0
  for (code in columns_41270[[code_col_name]]) { # Loop through each diagnosis code within one column
    index <- index + 1
    if ((code %in% anxiety_disorder_codes_ICD10) & (columns_41270[[index, date_col_name]] < columns_41270[[index, "53-2.0"]])) {
      final_columns_41270[nrow(final_columns_41270) + 1,] <- list(code_col_name,
                                                                  columns_41270[[index, "eid"]],
                                                                  columns_41270[[index, code_col_name]],
                                                                  columns_41270[[index, date_col_name]],
                                                                  columns_41270[[index, "53-2.0"]] # The date that the participant attended the first biobank imaging session
                                                                  )
    }
  }
}

case_eids_41270 <- 
  final_columns_41270 %>% 
  select(eid)

# Combining all case eids from all four data fields into one variable
# Each participant is included once only (duplicates removed) - if a participant meets the case criteria for more than one data-field/criteria, there won't be multiple entries of their information
case_eids_all <-  
  bind_rows(case_eids_symptom_based_GAD, case_eids_20544, case_eids_20002, case_eids_41271, case_eids_41270) %>% 
  distinct(eid)

# Uncomment to view number of case participants from each data-field/criteria
  cat("Anxiety cases from mental health questionnaire symptoms: ", nrow(case_eids_symptom_based_GAD), "\n", sep = "")
  cat("Anxiety cases from data-field 20544 (Online follow-up mental health questionnaire: self-reported anxiety): ", nrow(case_eids_20544), "\n", sep = "")
  cat("Anxiety cases from data-field 20002 (Assessment center verbal interview: self-reported anxiety/panic attacks): ", nrow(case_eids_20002), "\n", sep = "")
  cat("Anxiety cases from data-field 41271 (ICD9 hospital diagnoses): ", nrow(case_eids_41271), "\n", sep = "")
  cat("Anxiety cases from data-field 41270 (ICD10 hospital diagnoses): ", nrow(case_eids_41270), "\n", sep = "")
  cat("Total number of anxiety cases: ", nrow(case_eids_all), "\n\n", sep = "")
  
  

# Finding participants who have rs-fmri data from the first imaging session AND meet the case criteria of ever having an anxiety disorder
rsfmri_session_1_any_anxiety <-
  rsfmri_session_1 %>% 
  filter(eid %in% case_eids_all$eid)
  

###############################################################################################################################################
# MEDICATIONS
###############################################################################################################################################

# Find the participants which are taking SSRI medications
# Medicine codes correspond to medications in Data-Field 20003: Treatment/medication code
# Some participants had their SSRI medication coded as a brand name rather than its generic name
# Medicine codes for SSRIs:
# citalopram: 1140921600 
# escitalopram: 1141180212
# fluoxetine: 1140879540 
# fluvoxamine: 1140879544
# paroxetine: 1140867888 
# sertraline: 1140867878
# Cipramil (Citalopram): 1141151946
# Cipralex (Escitalopram): 1141190158
# Prozac (Fluoxetine): 1140867876
# Oxactin (Fluoxetine): 1141174756
# Faverin (Fluvoxamine): 1140867860
# Seroxat (Paroxetine): 1140882236
# Lustral (Sertraline): 1140867884
ssri_codes <- c(1140921600, 1141180212, 1140879540, 1140879544, 
                1140867888, 1140867878, 1141151946, 1141190158, 
                1140867876, 1141174756, 1140867860, 1140882236, 1140867884)


###############################################################################################################################################
# GROUPS FOR COMPARISON: IMAGING SESSION 1
###############################################################################################################################################

# Case groups:
# Anxiety Disorder + SSRI
# Anxiety Disorder + No medications

# Control group:
# Mentally Healthy + No medications


# Session 1 Anxiety Disorder + SSRI
anxiety_ssri_session_1 <- rsfmri_session_1_any_anxiety %>% 
  filter(if_any(starts_with("20003-2"), ~ . %in% ssri_codes)) %>% 
  filter(if_all(starts_with("20003-2"), ~ . %in% ssri_codes | is.na(.))) %>% # This line excludes any participants taking non-SSRI medications
  cbind(group = "anxiety_ssri_session_1", .) # Adds a column indicating the group name for participants within this group
  
# Session 1 Anxiety Disorder + No medications
anxiety_no_medications_session_1 <- rsfmri_session_1_any_anxiety %>% 
  filter(if_all(starts_with("20003-2"), ~ is.na(.))) %>% 
  cbind(group = "anxiety_no_medications_session_1", .) # Adds a column indicating the group name for participants within this group


# Session 1 Mentally Healthy + No medications
controls_session_1 <- rsfmri_session_1 %>% 
  filter((if_all(starts_with(c("20003-2", "20544-")), ~ is.na(.)))) %>% 
  cbind(group = "controls_session_1", .) # Adds a column indicating the group name for participants within this group


# Display the number of participants in each group for FIRST imaging session
cat(
  "FIRST imaging session groups:\n",
  "Session 1 Anxiety Disorder + SSRI:", nrow(anxiety_ssri_session_1),"\n",
  "Session 1 Anxiety Disorder + No medications:", nrow(anxiety_no_medications_session_1),"\n",
  "Session 1 Mentally Healthy + No medications:", nrow(controls_session_1),"\n\n"
)

# Function that saves groups as a data frame in an rds file for quick access
save_groups_to_rds <- function(anxiety_ssri_session_1, anxiety_no_medications_session_1, controls_session_1) {
  all_groups_dataframe <- bind_rows(anxiety_ssri_session_1, anxiety_no_medications_session_1, controls_session_1)
  saveRDS(all_groups_dataframe, file = "Data/all_groups_dataframe.rds")
}
# Uncomment and run the line below when rds file needs to be created
# save_groups_to_rds(anxiety_ssri_session_1, anxiety_no_medications_session_1, controls_session_1)
cat("The grouped participants data has been saved in the directory \"Data\" as an rds file: \"all_groups_dataframe.rds\"", "\n\n")


