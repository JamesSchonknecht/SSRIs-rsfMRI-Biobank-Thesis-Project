# This script defines the codes across multiple different data-fields which correspond to 
# diagnoses which may act as confounding factors in the group comparisons
# These diagnoses are also excluded by running this script
# The main output is 'filtered_data_list[[1]]'
  # This is a data frame with all participants with one or more confounding diagnoses excluded

library(tidyverse)

###############################################################################################################################################
# ONLINE FOLLOW-UP QUESTIONNAIRE SELF-REPORTED DIAGNOSES (DATA-FIELD 20544)
###############################################################################################################################################

# Diagnoses are coded with integer numbers (Data-Field 20544)
# Schizophrenia: 2
# Any other type of psychosis or psychotic illness: 3
# Obsessive compulsive disorder (OCD): 7
# Mania, hypomania, bipolar or manic-depression: 10 
# Prefer not to answer (group A): -818	
# Prefer not to answer (group B): -819
exclusion_diagnosis_codes_20544 <- c(2, 3, 7, 10, -818, -819)

###############################################################################################################################################
# SELF-REPORTED DIAGNOSES (DATA-FIELDS 20001 AND 20002)
###############################################################################################################################################

# Self-reported cancer (Data-Field 20001)
  # All cancer diagnoses will be excluded (i.e. if there are any non NA values in field 20001 corresponding to a time before imaging session occurred)


# This saves the code numbers for Data-Field 20002 as a string
comment_20002 <- "
  # Self-reported neurological diagnoses
    # brain abscess/intracranial abscess: 1245
    # cranial nerve problem/palsy: 1249
    # bell's palsy/facial nerve palsy: 1250
    # trigemminal neuralgia: 1523
    # spinal cord disorder: 1251
    # paraplegia: 1252
    # spina bifida: 1524	
    # peripheral nerve disorder: 1254
    # acute infective polyneuritis/guillain-barre syndrome: 1256
    # chronic/degenerative neurological problem: 1258
    # epilepsy: 1264	
    # migraine: 1265
    # cerebral palsy: 1433
    # other neurological problem: 1434
    # myasthenia gravis: 1437
    # myasthenia gravis (#2): 1260
    # benign / essential tremor: 1525
    # polio / poliomyelitis: 1526
    # meningioma / benign meningeal tumour: 1659
    # benign neuroma: 1683
    # neurological injury/trauma: 1240
    # head injury: 1266
    # spinal injury: 1267
    # motor neurone disease: 1259
    # multiple sclerosis: 1261
    # parkinsons disease: 1262
    # dementia/alzheimers/cognitive impairment: 1263
    # other demyelinating disease (not multiple sclerosis): 1397
  # Self-reported psychiatric diagnoses
    # schizophrenia: 1289
    # mania/bipolar disorder/manic depression: 1291
    # alcohol dependency: 1408
    # opioid dependency: 1409
    # other substance abuse/dependency: 1410
    # post-traumatic stress disorder: 1469	
    # obsessive compulsive disorder: 1615
  # Self-reported cardiovascular diagnoses
    # stroke: 1081
    # ischaemic stroke: 1583
    # transient ischaemic attack: 1082
    # subdural haemorrhage/haematoma: 1083
    # subarachnoid haemorrhage: 1086
    # heart failure/pulmonary oedema: 1076
    # peripheral vascular disease: 1067
    # heart attack/myocardial infarction: 1075
  # Self-reported pulmonary diagnoses
    # respiratory failure: 1124
"

# Extracts the numerical codes for self-reported diagnoses from the above comment string
comment_20002 <- str_remove_all(comment_20002, "\t")
str_view_all(comment_20002, regex("(?<=: )(\\d+$)", multiline = TRUE))
exclusion_diagnosis_codes_20002 <- str_extract_all(comment_20002, regex("(?<=: )(\\d+$)", multiline = TRUE))[[1]]
exclusion_diagnosis_codes_20002 <- as.numeric(exclusion_diagnosis_codes_20002)


###############################################################################################################################################
# ICD9 HOSPITAL DIAGNOSES (DATA-FIELD 41271) + 
# ICD10 HOSPITAL DIAGNOSES (DATA-FIELD 41270)
###############################################################################################################################################

# This saves the code numbers for ICD9 hospital diagnoses (Data-Field 41271) as a string
comment_41271 <- "
# ICD9 Hospital Diagnoses
  # Mental Disorders
    # Senile and presenile organic psychotic conditions: 290
    # Schizophrenic psychoses: 295
    # Affective psychoses: 296
    # Obsessive-compulsive disorders: 3003
    # Specific nonpsychotic mental disorders following organic brain damage: 310
    # Mental retardation: 317-319

  # Diseases of the nervous system and the sense organs
    # Intracranial abscess: 3240
    # Hereditary and degenerative diseases of the central nervous system: 330-337
    # Other disorders of the central nervous system: 340-349
    # Myasthenia gravis: 3580
    
  # Diseases of the circulatory system
    # Cerebrovascular disease: 430-438
" 
  

# This saves the code numbers for ICD10 hospital diagnoses (Data-Field 41270) as a string
comment_41270 <- "
# ICD10 Hospital Diagnoses
  # Mental and behavioural disorders
    # Dementia in Alzheimer's disease: F00
    # Vascular dementia: F01
    # Dementia in other diseases classified elsewhere: F02
    # Unspecified dementia: F03
    # Organic amnesic syndrome, not induced by alcohol and other psychoactive substances: F04
    # Other mental disorders due to brain damage and dysfunction and to physical disease: F06
    # Personality and behavioural disorders due to brain disease, damage and dysfunction: F07
    # Schizophrenia: F20
    # Persistent delusional disorders: F22
    # Acute and transient psychotic disorders: F23 
    # Induced delusional disorder: F24 
    # Schizoaffective disorders: F25 
    # Other nonorganic psychotic disorders: F28 
    # Unspecified nonorganic psychosis: F29 
    # Manic episode: F30 
    # Bipolar affective disorder: F31 
    # Obsessive-compulsive disorder: F42 
    # Mental retardation: F70-F79 

  # Diseases of the nervous system
    # Systemic atrophies primarily affecting the central nervous system: G10-G14
    # Parkinson's disease: G20
    # Other degenerative diseases of the nervous system: G30-G32
    # Demyelinating diseases of the central nervous system: G35-G37
    # Epilepsy: G40
    # Status epilepticus: G41
    # Migraine: G43
    # Transient cerebral ischaemic attacks and related syndromes: G45
    # Vascular syndromes of brain in cerebrovascular diseases: G46
    # Cerebral palsy and other paralytic syndromes: G80-G83

  # Diseases of the circulatory system
    # Cerebrovascular diseases: I60-I69
  # Pulmonary diagnoses
    # Chronic respiratory failure: J961
"

# Calls a function (get_ICD_codes) from the 'get_ICD_codes.R' script to return two dataframes
# One dataframe contains all the codes and associated exclusion diagnoses for ICD9, the other is the same, but for ICD10 codes
# Note: The diagnoses differ between the two dataframes, as the classifications and hierarchy of different diagnoses are very different between ICD9 and ICD10
source("Code/R_code/get_ICD_codes.R")
exclusion_diagnosis_codes_41271 <- get_ICD_codes(9)
exclusion_diagnosis_codes_41270 <- get_ICD_codes(10)


###############################################################################################################################################
# FUNCTIONS TO EXCLUDE PARTICIPANTS WITH CONFOUNDING DIAGNOSES
###############################################################################################################################################

# Inputs:
  # data: The data frame to exclude participants from - initially this is the full data, but will be called multiple times to eliminate participants using different data-fields
  # exclusion_diagnosis_codes_xxxxx: A character vector containing all of the diagnoses for one specific data-field e.g. <exclusion_diagnosis_codes_20544>
  # numerical_code: The numerical code for a data-field e.g. <20544> for "Online follow-up questionnaire self reported diagnoses"

# This function excludes participants from one data-field at a time
# It is called multiple times to sequentially exclude participants based on one data field at a time
eliminate_from_data_field <- function(data, exclusion_diagnosis_codes_xxxxx, numerical_code) {
  
  numerical_code <- as.character(numerical_code)
  
  if (numerical_code =="20544") { # Excluding participants with a number of self-reported mental health conditions reported in the online mental health questionnaire
    excluded_subjects <- 
        data %>% 
        filter(if_any(starts_with("20544-"), ~ . %in% exclusion_diagnosis_codes_xxxxx))
    
  } else if (numerical_code == "20001") { # Excluding any self-reported cancer diagnoses reported at any session prior to and including the imaging session
      numerical_code <- "20001"
      excluded_subjects <- 
        data %>% 
        filter(if_any(starts_with(c("20001-0.", "20001-1.", "20001-2.")), ~ !is.na(.)))
    
  } else if (numerical_code == "20002") { # Excluding participants who self-reported certain diagnoses at assessment center visits up to and including the imaging session
      excluded_subjects <- 
        data %>% 
        filter(if_any(starts_with(c("20002-0.", "20002-1.", "20002-2.")), ~ . %in% exclusion_diagnosis_codes_xxxxx))
      
  } else if (numerical_code == "41271") { # Excluding participants based on ICD9 coded diagnoses from hospital records
      excluded_subjects <-
        data %>%
        filter(if_any(starts_with(paste0(numerical_code, "-")), ~ . %in% exclusion_diagnosis_codes_xxxxx))

      columns_41271 <-
        excluded_subjects %>%
        select(starts_with(c("eid", "41271-"))) %>%
        mutate(across(starts_with("eid"), as.integer)) %>%
        mutate(across(starts_with("41271-"), as.character))

      code_col_names <-
        excluded_subjects %>%
        select(starts_with(paste0(numerical_code, "-"))) %>%
        names()

      final_columns_41271 <- tibble(code_col_name = character(), eid = integer(), code = character())
      for (code_col_name in code_col_names) { # Loop through each column name starting with 41271- (every ICD9 diagnosis)
        index <- 0
        for (code in columns_41271[[code_col_name]]) { # Loop through each diagnosis code within one column
          index <- index + 1
          if (code %in% exclusion_diagnosis_codes_xxxxx) {
            final_columns_41271[nrow(final_columns_41271) + 1,] <- list(code_col_name,
                                                        columns_41271[[index, "eid"]],
                                                        code
                                                        )
          }
        }
        excluded_subjects <-
          excluded_subjects %>%
          filter(eid %in% final_columns_41271$eid)
      }
      
  } else if (numerical_code == "41270") { # Excluding participants based on ICD10 coded diagnoses from hospital records
      excluded_subjects <- 
        data %>% 
        filter(if_any(starts_with(paste0(numerical_code, "-")), ~ . %in% exclusion_diagnosis_codes_xxxxx)) %>% 
        filter(!(is.na(`53-2.0`))) # Excluding participants with missing date of attending assessment center

      columns_41270 <- 
        excluded_subjects %>% 
        select(starts_with(c("eid", "41270-", "41280-", "53-2.0"))) %>% 
        mutate(across(starts_with("eid"), as.integer)) %>%
        mutate(across(starts_with("41270-"), as.character)) %>% 
        mutate(across(starts_with("41280-"), as.Date)) %>% 
        mutate(across(starts_with("53-2.0"), as.Date))
      
      code_col_names <- 
        excluded_subjects %>% 
        select(starts_with(paste0(numerical_code, "-"))) %>% 
        names()
      
      # This for loop and the nested for loop go through each column associated with an ICD10 diagnosis and extract rows which represent participants...
      # that were diagnosed with an exclusion condition BEFORE the imaging session occurred, ignoring those diagnosed after the imaging session
      final_columns_41270 <- tibble(code_col_name = character(), eid = integer(), code = character(), diagnosis_date = Date(), assessment_centre_date = Date())
      for (code_col_name in code_col_names) { # Loop through each column name starting with 41270
        date_col_name <- sub("70-", "80-", code_col_name)
        
        index <- 0
        for (code in columns_41270[[code_col_name]]) { # Loop through each diagnosis code within one column
          index <- index + 1
          if ((code %in% exclusion_diagnosis_codes_xxxxx) & (columns_41270[[index, date_col_name]] < columns_41270[[index, "53-2.0"]])) { # Only excludes participants who received an exclusion diagnosis prior to the imaging session
            final_columns_41270[nrow(final_columns_41270) + 1,] <- list(code_col_name,
                                                        columns_41270[[index, "eid"]],
                                                        columns_41270[[index, code_col_name]],
                                                        columns_41270[[index, date_col_name]],
                                                        columns_41270[[index, "53-2.0"]] # The date that the participant attended the first biobank imaging session
            )
          }
        }
        # Storing only the eids of participants diagnosed with an ICD10 exclusion condition before the imaging session
        # This is a single column data frame
        excluded_subjects <- 
          excluded_subjects %>% 
          filter(eid %in% final_columns_41270$eid)
    }
  }
  # Excluding participants from the data set based on the data-field code input
  filtered_data <-
    data %>% 
    filter(!(eid %in% excluded_subjects$eid))
  
  number_eliminated <- nrow(excluded_subjects)
  return(list(filtered_data, number_eliminated))
}


# This function calls the above function (eliminate_from_data_field) once for each data field used in exclusion criteria
# This function will be called within an external script to exclude all confounding diagnoses
eliminate_confound_diagnoses <- function() {
  # Loops through each data-field code and calls the eliminate_from_data_field function to remove confounds for each field
  # The number of participants found with a diagnosis which excludes them will be displayed for each data-field 
  # The total number of participants excluded will also be displayed
  filtered_data_list <- list()
  filtered_data_list[[1]] <- ukbb_filtered_full_data
  filtered_data_list[[2]] <- 0
  exclusion_diagnosis_codes_all <- list(`20544` = exclusion_diagnosis_codes_20544, # Online follow-up self-reported mental health diagnoses.
                                        `20001` = NULL, # Assessment centre self-reported cancer diagnoses. NULL because all cancer diagnoses will be excluded
                                        `20002` = exclusion_diagnosis_codes_20002, # Assessment centre self-reported non-cancer diagnoses
                                        `41271` = exclusion_diagnosis_codes_41271[[1]], # ICD9 hospital diagnoses
                                        `41270` = exclusion_diagnosis_codes_41270[[1]] # ICD10 hospital diagnoses
  )
  total_number_eliminated <- 0
  for (code in names(exclusion_diagnosis_codes_all)) {
    cat("Currently processing data-field", code, "\n")
    filtered_data_list <- eliminate_from_data_field(filtered_data_list[[1]], exclusion_diagnosis_codes_all[[code]], code)
    cat("Number of participants excluded due to diagnoses in data-field ", code, ": ", filtered_data_list[[2]], "\n\n", sep = "")
    total_number_eliminated <- total_number_eliminated + filtered_data_list[[2]]
  }
  cat("Total number of participants excluded:", total_number_eliminated, "\n\n")
  return(filtered_data_list[[1]])
}
