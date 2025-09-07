# These are the symptoms as defined by Davis et al. (2019) which classify subjects as having GAD (generalized anxiety disorder)
str <- " 
Worried tense or anxious (20421) = Yes
AND
Duration (20420) >= 6 months or All my life
AND
Most days (20538) = Yes
AND
Excessive: More than most (20425) OR Stronger than most (20542)
AND
Number of issues: More than one thing (20543) OR Different worries (20540)
AND
Difficult to control: Difficult to stop worrying (20541) OR Couldn’t put it out of mind (20539) OR Difficult to control (20537)
AND
Functional impairment: Role interference (20418) = Some or A lot
AND
3 somatic symptoms out of:
  Restless. 20426; Keyed up or on edge. 20423; Easily tired. 20429; Having difficulty keeping your mind on what you were doing. 20419; More irritable than usual. 20422; Having tense, sore, or aching muscles. 20417; Often having trouble falling or staying asleep. 20427
"

# Check which data-field codes will be extracted from the string above, as well as counts them
str_view(str, regex("2\\d+", multiline = TRUE))
str_count(str, "2\\d+")

# Extracts data field codes from string above (str)
# Creates a character vector (GAD_symptoms_data_fields) containing all data-field codes
GAD_symptoms_data_fields <- str_extract_all(str, "2\\d+")[[1]]
GAD_symptoms_data_fields <- paste(GAD_symptoms_data_fields, "-0.0", sep = "")

# Function that returns a vector of the eids (identification codes) of subjects classified as GAD cases based on self-reported symptoms 
get_symptom_based_GAD_case_eids <- function(data, GAD_symptoms_data_fields) {
  data <- data %>% select(all_of(c("eid", GAD_symptoms_data_fields))) %>% na.omit()
  somatic_symptoms <- c("20426-0.0", "20423-0.0", "20429-0.0", "20419-0.0", "20422-0.0", "20417-0.0", "20427-0.0")
  data <-
    data %>%
    mutate(`20426-0.0` = (`20426-0.0` == 1), 
           `20423-0.0` = (`20423-0.0` == 1), 
           `20429-0.0` = (`20429-0.0` == 1), 
           `20419-0.0` = (`20419-0.0` == 1), 
           `20422-0.0` = (`20422-0.0` == 1), 
           `20417-0.0` = (`20417-0.0` == 1), 
           `20427-0.0` = (`20427-0.0` == 1)
    )
  
  # Creating a column (somatic_symptoms_total) that shows the total number of somatic symptoms an individual has experienced
  data <-
    data %>% 
    mutate(
      somatic_symptoms_total = rowSums(data[somatic_symptoms])
    )
  
  # Filtering to obtain only participants meeting the symptom-based criteria for ever having generalized anxiety disorder as defined by Davis et al. (2019)
  data <-
    data %>% 
    filter(
      (`20421-0.0` == 1) &
      ((`20420-0.0` == -999) | (`20420-0.0` >= 6)) &
      (`20538-0.0` == 1) &
      ((`20425-0.0` == 1) | (`20542-0.0` == 1)) &
      ((`20543-0.0` == 2) | (`20540-0.0` == 1)) &
      ((`20541-0.0` == 1) | (`20539-0.0` %in% c(1, 2, 3)) | (`20537-0.0` %in% c(1, 2, 3))) &
      (`20418-0.0` %in% c(2, 3)) &
      (somatic_symptoms_total >= 3)
    ) 
  
  symptom_based_GAD_case_eids <- 
    data %>% 
    select(eid)
  
  return(symptom_based_GAD_case_eids)
}