# This script is specific for .csv files (wont work for .tab files)
# This script extracts the data-field codes (column names) from the comment listing what each code represents
# This saves needing to input every code manually (apart from writing the original comments)
# The output is a string in the form: "eid", "20227", "25752", "25753"...
# This string can be copied and pasted into the "loading_data_script.R" and assigned to the "keep_columns" variable

library(tidyverse)

# Replace everything between the double quotes ("") with the most recent comment from the "loading_data_script.R" file
data_field_string <- "
# Column codes to keep
# It is not needed to include instance number and array number here (all instances and arrays will be included)
# Data-field codes can be found through the UK biobank showcase
  # eid: Participant identifier
  # 20227: Functional brain images - resting - NIFTI
  # 25752: rfMRI partial correlation matrix, dimension 25
  # 25753: rfMRI partial correlation matrix, dimension 100
  # 24441: Mean relative head motion from rfMRI
  # 25000: Volumetric scaling from T1 head image to standard space
  # 20544: Mental health problems ever diagnosed by a professional
  # 20003: Treatment/medication code
  # 2070: Frequency of tenseness / restlessness in last 2 weeks
  # 2050: Frequency of depressed mood in last 2 weeks
  # 20001: Cancer code, self-reported
  # 20002: Non-cancer illness code, self-reported
  # 20432: Ongoing behavioural or miscellaneous addiction
  # 41271: Diagnoses - ICD9
  # 41270: Diagnoses - ICD10
  # 41281: Date of first in-patient diagnosis - ICD9
  # 41280: Date of first in-patient diagnosis - ICD10
  # 21003: Age when attended assessment centre
  # 53: Date of attending assessment centre
  # 34: Year of Birth
  # 52: Month of Birth
  # 31: Sex
  # 20016: Fluid intelligence score
  # 6138: Education level
  # 54: Assessment centre
  # 25923: Echo Time for rfMRI
  # 20400: Date of completing mental health questionnaire
  # 1970: Nervous feelings
  # 1980: Worrier / anxious feelings
  # 1990: Tense / 'highly strung'
  # 2000: Worry too long after embarrassment
  # 2010: Suffer from 'nerves'
  # 20421: Ever felt worried, tense, or anxious for most of a month or longer
  # 20420: Longest period spent worried or anxious
  # 20538: Worried most days during period of worst anxiety
  # 20425: Ever worried more than most people would in similar situation
  # 20542: Stronger worrying (than other people) during period of worst anxiety
  # 20543: Number of things worried about during worst period of anxiety
  # 20540: Multiple worries during worst period of anxiety
  # 20541: Difficulty stopping worrying during worst period of anxiety
  # 20539: Frequency of inability to stop worrying during worst period of anxiety
  # 20537: Frequency of difficulty controlling worry during worst period of anxiety
  # 20418: Impact on normal roles during worst period of anxiety
  # 20426: Restless during period of worst anxiety
  # 20423: Keyed up or on edge during worst period of anxiety
  # 20429: Easily tired during worst period of anxiety
  # 20419: Difficulty concentrating during worst period of anxiety
  # 20422: More irritable than usual during worst period of anxiety
  # 20417: Tense, sore, or aching muscles during worst period of anxiety
  # 20427: Frequent trouble falling or staying asleep during worst period of anxiety
"

# Displays the comments above with data-field names, highlighting the extracted data-field name inside angle brackets: e.g.<f.20227>
str_view(data_field_string, regex("\\w+:", multiline = TRUE))

# Displays the number of data-fields found in the comments above
cat("###################", "NUMBER OF DATA-FIELDS:", str_count(data_field_string, "# \\w+:"), "###################", "\n")

# Extracts the data-field codes into a single string
data_fields_list <- str_extract_all(data_field_string, "\\d+:") # Excludes "eid" to avoid adding a '-' (eid-) in final output
final_data_field_string <- data_fields_list[[1]] %>% gsub(":", "", .) 
final_data_field_string <- paste("\"", final_data_field_string, "-\"", sep = "", collapse = ", ")
final_data_field_string <- paste0("(", "\"eid\", ", final_data_field_string, ")") # "eid" is now added to beginning of output

# Displays the final string to be copy-pasted and assigned to the "keep_columns" variable in the "loading_data_script.R" file
cat(final_data_field_string)
