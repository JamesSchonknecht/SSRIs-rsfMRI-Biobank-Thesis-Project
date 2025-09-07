# This script selects and loads relevant columns from the full uk biobank dataset in csv format
# It outputs an rds file (custom R binary file type) which can be very quickly loaded into the R workspace as a dataframe
# This script only needs to be run once to generate the rds file, and takes around 30 minutes to finish

library(tidyverse)

###############################################################################################################################################
# LOADING DATA
###############################################################################################################################################

# Load in 1000 rows of the data from the ukb671881.tab file (this file is too large to load in all at once)
ukbb <- read_csv(file = "Data/ukb673187.csv", col_names = TRUE, n_max = 1000) # Make sure this file path is correct

# Gathers column data type to be used later
ukbb_col_types <- spec(ukbb)

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

# Vector containing all data-field codes from above
# Data-field codes can be added manually below, or the script "csv_extract_data_field_names_from_comments.R" can extract the codes from
# the comment listing data-field codes and names above
# If adding/removing data-fields from this, remember to do the same to keep_columns inside 'select_columns' function below
keep_columns <- c("eid", "20227-", "25752-", "25753-", "24441-", "25000-", 
                  "20544-", "20003-", "2070-", "2050-", "20001-", "20002-", 
                  "20432-", "41271-", "41270-", "41281-", "41280-", "21003-", 
                  "53-", "34-", "52-", "31-", "20016-", "6138-", "54-", "25923-", 
                  "20400-", "1970-", "1980-", "1990-", "2000-", "2010-", "20421-", 
                  "20420-", "20538-", "20425-", "20542-", "20543-", "20540-", 
                  "20541-", "20539-", "20537-", "20418-", "20426-", "20423-", 
                  "20429-", "20419-", "20422-", "20417-", "20427-")


# This is the callback function (see below) which processes each chunk of 10,000 rows one at a time
# Make sure that the "keep_columns" variable inside this function is the same as "keep_columns" defined above
select_columns <- function(dat, pos){
  keep_columns <-  c("eid", "20227-", "25752-", "25753-", "24441-", "25000-", 
                     "20544-", "20003-", "2070-", "2050-", "20001-", "20002-", 
                     "20432-", "41271-", "41270-", "41281-", "41280-", "21003-", 
                     "53-", "34-", "52-", "31-", "20016-", "6138-", "54-", "25923-", 
                     "20400-", "1970-", "1980-", "1990-", "2000-", "2010-", "20421-", 
                     "20420-", "20538-", "20425-", "20542-", "20543-", "20540-", 
                     "20541-", "20539-", "20537-", "20418-", "20426-", "20423-", 
                     "20429-", "20419-", "20422-", "20417-", "20427-")
  
  withdrawn <- read_csv(file = "Data/w70132_2023-04-25.csv", col_names = FALSE, show_col_types = FALSE) 
  
  dat %>% 
    filter(!(eid %in% withdrawn$X1)) %>% 
    select(starts_with(keep_columns))
}

# Load the full dataset into R by splitting the data into smaller chunks
# This loads in one chunk of the data at a time and filters each chunk to only include columns of interest
# Loading the full dataset all at once without chunking results in R crashing due to the large size of the data
# This takes about 30-35 minutes (timed as 30.0 minutes) to complete (much longer than older basket's .tab file which took 10-15 minutes due to the much larger csv file size)
start_time <- Sys.time()
ukbb_filtered_full_data <- read_csv_chunked(file = "Data/ukb673187.csv",
                                            chunk_size = 10000,
                                            col_names = TRUE, col_types = ukbb_col_types$cols,
                                            callback = DataFrameCallback$new(select_columns)
                                            )

end_time <- Sys.time()
(end_time - start_time) %>% cat("Time taken to run read_csv_chunked:", ., "minutes\n")


# Save the dataframe with only columns of interest to the hard drive
# The file saved is a custom R binary file type (rds) which can be quickly loaded back into the R workspace (without needing to run this script again)
# The rds file size will be around 75MB
saveRDS(ukbb_filtered_full_data, "Data/ukbb_filtered_full_data.rds")
