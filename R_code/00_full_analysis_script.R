# This script is the main script which can be used to run each individual part of the analysis
# The script gives prompts for user input, and sources other scripts as needed

###############################################################################################################################################
# INSTALL ALL NECESSARY PACKAGES
###############################################################################################################################################

# Display the packages required to run the scripts
cat("These packages are required to run the scripts:", "\n")
cat("  tidyverse", "\n")
cat("  MatchIt", "\n")
cat("  optmatch", "\n")
cat("  cobalt", "\n")
package_names <- c("tidyverse", "MatchIt", "optmatch", "cobalt")

# Prompt user and install any required packages that are not already installed
# These packages will be loaded as required within other scripts
install_required_packages <- readline("Do you want to install any missing packages now (y/n)?: ")
if (tolower(install_required_packages) == "y") {
  install.packages(package_names)
}

# Suppress warnings about packages being built under previous versions of R
suppressWarnings(library(tidyverse, quietly = TRUE))
suppressWarnings(library(MatchIt, quietly = TRUE))
suppressWarnings(library(optmatch, quietly = TRUE))
suppressWarnings(library(cobalt, quietly = TRUE))

###############################################################################################################################################
# PROMPT FOR USER INPUT
###############################################################################################################################################

# Prompt for folder path to where all R scripts have been saved
# If 'enter' is pressed rather than a file path, the default directory 'Code/R_code' will be used
r_script_directory <- readline("Please enter the folder path to where the R scripts are saved, or press enter for the default directory: ")
if (r_script_directory == "") {
  r_script_directory <- "Code/R_code"
}

# Prompt user for which scripts should be run
  # 01_csv_loading_data takes the full dataset from a csv file and creates a much smaller rds file inside the "Data" folder
  # 02_participant_grouping requires the rds file created by the 01_csv_loading_data script. It excludes subjects and sorts them into groups before saving this data as "all_groups_dataframe" inside the "Data" folder
  # 03_matching_participants requires the rds file created by the 02_participant_grouping script. It matches subjects within each group based on covariate balance
  # 04_create_NIFTI_image_bulk_files creates bulk files which are used by Biobank's `ukbfetch` utility to download NIFTI images for groups of subjects
source_01_csv_loading_data <- readline("Load data from csv file? (this will take 30-35 minutes, and only needs to be run once) (y/n): ")
source_02_participant_grouping <- readline("Sort participants into groups? (y/n): ")
source_03_matching_participants <- readline("Match participants? (y/n): ")
source_04_create_NIFTI_image_bulk_files <- readline("Create bulk files for ukbfetch? (y/n): ")


###############################################################################################################################################
# LOADING DATA
###############################################################################################################################################

if (tolower(source_01_csv_loading_data) == "y") {
  start_time_01 <- Sys.time()
  cat("Loading data from csv. This will take around 30-35 minutes...", "\n")
  source(paste0(r_script_directory, "/01_csv_loading_data.R"))
  cat("Finished loading data from csv", "\n")
  cat("The data has been saved in the directory \"Data\" as an rds file: \"ukbb_filtered_full_data.rds\"", "\n")
  end_time_01 <- Sys.time()
  time_taken_01 <- as.numeric(end_time_01 - start_time_01, units = "mins") # Calculate the time taken for this script to complete
}

###############################################################################################################################################
# GROUPING PARTICIPANTS
###############################################################################################################################################

if (tolower(source_02_participant_grouping) == "y") {
  start_time_02 <- Sys.time()
  cat("Sorting participants into groups...", "\n")
  source(paste0(r_script_directory, "/02_participant_grouping.R"))
  cat("Finished sorting participants into groups", "\n")
  end_time_02 <- Sys.time()
  time_taken_02 <- as.numeric(end_time_02 - start_time_02, units = "mins") # Calculate the time taken for this script to complete
}

###############################################################################################################################################
# MATCHING PARTICIPANTS
###############################################################################################################################################

if (tolower(source_03_matching_participants) == "y") {
  start_time_03 <- Sys.time()
  cat("Matching participants...", "\n")
  source(paste0(r_script_directory, "/03_matching_participants.R"))
  cat("Finished matching participants", "\n")
  end_time_03 <- Sys.time()
  time_taken_03 <- as.numeric(end_time_03 - start_time_03, units = "mins") # Calculate the time taken for this script to complete
}

###############################################################################################################################################
# CREATING BULK FILES FOR NIFTI IMAGE DOWNLOADS
###############################################################################################################################################

if (tolower(source_04_create_NIFTI_image_bulk_files) == "y") {
  start_time_04 <- Sys.time()
  cat("Creating bulk files...", "\n")
  source(paste0(r_script_directory, "/04_create_NIFTI_image_bulk_files.R"))
  cat("Finished creating bulk files", "\n")
  end_time_04 <- Sys.time()
  time_taken_04 <- as.numeric(end_time_04 - start_time_04, units = "mins") # Calculate the time taken for this script to complete
}

###############################################################################################################################################
# DISPLAYING TIME TAKEN FOR EACH SCRIPT TO RUN
###############################################################################################################################################

# Function that takes name of script as input, checks if the time taken for a script to run exists, then displays the time in minutes to the console
display_script_run_time <- function(script_name) {
  if (script_name == "01_csv_loading_data.R" && exists("time_taken_01")) {
    cat("01_csv_loading_data.R took", time_taken_01, "minutes to run\n")
    
  } else if (script_name == "02_participant_grouping.R" && exists("time_taken_02")) {
    cat("02_participant_grouping.R took", time_taken_02, "minutes to run\n")
    
  } else if (script_name == "03_matching_participants.R" && exists("time_taken_03")) {
    cat("03_matching_participants.R took", time_taken_03, "minutes to run\n")
    
  } else if (script_name == "04_create_NIFTI_image_bulk_files.R" && exists("time_taken_04")) {
    cat("04_create_NIFTI_image_bulk_files.R took", time_taken_04, "minutes to run\n")
    
  }
}

# Defining the names of all scripts that can be run by this script
script_name_vector <- c("01_csv_loading_data.R", "02_participant_grouping.R", "03_matching_participants.R", "04_create_NIFTI_image_bulk_files.R")

# Loop through every script that was run and display the time taken in minutes using the `display_script_run_time` function above
for (script_name in script_name_vector) {
  display_script_run_time(script_name)
}

