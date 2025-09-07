# This script will be used to create 'bulk files', which are used to download the NIFTI resting state functional MRI data files from UK biobank
# The bulk files will be used with the ukbfetch utility to download NIFTI data for each group of subjects into seperate folders (one folder for each group)
# UK Biobank has a utility called 'ukbconv' which can be used to create bulk files, but this requires a .enc_ukb file (which was not available when this script was created)
# Also, the 'ukbconv' utility can only download all instances of the NIFTI images (i.e. it will download both first imaging session data and second imaging session data for all subjects, which are not relevant for this project)

library(tidyverse)

matched_all_groups <- readRDS(file ="Data/matched_all_groups.rds")

group_names <- names(table(matched_all_groups$group))

# Loop through each group, extract eids of subjects in that group, then create a txt file in the same format used as a UK Biobank 'Bulk file'
for (group_name in group_names) {
  print(group_name)
  
  # Get the eids for one of three subject groups (in numerical format)
  group_eids <-
    matched_all_groups %>%
    filter(group == group_name) %>% 
    select(eid)

  # Convert eids to text strings
  group_eids <- 
    group_eids[[1]] %>% 
    as.character()
  
  # Create lines using each subject eid, with one subject eid per line
  # The lines are in this format:
    # eid_20227_2_0
  file_lines <- paste(group_eids, "20227_2_0")
  
  # Create 'Bulk file' name
  file_name <- paste0("Data/", group_name, ".txt")
  
  # Create the 'Bulk file' for a group using the lines and name above
  writeLines(file_lines, con = file_name)
}

cat("The bulk files have been saved in the \'Data\' directory. They will need to be moved to the group directories where the NIFTI files will be downloaded to")
