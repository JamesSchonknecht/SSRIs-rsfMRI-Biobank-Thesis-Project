library(tidyverse)

# Compare new bulk files with old for anxiety + no medication group
new_anxiety_no_medications <- readLines(con = "Data/anxiety_no_medications_session_1.txt")
old_anxiety_no_medications <- readLines(con = "NIFTI Images/Image_Files/Group_NIFTI_Images/anxiety_no_medications_session_1/anxiety_no_medications_session_1.txt")
keep_anxiety_no_medications <- new_anxiety_no_medications %in% old_anxiety_no_medications %>% which() %>% new_anxiety_no_medications[.]
needed_anxiety_no_medications <- which(!(new_anxiety_no_medications %in% old_anxiety_no_medications)) %>% new_anxiety_no_medications[.]
discard_anxiety_no_medications <- which(!(old_anxiety_no_medications %in% new_anxiety_no_medications)) %>% old_anxiety_no_medications[.]

length(keep_anxiety_no_medications)
length(needed_anxiety_no_medications)
length(discard_anxiety_no_medications)

# Needed bulk file for no med group
writeLines(needed_anxiety_no_medications, con = "Data/needed_anxiety_no_medications_session_1.txt")

# Discard bulk file for no med group
writeLines(discard_anxiety_no_medications, con = "Data/discard_anxiety_no_medications_session_1.txt")

# Compare new bulk files with old for anxiety + SSRI group
new_anxiety_ssri <- readLines(con = "Data/anxiety_ssri_session_1.txt")
old_anxiety_ssri <- readLines(con = "NIFTI Images/Image_Files/Group_NIFTI_Images/anxiety_ssri_session_1/anxiety_ssri_session_1.txt")
keep_anxiety_ssri <- new_anxiety_ssri %in% old_anxiety_ssri %>% which() %>% new_anxiety_ssri[.]
needed_anxiety_ssri <- which(!(new_anxiety_ssri %in% old_anxiety_ssri)) %>% new_anxiety_ssri[.]
discard_anxiety_ssri <- which(!(old_anxiety_ssri %in% new_anxiety_ssri)) %>% old_anxiety_ssri[.]


length(keep_anxiety_ssri)
length(needed_anxiety_ssri)
length(discard_anxiety_ssri)

# Compare new bulk files with old for control group
new_controls <- readLines(con = "Data/controls_session_1.txt")
old_controls <- readLines(con = "NIFTI Images/Image_Files/Group_NIFTI_Images/controls_session_1/controls_session_1.txt")
keep_controls <- new_controls %in% old_controls %>% which() %>% new_controls[.]
needed_controls <- which(!(new_controls %in% old_controls)) %>% new_controls[.]
discard_controls <- which(!(old_controls %in% new_controls)) %>% old_controls[.]

length(keep_controls)
length(needed_controls)
length(discard_controls)

# Needed bulk file for control group
writeLines(needed_controls, con = "Data/needed_controls_session_1.txt")


# Moving cleaned, standard space functional data to external HDD

# No med group
discard_clean_standard_anxiety_no_medications_file_names <- gsub(" ", "_", discard_anxiety_no_medications) %>% paste0(., "/filtered_func_data_clean_standard.nii.gz")
source_dir <- "NIFTI Images/Image_Files/Group_NIFTI_Images/anxiety_no_medications_session_1"
dest_dir <- "G:/Biobank Project NIFTI Backup/anxiety_no_meds"
for (file_name in discard_clean_standard_anxiety_no_medications_file_names) {
  file_path <- paste0(source_dir, "/", file_name)
  new_full_name <- gsub("/", "_", file_name) %>% paste0(dest_dir, "/", .)
  
  success <- file.rename(file_path, new_full_name)
}

source_dir <- "NIFTI Images/Image_Files/Group_NIFTI_Images/anxiety_no_medications_session_1"
discard_anxiety_no_medications_directories <- gsub(" ", "_", discard_anxiety_no_medications) %>% paste0(source_dir, "/", .)
sapply(discard_anxiety_no_medications_directories, function(dir) {
  unlink(dir, recursive = TRUE)
  cat(paste("Deleted directory:", dir, "\n"))
})

# SSRI group
discard_clean_standard_anxiety_ssri_file_names <- gsub(" ", "_", discard_anxiety_ssri) %>% paste0(., "/filtered_func_data_clean_standard.nii.gz")
source_dir <- "NIFTI Images/Image_Files/Group_NIFTI_Images/anxiety_ssri_session_1"
dest_dir <- "G:/Biobank Project NIFTI Backup/anxiety_ssri"
for (file_name in discard_clean_standard_anxiety_ssri_file_names) {
  file_path <- paste0(source_dir, "/", file_name)
  new_full_name <- gsub("/", "_", file_name) %>% paste0(dest_dir, "/", .)
  
  success <- file.rename(file_path, new_full_name)
}

source_dir <- "NIFTI Images/Image_Files/Group_NIFTI_Images/anxiety_ssri_session_1"
discard_anxiety_ssri_directories <- gsub(" ", "_", discard_anxiety_ssri) %>% paste0(source_dir, "/", .)
sapply(discard_anxiety_ssri_directories, function(dir) {
  unlink(dir, recursive = TRUE)
  cat(paste("Deleted directory:", dir, "\n"))
})

# Control group
discard_clean_standard_controls_file_names <- gsub(" ", "_", discard_controls) %>% paste0(., "/filtered_func_data_clean_standard.nii.gz")
source_dir <- "NIFTI Images/Image_Files/Group_NIFTI_Images/controls_session_1"
dest_dir <- "G:/Biobank Project NIFTI Backup/controls"
for (file_name in discard_clean_standard_controls_file_names) {
  file_path <- paste0(source_dir, "/", file_name)
  new_full_name <- gsub("/", "_", file_name) %>% paste0(dest_dir, "/", .)
  
  success <- file.rename(file_path, new_full_name)
}

source_dir <- "NIFTI Images/Image_Files/Group_NIFTI_Images/controls_session_1"
discard_controls_directories <- gsub(" ", "_", discard_controls) %>% paste0(source_dir, "/", .)
sapply(discard_controls_directories, function(dir) {
  unlink(dir, recursive = TRUE)
  cat(paste("Deleted directory:", dir, "\n"))
})




# Checking correct participants are in each folder

# No meds group
matched_all_groups <- readRDS("Data/matched_all_groups.rds")

matched_all_groups$group %>% unique()

rds_anxiety_no_meds <- matched_all_groups %>% filter(group == "anxiety_no_medications_session_1")
rds_anxiety_no_meds_participant_directories <- 
  rds_anxiety_no_meds$eid %>% 
  as.character() %>% 
  paste0(., "_20227_2_0")

actual_anxiety_no_meds <- list.dirs(path = "NIFTI Images/Image_Files/Group_NIFTI_Images/anxiety_no_medications_session_1/",
                                    recursive = FALSE,
                                    full.names = FALSE)

identical(rds_anxiety_no_meds_participant_directories, actual_anxiety_no_meds)
(rds_anxiety_no_meds_participant_directories == actual_anxiety_no_meds) %>% sum()

source_dir <- "NIFTI Images/Image_Files/Group_NIFTI_Images/anxiety_no_medications_session_1"
anxiety_no_meds_to_delete <- which(!(actual_anxiety_no_meds %in% rds_anxiety_no_meds_participant_directories)) %>% actual_anxiety_no_meds[.]
discard_anxiety_no_meds_directories <- paste0(source_dir, "/", anxiety_no_meds_to_delete)

sapply(discard_anxiety_no_meds_directories, function(dir) {
  unlink(dir, recursive = TRUE)
  cat(paste("Deleted directory:", dir, "\n"))
})


# SSRI group

rds_anxiety_ssri <- matched_all_groups %>% filter(group == "anxiety_ssri_session_1")
rds_anxiety_ssri_participant_directories <- 
  rds_anxiety_ssri$eid %>% 
  as.character() %>% 
  paste0(., "_20227_2_0")

actual_anxiety_ssri <- list.dirs(path = "NIFTI Images/Image_Files/Group_NIFTI_Images/anxiety_ssri_session_1/",
                                    recursive = FALSE,
                                    full.names = FALSE)

identical(rds_anxiety_ssri_participant_directories, actual_anxiety_ssri)
(rds_anxiety_ssri_participant_directories %in% actual_anxiety_ssri) %>% sum()

(actual_anxiety_ssri == rds_anxiety_ssri_participant_directories)

source_dir <- "NIFTI Images/Image_Files/Group_NIFTI_Images/anxiety_ssri_session_1"
anxiety_ssri_to_delete <- which(!(actual_anxiety_ssri %in% rds_anxiety_ssri_participant_directories)) %>% actual_anxiety_ssri[.]
discard_anxiety_ssri_directories <- paste0(source_dir, "/", anxiety_ssri_to_delete)

sapply(discard_anxiety_ssri_directories, function(dir) {
  unlink(dir, recursive = TRUE)
  cat(paste("Deleted directory:", dir, "\n"))
})

# Control group

rds_controls <- matched_all_groups %>% filter(group == "controls_session_1")
rds_controls_participant_directories <- 
  rds_controls$eid %>% 
  as.character() %>% 
  paste0(., "_20227_2_0")

actual_controls <- list.dirs(path = "NIFTI Images/Image_Files/Group_NIFTI_Images/controls_session_1/",
                                 recursive = FALSE,
                                 full.names = FALSE)

identical(rds_controls_participant_directories, actual_controls)
(rds_controls_participant_directories == actual_controls) %>% sum()

source_dir <- "NIFTI Images/Image_Files/Group_NIFTI_Images/controls_session_1"
controls_to_delete <- which(!(actual_controls %in% rds_controls_participant_directories)) %>% actual_controls[.]
discard_controls_directories <- paste0(source_dir, "/", controls_to_delete)

sapply(discard_controls_directories, function(dir) {
  unlink(dir, recursive = TRUE)
  cat(paste("Deleted directory:", dir, "\n"))
})
