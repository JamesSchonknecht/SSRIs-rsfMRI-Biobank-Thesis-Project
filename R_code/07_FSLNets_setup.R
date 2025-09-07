library(tidyverse)

# Creating directory containing all subject timeseries txt files called groupICA100.dr
# This directory will be used by nets.load to create a node timeseries object (ts)
all_dirs <- list.dirs("NIFTI Images/Image_Files/Group_NIFTI_Images/", recursive = FALSE, full.names = TRUE)
matched_dirs <- grep("_session_1", all_dirs, value = TRUE)


# Different method with for loop
group_dirs <- list()
for (group in seq_along(matched_dirs)){
  group_name <- matched_dirs[group] %>% str_extract_all(., "\\w+_session_1$") %>% unlist()
  group_dirs[[group_name]] <- list.dirs(matched_dirs[group], recursive = FALSE, full.names = TRUE)
}
dual_regr_stage_1_file_paths <- lapply(group_dirs, function(x) paste0(x, "/fMRI/rfMRI_100.dr/dr_stage1.txt"))


new_stage_1_directory <- "NIFTI Images/Image_Files/Group_NIFTI_Images/FSLNets_Analysis/groupICA100.dr"
for (file in unlist(dual_regr_stage_1_file_paths)){
  group <- str_extract_all(file, "\\w+_session_1")[[1]]
  eid <- str_extract_all(file, "(?<=session_1/)(\\d+)")[[1]]
  new_file_path <- paste(group, eid, "dr_stage1.txt", sep = "_") %>% paste(new_stage_1_directory, ., sep = "/")

  # file.copy(file, new_file_path)
}



# Creating list of "Good nodes" - 55 nodes that were not clearly artefactual as decided by UK Biobank
# Each node number has 1 subtracted from it, as biobank labels the components starting at 1, but fsleyes displays components starting at 0
good_nodes <- readLines(con = "NIFTI Images/Image_Files/Group_NIFTI_Images/FSLNets_Analysis/bmri_group_means/UKBiobank_BrainImaging_GroupMeanTemplates/rfMRI_GoodComponents_d100_v1.txt")
good_nodes_split <- 
  str_split(good_nodes, "\\s+")[[1]] %>%
  as.numeric()
good_nodes_split_minus_1 <- 
  (good_nodes_split - 1) %>% 
  as.character() %>% 
  paste(., collapse = ", ")
  

# Creating the command to paste into the python environment of FSLNets
goodnodes_command <- 
  gsub("  ", " ", good_nodes_split_minus_1) %>%
  paste0("goodnodes = [", ., "]")

cat(goodnodes_command)
