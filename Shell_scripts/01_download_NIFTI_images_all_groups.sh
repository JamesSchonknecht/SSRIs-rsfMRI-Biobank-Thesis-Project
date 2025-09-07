#!/bin/bash

# This Bash script enters each of the group directories: anxiety_no_medications_session_1, anxiety_ssri_session_1, controls_session_1, and executes
# the ukbfetch utility to download NIFTI images for all subjects within each group

# Get the start time of running this script
start_time=$(date +%s)

# Location of parent directory where all group NIFTI image directories are located
parent_directory="/media/sf_NIFTI_Images/Image_Files/Group_NIFTI_Images"

# Creating an associative array where the key is the directory name and the value is the .txt file name with participant eids and data-field to be downloaded (this .txt file has the same format as a bulk file, which can be created using UK Biobank's ukbconv utility)
declare -A dir_file_map
dir_file_map=(
  ["anxiety_no_medications_session_1"]="anxiety_no_medications_session_1.txt"
  ["anxiety_ssri_session_1"]="anxiety_ssri_session_1.txt"
  ["controls_session_1"]="controls_session_1.txt"
)

# Loop through the directory for each of the three groups of subjects
for dir in "${!dir_file_map[@]}"
do
  file=${dir_file_map[$dir]}
  cd "$parent_directory/$dir"
  # Execute ukbfetch utility to download the NIFTI images for all subjects within one group at a time
  ./ukbfetch -b"$file" -***REMOVED***.key
done

# Get the time that this script finishes execution
end_time=$(date +%s)

# Calculate the time taken for the script to run in seconds
time_diff=$(( end_time - start_time))

echo "Download of all zip files took $time_diff seconds"
