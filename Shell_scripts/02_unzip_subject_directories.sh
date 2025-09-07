#!/bin/bash

# This script extracts/unzips the .zip file directories for every subject within all three groups
# The subject .zip directories are replaced by their extracted versions of the same name
# This script can be run rather than manually extracting the .zip files in each subject group


# Get the start time of running this script
start_time=$(date +%s)

# Loop through each subject group
for i in $(ls -d *_session_1)
do
echo "Extracting subject zip files in directory: ${i}"
  # Loop through each individual subject's .zip file
  for j in "${i}"/*_20227_2_0.zip
  do
  # Extract/unzip the subject's .zip file to a new directory of the same name, and remove the original .zip file
    base_name=$(basename "${j}")
    unzipped_dir_name="${base_name%.zip}"
    mkdir -p "${i}/$unzipped_dir_name"
    unzip "${j}" -d "${i}/$unzipped_dir_name" && rm "${j}"
  done
done

# Get the time that this script finishes execution
end_time=$(date +%s)

# Calculate the time taken for the script to run in seconds
time_diff=$(( end_time - start_time))

echo "Extraction of all zip files took $time_diff seconds"
