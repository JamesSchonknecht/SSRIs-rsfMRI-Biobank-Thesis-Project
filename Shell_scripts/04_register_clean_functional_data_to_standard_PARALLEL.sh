#!/bin/bash

# This script registers cleaned functional data (with file name: filtered_func_data_clean.nii.gz) for every subject to the MNI152 (2mm) standard space
# The final functional data registered to standard space will be named (filtered_func_data_clean_standard.nii.gz) within each subject's directory
# This script also creates a txt file called "inputlist.txt" which contains a list of the filepaths to the filtered_func_data_clean_standard.nii.gz for each subject. This list is needed for running group ICA
# This script should be run after after every subject's directory has been extracted/unzipped within all three groups
# This script took 17 hours to register all subjects using 4 jobs (using 6 core i5 12400f CPU and 32GB RAM)
# THIS SCRIPT REGISTERS MULTIPLE SUBJECTS TO STANDARD SPACE IN PARALLEL IN ORDER TO REDUCE PROCESSING TIME BY UTILISING MULTIPLE CPU CORES RATHER THAN ONLY ONE
# THE TOOL 'GNU PARALLEL' IS USED TO RUN THE REGISTRATION IN PARALLEL, AND NEEDS TO BE INSTALLED BEFORE RUNNING THIS SCRIPT USING: `sudo apt-get install parallel`

# Get the start time of running this script
start_time=$(date +%s)

# Create the name for a txt file that will store the directories of any subject that did not have usable functional MRI data to be registered to standard space
missing_func_data_directories="missing_func_data_directories.txt"

# Function that processes each subject's data
process_subject() {
  j=$1

  func_data_path="${j}/fMRI/rfMRI.ica/filtered_func_data_clean.nii.gz"
  standard_file_path="${j}/filtered_func_data_clean_standard.nii.gz"

  # Skips registration for any subjects that have already had their data registered to standard space (so if the script is partially run, it will not register subjects a second time when run again)
  if [ -f "$standard_file_path" ]
  then
    echo "${j} has already been processed. Skipping this subject..."
  # Registers usable cleaned functional data for a subject who has not already been registered to standard space
  elif [ -f "$func_data_path" ]
    then
    echo "Registering resting state fMRI data in directory ${j} to standard space..."
    applywarp -r $FSLDIR/data/standard/MNI152_T1_2mm_brain.nii.gz \
              -i ${j}/fMRI/rfMRI.ica/filtered_func_data_clean.nii.gz \
              -o ${j}/filtered_func_data_clean_standard.nii.gz \
              -w ${j}/fMRI/rfMRI.ica/reg/example_func2standard_warp.nii.gz
  # Checks for any subject directories that do not have cleaned functional data. There should be no subjects with missing cleaned data (if there is, something has gone wrong)
  # The directories of any subjects with unusable data are stored in a txt file called 'missing_func_data_directories.txt'
  else
    echo "Skipped directory ${j}: No cleaned functional data found"
    echo "${j}" >> $missing_func_data_directories.txt
  fi
}

# Export the process_subject function so it can be used by GNU parallel
export -f process_subject

# Use GNU Parallel to process each subject's data in parallel
# Each job uses one CPU core - so this needs to be adjusted based off the computer this script is run on. If the `-j 5` option is removed, the maximum number of available CPU cores will be used
# This also depends on the RAM of the computer the script is run on. Higher number of jobs uses more RAM, which can lead lead to a 'killed' error due to running out of memory
# Decrease number further if getting errors to prevent these errors
ls -d */*_20227_2_0 | parallel -j 5  process_subject

# Save the file paths to all subject's cleaned, registered (to standard space) data to a txt file called `inputlist.txt`
# If `inputlist.txt` already exists, it will be overwritten
ls -1 */*_20227_2_0/filtered_func_data_clean_standard.nii.gz > inputlist.txt

# Get the time that this script finishes execution
end_time=$(date +%s)

# Calculate the time taken for the script to run in seconds
time_diff=$(( end_time - start_time))

echo "Registration of all subject's functional data to MNI152 standard space took $time_diff seconds"
