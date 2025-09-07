#!/bin/bash

# This script removes unnecessary files from every participant's directory in order to free up storage space
# Run this once all zip files have been unzipped

# Remove the raw rfMRI data (rfMRI.nii.gz), as this is not needed for the following analysis and uses a significant amount of storage space
rm */*_20227_2_0/fMRI/rfMRI.nii.gz
echo "Raw rfMRI data (rfMRI.nii.gz) has been deleted for all subjects"
