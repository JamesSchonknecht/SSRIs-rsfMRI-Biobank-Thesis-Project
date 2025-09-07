#!/bin/bash

# This script runs randomise to perform stage 3 of dual regression. This involves non-parametric permutation testing to statistically compare all groups of subjects.
# The tests used are post hoc t-tests to compare the three subject groups found to be significally different by ANCOVA tests to find the specific group differences, while controlling for 6 covariates.
# Randomise is run in parallel to reduce processing time. This requires installation of GNU parallel using `sudo apt-get install parallel`

doRandomise() {
    file=$1
    echo "Processing file: $file"

    # Extract the base name of the input file, remove the `.nii.gz` extension, and replace `stage2` with `stage3`
    base=$(basename $file .nii.gz | sed 's/stage2/stage3/')

    # Call the randomise command for the current input file
    randomise -i $file -o t_tests/Outputs/${base} -d t_tests_design/design.mat -t t_tests_design/t_tests_design.con -m groupICA25/mask -n 10000 -T
}

export -f doRandomise

# Limit the number of CPU cores (number of jobs) used by replacing the number after the -j option
parallel -j 4 doRandomise ::: t_tests/Inputs/dr_stage2_ic*.nii.gz
