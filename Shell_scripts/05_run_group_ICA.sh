#!/bin/sh

# This script calls FSL's melodic to perform group ICA (Independent Component Analysis) on the subjects from all three study groups
# The outputs will be located in the `groupICA25` folder
# A dimensionality of 25 components will be used
# The main output is called `melodic_IC.nii.gz`. This is a 4D image with 25 volumes, with each volume corresponding to an ICA component
# The melodic_IC.nii.gz image can be used as a group level template which is input as the spatial basis for dual regression
# This script took 4hrs 30mins to run (on James' computer)

melodic -i inputlist.txt -o groupICA25 \
  --tr=0.735 --nobet -a concat \
  -m $FSLDIR/data/standard/MNI152_T1_2mm_brain_mask.nii.gz \
  --report --Oall -d 25
