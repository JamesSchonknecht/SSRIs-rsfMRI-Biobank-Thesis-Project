#!/bin/sh

# This script runs FSL's dual_regression command using the group level ICA spatial maps, design matrix file, contrast file and txt file containing
# file paths of all subject's cleaned, registered functional data
# Runs stage 1 and stage 2 of dual regression. Stage 3 will be run in the next script to statistically compare specific networks.

dual_regression groupICA25/melodic_IC 1 design/design.mat design/design.con 0 groupICA25.dr $(cat inputlist.txt)
