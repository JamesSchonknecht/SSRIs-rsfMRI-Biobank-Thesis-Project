#!/bin/sh

# This script uses FSL's `Text2Vest` tool to convert design files (design matrix, design contrast and F-test files) from .txt format to .mat, .con and .fts formats respectively
# in order to be compatible with FSL's dual_regression command

### ANCOVA DESIGN FILES ###

# Convert design matrix file from .txt format to FSL's .mat format
Text2Vest ANCOVA_design/design_mat.txt ANCOVA_design/design.mat

# Convert ANCOVA design contrast file from .txt format to FSL's .con format
Text2Vest ANCOVA_design/ancova_design_con.txt ANCOVA_design/ancova_design.con

# Convert f-test file from .txt format to FSL's .fts format
Text2Vest ANCOVA_design/design_fts.txt ANCOVA_design/design.fts



### POST HOC T-TEST DESIGN FILES ###

# Convert design matrix file from .txt format to FSL's .mat format
Text2Vest t_tests_design/design_mat.txt t_tests_design/design.mat

# Convert t-test design contrast file from .txt format to FSL's .con format
Text2Vest t_tests_design/t_tests_design_con.txt t_tests_design/t_tests_design.con
