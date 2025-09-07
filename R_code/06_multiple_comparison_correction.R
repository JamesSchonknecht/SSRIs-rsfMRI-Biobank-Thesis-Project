

library(tidyverse)
library(officer)
library(flextable)

###############################################################################################################################################
# PERFORMING HOLM-BONFERRONI CORRECTION ON P-VALUES OBTAINED BY ANCOVA F-TESTS
###############################################################################################################################################

# Output from check_tfce_corrp_values.sh shell script, giving (1 - p-value) for each ICA component
ancova_script_output <- "
Outputs/dr_stage3_ic0000_tfce_corrp_fstat1.nii.gz 0.000000 0.732100
Outputs/dr_stage3_ic0001_tfce_corrp_fstat1.nii.gz 0.000000 0.309000
Outputs/dr_stage3_ic0002_tfce_corrp_fstat1.nii.gz 0.000000 0.255400
Outputs/dr_stage3_ic0003_tfce_corrp_fstat1.nii.gz 0.000000 0.768400
Outputs/dr_stage3_ic0004_tfce_corrp_fstat1.nii.gz 0.000000 0.040200
Outputs/dr_stage3_ic0005_tfce_corrp_fstat1.nii.gz 0.000000 0.964300
Outputs/dr_stage3_ic0007_tfce_corrp_fstat1.nii.gz 0.000000 0.030100
Outputs/dr_stage3_ic0008_tfce_corrp_fstat1.nii.gz 0.000000 0.464600
Outputs/dr_stage3_ic0009_tfce_corrp_fstat1.nii.gz 0.000000 0.734100
Outputs/dr_stage3_ic0010_tfce_corrp_fstat1.nii.gz 0.000000 0.417500
Outputs/dr_stage3_ic0011_tfce_corrp_fstat1.nii.gz 0.000000 0.987900
Outputs/dr_stage3_ic0012_tfce_corrp_fstat1.nii.gz 0.000000 0.996800
Outputs/dr_stage3_ic0013_tfce_corrp_fstat1.nii.gz 0.000000 0.107500
Outputs/dr_stage3_ic0014_tfce_corrp_fstat1.nii.gz 0.000000 0.299400
Outputs/dr_stage3_ic0015_tfce_corrp_fstat1.nii.gz 0.000000 0.085200
Outputs/dr_stage3_ic0016_tfce_corrp_fstat1.nii.gz 0.000000 0.993900
Outputs/dr_stage3_ic0017_tfce_corrp_fstat1.nii.gz 0.000000 0.043700
Outputs/dr_stage3_ic0018_tfce_corrp_fstat1.nii.gz 0.000000 0.493400
"



# Extract ICA component names from above output
ancova_script_output_split <- (strsplit(ancova_script_output, split = "\n")[[1]])[-1]
str_view_all(ancova_script_output, "ic00\\d{2}")
ancova_component_names <- str_extract_all(ancova_script_output, "ic00\\d{2}")[[1]]

# Extract max (1 - p-value from each ICA component)
str_view_all(ancova_script_output, "(?<=0\\s)(0.\\d+)")
ancova_one_minus_p <- str_extract_all(ancova_script_output, "(?<=0\\s)(0.\\d+)")[[1]] %>% as.numeric()

# Calculate vector of p-values for F-tests
ancova_p_values <- 1 - ancova_one_minus_p
print(sort(ancova_p_values))

# Adjust p-values using Holm-Bonferroni correction
ancova_adjusted_p_values <- p.adjust(ancova_p_values, method = "BH")
print(sort(ancova_adjusted_p_values))

# Display which ICA components found statistically significant differences between the three subject groups
ancova_significant_components <- (ancova_adjusted_p_values < 0.055) %>% which() %>% ancova_component_names[.]
cat("ANCOVA F-tests found the following ICA components to be statistically significant, after correction for multiple comparisons using Bonferroni correction: \n")
cat(ancova_significant_components, "\n")


# Creating correct Independent Component names for dissertation table
ancova_IC_names <- toupper(ancova_component_names) %>% 
  str_replace(., "IC00", "IC") %>% 
  str_replace(., "IC0", "IC")

ancova_IC_names[1:6] <- sapply(ancova_IC_names[1:6], function(x) {
  num <- as.numeric(gsub("IC", "", x))
  paste0("IC", num + 1)
})


# Creating table for dissertation ANCOVA results section
ancova_table <- tibble(
  `Independent Component` = ancova_IC_names, 
  `p-value` = round(ancova_p_values, 3), 
  `Adjusted p-value` = round(ancova_adjusted_p_values, 3)
)

ancova_table %>% View()

ancova_ft <- flextable(ancova_table)

ancova_doc <- read_docx()
ancova_doc <- body_add_flextable(ancova_doc, value = ancova_ft)

print(ancova_doc, target = "Data/ancova_table.docx")

###############################################################################################################################################
# PERFORMING HOLM-BONFERRONI CORRECTION ON P-VALUES OBTAINED BY POST-HOC T-TESTS
###############################################################################################################################################


t_tests_script_output <- "
Outputs/dr_stage3_ic0012_tfce_corrp_tstat1.nii.gz 0.000000 0.969100
Outputs/dr_stage3_ic0012_tfce_corrp_tstat2.nii.gz 0.000000 0.055400
Outputs/dr_stage3_ic0012_tfce_corrp_tstat3.nii.gz 0.000000 0.708500
Outputs/dr_stage3_ic0012_tfce_corrp_tstat4.nii.gz 0.000000 0.187600
Outputs/dr_stage3_ic0012_tfce_corrp_tstat5.nii.gz 0.000000 0.171400
Outputs/dr_stage3_ic0012_tfce_corrp_tstat6.nii.gz 0.000000 0.999600
Outputs/dr_stage3_ic0011_tfce_corrp_tstat1.nii.gz 0.000000 0.944600
Outputs/dr_stage3_ic0011_tfce_corrp_tstat2.nii.gz 0.000000 0.028400
Outputs/dr_stage3_ic0011_tfce_corrp_tstat3.nii.gz 0.000000 0.422400
Outputs/dr_stage3_ic0011_tfce_corrp_tstat4.nii.gz 0.000000 0.978800
Outputs/dr_stage3_ic0011_tfce_corrp_tstat5.nii.gz 0.000000 0.306000
Outputs/dr_stage3_ic0011_tfce_corrp_tstat6.nii.gz 0.000000 0.999600
Outputs/dr_stage3_ic0016_tfce_corrp_tstat1.nii.gz 0.000000 0.916000
Outputs/dr_stage3_ic0016_tfce_corrp_tstat2.nii.gz 0.000000 0.117600
Outputs/dr_stage3_ic0016_tfce_corrp_tstat3.nii.gz 0.000000 0.504400
Outputs/dr_stage3_ic0016_tfce_corrp_tstat4.nii.gz 0.000000 0.955400
Outputs/dr_stage3_ic0016_tfce_corrp_tstat5.nii.gz 0.000000 0.421000
Outputs/dr_stage3_ic0016_tfce_corrp_tstat6.nii.gz 0.000000 0.999600
"

# Extract ICA component names from above output
t_tests_script_output_split <- (strsplit(t_tests_script_output, split = "\n")[[1]])[-1]
str_view_all(t_tests_script_output, "ic00\\S+")
t_tests_component_names <- str_extract_all(t_tests_script_output, "ic00\\S+")[[1]]

# Extract max (1 - p-value from each ICA component)
str_view_all(t_tests_script_output, "(?<=0\\s)(0.\\d+)")
t_tests_one_minus_p <- str_extract_all(t_tests_script_output, "(?<=0\\s)(0.\\d+)")[[1]] %>% as.numeric()

# Calculate vector of p-values for t-tests
t_tests_p_values <- 1 - t_tests_one_minus_p
print(sort(t_tests_p_values))

# Adjust p-values using Holm-Bonferroni correction
t_tests_adjusted_p_values <- p.adjust(t_tests_p_values, method = "BH")
print(sort(t_tests_adjusted_p_values))

# Display which t-tests found statistically significant results
t_tests_significant_p_values <- (t_tests_adjusted_p_values < 0.05) %>% which() %>% t_tests_adjusted_p_values[.]
t_tests_significant_names <- (t_tests_adjusted_p_values < 0.05) %>% which() %>% t_tests_component_names[.]
print(t_tests_significant_names)
print(t_tests_significant_p_values)

# Creating table for dissertation results of pairwise t-test
t_tests_component_names_table <- 
  t_tests_component_names %>% 
  str_replace(., "ic00", "IC") %>% 
  str_replace(., "_tfce_corrp_tstat", " tstat") %>% 
  str_replace(., ".nii.gz", "")

t_tests_component_names_table

t_tests_table <- tibble(
  `Pairwise t-test` = t_tests_component_names_table,
  `p-value` = round(t_tests_p_values, 3),
  `Adjusted p-value` = round(t_tests_adjusted_p_values, 3)
)

t_tests_table %>% View()

t_tests_ft <- flextable(t_tests_table)

t_tests_doc <- read_docx()
t_tests_doc <- body_add_flextable(t_tests_doc, value = t_tests_ft)

print(t_tests_doc, target = "Data/t_tests_table.docx")
