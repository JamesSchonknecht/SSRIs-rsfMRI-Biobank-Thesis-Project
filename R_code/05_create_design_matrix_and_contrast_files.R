# This script creates the group-level design matrix file (design.mat) and the contrasts of group-level design matrix file (design.con)
# The FSL Glm gui will not function when trying to create a design matrix with 560 subjects (and it would take too long to manually input values for every subject)

library(tidyverse)
library(car)

###############################################################################################################################################
# CHECKING COVARIATES FOR MULTICOLLINEARITY
###############################################################################################################################################

# Load the data-frame
matched_all_groups <- readRDS(file = "Data/matched_all_groups.rds")


# List of all covariates:
  # Age
  # Age^2
  # Sex
  # Date
  # Head motion
  # (Head motion)^2
  # Number of anxiety symptoms
  # (Number of anxiety symptoms)^2
  # Depressed mood frequency
  # (Depressed mood frequency)^2

# Only the non-squared covariates will be included in the Glm analysis, as including squared covariates...
# results in multicollinearity (so the independent variables in the Glm analysis are related to each other and not actually independent,
# making it difficult to determine which independent variables influence the dependent variable (BOLD signal))
  # This was confirmed by first running the below code including squared terms and finding large (>200) VIF values
covariate_col_names <- c("age", "date", "head_motion", "number_of_anxiety_symptoms","depressed_mood_frequency")

# Creating an empty vector that will be filled with the max VIF (Variance Inflation Factor) values for each covariate
# VIF is a measure of the amount of multicollinearity between independent variables, with higher values indicating higher multicollinearity
# From https://www.investopedia.com/terms/v/variance-inflation-factor.asp#:~:text=In%20general%20terms%2C,variables%20are%20highly%20correlated2
  # VIF equal to 1 = variables are not correlated
  # VIF between 1 and 5 = variables are moderately correlated 
  # VIF greater than 5 = variables are highly correlated
vif_values <- numeric(length(covariate_col_names))

# Loop through each covariate, fit it to a linear model as the response variable with all other covariates as predictors, and calculate 
# maximum VIF values to assess for multicollinearity
for (i in seq_along(covariate_col_names)) {
  # Get all predictor variable names except for the current variable
  predictors <- covariate_col_names[-i]
  
  # Create a formula with the current variable as the outcome and all others as predictors
  formula_str <- paste(covariate_col_names[i], "~", paste(predictors, collapse = " + "))
  
  # Fit a linear model of the current independent variable as the outcome, with all other independent variables as predictors
  model <- lm(as.formula(formula_str), data = matched_all_groups)
  
  # Calculate and store the maximum VIF value for the current independent variable
  vif_values[i] <- max(vif(model))
  
  # Display the maximum VIF value with the current covariate as the response variable
  cat("Max VIF value in the model with", covariate_col_names[i], "as the response variable:", vif_values[i], "\n")
}


###############################################################################################################################################
# CREATING GROUP LEVEL DESIGN MATRIX FILE
###############################################################################################################################################

matched_all_groups$sex_numeric <- ifelse(matched_all_groups$sex == "Female", 0, 1)

# Calculating the grand mean of each of the covariates (the mean values across all subject groups)
mean_age <- mean(matched_all_groups$age)
mean_sex <- mean(matched_all_groups$sex_numeric)
mean_date <- mean(matched_all_groups$date)
mean_head_motion <- mean(matched_all_groups$head_motion)
mean_number_of_anxiety_symptoms <- mean(matched_all_groups$number_of_anxiety_symptoms)
mean_depressed_mood_frequency <- mean(matched_all_groups$depressed_mood_frequency)

# Calculating demeaned covariate values for all subjects and adding associated columns to the data-frame
matched_all_groups <- 
  matched_all_groups %>% 
  mutate(
    demeaned_age = age - mean_age,
    demeaned_sex = sex_numeric - mean_sex,
    demeaned_date = date - mean_date,
    demeaned_head_motion = head_motion - mean_head_motion,
    demeaned_number_of_anxiety_symptoms = number_of_anxiety_symptoms - mean_number_of_anxiety_symptoms,
    demeaned_depressed_mood_frequency = depressed_mood_frequency - mean_depressed_mood_frequency
  )

# Reading in the template design matrix file created using the FSL Glm gui
template_matrix <- readLines(con = "NIFTI Images/Image_Files/Group_NIFTI_Images/template_design/template_design.mat")

# Creating data-frame that contains the matrix values for all subjects (across all three groups)
matrix_file_dataframe <- 
  matched_all_groups %>% 
  arrange(desc(group == "anxiety_no_medications_session_1")) %>% 
  mutate(
    group_no_med = ifelse(group == "anxiety_no_medications_session_1", "1", "0"),
    group_ssri = ifelse(group == "anxiety_ssri_session_1", "1", "0"),
    group_control = ifelse(group == "controls_session_1", "1", "0"),
    age = demeaned_age,
    sex = demeaned_sex,
    date = demeaned_date,
    he_mot = demeaned_head_motion,
    anx_symp = demeaned_number_of_anxiety_symptoms,
    depr_freq = demeaned_depressed_mood_frequency,
    .keep = "none"
  ) %>% 
  select(group_no_med, group_ssri, group_control, age, sex, date, he_mot, anx_symp, depr_freq) # Reordering columns

# Creating the txt file which will be converted to the matrix file format needed by FSL using FSL's Text2Vest tool
write.table(matrix_file_dataframe, file = "Data/design_mat.txt", sep = " ", row.names = F, col.names = F, quote = F, append = F)


###############################################################################################################################################
# CREATING GROUP LEVEL ANCOVA DESIGN CONTRASTS FILE
###############################################################################################################################################

# Create a data-frame containing the contrast vectors
# This will run an ANCOVA on the 3 groups, testing if the means of all groups are equal
# The first 2 contrasts are derived from:
  # mean(group_no_med) = mean(group_ssri) = mean(group_control)... which is equivalent to:
  # mean(group_no_med) - mean(group_control) = mean(group_ssri) - mean(group_control) = 0
ancova_contrast_vectors_dataframe <- rbind(c("1", "0", "-1", "0", "0", "0", "0", "0", "0"),
                                    c("0", "1", "-1", "0", "0", "0", "0", "0", "0"))
ancova_contrast_vectors_dataframe <- as.data.frame(ancova_contrast_vectors_dataframe)

# Write the contrast vector data-frame to a txt file which will be converted to the format needed by FSL using FSL's Text2Vest tool 
write.table(ancova_contrast_vectors_dataframe, file = "Data/ancova_design_con.txt", append = F, quote = F, sep = " ", row.names = F, col.names = F)


###############################################################################################################################################
# CREATING ANCOVA F-TESTS FILE
###############################################################################################################################################

writeLines("1 1", con = "Data/design_fts.txt")

###############################################################################################################################################
# CREATING GROUP LEVEL POST-HOC T-TEST DESIGN CONTRASTS FILE
###############################################################################################################################################

# Contrasts for t-tests for independent components found to be significantly different between subject groups
t_tests_contrast_vectors_dataframe <- rbind(c("1", "-1", "0", "0", "0", "0", "0", "0", "0"), # Anxiety + No medicines > Anxiety + SSRI
                                    c("-1", "1", "0", "0", "0", "0", "0", "0", "0"), # Anxiety + SSRI > Anxiety + No medicines
                                    c("1", "0", "-1", "0", "0", "0", "0", "0", "0"), # Anxiety + No medicines > Controls
                                    c("-1", "0", "1", "0", "0", "0", "0", "0", "0"), # Controls > Anxiety + No medicines
                                    c("0", "1", "-1", "0", "0", "0", "0", "0", "0"), # Anxiety + SSRI > Controls
                                    c("0", "-1", "1", "0", "0", "0", "0", "0", "0")) # Controls > Anxiety + SSRI
t_tests_contrast_vectors_dataframe <- as.data.frame(t_tests_contrast_vectors_dataframe)

write.table(t_tests_contrast_vectors_dataframe, file = "Data/t_tests_design_con.txt", append = F, quote = F, sep = " ", row.names = F, col.names =  F)



###############################################################################################################################################
# CREATING SEX, AGE, ANXIETY SYMPTOMS AND DEPRESSION SYMPTOMS ANCOVA DESIGN CONTRAST FILE
###############################################################################################################################################
ancova_contrast_vectors_dataframe_others <- rbind(c("0", "0", "0", "1", "0", "0", "0", "0", "0"), # Positive effect of age
                                                  c("0", "0", "0", "-1", "0", "0", "0", "0", "0"), # Negative effect of age
                                                  c("0", "0", "0", "0", "1", "0", "0", "0", "0"), # Positive effect of sex
                                                  c("0", "0", "0", "0", "-1", "0", "0", "0", "0"), # Negative effect of sex
                                                  c("0", "0", "0", "0", "0", "0", "0", "1", "0"), # Positive effect of anxiety symptoms
                                                  c("0", "0", "0", "0", "0", "0", "0", "-1", "0"), # Negative effect of anxiety symptoms
                                                  c("0", "0", "0", "0", "0", "0", "0", "0", "1"), # Positive effect of depression symptoms
                                                  c("0", "0", "0", "0", "0", "0", "0", "0", "-1")) # Negative effect of depression symptoms

ancova_contrast_vectors_dataframe_others <- as.data.frame(ancova_contrast_vectors_dataframe_others)

# Write the contrast vector data-frame to a txt file which will be converted to the format needed by FSL using FSL's Text2Vest tool 
write.table(ancova_contrast_vectors_dataframe_others, file = "Data/others_design_con.txt", append = F, quote = F, sep = " ", row.names = F, col.names = F)

