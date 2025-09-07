# This script matches participants from the grouped participants rds file (created by the 02_participant_grouping.R script) and matches subsets of each group to balance covariate distribution
# This is done using propensity score analysis using the MatchIt package

library(tidyverse)

cat("This script takes approximately 6-7 minutes to complete \n\n")
###############################################################################################################################################
# PREPARING DATA FOR MATCHING
###############################################################################################################################################
# Covariate data-fields:
  # 21003: Age when attended assessment centre
  # 31: Sex
  # 53: Date of attending assessment centre
  # 24441: Mean relative head motion from rfMRI

covariate_data_fields <- c("21003-2.0", "31-0.0", "53-2.0", "24441-2.0")
all_groups_dataframe <- readRDS("Data/all_groups_dataframe.rds")
participants_with_covariate_data <- all_groups_dataframe # Initialising the dataframe which will be modified to include only participants with no missing covariate data

# Function that loops through each covariate data-field and excludes participants missing any covariate data
# The function returns a dataframe where all participants have no missing covariate data
exclude_missing_covariate_data <- function(participants_with_covariate_data, covariate_data_fields) {
  for (data_field in covariate_data_fields){
    cat("Number of participants removed due to missing data in data-field ", data_field, ": ", sum(is.na(participants_with_covariate_data[[data_field]])), "\n", sep = "")
    
    participants_with_covariate_data <- 
      participants_with_covariate_data %>% 
      filter(!(is.na(!!sym(data_field))))
  }
  
  if (nrow(all_groups_dataframe) != nrow(participants_with_covariate_data)) {
    cat("\n")
    cat("Number of participants removed from group \"anxiety_ssri_session_1\":", sum(all_groups_dataframe$group == "anxiety_ssri_session_1") - sum(participants_with_covariate_data$group == "anxiety_ssri_session_1"), "\n")
    cat("Number of participants removed from group \"anxiety_no_medications_session_1\":", sum(all_groups_dataframe$group == "anxiety_no_medications_session_1") - sum(participants_with_covariate_data$group == "anxiety_no_medications_session_1"), "\n")
    cat("Number of participants removed from group \"controls_session_1\":", sum(all_groups_dataframe$group == "controls_session_1") - sum(participants_with_covariate_data$group == "controls_session_1"), "\n\n")
    
  }
  return(participants_with_covariate_data)
}

covariates_dataframe <- exclude_missing_covariate_data(participants_with_covariate_data, covariate_data_fields)


# Checking the number of participants remaining in groups after removing participants with missing covariates
  # table(factor(covariates_dataframe$group, levels = c("anxiety_ssri_session_1", "anxiety_no_medications_session_1", "controls_session_1")))


# Anxiety symptoms at time of imaging appointment - these will be used to match participants in the two case groups for similar levels of anxiety symptoms
# This is in order to isolate the effect of having an anxiety disorder from the effect of taking an SSRI on rsfMRI data
  # 1970: Nervous feelings
  # 1980: Worrier / anxious feelings
  # 1990: Highly strung
  # 2000: Worry too long after embarrassment
  # 2010: Suffer from 'nerves'
anxiety_symptoms_covariate_data_fields <- c("1970-2.0", "1980-2.0", "1990-2.0", "2000-2.0", "2010-2.0")

# Exclude participants who had missing data (NA) for any of the covariate anxiety symptom data-fields
nrow_before <- nrow(covariates_dataframe)
covariates_dataframe <- 
  covariates_dataframe %>% 
  filter(!(if_any(anxiety_symptoms_covariate_data_fields, ~ is.na(.))))
nrow_after <- nrow(covariates_dataframe)
cat("Number of participants excluded due to NA values in covariate anxiety symptoms:", nrow_before - nrow_after, "\n")


# Exclude participants who answered "Prefer not to answer" (coded as -3) for any of the covariate anxiety symptom data-fields
nrow_before <- nrow(covariates_dataframe)
covariates_dataframe <- 
  covariates_dataframe %>% 
  filter(!(if_any(anxiety_symptoms_covariate_data_fields, ~ . %in% c(-3))))
nrow_after <- nrow(covariates_dataframe)
cat("Number of participants excluded due to \"Prefer not to answer\" responses in any covariate anxiety symptom data-field:", nrow_before - nrow_after, "\n")

# Exclude participants who answered "Do not know" (coded as -1) for ALL covariate anxiety symptom data-fields
nrow_before <- nrow(covariates_dataframe)
covariates_dataframe <- 
  covariates_dataframe %>% 
  filter(!(if_all(anxiety_symptoms_covariate_data_fields, ~ . == -1)))
nrow_after <- nrow(covariates_dataframe)
cat("Number of participants excluded due to \"Do not know\" responses in ALL covariate anxiety symptom data-fields:", nrow_before - nrow_after, "\n\n")

# Adding a new column to the anxiety symptom covariate dataframe which contains the total number of times a subject answered "Yes" to having an anxiety symptom (ignoring "Do not know" answers)
# This block takes about 1 minute to complete
cat("Calculating total number of \"Yes\" answers given by every subject individually - this will take about 1 minute...", "\n")
covariates_dataframe <- 
  covariates_dataframe %>% 
  rowwise() %>% 
  mutate(number_of_anxiety_symptoms = sum(c_across(anxiety_symptoms_covariate_data_fields) == 1)) %>% 
  ungroup()


# Depression symptoms at time of imaging appointment - these will be used to match the subjects in the two case groups
# This is based on data-field 2050: "Frequency of depressed mood in last 2 weeks" where numerical codes correspond to:
  # 1: Not at all
  # 2: Several days
  # 3: More than half the days
  # 4: Nearly every day
  # -1: Do not know
  # -3: Prefer not to answer

# Excluding participants with NA (missing) values
nrow_before <- nrow(covariates_dataframe)
covariates_dataframe <- 
  covariates_dataframe %>% 
  filter(!(is.na(`2050-2.0`)))
nrow_after <- nrow(covariates_dataframe)
cat("Number of participants excluded due to NA responses in depression symptom data-field:", nrow_before - nrow_after, "\n")

# Excluding participants who answered "Prefer not to answer"
nrow_before <- nrow(covariates_dataframe)
covariates_dataframe <- 
  covariates_dataframe %>% 
  filter(!(`2050-2.0` == -3))
nrow_after <- nrow(covariates_dataframe)
cat("Number of participants excluded due to \"Prefer not to answer\" responses in depression symptom data-field:", nrow_before - nrow_after, "\n")

# Excluding participants who answered "Do not know"
nrow_before <- nrow(covariates_dataframe)
covariates_dataframe <- 
  covariates_dataframe %>% 
  filter(!(`2050-2.0` == -1))
nrow_after <- nrow(covariates_dataframe)
cat("Number of participants excluded due to \"Do not know\" responses in depression symptom data-field:", nrow_before - nrow_after, "\n\n")

# Displaying the number of participants remaining in each group
table(factor(covariates_dataframe$group, levels = c("anxiety_ssri_session_1", "anxiety_no_medications_session_1", "controls_session_1")))


###############################################################################################################################################
# MATCHING PARTICIPANTS IN CASE GROUPS (anxiety_ssri_session_1 and anxiety_no_medications_session_1)
###############################################################################################################################################
library(MatchIt)
library(optmatch)
library(cobalt)

cat("Performing propensity score analysis to match participants...", "\n")

# Converting the "Date of attending assessment centre" (`53-2.0`) column from date format to numeric format (number of days since the earliest imaging session attendance)
earliest_date <- min(covariates_dataframe$`53-2.0`, na.rm = TRUE) # Finding the earliest date of a subject attending an imaging session
covariates_dataframe$`53-2.0` <- as.numeric(covariates_dataframe$`53-2.0` - earliest_date) # Dates are converted into numeric value of number of days since earliest attendance date


# Creating a new dataframe containing only the two case groups. The group names are recoded as 0 or 1 to work with the glm function
  # 0: anxiety_no_medications_session_1
  # 1: anxiety_ssri_session_1
# Biological sex is also recoded:
  # 0: Female
  # 1: Male
covariates_dataframe_cases <- 
  covariates_dataframe %>% 
  filter(group %in% c("anxiety_ssri_session_1", "anxiety_no_medications_session_1")) %>% 
  mutate(group = ifelse(group == "anxiety_ssri_session_1", 1, 0)) %>%
  mutate(`31-0.0` = factor(`31-0.0`, levels = c(0, 1), labels = c("Female", "Male")))

# Renaming all covariable columns, as the original format does not work in the matchit function
names(covariates_dataframe_cases)[names(covariates_dataframe_cases) == "21003-2.0"] <- "age"
names(covariates_dataframe_cases)[names(covariates_dataframe_cases) == "31-0.0"] <- "sex"
names(covariates_dataframe_cases)[names(covariates_dataframe_cases) == "53-2.0"] <- "date"
names(covariates_dataframe_cases)[names(covariates_dataframe_cases) == "24441-2.0"] <- "head_motion"
names(covariates_dataframe_cases)[names(covariates_dataframe_cases) == "2050-2.0"] <- "depressed_mood_frequency"


# Checking which covariates have a statistically significant relationship to a subject being in the "anxiety_ssri_session_1" vs the "anxiety_no_medications_session_1"
# i.e. the covariates which are related to a case subject taking an ssri vs not taking any medications
covariates_dataframe_cases_covariates_significance <- 
  glm(group ~
        age +
        sex +
        head_motion +
        depressed_mood_frequency +
        number_of_anxiety_symptoms,
      data = covariates_dataframe_cases,
      family = "binomial")

summary(covariates_dataframe_cases_covariates_significance)


# Matching subjects in the anxiety_ssri_session_1 with subjects in the anxiety_no_medications_session_1 group (the two case groups)
# Genetic matching is used
set.seed(1234)
psa_genetic_cases_only <- 
  matchit(group ~
            age +
            I(age^2) +
            sex +
            date +
            head_motion +
            I(head_motion^2) +
            number_of_anxiety_symptoms + 
            I(number_of_anxiety_symptoms^2) +
            depressed_mood_frequency +
            I(depressed_mood_frequency^2),
          data = covariates_dataframe_cases,
          distance = "glm",
          method="genetic", 
          link = "logit",
          ratio = 1,
          pop.size = 200,
          replace = F)

love_plot_cases_covariate_names <- c(age = "Age",
                                   `I(age^2)` = "Age^2",
                                       sex = "Sex(Male)",
                                       date = "Date of scan",
                                       head_motion = "Head motion",
                                       `I(head_motion^2)` = "Head motion^2",
                                       number_of_anxiety_symptoms = "Number of anxiety symptoms",
                                       `I(number_of_anxiety_symptoms^2)` = "Number of anxiety symptoms^2",
                                       depressed_mood_frequency = "Depressed mood frequency",
                                       `I(depressed_mood_frequency^2)` = "Depressed mood frequency^2"
                                     )

love.plot(bal.tab(psa_genetic_cases_only),
          stat = c("m", "v"),
          grid = TRUE,
          threshold = c(m=.1, v=1.25),
          stars = "raw",
          var.names = love_plot_cases_covariate_names,
          drop.distance = TRUE,
          colors = c("#FF475C", "#04A1FE"),
          shapes = c("circle"),
          size = 4) # Love plot that shows the covariate balances before and after matching (shows standardised mean differences and variance ratios)


psa_genetic_cases_only_factor_modified <- psa_genetic_cases_only


# Changing covariates from numerical to factors within Matchit object
psa_genetic_cases_only_factor_modified$X$number_of_anxiety_symptoms <- as.factor(psa_genetic_cases_only_factor_modified$X$number_of_anxiety_symptoms)
psa_genetic_cases_only_factor_modified$X$depressed_mood_frequency <- as.factor(psa_genetic_cases_only_factor_modified$X$depressed_mood_frequency)

# Display details about the matching performed above, including number of subjects matched
  # 'Control' refers to the anxiety_no_medications_session_1 group
  # 'Treated' refers to the anxiety_ssri_session_1 group
psa_genetic_cases_only_factor_modified
summary(psa_genetic_cases_only_factor_modified, un = FALSE)

# Visualizing the balance of covariates before and after matching
plot(psa_genetic_cases_only_factor_modified, type = "jitter", interactive = FALSE) # Jitter plot of propensity scores

# Uncomment to view density plots of covariates before and after matching
plot(psa_genetic_cases_only_factor_modified, type = "density", interactive = TRUE,
     which.xs = ~age + I(age^2) + sex + number_of_anxiety_symptoms + depressed_mood_frequency) # Density plots of covariates before and after matching (light grey: anxiety_no_medications_session_1, black: anxiety_ssri_session_1)


# Creates a dataframe with only participants included in the above matching of the two case groups (anxiety with or without SSRI)
matched_cases <- match.data(psa_genetic_cases_only_factor_modified)

# Creates a dataframe which includes participants in the anxiety_ssri_session_1 case group (anxiety with SSRI)
# This is coded as `1` in this dataframe
matched_cases_anxiety_ssri_session_1 <- subset(matched_cases, group == 1)

# Creates a dataframe which includes participants in the anxiety_no_medications_session_1 case group (anxiety without SSRI)
# This is coded as `0` in this dataframe
matched_cases_anxiety_no_medications_session_1 <- subset(matched_cases, group == 0)


###############################################################################################################################################
# MATCHING PARTICIPANTS IN CASE GROUPS WITH MENTALLY HEALTHY CONTROLS
###############################################################################################################################################

# Creating a new dataframe containing only the two case groups AND the control group. The group names are recoded as 0 or 1 to work with the glm function
# The two case groups are combined and coded as 1 and a new column is created to indicate if a subject is a case or control (this leaves the original 'group' column so we still know what group a subject is in)
  # 1: anxiety_no_medications_session_1
  # 1: anxiety_ssri_session_1
  # 0: controls_session_1
# Biological sex is also recoded:
  # 0: Female
  # 1: Male
covariates_dataframe_cases_and_controls <- 
  covariates_dataframe %>% 
  filter((eid %in% matched_cases$eid) | (group == "controls_session_1")) %>% # Only include previously matched case subjects and all mentally healthy control subjects
  mutate(group_case_or_control = ifelse(group %in% c("anxiety_ssri_session_1", "anxiety_no_medications_session_1"), 1, 0)) %>%
  mutate(`31-0.0` = factor(`31-0.0`, levels = c(0, 1), labels = c("Female", "Male")))

names(covariates_dataframe_cases_and_controls)[names(covariates_dataframe_cases_and_controls) == "21003-2.0"] <- "age"
names(covariates_dataframe_cases_and_controls)[names(covariates_dataframe_cases_and_controls) == "31-0.0"] <- "sex"
names(covariates_dataframe_cases_and_controls)[names(covariates_dataframe_cases_and_controls) == "53-2.0"] <- "date"
names(covariates_dataframe_cases_and_controls)[names(covariates_dataframe_cases_and_controls) == "24441-2.0"] <- "head_motion"

# Checking which covariates have a statistically significant relationship to a subject being in either case group ("anxiety_ssri_session_1" OR "anxiety_no_medications_session_1") vs control group ("controls_session_1")
# i.e. the covariates which are related to a subject ever having an anxiety disorder
covariates_dataframe_cases_and_controls_covariates_significance <- 
  glm(group_case_or_control ~
        age +
        sex +
        head_motion,
      data = covariates_dataframe_cases_and_controls,
      family = "binomial")

summary(covariates_dataframe_cases_and_controls_covariates_significance)


# Matching all case subjects from the two case groups with subjects in the mentally healthy control group (controls_session_1)
# Matching balances the same covariates as the matching of the two case groups EXCEPT for covariates related to anxiety or depression symptoms
# Uses the genetic matching method
cat("Matching case subjects in both case groups to control subjects. This takes around 5 minutes to complete...\n")
set.seed(1234)
psa_genetic_cases_and_controls <- 
  matchit(group_case_or_control ~
            age +
            I(age^2) +
            sex +
            date +
            head_motion +
            I(head_motion^2),
          data = covariates_dataframe_cases_and_controls,
          distance = "glm",
          method="genetic", 
          link = "logit",
          ratio = 1,
          pop.size = 200,
          replace = F)


# Display details about the matching performed above, including number of subjects matched
# 'Control' refers to the controls_session_1
# 'Treated' refers to the two case groups; anxiety_ssri_session_1 AND anxiety_no_medications_session_1
psa_genetic_cases_and_controls
summary(psa_genetic_cases_and_controls, un = FALSE)

love_plot_cases_controls_covariate_names <- c(age = "Age",
                                     `I(age^2)` = "Age^2",
                                     sex = "Sex(Male)",
                                     date = "Date of scan",
                                     head_motion = "Head motion",
                                     `I(head_motion^2)` = "Head motion^2"
)

                                     love.plot(bal.tab(psa_genetic_cases_and_controls),
                                               stat = c("m", "v"),
                                               grid = TRUE,
                                               threshold = c(m=.1, v=1.25),
                                               stars = "raw",
                                               var.names = love_plot_cases_controls_covariate_names,
                                               drop.distance = TRUE,
                                               colors = c("#FF475C", "#04A1FE"),
                                               shapes = c("circle"),
                                               size = 4) # Love plot that shows the covariate balances before and after matching (shows standardised mean differences and variance ratios)
                                     

love.plot(bal.tab(psa_genetic_cases_and_controls),
          stat = c("m", "v"),
          grid = TRUE,
          threshold = c(m=.1, v=1.25),
          stars = "raw") # Love plot that shows the covariate balances before and after matching (shows standardised mean differences and variance ratios)


# Visualizing the balance of covariates before and after matching
plot(psa_genetic_cases_and_controls, type = "jitter", interactive = FALSE) # Jitter plot of propensity scores

# Uncomment to view density plots of covariates before and after matching
# plot(psa_genetic_cases_and_controls, type = "density", interactive = TRUE,
#      which.xs = ~age + I(age^2) + sex + head_motion + I(`head_motion`^2)) # Density plots of covariates before and after matching (light grey: anxiety_no_medications_session_1, black: anxiety_ssri_session_1)


matched_all_groups <- match.data(psa_genetic_cases_and_controls)

###############################################################################################################################################
# SAVING MATCHED PARTICIPANT DATAFRAMES AS RDS FILES
###############################################################################################################################################

# Case groups and matched control group participants
# saveRDS(matched_all_groups, file = "Data/matched_all_groups.rds")


# Unmatched cases
cases_unmatched_summary <- 
  covariates_dataframe_cases %>%
  select(group, sex) %>% 
  group_by(group) %>% 
  summarise(males = sum(sex == "Male"),
            females = sum(sex == "Female"),
            percent_female = (females/(males + females))*100,
            percent_male = (males/(males + females))*100)

cases_unmatched_summary %>% View()  


# Matched cases
cases_matched_summary <-
matched_all_groups %>% 
  select(group, sex) %>% 
  group_by(group) %>% 
  summarise(males = sum(sex == "Male"),
            females = sum(sex == "Female"),
            percent_female = (females/(males + females))*100,
            percent_male = (males/(males + females))*100)


View(cases_matched_summary)
  