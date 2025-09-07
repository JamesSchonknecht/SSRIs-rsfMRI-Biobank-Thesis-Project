library(tidyverse)

# Load matched dataframe
matched_all_groups <- readRDS(file = "Data/matched_all_groups.rds")

# Select only columns of interest
demo_chara_matched_all_groups <-
  matched_all_groups %>% 
  select(group, age, sex, starts_with(c("21000", "20003-2"))) %>% 
  mutate(across(starts_with("21000"),
                ~case_when(
                  str_detect(., "^1") ~ "White",
                  str_detect(., "^2") ~ "Mixed",
                  str_detect(., "^3") ~ "Asian or Asian British",
                  str_detect(., "^4") ~ "Black or Black British",
                  str_detect(., "^5") ~ "Chinese",
                  str_detect(., "^6") ~ "Other ethnic group",
                  str_detect(., "-1") ~ "Do not know",
                  str_detect(., "-3") ~ "Prefer not to answer",
                  TRUE ~ as.character(.)
                ))) %>% 
  rowwise() %>% 
  mutate(is_white = "White" %in% c_across(starts_with("2100")),
         is_mixed = "Mixed" %in% c_across(starts_with("2100")),
         is_asian_or_asian_british = "Asian or Asian British" %in% c_across(starts_with("2100")),
         is_black_or_black_british = "Black or Black British" %in% c_across(starts_with("2100")),
         is_chinese = "Chinese" %in% c_across(starts_with("2100")),
         is_other_eth = "Other ethnic group" %in% c_across(starts_with("2100"))) %>% 
  ungroup()

View(demo_chara_matched_all_groups)

# Create summary table
summary_demo_chara_matched_all_groups <- 
  demo_chara_matched_all_groups %>% 
  group_by(group) %>% 
  summarise(mean_age = mean(age, na.rm = T),
            sd_age = sd(age, na.rm = T),
            n_male = sum(sex == "Male", na.rm = T),
            n_female = sum(sex == "Female", na.rm = T),
            percent_male = n_male/(n_male+n_female)*100,
            percent_female = n_female/(n_male+n_female)*100,
            n_white = sum(is_white == T, na.rm = T),
            percent_white = (n_white/n())*100,
            n_mixed = sum(is_mixed == T, na.rm = T),
            percent_mixed = (n_mixed/n())*100,
            n_asian_or_asian_british = sum(is_asian_or_asian_british == T, na.rm = T),
            percent_asian_or_asian_british = (n_asian_or_asian_british/n())*100,
            n_black_or_black_british = sum(is_black_or_black_british == T, na.rm = T),
            percent_black_or_black_british = (n_black_or_black_british/n())*100,
            n_chinese = sum(is_chinese == T, na.rm = T),
            percent_chinese = (n_chinese/n())*100,
            n_other_eth = sum(is_other_eth == T, na.rm = T),
            percent_other_eth = (n_other_eth/n())*100,
            n_total = n()) %>% 
  arrange(factor(group, levels = c("anxiety_ssri_session_1", "anxiety_no_medications_session_1", "controls_session_1")))

View(summary_demo_chara_matched_all_groups)

summary_demo_chara_matched_all_groups %>% select(group,starts_with("n_")) %>% View()
summary_demo_chara_matched_all_groups %>% select(group,starts_with("percent_")) %>% View()


# Calculate total sample characteristics
mean_age_total  <-  mean(demo_chara_matched_all_groups$age, na.rm = T)
sd_age_total <- sd(demo_chara_matched_all_groups$age, na.rm = T)

n_male_total <- sum(demo_chara_matched_all_groups$sex == "Male", na.rm = T)
n_female_total <- sum(demo_chara_matched_all_groups$sex == "Female", na.rm = T)
n_white_total <- sum(demo_chara_matched_all_groups$is_white == TRUE, na.rm = T)
n_mixed_total <- sum(demo_chara_matched_all_groups$is_mixed == TRUE, na.rm = T)
n_asian_or_asian_british_total <- sum(demo_chara_matched_all_groups$is_asian_or_asian_british == TRUE, na.rm = T)
n_black_or_black_british_total <- sum(demo_chara_matched_all_groups$is_black_or_black_british == TRUE, na.rm = T)
n_chinese_total <- sum(demo_chara_matched_all_groups$is_chinese == TRUE, na.rm = T)
n_other_eth_total <- sum(demo_chara_matched_all_groups$is_other_eth == TRUE, na.rm = T)
  
percent_female_total <- (n_female_total/(n_male_total+n_female_total))*100
percent_male_total <- (n_male_total/(n_male_total+n_female_total))*100
percent_white_total <- (n_white_total/(n_white_total+n_mixed_total+n_asian_or_asian_british_total+n_black_or_black_british_total+n_chinese_total+n_other_eth_total))*100
percent_mixed_total <- (n_mixed_total/(n_white_total+n_mixed_total+n_asian_or_asian_british_total+n_black_or_black_british_total+n_chinese_total+n_other_eth_total))*100
percent_asian_or_asian_british_total <- (n_asian_or_asian_british_total/(n_white_total+n_mixed_total+n_asian_or_asian_british_total+n_black_or_black_british_total+n_chinese_total+n_other_eth_total))*100
percent_black_or_black_british_total <- (n_black_or_black_british_total/(n_white_total+n_mixed_total+n_asian_or_asian_british_total+n_black_or_black_british_total+n_chinese_total+n_other_eth_total))*100
percent_chinese_total <- (n_chinese_total/(n_white_total+n_mixed_total+n_asian_or_asian_british_total+n_black_or_black_british_total+n_chinese_total+n_other_eth_total))*100
percent_other_eth_total <- (n_other_eth_total/(n_white_total+n_mixed_total+n_asian_or_asian_british_total+n_black_or_black_british_total+n_chinese_total+n_other_eth_total))*100


# Determine anxiety disorder subtypes



anxiety_subtypes_matched_all_groups <- 
  matched_all_groups %>% 
  select(group, eid, GAD_symptoms_data_fields, starts_with(c("20544-", "20002-", "41271-", "41270-"))) %>% 
  mutate(across(starts_with("41271-"), as.character))

source("Code/R_code/GAD_field_extraction.R") # Running this script to obtain datafield codes used in determining GAD status based off mental health online questionnaire (GAD_symptoms_data_fields)
case_eids_symptom_based_GAD <- get_symptom_based_GAD_case_eids(anxiety_subtypes_matched_all_groups, GAD_symptoms_data_fields)
case_eids_symptom_based_GAD <- as.character(case_eids_symptom_based_GAD$eid)

View(anxiety_subtypes_matched_all_groups)


# Creating modified dataframe with columns indicating which anxiety disorder subtypes participants have
anxiety_subtypes_matched_all_groups <- 
  matched_all_groups %>% 
  select(group, eid, GAD_symptoms_data_fields, starts_with(c("20544-", "20002-", "41271-", "41270-"))) %>% 
  mutate(across(everything(), as.character)) %>% 
  rowwise() %>% 
  mutate(
    has_SAD = case_when( # Social Anxiety Disorder
      "1" %in% c_across(starts_with("20544-")) ~ TRUE,
      "F401" %in% c_across(starts_with("41270-")) ~ TRUE, # ICD10
      TRUE ~ FALSE
    ),
    has_PD = case_when( # Panic Disorder
      "6" %in% c_across(starts_with("20544-")) ~ TRUE,
      "F410" %in% c_across(starts_with("41270-")) ~ TRUE, # ICD10
      TRUE ~ FALSE
    ),
    has_GAD = case_when( # Generalized Anxiety Disorder
      eid %in% case_eids_symptom_based_GAD ~ TRUE,
      "15" %in% c_across(starts_with("20544-")) ~ TRUE,
      "F411" %in% c_across(starts_with("41270-")) ~ TRUE, # ICD10
      TRUE ~ FALSE
    ),
    has_AGOR = case_when( # Agoraphobia
      "17" %in% c_across(starts_with("20544-")) ~ TRUE,
      "F400" %in% c_across(starts_with("41270-")) ~ TRUE, # ICD10
      TRUE ~ FALSE
    ),
    has_UNSP = case_when( # Unspecified anxiety disorder
    (sum(c_across(starts_with("has_"))) == 0) & ("1287" %in% c_across(starts_with("20002-"))) ~ TRUE,
    (sum(c_across(starts_with("has_"))) == 0) & ("3000" %in% c_across(starts_with("41271-"))) ~ TRUE, # ICD9
    TRUE ~ FALSE
    )
  )


anxiety_subtypes_matched_all_groups %>% select(starts_with("has_"))%>%  View()
sum(anxiety_subtypes_matched_all_groups[["has_SAD"]])
sum(anxiety_subtypes_matched_all_groups[["has_PD"]])
sum(anxiety_subtypes_matched_all_groups[["has_GAD"]])
sum(anxiety_subtypes_matched_all_groups[["has_AGOR"]])
sum(anxiety_subtypes_matched_all_groups[["has_UNSP"]])



summary_anxiety_subtypes_matched_all_groups <- 
  anxiety_subtypes_matched_all_groups %>% 
  select(group, eid, starts_with("has_")) %>% 
  group_by(group) %>% 
  summarise(n_SAD = sum(has_SAD == T, na.rm = T),
            n_PD = sum(has_PD == T, na.rm = T),
            n_GAD = sum(has_GAD == T, na.rm = T),
            n_AGOR = sum(has_AGOR == T, na.rm = T),
            n_UNSP = sum(has_UNSP == T, na.rm = T),
            percent_SAD = (n_SAD/n())*100,
            percent_PD = (n_PD/n())*100,
            percent_GAD = (n_GAD/n())*100,
            percent_AGOR = (n_AGOR/n())*100,
            percent_UNSP = (n_UNSP/n())*100
  ) %>% 
  arrange(factor(group, levels = c("anxiety_ssri_session_1", "anxiety_no_medications_session_1", "controls_session_1")))


View(summary_anxiety_subtypes_matched_all_groups)

summary_anxiety_subtypes_matched_all_groups %>% select(starts_with("n_")) %>% View()
summary_anxiety_subtypes_matched_all_groups %>% 
  select(starts_with("percent_")) %>% 
  mutate(across(where(is.double), round, digits = 1)) %>% 
  print()

total_n_SAD <- sum(summary_anxiety_subtypes_matched_all_groups$n_SAD)
total_n_PD <- sum(summary_anxiety_subtypes_matched_all_groups$n_PD)
total_n_GAD <- sum(summary_anxiety_subtypes_matched_all_groups$n_GAD)
total_n_AGOR <- sum(summary_anxiety_subtypes_matched_all_groups$n_AGOR)
total_n_UNSP <- sum(summary_anxiety_subtypes_matched_all_groups$n_UNSP)

total_n_vector <- c(total_n_SAD, total_n_PD, total_n_GAD, total_n_AGOR, total_n_UNSP)
print(total_n_vector)

total_percent_SAD <- (total_n_SAD/488)*100
total_percent_PD <- (total_n_PD/488)*100
total_percent_GAD <- (total_n_GAD/488)*100
total_percent_AGOR <- (total_n_AGOR/488)*100
total_percent_UNSP <- (total_n_UNSP/488)*100

total_percent_vector <- c(total_percent_SAD, total_percent_PD, total_percent_GAD, total_percent_AGOR, total_percent_UNSP)
print(total_percent_vector)

# Determine SSRIs taken by anxiety + SSRI group
medicines_code_tsv <- read_tsv(file = "Data/medicine_codes.tsv")
ssri_codes <- c(1140921600, 1141180212, 1140879540, 1140879544, 
                1140867888, 1140867878, 1141151946, 1141190158, 
                1140867876, 1141174756, 1140867860, 1140882236, 1140867884)

ssri_names <- 
  medicines_code_tsv %>% 
  filter(coding %in% ssri_codes) %>% 
  mutate(meaning = case_when(
    meaning %in% c("citalopram", "cipramil 10mg tablet") ~ "citalopram",
    meaning %in% c("fluoxetine", "prozac 20mg capsule", "oxactin 20mg capsule") ~ "fluoxetine",
    meaning %in% c("escitalopram", "cipralex 5mg tablet") ~ "escitalopram",
    meaning %in% c("fluvoxamine", "faverin 50mg tablet") ~ "fluvoxamine",
    meaning %in% c("sertraline", "lustral 50mg tablet") ~ "sertraline",
    meaning %in% c("paroxetine", "seroxat 20mg tablet") ~ "paroxetine",
    TRUE ~ meaning
  ))

demo_chara_matched_all_groups_SSRI_summary <- 
  demo_chara_matched_all_groups %>% 
  select(group, starts_with("20003-2")) %>%
  group_by(`20003-2.0`) %>% 
  summarise(count = n()) %>% 
  left_join(ssri_names, by = c("20003-2.0" = "coding")) %>% 
  group_by(meaning) %>% 
  summarise(count = sum(count),
            percent = (count/122)*100)


View(demo_chara_matched_all_groups_SSRI_summary)
round(as.numeric(demo_chara_matched_all_groups_SSRI_summary$percent), 1) %>% paste0(" (", ., "%)") %>% cat()


# Determining how many participants were taking each SSRI at each session
ukbb_filtered_full_data <- readRDS(file = "Data/ukbb_filtered_full_data.rds")

ssri_names <- 
  medicines_code_tsv %>% 
  filter(coding %in% ssri_codes)

ukbb_filtered_full_data_SSRIs <- 
  ukbb_filtered_full_data %>% 
  select(eid, starts_with("20003-")) %>% 
  pivot_longer(.,
               cols = starts_with("20003-"),
               names_to = "Column",
               values_to = "Value",
               values_drop_na = TRUE)

ukbb_filtered_full_data_SSRIs_summary <- 
  ukbb_filtered_full_data_SSRIs %>% 
  filter(Value %in% ssri_codes)
  # group_by(Column, Value) %>% 
  # summarise(count = n()) %>% 
  # mutate(Column = as.character(Column))



# All sessions combined
all_sessions_data <- 
  ukbb_filtered_full_data_SSRIs_summary %>% 
  mutate(Column = as.character(Column)) %>% 
  left_join(ssri_names, by = c("Value" = "coding")) %>% 
  group_by(meaning) %>% 
  summarise(count = n()) %>% 
  mutate(percent = (count/24096)*100) %>% 
  select(meaning, count, percent) %>% 
  arrange(desc(percent))

View(all_sessions_data)
sum(all_sessions_data$count)


all_sessions_data <- 
  ukbb_filtered_full_data_SSRIs_summary %>% 
  mutate(Column = as.character(Column)) %>% 
  left_join(ssri_names, by = c("Value" = "coding")) %>% 
  group_by(eid, .add = TRUE) %>% 
  summarise(count = n()) %>% 
  select(eid, count) %>%
  filter(count > 1)


View(all_sessions_data)
sum(all_sessions_data$count)

test_list <- list()
for (session in 0:3) {
  col <- paste0("20003-", session)
  print(col)

test <- 
  ukbb_filtered_full_data_SSRIs %>% 
  filter(str_starts(Column, col)) %>% 
  group_by(eid, Column) %>% 
  summarise(count = n()) %>% 
  ungroup() %>%
  group_by(eid) %>%
  summarise(count = n())

  test_list[[col]] <- test
}

View(test)
min(test$count)

# Exporting above table into a word document
install.packages(c("officer", "flextable"))
library(officer)
library(flextable)

word_table <- flextable(all_sessions_data)

doc <- read_docx()

doc <- body_add_flextable(doc, value = word_table)

print(doc, target = "Data/SSRIs_total_all_sessions.docx")

# Splitting into each session
results_list <- list()
for (session in 0:3) {
  col <- paste0("20003-", session)
  print(col)
  
  
  session_data <-
    ukbb_filtered_full_data_SSRIs_summary %>%
    filter(str_starts(Column, col)) %>%
    group_by(Value) %>%
    left_join(ssri_names, by = c("Value" ="coding")) %>%
    ungroup() %>%
    group_by(meaning) %>%
    summarise(count = n()) %>%
    select(meaning, count) %>% 
    arrange(desc(count))
  
  total_count <- sum(session_data$count)
  
  session_data <-
    session_data %>% 
    mutate(percent = (count / total_count) * 100)
  
  total_row <- 
    session_data %>% 
    summarise(meaning = "total",
              count = sum(count, na.rm = TRUE),
              percent = sum(percent, na.rm = TRUE))
  
  session_data <- bind_rows(session_data, total_row)

  results_list[[col]] <- session_data
  }

results_list[[3]] %>% View()

# Exporting above table into a word document
install.packages(c("officer", "flextable"))
library(officer)
library(flextable)

word_table <- flextable(results_list[[3]])

doc <- read_docx()

doc <- body_add_flextable(doc, value = word_table)

print(doc, target = "Data/SSRIs_first_imaging_session.docx")




# Statistical tests

# Age
anova_age <- aov(age ~group, data = matched_all_groups)
summary (anova_age)

# Sex
tab_sex <- table(matched_all_groups$group, matched_all_groups$sex)
chisq_sex <- chisq.test(tab_sex)
chisq_sex$p.value

# Ethnicity
ethnicity_matrix <- matrix(c(120, 117, 231,
                             0, 2, 6,
                             1, 2, 5,
                             0, 0, 3,
                             0, 1, 0,
                             1, 0, 1),
                           ncol = 3, byrow = TRUE)

colnames(ethnicity_matrix) <- c("Anxiety + SSRI", "Anxiety + NM", "Controls")
rownames(ethnicity_matrix) <- c("White", "Mixed", "Asian or AB", "Black or BB", "Chinese", "Other")

fisher_test_ethnicity <- fisher.test(ethnicity_matrix, simulate.p.value = TRUE, B = 1e7)
fisher_test_ethnicity$p.value

fisher_test_ethnicity <- fisher.test(ethnicity_matrix)
fisher_test_ethnicity$p.value

# Anxiety Subtype
anxiety_matrix <- matrix(c(8, 4,
                           23, 32,
                           83, 91,
                           2, 0,
                           31, 13),
                         ncol = 2, byrow = TRUE)

colnames(anxiety_matrix) <- c("Anxiety + SSRI", "Anxiety + NM")
rownames(anxiety_matrix) <- c("SAD", "PD", "GAD", "AG", "Unspecified")

fisher_test_anxiety <- fisher.test(anxiety_matrix)
fisher_test_anxiety


# Create a vector to store p-values from the individual tests
p_values <- numeric(nrow(anxiety_matrix))

# Perform Fisher's Exact Test for each subtype against the others combined
for (i in 1:nrow(anxiety_matrix)) {
  # Current subtype
  current_subtype <- anxiety_matrix[i, ]
  
  # All other subtypes combined
  other_subtypes <- colSums(anxiety_matrix[-i, ])
  
  # Construct a 2x2 matrix
  subtype_matrix <- rbind(current_subtype, other_subtypes)
  
  # Test
  test_result <- fisher.test(subtype_matrix)
  p_values[i] <- test_result$p.value
}

# Bonferroni correction
alpha <- 0.05
adjusted_alpha <- alpha / length(p_values)

# Check which subtypes are statistically significant after correction
significant_subtypes <- rownames(anxiety_matrix)[p_values < adjusted_alpha]

# Output results
data.frame(Anxiety_Subtype = rownames(anxiety_matrix), P_Value = p_values, 
           Significant_Bonferroni = p_values < adjusted_alpha)



# Finding information on dates of scans
included_participants <- matched_all_groups$eid
all_groups_dataframe <- readRDS(file = "Data/all_groups_dataframe.rds")


matched_all_groups_dates <- 
  all_groups_dataframe %>% 
  select(group, eid, `53-2.0`) %>% 
  filter(eid %in% included_participants)

View(matched_all_groups_dates)

min(matched_all_groups_dates$`53-2.0`)
mean.Date(matched_all_groups_dates$`53-2.0`)
range(matched_all_groups_dates$`53-2.0`)
sd(matched_all_groups_dates$`53-2.0`)


# # FINDING MEAN ANXIETY AND DEPRESSION SCORES FOR EACH GROUP
# library(car)
# 
# matched_all_groups <- readRDS("Data/matched_all_groups.rds")
# 
# matched_all_groups_anx_dep <-
#   matched_all_groups %>% 
#   select(group, number_of_anxiety_symptoms, depressed_mood_frequency)
# 
# 
# groups_anx_dep_summary <- 
#   matched_all_groups_anx_dep %>% 
#   group_by(group) %>% 
#   summarise(`Mean Anxiety` = round(mean(number_of_anxiety_symptoms), 1),
#             `Mean Depression Freq` = round(mean(depressed_mood_frequency), 1),
#             `SD Anxiety` = round(sd(number_of_anxiety_symptoms, na.rm = TRUE), 1),
#             `SD Depression Freq` = round(sd(depressed_mood_frequency, na.rm = TRUE), 1)
#             )
# 
# groups_anx_dep_summary %>% View()

# FINDING MEAN ANXIETY AND MEDIAN DEPRESSION SCORES FOR EACH GROUP
library(car)

matched_all_groups <- readRDS("Data/matched_all_groups.rds")

matched_all_groups_anx_dep <-
  matched_all_groups %>% 
  select(group, number_of_anxiety_symptoms, depressed_mood_frequency)

groups_anx_dep_summary <- 
  matched_all_groups_anx_dep %>% 
  group_by(group) %>% 
  summarise(`Mean Anxiety` = round(mean(number_of_anxiety_symptoms), 1),
            `Median Depression Freq` = median(depressed_mood_frequency),
            `SD Anxiety` = round(sd(number_of_anxiety_symptoms, na.rm = TRUE), 1),
            `1st Quartile Depression Freq` = quantile(depressed_mood_frequency, 0.25, na.rm = TRUE),
            `3rd Quartile Depression Freq` = quantile(depressed_mood_frequency, 0.75, na.rm = TRUE))

groups_anx_dep_summary %>% View()

# Calculate the distribution of depression for each group (participant counts)
count_dist <- matched_all_groups_anx_dep %>%
  select(-number_of_anxiety_symptoms) %>% 
  group_by(group, depressed_mood_frequency) %>%
  summarise(count = n()) %>%
  spread(depressed_mood_frequency, count, fill = 0)

# Calculate the frequency distribution of depression for each group
freq_dist <- matched_all_groups_anx_dep %>%
  select(-number_of_anxiety_symptoms) %>% 
  group_by(group, depressed_mood_frequency) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count / sum(count) * 100, 1)) %>%
  select(-count) %>%
  spread(depressed_mood_frequency, percentage, fill = 0)

# Check for normality
shapiro.test(matched_all_groups_anx_dep$number_of_anxiety_symptoms)
shapiro.test(matched_all_groups_anx_dep$depressed_mood_frequency)

# Check for homogeneity of variance
leveneTest(number_of_anxiety_symptoms ~ group, data = matched_all_groups_anx_dep)
leveneTest(depressed_mood_frequency ~ group, data = matched_all_groups_anx_dep)

# Check shapes of anxiety group distributions
ggplot(matched_all_groups, aes(x = number_of_anxiety_symptoms, fill = group)) +
  geom_density(alpha = 0.5, position = "identity") +
  theme_bw() 

# Check shapes of depression group distributions
ggplot(matched_all_groups, aes(x = depressed_mood_frequency, fill = group)) +
  geom_density(alpha = 0.5, position = "identity") +
  theme_bw() 

# Perform the Kruskal-Wallis H test
kruskal_anx <- kruskal.test(number_of_anxiety_symptoms ~ group, data = matched_all_groups_anx_dep)
kruskal_depr <- kruskal.test(depressed_mood_frequency ~ group, data = matched_all_groups_anx_dep)
print(kruskal_anx)
print(kruskal_depr)

# Perform pairwise comparisons after Kruskal-Wallis test
pairwise_anx <- pairwise.wilcox.test(matched_all_groups_anx_dep$number_of_anxiety_symptoms, matched_all_groups_anx_dep$group, p.adjust.method = "bonferroni")
pairwise_depr <- pairwise.wilcox.test(matched_all_groups_anx_dep$depressed_mood_frequency, matched_all_groups_anx_dep$group, p.adjust.method = "bonferroni")
print(pairwise_anx)
print(pairwise_depr)


# TESTING AGE
library(car)

matched_all_groups <- readRDS("Data/matched_all_groups.rds")

matched_all_groups_age <-
  matched_all_groups %>% 
  select(group, age)

# Check for normality
shapiro.test(matched_all_groups_age$age)

# Check for homogeneity of variance
age_var <- leveneTest(age ~ group, data = matched_all_groups)
print(age_var)

age_data <- matched_all_groups_age$age

# Histogram with a normal density curve
ggplot(data.frame(age_data), aes(x=age_data)) +
  geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
  stat_function(fun=dnorm, args=list(mean=mean(age_data), sd=sd(age_data)), color="red", size=1) +
  ggtitle("Histogram of Age with Normal Curve") +
  xlab("Age") + ylab("Density")

# Q-Q plot
qqnorm(age_data)
qqline(age_data, col = "red", lwd = 2)

# Check for homogeneity of variance
leveneTest(age ~ group, data = matched_all_groups)

# Check age distributions with density plots
ggplot(matched_all_groups, aes(x = age, fill = group)) +
  geom_density(alpha = 0.9, position = "identity") +
  theme_bw() 

# Perform the Kruskal-Wallis H test
kruskal_age <- kruskal.test(age ~ group, data = matched_all_groups)
print(kruskal_age)

# Get median ages for groups
group_ages <- 
  matched_all_groups %>% 
  group_by(group) %>% 
  summarise(
    Q1 = quantile(age, 0.25, na.rm = TRUE),
    `Median Age` = median(age, na.rm = TRUE),
    Q3 = quantile(age, 0.75, na.rm = TRUE),
    IQR = IQR(age, na.rm = TRUE)
    )


group_ages %>% View()
