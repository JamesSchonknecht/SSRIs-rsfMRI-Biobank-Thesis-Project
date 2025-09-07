library(tidyverse)
library(officer)
library(flextable)


# IC11 tstat6
IC11_tstat6 <- read.delim("C:/Users/james/Desktop/Biobank NEW/NIFTI Images/Image_Files/Group_NIFTI_Images/t_tests/IC11/IC11_cluster_info.txt", header = T)

IC11_tstat6_final <- 
  IC11_tstat6 %>% 
  select(Voxels, starts_with("COG"), MAX) %>% 
  filter(Voxels >= 20) %>% 
  mutate("Minimum p-value" = 1-MAX) %>% 
  select(-MAX)

View(IC11_tstat6_final)

# IC12 tstat6
IC12_tstat6 <- read.delim("C:/Users/james/Desktop/Biobank NEW/NIFTI Images/Image_Files/Group_NIFTI_Images/t_tests/IC12/IC12_cluster_info.txt", header = T)

IC12_tstat6_final <- 
  IC12_tstat6 %>% 
  select(Voxels, starts_with("COG"), MAX) %>% 
  filter(Voxels >= 20) %>% 
  mutate("Minimum p-value" = 1-MAX) %>% 
  select(-MAX)

View(IC12_tstat6_final)

#IC16 tstat6
IC16_tstat6 <- read.delim("C:/Users/james/Desktop/Biobank NEW/NIFTI Images/Image_Files/Group_NIFTI_Images/t_tests/IC16/IC16_cluster_info.txt", header = T)

IC16_tstat6_final <- 
  IC16_tstat6 %>% 
  select(Voxels, starts_with("COG"), MAX) %>% 
  filter(Voxels >= 20) %>% 
  mutate("Minimum p-value" = 1-MAX) %>% 
  select(-MAX)

View(IC16_tstat6_final)



# Creating flextables for each IC
IC11_word_table <- flextable(IC11_tstat6_final)
IC12_word_table <- flextable(IC12_tstat6_final)
IC16_word_table <- flextable(IC16_tstat6_final)

# Creating word document with tables for each IC
word_doc <- read_docx()

# Adding flextables to word document
word_doc <- body_add_flextable(x = word_doc, value = IC11_word_table)
word_doc <- body_add_break(word_doc, pos = "after")
word_doc <- body_add_flextable(x = word_doc, value = IC12_word_table)
word_doc <- body_add_break(word_doc, pos = "after")
word_doc <- body_add_flextable(x = word_doc, value = IC16_word_table)

# Save the word document
print(word_doc, "Data/clusters_tables.docx")





