library(tidyverse)
library(officer)
library(flextable)

### CORTICAL ###

# Functional connectivity cortical differences (post hoc pairwise t-tests)
differences <- readLines(con = "C:/Users/james/Desktop/Biobank NEW/NIFTI Images/Image_Files/Group_NIFTI_Images/t_tests/differences.txt")

IC11_cort_diff <- differences[2:26]

IC12_cort_diff <- differences[30:42]

IC16_cort_diff <- differences[46:59]

IC_cort_diff_list <- list("IC11_cort_diff" = IC11_cort_diff, "IC12_cort_diff" = IC12_cort_diff, "IC16_cort_diff" = IC16_cort_diff)
IC_cort_diff_tibb_list <- list()

for (ic in names(IC_cort_diff_list)) {
  print(ic)
  print(IC_cort_diff_list[ic])
  
  data <- IC_cort_diff_list[[ic]]
  split_data <- 
    data[!grepl("^=+", data)] %>% 
    str_split(., ":")
  
  region_names <- sapply(split_data, '[', 1)
  percentages <- as.numeric(sapply(split_data, '[', 2))
  
  IC_tibble <- 
    tibble(
      `Cortical region` = region_names,
      `Average probability (%)` = percentages
    ) %>% 
    arrange(desc(`Average probability (%)`)) 
    # mutate(`Relative `Average probability (%)`` = round(((`Average probability (%)` / sum(`Average probability (%)`)) * 100), 4))
  
  IC_cort_diff_tibb_list[[ic]] <- IC_tibble
}

View(IC_cort_diff_tibb_list$IC11_cort_diff)

View(IC_cort_diff_tibb_list$IC12_cort_diff)

View(IC_cort_diff_tibb_list$IC16_cort_diff)


# Melodic components
network_regions <- readLines(con = "C:/Users/james/Desktop/Biobank NEW/NIFTI Images/Image_Files/Group_NIFTI_Images/t_tests/network_regions.txt")
print(network_regions)

IC_11_network <- network_regions[1:36]

IC_12_network <- network_regions[40:68]

IC_16_network <- network_regions[72:96]

IC_network_list <- list("IC11_net" = IC_11_network, "IC12_net" = IC_12_network, "IC16_net" = IC_16_network)
IC_network_tibb_list <- list()

for (ic in names(IC_network_list)) {
  
  data <- IC_network_list[[ic]]
  split_data <- 
    data[!grepl("^=+", data)] %>% 
    str_split(., ":")
  
  region_names <- sapply(split_data, '[', 1)
  percentages <- as.numeric(sapply(split_data, '[', 2))
  
  IC_tibble <- 
    tibble(
      `Cortical region` = region_names,
      `Average probability (%)` = percentages
    ) %>% 
    arrange(desc(`Average probability (%)`))
  
  IC_network_tibb_list[[ic]] <- IC_tibble
}

View(IC_network_tibb_list$IC11_net)

View(IC_network_tibb_list$IC12_net)

View(IC_network_tibb_list$IC16_net)


# Creating word document with tables for melodic networks for each IC 
net_word_doc <- read_docx()

# Creating flextables from and network tables
IC11_cort_net_table <- flextable(IC_network_tibb_list$IC11_net)
IC12_cort_net_table <- flextable(IC_network_tibb_list$IC12_net)
IC16_cort_net_table <- flextable(IC_network_tibb_list$IC16_net)

# Adding flextables to word document
net_word_doc <- body_add_flextable(net_word_doc, value = IC11_cort_net_table)
net_word_doc <- body_add_break(net_word_doc, pos = "after")
net_word_doc <- body_add_flextable(net_word_doc, value = IC12_cort_net_table)
net_word_doc <- body_add_break(net_word_doc, pos = "after")
net_word_doc <- body_add_flextable(net_word_doc, value = IC16_cort_net_table)

# Save word doc of tables of melodic network for each IC
print(net_word_doc, "Data/IC_melodic_network.docx")




# Creating word document with tables for differences for each IC 
cort_diff_word_doc <- read_docx()

# Creating flextables from difference tables
IC11_cort_diff_table <- flextable(IC_cort_diff_tibb_list$IC11_cort_diff)
IC12_cort_diff_table <- flextable(IC_cort_diff_tibb_list$IC12_cort_diff)
IC16_cort_diff_table <- flextable(IC_cort_diff_tibb_list$IC16_cort_diff)

# Adding flextables to word document
cort_diff_word_doc <- body_add_flextable(cort_diff_word_doc, value = IC11_cort_diff_table)
cort_diff_word_doc <- body_add_break(cort_diff_word_doc, pos = "after")
cort_diff_word_doc <- body_add_flextable(cort_diff_word_doc, value = IC12_cort_diff_table)
cort_diff_word_doc <- body_add_break(cort_diff_word_doc, pos = "after")
cort_diff_word_doc <- body_add_flextable(cort_diff_word_doc, value = IC16_cort_diff_table)

# Save word doc of tables of differences for each IC
print(cort_diff_word_doc, "Data/IC_differences.docx")



### SUBCORTICAL ###
differences <- readLines(con = "C:/Users/james/Desktop/Biobank NEW/NIFTI Images/Image_Files/Group_NIFTI_Images/t_tests/differences.txt")

IC11_subcort_diff <- differences[64:68]

IC12_subcort_diff <- differences[72:76]

IC16_subcort_diff <- differences[80:84]

IC_subcort_diff_list <- list("IC11_subcort_diff" = IC11_subcort_diff, "IC12_subcort_diff" = IC12_subcort_diff, "IC16_subcort_diff" = IC16_subcort_diff)
IC_subcort_diff_tibb_list <- list()


for (ic in names(IC_subcort_diff_list)) {
  print(ic)
  print(IC_subcort_diff_list[ic])
  
  data <- IC_subcort_diff_list[[ic]]
  split_data <- 
    data[!grepl("^=+", data)] %>% 
    str_split(., ":")
  
  region_names <- sapply(split_data, '[', 1)
  percentages <- as.numeric(sapply(split_data, '[', 2))
  
  IC_tibble <- 
    tibble(
      `Cortical region` = region_names,
      `Average probability (%)` = percentages
    ) %>% 
    arrange(desc(`Average probability (%)`)) 
  # mutate(`Relative `Average probability (%)`` = round(((`Average probability (%)` / sum(`Average probability (%)`)) * 100), 4))
  
  IC_subcort_diff_tibb_list[[ic]] <- IC_tibble
}

View(IC_subcort_diff_tibb_list$IC11_subcort_diff)

View(IC_subcort_diff_tibb_list$IC12_subcort_diff)

View(IC_subcort_diff_tibb_list$IC16_subcort_diff)


# Melodic components
network_regions <- readLines(con = "C:/Users/james/Desktop/Biobank NEW/NIFTI Images/Image_Files/Group_NIFTI_Images/t_tests/network_regions.txt")
print(network_regions)

IC_11_subcort_network <- network_regions[102:106]

IC_12_subcort_network <- network_regions[110:123]

IC_16_subcort_network <- network_regions[127:134]

IC_subcort_network_list <- list("IC11_subcort_net" = IC_11_subcort_network, "IC12_subcort_net" = IC_12_subcort_network, "IC16_subcort_net" = IC_16_subcort_network)
IC_subcort_network_tibb_list <- list()

for (ic in names(IC_subcort_network_list)) {
  
  data <- IC_subcort_network_list[[ic]]
  split_data <- 
    data[!grepl("^=+", data)] %>% 
    str_split(., ":")
  
  region_names <- sapply(split_data, '[', 1)
  percentages <- as.numeric(sapply(split_data, '[', 2))
  
  IC_tibble <- 
    tibble(
      `Cortical region` = region_names,
      `Average probability (%)` = percentages
    ) %>% 
    arrange(desc(`Average probability (%)`))
  
  IC_subcort_network_tibb_list[[ic]] <- IC_tibble
}

View(IC_subcort_network_tibb_list$IC11_subcort_net)

View(IC_subcort_network_tibb_list$IC12_subcort_net)

View(IC_subcort_network_tibb_list$IC16_subcort_net)



# Creating word document with tables for melodic networks for each IC 
subcort_net_word_doc <- read_docx()

# Creating flextables from and network tables
IC11_subcort_net_table <- flextable(IC_subcort_network_tibb_list$IC11_subcort_net)
IC12_subcort_net_table <- flextable(IC_subcort_network_tibb_list$IC12_subcort_net)
IC16_subcort_net_table <- flextable(IC_subcort_network_tibb_list$IC16_subcort_net)

# Adding flextables to word document
subcort_net_word_doc <- body_add_flextable(subcort_net_word_doc, value = IC11_subcort_net_table)
subcort_net_word_doc <- body_add_break(subcort_net_word_doc, pos = "after")
subcort_net_word_doc <- body_add_flextable(subcort_net_word_doc, value = IC12_subcort_net_table)
subcort_net_word_doc <- body_add_break(subcort_net_word_doc, pos = "after")
subcort_net_word_doc <- body_add_flextable(subcort_net_word_doc, value = IC16_subcort_net_table)

# Save word doc of tables of melodic network for each IC
print(subcort_net_word_doc, "Data/IC_melodic_subcort_network.docx")



# Creating word document with tables for subcortical differences for each IC 
subcort_diff_word_doc <- read_docx()

# Creating flextables from difference tables
IC11_subcort_diff_table <- flextable(IC_subcort_diff_tibb_list$IC11_subcort_diff)
IC12_subcort_diff_table <- flextable(IC_subcort_diff_tibb_list$IC12_subcort_diff)
IC16_subcort_diff_table <- flextable(IC_subcort_diff_tibb_list$IC16_subcort_diff)

# Adding flextables to word document
subcort_diff_word_doc <- body_add_flextable(subcort_diff_word_doc, value = IC11_subcort_diff_table)
subcort_diff_word_doc <- body_add_break(subcort_diff_word_doc, pos = "after")
subcort_diff_word_doc <- body_add_flextable(subcort_diff_word_doc, value = IC12_subcort_diff_table)
subcort_diff_word_doc <- body_add_break(subcort_diff_word_doc, pos = "after")
subcort_diff_word_doc <- body_add_flextable(subcort_diff_word_doc, value = IC16_subcort_diff_table)

# Save word doc of tables of differences for each IC
print(subcort_diff_word_doc, "Data/IC_subcort_differences.docx")



### CEREBELLUM ###

# Melodic Components

network_regions <- readLines(con = "C:/Users/james/Desktop/Biobank NEW/NIFTI Images/Image_Files/Group_NIFTI_Images/t_tests/network_regions.txt")
print(network_regions)


# Function to sum numbers for left and right cerebellum and vermis

cerebellum_sums <- function(data) {
  # Extract and sum for left cerebellum
  left_values <- str_extract(data[str_detect(data, "^Left")], "(?<=:)[0-9.]+")
  left_sum <- sum(as.numeric(left_values))
  
  # Extract and sum for right cerebellum
  right_values <- str_extract(data[str_detect(data, "^Right")], "(?<=:)[0-9.]+")
  right_sum <- sum(as.numeric(right_values))
  
  # Extract and sum for vermis
  vermis_values <- str_extract(data[str_detect(data, "^Vermis")], "(?<=:)[0-9.]+")
  vermis_sum <- sum(as.numeric(vermis_values))
  
  
  cerebellum_sums <- list(left_sum = left_sum, right_sum = right_sum, vermis_sum = vermis_sum)
  
  return(cerebellum_sums)
  
  left_sum
  right_sum
  vermis_sum
}

IC_11_network_cerebellum <- network_regions[139:163]

IC_12_network_cerebellum <- network_regions[167:190]

IC_16_network_cerebellum <- network_regions[194:213]


IC_11_network_cerebellum_sum <- cerebellum_sums(IC_11_network_cerebellum)

IC_12_network_cerebellum_sum <- cerebellum_sums(IC_12_network_cerebellum)

IC_16_network_cerebellum_sum <- cerebellum_sums(IC_16_network_cerebellum)







