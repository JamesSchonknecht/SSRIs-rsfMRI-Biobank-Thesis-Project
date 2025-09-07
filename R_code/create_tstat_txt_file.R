library(tidyverse)

str <- "
t_tests_contrast_vectors_dataframe <- rbind(c('1', '-1', '0', '0', '0', '0', '0', '0', '0'), # Anxiety + No medicines > Anxiety + SSRI
                                    c('-1', '1', '0', '0', '0', '0', '0', '0', '0'), # Anxiety + SSRI > Anxiety + No medicines
                                    c('1', '0', '-1', '0', '0', '0', '0', '0', '0'), # Anxiety + No medicines > Controls
                                    c('-1', '0', '1', '0', '0', '0', '0', '0', '0'), # Controls > Anxiety + No medicines
                                    c('0', '1', '-1', '0', '0', '0', '0', '0', '0'), # Anxiety + SSRI > Controls
                                    c('0', '-1', '1', '0', '0', '0', '0', '0', '0')) # Controls > Anxiety + SSRI
t_tests_contrast_vectors_dataframe <- as.data.frame(t_tests_contrast_vectors_dataframe)
"
lines <- str_extract_all(str, "(?<=# )(.+)")[[1]]

writeLines(lines, con = "Data/lines.txt")
