library(tidyverse)
library(officer)
library(flextable)

doc <- readLines(con = "C:/Users/james/Desktop/James Dissertation Draft.txt")

str_view_all(string = doc, pattern = "[A-Z]{2,}")

two_capitals <- str_extract_all(string = doc, pattern = "[A-Z]{2,}")

two_capitals_character <- unlist(two_capitals) %>% unique()

two_capitals_tibble <- tibble(two_capitals_character)

writeLines(text = two_capitals_character, con = "Data/abbreviations.txt")

final_edited_abbreviations <- readLines(con = "Data/abbreviations.txt")

final_tibble <- tibble(final_edited_abbreviations)

word_doc <- read_docx()
table <- flextable(final_tibble)

word_doc <- body_add_flextable(word_doc, value = table)

print(word_doc, target = "Data/abbreviations_word_table.docx")
