# This script contains one function (get_ICD_codes) which extracts ICD9 or ICD10 confound diagnoses codes and meanings
# Input:
  # ICD_version - This is a numerical value - either 9 or 10 corresponding to ICD9 or ICD10
# Output: 
  # selected_codes_and_meanings - This is a data frame with 2 columns: 'coding' (the numerical code for a diagnosis) and 'meaning' (the actual diagnosis name)

get_ICD_codes <- function(ICD_version) {

  # Converts the numerical input (9 or 10) to a character ("9" or "10") and chooses either the ICD9 or ICD10 comment based on the function input
  ICD_version <- as.character(ICD_version)
  if (ICD_version == "9") {
    comment_name <- comment_41271
  } else if (ICD_version == "10") {
     comment_name <- comment_41270
  } else {
      stop("Input needs to be either <9> or <10> for ICD version")
  }
  
  # Creates the name of the file location, based on the input (9 or 10) to choose the correct _coding.tsv file
  ICD_tsv_file_location <- paste0("Data/ICD", ICD_version, "_coding.tsv")
  
  # Uncomment if wanting to check all of the codes that will be extracted
  # Check which codes will be extracted from the above comment string and count the number of codes
    # str_view_all(comment_name, "(?<=: )(\\S+)")
    # str_count(comment_name, "(?<=: )(\\S+)")
  
  # Replacing the code ranges (e.g. I60-I69) with individual codes (e.g. I60, I61, I62... etc)
  # All of the individual codes extracted from the ranges are stored in a character vector: ICD_confound_codes
  # str_view_all(comment_name, "(?<=: )(\\S+-\\S+)") # Uncomment to view strings to be extracted
  ICD_code_ranges <- str_extract_all(comment_name, "(?<=: )(\\S+-\\S+)")[[1]]
  ICD_confound_codes <- character()
  for (i in seq_along(ICD_code_ranges)) {
    num_seq <- c(str_extract_all(ICD_code_ranges[i], "\\d+")[[1]]) %>% lapply(as.numeric) %>% unlist()
    num_seq <- seq(num_seq[[1]], num_seq[[2]])
    letter <- str_extract(ICD_code_ranges[i], "[A-Z]*") # This extracts the letter for ICD10 codes, but does nothing for ICD9 codes (which have no letter)
    ICD_confound_codes <- paste0(letter, num_seq) %>% c(ICD_confound_codes, .)
  }
  
  # Extracting all ICD codes which aren't formatted as a range (i.e those not extracted above) and appending them into the ICD confound code vector (ICD_confound_codes)
  # E.g. F00
  # str_view_all(comment_name, "(?<=: )(\\w+)(?=\\s+)") # Uncomment to view strings to be extracted
  # str_count(comment_name, "(?<=: )(\\w+)(?=\\s+)") # Uncomment to count strings to be extracted
  ICD_confound_codes <- (str_extract_all(comment_name, "(?<=: )(\\w+)(?=\\s+)")[[1]]) %>% c(ICD_confound_codes)
  
  # Loads the tsv file for either ICD9 or ICD10 diagnoses (depending on function input) containing all codes and associated diagnoses
  # These are named 'ICD9_coding.tsv' and 'ICD10_coding.tsv'
  # These files are available to download from the biobank showcase
    # ICD9: https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=87
    # ICD10: https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=19
  ICD_tsv <- read_tsv(ICD_tsv_file_location)
  
  # Formatting the code prefixes in ICD_confound_codes with a regular expression suffix "\\d*"
  # This allows for selection of any codes starting with a prefix in ICD_confound_codes and ending with 0 or more digits
  # i.e. this selects all subtypes of a diagnosis class
  # e.g. the prefix "F00" (Dementia in Alzheimer's disease) will result in matching F000 (Dementia in Alzheimer's disease with early onset)...
  # F001 (Dementia in Alzheimer's disease with late onset), F002 (Dementia in Alzheimer's disease, atypical or mixed type)... etc
  patterns <- paste0("^", ICD_confound_codes, "\\d*")
  
  # Creates a character vector containing the codes within the biobank dataset for ICD diagnoses to be excluded
  exclusion_diagnosis_codes <- character()
  for(code in patterns) {
    new_codes <- 
      grepl(code, ICD_tsv$coding) %>% 
      which() %>% 
      ICD_tsv$coding[.]
    
    exclusion_diagnosis_codes <- c(exclusion_diagnosis_codes, new_codes)
  }    
  
  # Creates a data frame of the ICD diagnosis codes to be excluded, and their associated meanings 
  selected_codes_and_meanings <- 
    ICD_tsv %>% 
    filter((selectable == "Y") & (coding %in% exclusion_diagnosis_codes)) %>% 
    select(coding, meaning)
  
  # Uncomment if wanting to view codes and associated diagnoses
  # Display the data frame created above in text format that can be copy-pasted elsewhere, as well as in table format 
    # gsub("\\.", "", selected_codes_and_meanings$meaning) %>% sub(" ", ": ", .) %>% paste0(collapse = "\n") %>% cat()
    # selected_codes_and_meanings %>% View()
  
  return(selected_codes_and_meanings)
}