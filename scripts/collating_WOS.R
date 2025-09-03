# This script pulls all the web of science results into one dataframe (each page of results had to be downloaded separately), deduplicates,  and adds DOIs
# By: Jordana Sevigny, jordana.sevigny@gmail.com

# Load libraries
library(readxl)
library(dplyr)
library(purrr)
library(rcrossref)
library(openxlsx)

# Set the directory containing Excel files
directory <- "data/WOS_search29"

# Get all Excel file paths
files <- list.files(directory, pattern = "\\.xls[x]?$", full.names = TRUE)

# Read and combine all files row-wise (files are 5-16 because I was trialling the download process the first 5 times to see how many I could download at a time)
data_combined <- files %>%
  map_df(~ read_excel(.x, col_types = "text"))  # Reads each file and binds them row-wise

# Print or save the combined dataset
print(data_combined)

# Randomize row order
data_combined <- data_combined %>%
  sample_frac(1) %>%  # Shuffle rows
  mutate(PreDD_Index = row_number()) %>%
  select(where(~ !all(is.na(.)))) %>%
  mutate(r_DOI = NA)


# Loop through each row
for (i in seq_len(nrow(data_combined))) {
  title <- data_combined$`Article Title`[i]  # Get the article title
  
  if (!is.na(title) && title != "") {  # Check if title is valid
    tryCatch({
      Sys.sleep(1.5)  # Slower requests prevent rate limiting
      result <- cr_works(query = title)$data$doi  
      
      if (length(result) > 0) {
        data_combined$r_DOI[i] <- result[1]  # Store the first DOI found
      }
      
    }, error = function(e) {
      message(paste("Error at row", i, ":", conditionMessage(e)))
      Sys.sleep(5)  # Wait longer before retrying
    })
  }
  
  # Print progress every 10 iterations
  if (i %% 10 == 0) {
    message(paste("Processed", i, "of", nrow(data_combined)))
  }
}

na_indices <- which(is.na(data_combined$r_DOI))


# Loop through each row that didn't work the first time
for (i in seq_len(length(na_indices))) {
  title <- data_combined$`Article Title`[na_indices[i]]  # Get the article title
  
  if (!is.na(title) && title != "") {  # Check if title is valid
    tryCatch({
      Sys.sleep(1.5)  # Slower requests prevent rate limiting
      result <- cr_works(query = title)$data$doi  
      
      if (length(result) > 0) {
        data_combined$r_DOI[na_indices[i]] <- result[1]  # Store the first DOI found
      }
      
    }, error = function(e) {
      message(paste("Error at row", na_indices[i], ":", conditionMessage(e)))
      Sys.sleep(5)  # Wait longer before retrying
    })
  }
  
  # Print progress every 10 iterations
  if (i %% 1 == 0) {
    message(paste("Processed", na_indices[i], "of", nrow(data_combined)))
  }
}

na_indices <- which(is.na(data_combined$r_DOI))

# Check for duplicates
any(duplicated(data_combined$r_DOI))
data_combined$r_DOI[duplicated(data_combined$r_DOI) | duplicated(data_combined$r_DOI, fromLast = TRUE)]

# Deduplicate
data_combined_clean <- data_combined[!duplicated(data_combined$r_DOI), ]

# Move index column and r_DOI to start
data_combined_clean <- data_combined_clean %>%
  mutate(Index = row_number()) %>%
  select(Index, r_DOI, DOI, 'Article Title', 'Source Title', 'Publication Year', everything()) 

# Write to a new Excel file
write.xlsx(data_combined_clean, "processed_data/combined_search29.xlsx")

# Format DOIs to load into zotero
comma_separated_dois <- paste(data_combined_clean$r_DOI, collapse = ", ")
print(comma_separated_dois)
# copy and pasted these dois into zotero folder


