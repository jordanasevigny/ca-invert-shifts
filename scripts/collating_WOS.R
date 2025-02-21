library(readxl)
library(dplyr)
library(purrr)
library(rcrossref)
library(openxlsx)

# Set the directory containing Excel files
directory <- "data/WOS_search27"

# Get all Excel file paths
files <- list.files(directory, pattern = "\\.xls[x]?$", full.names = TRUE)

# Read and combine all files row-wise
data_combined <- files %>%
  map_df(~ read_excel(.x, col_types = "text"))  # Reads each file and binds them row-wise

# Print or save the combined dataset
print(data_combined)

# Randomize row order
data_combined <- data_combined %>%
  sample_frac(1) %>%  # Shuffle rows
  mutate(New_Index = row_number()) %>%
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

any(duplicated(data_combined$r_DOI))
data_combined$r_DOI[duplicated(data_combined$r_DOI) | duplicated(data_combined$r_DOI, fromLast = TRUE)]

# manually look through to remove some articles.
# specifically, I looked at r_DOI duplicates - there were 3
# one needed a different DOI
# second is a calcofi report (i left it in it)... will just duplicate article or be calcofi, either way will get screened out
# I did not alter the third. SEems like maybe a duplicate that was republished?
data_combined_clean = data_combined %>%
  mutate(r_DOI = ifelse(`Article Title` == "New records of ascidians from the NE Pacific:: a new species of Trididemnum, range extension and redescription of Aplidiopsis pannosum (Ritter, 1899) including its larva, and several non-indigenous species", "10.5281/zenodo.4525061", r_DOI))

# move index column to start
data_combined_clean <- data_combined_clean %>%
  select(New_Index, everything()) 

# write to a new Excel file
write.xlsx(data_combined_clean, "processed_data/combined_search27.xlsx")

# format DOIs to load into zotero
comma_separated_dois <- paste(data_combined_clean$r_DOI, collapse = ", ")
print(comma_separated_dois)
# copy and pasted these dois into zotero folder
