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




# Trialed the below code to see if it did a better job than zotero, but not really.

# # GET PDFs
# # remotes::install_github("ropensci/fulltext")
# # remotes::install_github("ropensci/rplos")
# # remotes::install_github("ropensci/microdemic")
# 
# library(rcrossref)   # For DOI metadata lookup
# library(fulltext)    # For fetching full-text articles
# library(httr)        # For handling HTTP requests
# library(rvest)       # For web scraping when needed
# 
# wos_search <- read.xlsx("processed_data/combined_search27.xlsx")
# doi_list <- wos_search$r_DOI
# 
# download_pdfs <- function(doi_list, save_dir = "processed_data/pdf_downloads2") {
#   # Create directory if it doesn’t exist
#   if (!dir.exists(save_dir)) dir.create(save_dir)
#   
#   # Create an empty vector to store DOIs without PDFs
#   missing_pdfs <- c()  
#   
#   for (doi in doi_list) {
#     # Get URL for full text (may not always return PDF link)
#     url <- tryCatch({
#       cr_works(doi)$data$link[[1]]$URL
#     }, error = function(e) NULL)  # Handle errors in DOI lookup
#     
#     if (!is.null(url)) {
#       pdf_path <- file.path(save_dir, paste0(gsub("/", "_", doi), ".pdf"))
#       
#       # Try downloading the file and check response
#       response <- tryCatch({
#         GET(url, write_disk(pdf_path, overwrite = TRUE))
#       }, error = function(e) NULL)  # Capture download failure
#       
#       # If response is NULL or an error occurred, add to missing list
#       if (is.null(response) || http_error(response)) {
#         message(paste("Download failed:", doi))
#         missing_pdfs <- c(missing_pdfs, doi)
#       } else {
#         message(paste("Downloaded:", doi))
#       }
#       
#     } else {
#       message(paste("No PDF found for:", doi))
#       missing_pdfs <- c(missing_pdfs, doi)
#     }
#   }
#   
#   # Return the list of missing PDFs
#   return(missing_pdfs)
# }
# 
# # Run function
# missing <- download_pdfs(doi_list)
# 
# # Find the papers missing
# missing_df <- data.frame(DOI = missing)
# missing_allmeta_df <- wos_search %>% filter(r_DOI %in% missing_df$DOI)
# 
# 
# 
# # trying another download method
# install.packages("roadoi")
# library(roadoi)
# 
# # Retrieve Open Access links for DOIs
# oa_pdfs <- oadoi_fetch(missing, email = "jsevigny@ucsc.edu")
# 
# # Extract available PDF links
# 
# pdf_links <- oa_pdfs %>% 
#   mutate(pdf_url = map_chr(best_oa_location, ~ if (!is.null(.x$url_for_pdf)) .x$url_for_pdf else NA))
# 
# pdf_urls <- pdf_links$pdf_url
# 
# # Print DOIs without Open Access PDFs
# oa_missing_pdfs <- doi_list[is.na(pdf_urls)]
# print(missing_pdfs)
# 
# # WOULD NEED TO STILL DOWNLOAD PDFS FROM URLs (CAPTURES 87/397 missing)
# 
# 

