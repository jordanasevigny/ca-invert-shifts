library(readxl)
library(dplyr)
library(purrr)

# Set the directory containing Excel files
directory <- "/Users/jordanasevigny/git/ENSO_CalCOFI_dispersals2024/data/WOS_search27"
dir.exists(directory)
basename(list.files(directory, full.names = TRUE))
# Get all Excel file paths
files <- list.files(directory, pattern = "\\.xls[x]?$", full.names = TRUE)

# Read and combine all files row-wise
data_combined <- files %>%
  map_df(~ read_excel(.x))  # Reads each file and binds them row-wise

# Print or save the combined dataset
print(data_combined)

# Optionally, write to a new Excel file
# write.xlsx(data_combined, "combined_data.xlsx")