library(tidyverse)
library(lubridate)
library(janitor)

# 1. DEFINE PATHS
# ------------------------------------------------------------------------------
# Source folder where your raw crime CSVs are stored
crime_base <- "C:/Users/NMRAI/Desktop/Obtained_Data/CrimeRates"

# The specific folder where you want to save the cleaned file
save_dir   <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data"

# Create the folder if it doesn't already exist
if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

# 2. LIST ALL FILES
# ------------------------------------------------------------------------------
crime_files <- list.files(
  path = crime_base,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE)

# 3. PROCESSING FUNCTION
# ------------------------------------------------------------------------------
clean_crime <- function(file) {
  # Read CSV and clean column headers (e.g., 'Crime type' becomes 'crime_type')
  df <- read_csv(file, show_col_types = FALSE) %>% clean_names()
  
  # Check if the 'month' column (the one with "2022-11") exists
  if("month" %in% names(df)) {
    df <- df %>%
      mutate(
        # A. Create a proper Date column (e.g., "2022-11-01")
        # We use .data$month to prevent the 'closure' error
        Date = as.Date(paste0(.data$month, "-01")),
        
        # B. Create Year column (e.g., 2022)
        Year = as.numeric(format(Date, "%Y")),
        
        # C. Create Month column with the full name (e.g., "November")
        Month = format(Date, "%B")
      ) %>%
      # Remove the original 'month' column and move the new ones to the front
      select(Date, Year, Month, everything(), -month)
  }
  
  return(df %>% distinct())
}

# 4. RUN CLEANING AND SAVE
# ------------------------------------------------------------------------------
if(length(crime_files) > 0) {
  cat("Merging and cleaning files. Please ensure 'Crime_Cleaned.csv' is closed in Excel...\n")
  
  CrimeData <- map_dfr(crime_files, clean_crime)
  
  if(nrow(CrimeData) > 0) {
    save_path <- file.path(save_dir, "Crime_Cleaned.csv")
    
    # Try to save, but catch errors if the file is open in Excel
    tryCatch({
      write_csv(CrimeData, save_path)
      cat("\n--- SUCCESS ---\n")
      message("The cleaned file has been saved to: ", normalizePath(save_path))
      
      # Show a quick preview of the results
      print(head(CrimeData %>% select(Date, Year, Month)))
      
    }, error = function(e) {
      cat("\n--- ERROR ---\n")
      message("Could not save the file! Make sure 'Crime_Cleaned.csv' is NOT open in Excel.")
    })
    
  } else {
    message("The combined data is empty. Check if your CSV files contain data.")
  }
} else {
  message("No CSV files were found in the path: ", crime_base)
}