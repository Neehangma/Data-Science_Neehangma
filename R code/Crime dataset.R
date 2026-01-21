library(tidyverse)

# 1. Paths
crime_base <- "C:/Users/NMRAI/Desktop/Obtained_Data/CrimeRates"
clean_path <- "C:/Users/NMRAI/Desktop/Cleaned_Data"

if(!dir.exists(clean_path)) dir.create(clean_path, showWarnings = FALSE)


# 2. List all CSV files recursively
crime_files <- list.files(
  path = crime_base,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)


# 3. Function to clean a single crime file
clean_crime <- function(file) {
  
  # Extract Year and Month from folder names dynamically
  # Assumes path like ".../CrimeRates/YYYY/MM/file.csv"
  path_parts <- str_split(file, .Platform$file.sep)[[1]]
  # Find the last two numeric folders in the path
  numeric_folders <- path_parts[str_detect(path_parts, "^[0-9]{2,4}$")]
  if(length(numeric_folders) >= 2){
    year <- as.integer(numeric_folders[length(numeric_folders)-1])
    month <- as.integer(numeric_folders[length(numeric_folders)])
  } else {
    year <- NA_integer_
    month <- NA_integer_
  }
  
  # Read CSV
  df <- read_csv(file, show_col_types = FALSE)
  
  # Standardize column names
  names(df) <- str_to_lower(names(df)) %>% str_replace_all(" ", "_")
  
  # Clean postcode if exists
  if("postcode" %in% names(df)) {
    df <- df %>%
      mutate(
        postcode = str_to_upper(str_replace_all(postcode, " ", "")),
        short_postcode = str_sub(postcode, 1, 4)
      )
  }
  
  # Add Year and Month
  df <- df %>%
    mutate(
      Year = year,
      Month = month
    )
  
  # Remove duplicates (do not filter NAs yet)
  df %>%
    distinct()
}


# 4. Clean and merge all crime files
CrimeData <- map_dfr(crime_files, clean_crime)

# Remove rows that are fully NA
CrimeData <- CrimeData %>% filter(if_any(everything(), ~ !is.na(.)))


# 5. Save cleaned Crime dataset
if(nrow(CrimeData) > 0){
  save_path <- file.path(clean_path, "Crime_Cleaned.csv")
  write_csv(CrimeData, save_path)
  message("✅ Crime cleaned data saved to: ", normalizePath(save_path))
} else {
  message("⚠️ Crime dataframe is empty. Nothing was saved.")
}
