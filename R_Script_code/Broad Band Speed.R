library(tidyverse)

# ==========================
# Base paths
# ==========================
broadband_folder <- "C:/Users/NMRAI/Desktop/Obtained_Data/BroadbandSpeed"
clean_path <- "C:/Users/NMRAI/Desktop/Cleaned_Data"

if(!dir.exists(clean_path)) dir.create(clean_path, showWarnings = FALSE)

# ==========================
# List CSV files
# ==========================
broadband_files <- list.files(broadband_folder, pattern = "\\.csv$", full.names = TRUE)

# ==========================
# Function to clean a single broadband file
# ==========================
clean_broadband <- function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  
  # Check column names
  cols <- tolower(names(df))
  names(df) <- str_replace_all(cols, " ", "_")  # convert to lowercase + underscores
  
  # Rename only if column exists
  if("average_download_speed_mbit_s" %in% names(df)) names(df)[names(df) == "average_download_speed_mbit_s"] <- "avg_download"
  if("maximum_download_speed_mbit_s" %in% names(df)) names(df)[names(df) == "maximum_download_speed_mbit_s"] <- "max_download"
  
  # Postcode cleanup
  if("postcode" %in% names(df)){
    df <- df %>%
      mutate(
        postcode = str_to_upper(str_replace_all(postcode, " ", "")),
        short_postcode = str_sub(postcode, 1, 4)
      )
  }
  
  # Convert numeric columns safely
  if("avg_download" %in% names(df)) df$avg_download <- as.numeric(str_remove_all(df$avg_download, "[^0-9\\.]"))
  if("max_download" %in% names(df)) df$max_download <- as.numeric(str_remove_all(df$max_download, "[^0-9\\.]"))
  
  # Remove duplicates and missing rows
  df %>%
    distinct() %>%
    filter(if_all(everything(), ~ !is.na(.) & . != "N/A"))
}

# ==========================
# Clean and merge all broadband files
# ==========================
Broadband <- map_dfr(broadband_files, clean_broadband)

# ==========================
# Save cleaned broadband data
# ==========================
if(nrow(Broadband) > 0){
  save_path <- file.path(clean_path, "Broadband_Cleaned.csv")
  write_csv(Broadband, save_path)
  message("✅ Broadband cleaned data saved to: ", normalizePath(save_path))
} else {
  message("⚠️ Broadband dataframe is empty. Nothing was saved.")
}
