library(tidyverse)

# ==========================
# 1. File paths
# ==========================
postcode_file <- "C:/Users/NMRAI/Desktop/Obtained_Data/Postcode_to_LSOA.csv"
clean_path <- "C:/Users/NMRAI/Desktop/Cleaned_Data"

# Create cleaned folder if it doesn't exist
if(!dir.exists(clean_path)) dir.create(clean_path, showWarnings = FALSE)

# ==========================
# 2. Read and clean dataset
# ==========================
Postcode_LSOA <- read_csv(postcode_file, show_col_types = FALSE) %>%
  # Rename the actual postcode column to "postcode"
  rename(postcode = pcds) %>%
  # Standardize column names to lowercase + underscores
  rename_with(~ str_to_lower(.) %>% str_replace_all(" ", "_")) %>%
  # Clean postcode
  mutate(
    postcode = str_to_upper(str_replace_all(postcode, " ", "")),
    short_postcode = str_sub(postcode, 1, 4)
  ) %>%
  # Remove duplicates and incomplete rows
  distinct() %>%
  filter(if_all(everything(), ~ !is.na(.) & . != "N/A"))

# ==========================
# 3. Save cleaned dataset
# ==========================
if(nrow(Postcode_LSOA) > 0){
  save_path <- file.path(clean_path, "Postcode_to_LSOA_Cleaned.csv")
  write_csv(Postcode_LSOA, save_path)
  message("✅ Postcode-to-LSOA cleaned data saved to: ", normalizePath(save_path))
} else {
  message("⚠️ Postcode-to-LSOA dataframe is empty. Nothing was saved.")
}
