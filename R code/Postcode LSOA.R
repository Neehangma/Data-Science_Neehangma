library(tidyverse)

# 1. Paths
postcode_file <- "C:/Users/NMRAI/Desktop/Obtained_Data/Postcode_to_LSOA.csv"
house_price_file <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/HousePrices_cleaned.csv"
output_path <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data"

# 2. Load Reference Data to fill the "NA" gaps
# We create a mapping from ShortPostcode to Town/District/County
cat("Loading House Price reference data...\n")
house_ref <- read_csv(house_price_file, show_col_types = FALSE) %>%
  select(shortPostcode, Town, District, County) %>%
  distinct(shortPostcode, .keep_all = TRUE) # Keep one record per postcode

# 3. Load and Clean Postcode Data
cat("Reading and cleaning Postcode to LSOA file...\n")
postcode_data <- read_csv(postcode_file, show_col_types = FALSE)

postcode_cleaned <- postcode_data %>%
  # Standardize column names
  rename_with(~str_to_lower(.) %>% str_replace_all(" ", "_")) %>%
  
  # Identify LSOA and Postcode columns
  mutate(
    lsoa_code_raw = coalesce(!!!select(., matches("lsoa|code"))),
    postcode_raw = coalesce(!!!select(., matches("^pcd|^postcode")))
  ) %>%
  
  # Extract clean codes
  mutate(
    LSOA_Code = str_to_upper(str_trim(lsoa_code_raw)),
    postcode_clean = str_to_upper(str_replace_all(postcode_raw, "\\s+", "")),
    ShortPostcode = str_extract(postcode_clean, "^[A-Z]{1,2}[0-9]{1,2}")
  ) %>%
  
  # Filter for Cheshire and Cumbria regions only
  filter(str_detect(ShortPostcode, "^CA|^CH|^CW|^WA")) %>%
  
  # 4. JOIN with reference data to replace NAs/Codes with Names
  # This replaces codes like 'E07000028' with 'CUMBERLAND'
  select(LSOA_Code, ShortPostcode) %>% 
  left_join(house_ref, by = c("ShortPostcode" = "shortPostcode")) %>%
  
  # 5. Final Polish
  distinct(LSOA_Code, .keep_all = TRUE) %>%
  # Remove any rows that still have NAs after the join
  drop_na(Town, District, County) %>%
  select(LSOA_Code, ShortPostcode, Town, District, County)

# 6. Save and Summary
output_file <- file.path(output_path, "postcode_to_LSOA_cleaned.csv")
write_csv(postcode_cleaned, output_file)

cat("\n========================================\n")
cat("CLEANING COMPLETE - NO NAs REMAINING\n")
cat("========================================\n")
cat("Total records saved:", nrow(postcode_cleaned), "\n")
print







