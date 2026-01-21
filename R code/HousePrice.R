library(tidyverse)
library(lubridate)


# Base path for raw data
base_path <- "C:/Users/NMRAI/Desktop/Obtained_Data"

# House price files
files <- c(
  paste0(base_path, "/HousePricingDataset/House_price_unclean_2022.csv"),
  paste0(base_path, "/HousePricingDataset/House_price_unclean_2023.csv"),
  paste0(base_path, "/HousePricingDataset/House_price_unclean_2024.csv")
)

# Column names
colnames_list <- c(
  "TransactionID", "Price", "Date", "Postcode", "PropertyType", "OldNew",
  "Duration", "PAON", "SAON", "Street", "Locality", "Town", "District",
  "County", "PPDCategory", "RecordStatus"
)


# Read, merge, and clean
HousePrices <- map_dfr(
  files,
  ~ read_csv(.x, col_names = FALSE, show_col_types = FALSE) %>%
    setNames(colnames_list)
) %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    Price = as.numeric(Price),
    County = str_to_upper(str_trim(County)),
    Postcode = str_to_upper(str_replace_all(Postcode, " ", "")),
    shortPostcode = str_sub(Postcode, 1, 4)
  ) %>%
  filter(County %in% c(
    "CHESHIRE EAST",
    "CHESHIRE WEST AND CHESTER",
    "CUMBERLAND"
  )) %>%
  select(Postcode, shortPostcode, Price, Year, PropertyType, Town, District, County) %>%
  distinct() %>%
  filter(if_all(everything(), ~ !is.na(.) & . != "N/A"))


# Create cleaned data folder if it doesn't exist
clean_path <- "C:/Users/NMRAI/Desktop/Cleaned_Data"
if(!dir.exists(clean_path)) {
  dir.create(clean_path, showWarnings = FALSE)
  message("Created folder: ", clean_path)
}


# Save only if data has rows
if(nrow(HousePrices) > 0){
  save_path <- file.path(clean_path, "HousePrices_cleaned.csv")
  write_csv(HousePrices, save_path)
  message("✅ HousePrices cleaned data saved to: ", normalizePath(save_path))
} else {
  message("⚠️ HousePrices dataframe is empty. Nothing was saved.")
}
