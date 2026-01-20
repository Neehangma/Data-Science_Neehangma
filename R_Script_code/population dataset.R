library(tidyverse)

# ==========================
# 1. Load Population dataset
# ==========================
population_file <- "C:/Users/NMRAI/Desktop/Obtained_Data/Population2011.csv"

PopulationData <- read_csv(population_file, show_col_types = FALSE) %>%
  # Ensure Postcode exists
  filter(!is.na(Postcode)) %>%
  # Create short postcode
  mutate(shortPostcode = str_sub(str_replace_all(str_to_upper(Postcode), " ", ""), 1, 4)) %>%
  # Group by short postcode and sum population
  group_by(shortPostcode) %>%
  summarise(
    Pop2011 = sum(Population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Estimate populations for future years
  mutate(
    Pop2022 = round(Pop2011 * 1.05),        # 5% growth from 2011 to 2022
    Pop2023 = round(Pop2022 * 1.0045),      # small annual growth
    Pop2024 = round(Pop2023 * 1.0042)
  ) %>%
  select(shortPostcode, Pop2022, Pop2023, Pop2024)

# ==========================
# 2. Merge with HousePrices to get Towns data
# ==========================
Towns <- HousePrices %>%
  left_join(PopulationData, by = "shortPostcode") %>%
  select(shortPostcode, Town, District, County, Pop2022, Pop2023, Pop2024) %>%
  distinct() %>%
  filter(if_all(everything(), ~ !is.na(.) & . != "N/A"))

# ==========================
# 3. Save cleaned Towns dataset
# ==========================
clean_path <- "C:/Users/NMRAI/Desktop/Cleaned_Data"
if(!dir.exists(clean_path)) {
  dir.create(clean_path, showWarnings = FALSE)
}

if(nrow(Towns) > 0){
  save_path <- file.path(clean_path, "Towns_Data_Cleaned.csv")
  write_csv(Towns, save_path)
  message("✅ Towns cleaned data saved to: ", normalizePath(save_path))
} else {
  message("⚠️ Towns dataframe is empty. Nothing was saved.")
}
