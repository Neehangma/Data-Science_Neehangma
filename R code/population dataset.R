library(tidyverse)

# 1. Load Population dataset
population_file <- "C:/Users/NMRAI/Desktop/Obtained_Data/Population2011.csv"

PopulationData <- read_csv(population_file, show_col_types = FALSE) %>%
  filter(!is.na(Postcode)) %>%
  # NEW LOGIC: Extract the "Outward" part (everything before the space)
  # This is much safer than just taking the first 4 characters
  mutate(shortPostcode = str_trim(str_extract(Postcode, "^[A-Z]{1,2}[0-9][A-Z0-9]?"))) %>%
  
  # Ensure Population is numeric (removes commas if they exist)
  mutate(Population = as.numeric(str_replace_all(as.character(Population), ",", ""))) %>%
  
  group_by(shortPostcode) %>%
  summarise(
    Pop2011 = sum(Population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Estimate populations
  mutate(
    Pop2022 = round(Pop2011 * 1.05),
    Pop2023 = round(Pop2022 * 1.0045),
    Pop2024 = round(Pop2023 * 1.0042)
  )

# 2. Merge with HousePrices 
# Ensure HousePrices$shortPostcode is also cleaned the same way before joining
Towns <- HousePrices %>%
  left_join(PopulationData, by = "shortPostcode") %>%
  select(shortPostcode, Town, District, County, Pop2022, Pop2023, Pop2024) %>%
  distinct() %>%
  # filter rows where we successfully matched a population
  filter(!is.na(Pop2022)) 

# 3. Save Cleaned Data
clean_path <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data"
if(!dir.exists(clean_path)) dir.create(clean_path, recursive = TRUE)

write_csv(Towns, file.path(clean_path, "Population_Data_Cleaned.csv"))
message("Success! Cleaned data saved with correctly mapped postcodes.")