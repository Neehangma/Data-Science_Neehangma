# Load required libraries
library(dplyr)
library(readr)

# Read the cleaned CSV files with full Windows paths
house_prices <- read_csv("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/HousePrices_cleaned.csv")
broadband <- read_csv("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/CleanedBroadband_Performance.csv")
crime <- read_csv("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/Crime_Cleaned.csv")

# Function to display rankings in the format shown
print_rankings <- function(data, rank_col, score_col, title) {
  cat("\n")
  cat(paste0(rep("=", 50), collapse = ""), "\n")
  cat(title, "\n")
  cat(paste0(rep("=", 50), collapse = ""), "\n")
  cat(sprintf("%-5s %-25s %s\n", "#", "Town", score_col))
  cat(sprintf("%-5s %-25s %s\n", "", "<chr>", "<dbl>"))
  cat(paste0(rep("-", 50), collapse = ""), "\n")
  
  for(i in 1:nrow(data)) {
    cat(sprintf("%-5d %-25s %.2f\n", i, data[[rank_col]][i], data[[score_col]][i]))
  }
  cat("\n")
}

# ===================================================================
# 1. TOP 10 TOWNS BY HOUSE SCORE (Affordability)
# ===================================================================
cat("House Prices columns:\n")
print(colnames(house_prices))
cat("\n")

# Calculate HouseScore (higher score = more affordable = lower price)
# Score on 0-10 scale, where 10 = most affordable (lowest price)
house_rankings <- house_prices %>%
  filter(!is.na(Town) & Town != "") %>%
  group_by(Town) %>%
  summarise(
    avg_price = mean(Price, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # Inverse normalization: lower price = higher score
    HouseScore = 10 * (1 - (avg_price - min(avg_price)) / (max(avg_price) - min(avg_price)))
  ) %>%
  arrange(desc(HouseScore)) %>%
  select(Town, HouseScore) %>%
  head(10)

print_rankings(house_rankings, "Town", "HouseScore", "TOP 10 TOWNS BY HOUSE SCORE (Affordability)")

# ===================================================================
# 2. TOP 10 TOWNS BY BROADBAND SCORE
# ===================================================================
cat("Broadband columns:\n")
print(colnames(broadband))
cat("\n")

# Broadband data has shortPostcode, need to join with house_prices to get Town
# Then calculate score based on download speed
broadband_with_town <- broadband %>%
  left_join(house_prices %>% select(shortPostcode, Town) %>% distinct(), 
            by = "shortPostcode")

broadband_rankings <- broadband_with_town %>%
  filter(!is.na(Town) & Town != "") %>%
  group_by(Town) %>%
  summarise(
    avg_download = mean(AvgDownload, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # Normalize to 0-10 scale: higher speed = higher score
    BroadbandScore = 10 * (avg_download - min(avg_download)) / (max(avg_download) - min(avg_download))
  ) %>%
  arrange(desc(BroadbandScore)) %>%
  select(Town, BroadbandScore) %>%
  head(10)

print_rankings(broadband_rankings, "Town", "BroadbandScore", "TOP 10 TOWNS BY BROADBAND SCORE")

# ===================================================================
# 3. TOP 10 TOWNS BY CRIME SCORE (Safety - Higher = Safer)
# ===================================================================
cat("Crime columns:\n")
print(colnames(crime))
cat("\n")

# Crime data has lsoa_name but no Town directly
# We need to extract town from lsoa_name or join with other data
# For now, let's use lsoa_name and count crimes per area
# Lower crime count = higher safety score

crime_rankings <- crime %>%
  filter(!is.na(lsoa_name) & lsoa_name != "") %>%
  group_by(lsoa_name) %>%
  summarise(
    crime_count = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    # Inverse normalization: fewer crimes = higher score
    CrimeScore = 10 * (1 - (crime_count - min(crime_count)) / (max(crime_count) - min(crime_count)))
  ) %>%
  arrange(desc(CrimeScore)) %>%
  select(Town = lsoa_name, CrimeScore) %>%
  head(10)

print_rankings(crime_rankings, "Town", "CrimeScore", "TOP 10 TOWNS BY CRIME SCORE (Safety)")

# ===================================================================
# EXPORT RESULTS TO CSV
# ===================================================================
# Create output directory if it doesn't exist
output_dir <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/top-10-towns"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(house_rankings, paste0(output_dir, "/top10_house_score.csv"))
write_csv(broadband_rankings, paste0(output_dir, "/top10_broadband_score.csv"))
write_csv(crime_rankings, paste0(output_dir, "/top10_crime_score.csv"))

cat("Results exported to output folder!\n")

# ===================================================================
# ALTERNATIVE: If you need to calculate scores from raw data
# ===================================================================

# NOTE: The crime data doesn't have a direct Town column
# If you need to map LSOA to Town, you may need additional mapping data
# or extract town names from the lsoa_name field

# Alternative approach - Extract town from lsoa_name
# Many LSOA names follow pattern like "Town Name 001A"
crime_with_town <- crime %>%
  mutate(
    # Extract potential town name (everything before numbers/letters at end)
    extracted_town = gsub("\\s+[0-9]+[A-Z]*$", "", lsoa_name)
  )

# Recalculate crime rankings with extracted town names
crime_rankings_alt <- crime_with_town %>%
  filter(!is.na(extracted_town) & extracted_town != "") %>%
  group_by(Town = extracted_town) %>%
  summarise(
    crime_count = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    # Inverse normalization: fewer crimes = higher score
    CrimeScore = 10 * (1 - (crime_count - min(crime_count)) / (max(crime_count) - min(crime_count)))
  ) %>%
  arrange(desc(CrimeScore)) %>%
  select(Town, CrimeScore) %>%
  head(10)

cat("\n")
cat("Alternative Crime Rankings (by extracted town name):\n")
print_rankings(crime_rankings_alt, "Town", "CrimeScore", "TOP 10 TOWNS BY CRIME SCORE - Alternative Method")