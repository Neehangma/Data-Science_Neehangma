# Load required libraries
library(dplyr)
library(readr)

# Read the cleaned CSV files with full Windows paths
house_prices <- read_csv("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/HousePrices_cleaned.csv")
broadband <- read_csv("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/CleanedBroadband_Performance.csv")
crime <- read_csv("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/Crime_Cleaned.csv")

cat("\n")
cat("==============================================================\n")
cat("CALCULATING OVERALL SCORES FOR ALL TOWNS\n")
cat("==============================================================\n\n")

# ===================================================================
# STEP 1: Calculate HouseScore by Town (0-10 scale)
# ===================================================================
cat("Step 1: Calculating HouseScore (Affordability)...\n")

house_scores <- house_prices %>%
  filter(!is.na(Town) & Town != "") %>%
  group_by(Town) %>%
  summarise(
    avg_house_price = mean(Price, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # HouseScore: 0-10 scale, 10 = most affordable (lowest price)
    HouseScore = 10 * (1 - (avg_house_price - min(avg_house_price)) / (max(avg_house_price) - min(avg_house_price)))
  ) %>%
  select(Town, avg_house_price, HouseScore)

cat("  ✓ HouseScore calculated for", nrow(house_scores), "towns\n\n")

# ===================================================================
# STEP 2: Calculate BroadbandScore by Town (0-10 scale)
# ===================================================================
cat("Step 2: Calculating BroadbandScore...\n")

# Join broadband with house_prices to get Town
broadband_with_town <- broadband %>%
  left_join(house_prices %>% select(shortPostcode, Town) %>% distinct(), 
            by = "shortPostcode")

broadband_scores <- broadband_with_town %>%
  filter(!is.na(Town) & Town != "") %>%
  group_by(Town) %>%
  summarise(
    avg_download_speed = mean(AvgDownload, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # BroadbandScore: 0-10 scale, 10 = fastest speed
    BroadbandScore = 10 * (avg_download_speed - min(avg_download_speed)) / (max(avg_download_speed) - min(avg_download_speed))
  ) %>%
  select(Town, avg_download_speed, BroadbandScore)

cat("  ✓ BroadbandScore calculated for", nrow(broadband_scores), "towns\n\n")

# ===================================================================
# STEP 3: Calculate CrimeScore by Town (0-10 scale)
# ===================================================================
cat("Step 3: Calculating CrimeScore (Safety)...\n")

# Extract town from lsoa_name and aggregate
crime_by_area <- crime %>%
  filter(!is.na(lsoa_name) & lsoa_name != "") %>%
  mutate(extracted_town = gsub("\\s+[0-9].*$", "", lsoa_name)) %>%
  group_by(extracted_town) %>%
  summarise(
    crime_count = n(),
    .groups = 'drop'
  )

# Create a proper town matching lookup
town_lookup <- house_scores$Town
names(town_lookup) <- toupper(house_scores$Town)

crime_scores <- crime_by_area %>%
  mutate(
    # Convert extracted town to uppercase for matching
    town_upper = toupper(extracted_town),
    # Match with actual town names from house_scores
    Town = case_when(
      town_upper %in% names(town_lookup) ~ town_lookup[town_upper],
      TRUE ~ extracted_town
    )
  ) %>%
  # Group by the matched Town name
  group_by(Town) %>%
  summarise(
    total_crimes = sum(crime_count),
    .groups = 'drop'
  ) %>%
  # Only keep towns that exist in house_scores
  filter(Town %in% house_scores$Town) %>%
  mutate(
    # CrimeScore: 0-10 scale, 10 = safest (fewest crimes)
    CrimeScore = 10 * (1 - (total_crimes - min(total_crimes)) / (max(total_crimes) - min(total_crimes)))
  ) %>%
  select(Town, total_crimes, CrimeScore)

cat("  ✓ CrimeScore calculated for", nrow(crime_scores), "towns\n")
cat("  ✓ Matched with house_scores towns\n\n")

# DIAGNOSTIC: Check town matching
cat("DIAGNOSTIC INFO:\n")
cat("  Towns in house_scores:", length(unique(house_scores$Town)), "\n")
cat("  Towns in crime_scores:", length(unique(crime_scores$Town)), "\n")
cat("  Towns with matching crime data:", 
    sum(house_scores$Town %in% crime_scores$Town), "\n\n")

# Show which towns DON'T have crime data
missing_crime <- house_scores$Town[!house_scores$Town %in% crime_scores$Town]
if(length(missing_crime) > 0) {
  cat("  Towns WITHOUT crime data (", length(missing_crime), "):\n")
  for(town in missing_crime) {
    cat("    -", town, "\n")
  }
  cat("\n")
}

# ===================================================================
# STEP 4: Combine All Scores and Calculate Overall Score
# ===================================================================
cat("Step 4: Combining all scores and calculating overall score...\n")

overall_scores <- house_scores %>%
  left_join(broadband_scores, by = "Town") %>%
  left_join(crime_scores, by = "Town")

# Calculate medians BEFORE the main mutate
median_broadband <- median(overall_scores$BroadbandScore, na.rm = TRUE)
median_crime <- median(overall_scores$CrimeScore, na.rm = TRUE)
median_download <- median(overall_scores$avg_download_speed, na.rm = TRUE)

cat("  Using median values for missing data:\n")
cat("    Median BroadbandScore:", sprintf("%.2f", median_broadband), "\n")
cat("    Median CrimeScore:", sprintf("%.2f", median_crime), "\n")
cat("    Median Download Speed:", sprintf("%.2f", median_download), "Mbps\n\n")

overall_scores <- overall_scores %>%
  mutate(
    # Handle missing values - use pre-calculated medians
    BroadbandScore = ifelse(is.na(BroadbandScore), 
                            median_broadband, 
                            BroadbandScore),
    CrimeScore = ifelse(is.na(CrimeScore), 
                        median_crime, 
                        CrimeScore),
    total_crimes = ifelse(is.na(total_crimes), 0, total_crimes),
    avg_download_speed = ifelse(is.na(avg_download_speed), 
                                median_download, 
                                avg_download_speed),
    
    # Calculate overall score (average of three scores)
    overall_score = (HouseScore + BroadbandScore + CrimeScore) / 3,
    
    # Add recommendation based on overall score
    Recommendation = case_when(
      overall_score >= 8.0 ~ "Excellent",
      overall_score >= 6.5 ~ "Consider Improvement",
      overall_score >= 5.0 ~ "Consider Improvement",
      TRUE ~ "Needs Improvement"
    )
  ) %>%
  # Select and reorder columns
  select(
    Town,
    avg_download_speed,
    BroadbandScore,
    HouseScore,
    avg_house_price,
    total_crimes,
    CrimeScore,
    overall_score,
    Recommendation
  ) %>%
  arrange(desc(overall_score))

cat("  ✓ Overall scores calculated for", nrow(overall_scores), "towns\n\n")

# ===================================================================
# STEP 5: Display Results
# ===================================================================
cat("\n")
cat("====================================================================================\n")
cat("TOP 30 TOWNS BY OVERALL SCORE\n")
cat("====================================================================================\n")
cat(sprintf("%-4s %-20s %-10s %-12s %-12s %-10s %s\n", 
            "#", "Town", "Overall", "House", "Broadband", "Crime", "Recommendation"))
cat(sprintf("%-4s %-20s %-10s %-12s %-12s %-10s %s\n", 
            "", "", "Score", "Score", "Score", "Score", ""))
cat("------------------------------------------------------------------------------------\n")

for(i in 1:min(30, nrow(overall_scores))) {
  cat(sprintf("%-4d %-20s %-10.2f %-12.2f %-12.2f %-10.2f %s\n", 
              i,
              substr(overall_scores$Town[i], 1, 20),
              overall_scores$overall_score[i],
              overall_scores$HouseScore[i],
              overall_scores$BroadbandScore[i],
              overall_scores$CrimeScore[i],
              overall_scores$Recommendation[i]))
}

cat("\n")

# Display detailed view for top 10
cat("\n")
cat("====================================================================================\n")
cat("DETAILED VIEW - TOP 10 TOWNS\n")
cat("====================================================================================\n")
print(overall_scores %>% 
        select(Town, HouseScore, BroadbandScore, CrimeScore, overall_score, Recommendation) %>%
        head(10), 
      n = 10)

cat("\n")

# ===================================================================
# STEP 6: Export to CSV File
# ===================================================================
cat("\n")
cat("====================================================================================\n")
cat("EXPORTING RESULTS\n")
cat("====================================================================================\n")

# Create output directory if it doesn't exist
output_dir <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Export the comprehensive overall scores
output_file <- paste0(output_dir, "/final_overall_scores.csv")
write_csv(overall_scores, output_file)

cat("\n✓ Results exported successfully!\n")
cat("  File: final_overall_scores.csv\n")
cat("  Location:", output_dir, "\n")
cat("  Total Towns:", nrow(overall_scores), "\n\n")

# ===================================================================
# STEP 7: Summary Statistics
# ===================================================================
cat("====================================================================================\n")
cat("SUMMARY STATISTICS\n")
cat("====================================================================================\n")

cat("\nOverall Score Distribution:\n")
cat("  Mean:  ", sprintf("%.2f", mean(overall_scores$overall_score, na.rm = TRUE)), "\n")
cat("  Median:", sprintf("%.2f", median(overall_scores$overall_score, na.rm = TRUE)), "\n")
cat("  Min:   ", sprintf("%.2f", min(overall_scores$overall_score, na.rm = TRUE)), "\n")
cat("  Max:   ", sprintf("%.2f", max(overall_scores$overall_score, na.rm = TRUE)), "\n")

cat("\nIndividual Score Averages:\n")
cat("  HouseScore:     ", sprintf("%.2f", mean(overall_scores$HouseScore, na.rm = TRUE)), "\n")
cat("  BroadbandScore: ", sprintf("%.2f", mean(overall_scores$BroadbandScore, na.rm = TRUE)), "\n")
cat("  CrimeScore:     ", sprintf("%.2f", mean(overall_scores$CrimeScore, na.rm = TRUE)), "\n")

cat("\nRecommendation Breakdown:\n")
rec_summary <- overall_scores %>% 
  count(Recommendation) %>%
  arrange(desc(n))
for(i in 1:nrow(rec_summary)) {
  cat("  ", rec_summary$Recommendation[i], ":", rec_summary$n[i], "towns\n")
}

# Show best performing towns in each category
cat("\n====================================================================================\n")
cat("TOP 5 TOWNS BY INDIVIDUAL METRICS\n")
cat("====================================================================================\n")

cat("\nBest HouseScore (Most Affordable):\n")
top_house <- overall_scores %>% 
  arrange(desc(HouseScore)) %>% 
  select(Town, HouseScore, avg_house_price) %>%
  head(5)
print(top_house, n = 5)

cat("\nBest BroadbandScore (Fastest Internet):\n")
top_broadband <- overall_scores %>% 
  arrange(desc(BroadbandScore)) %>% 
  select(Town, BroadbandScore, avg_download_speed) %>%
  head(5)
print(top_broadband, n = 5)

cat("\nBest CrimeScore (Safest):\n")
top_crime <- overall_scores %>% 
  arrange(desc(CrimeScore)) %>% 
  select(Town, CrimeScore, total_crimes) %>%
  head(5)
print(top_crime, n = 5)

cat("\n")
cat("====================================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("====================================================================================\n")
cat("\nThe file 'final_overall_scores.csv' contains:\n")
cat("  1. Town                  - Town name\n")
cat("  2. avg_download_speed    - Average broadband download speed (Mbps)\n")
cat("  3. BroadbandScore        - Broadband score (0-10, higher = better)\n")
cat("  4. HouseScore            - House affordability score (0-10, higher = more affordable)\n")
cat("  5. avg_house_price       - Average house price (£)\n")
cat("  6. total_crimes          - Total crimes recorded\n")
cat("  7. CrimeScore            - Crime/safety score (0-10, higher = safer)\n")
cat("  8. overall_score         - Overall combined score (0-10, higher = better)\n")
cat("  9. Recommendation        - Recommendation category\n")
cat("\n")
cat("SCORING FORMULA:\n")
cat("  Overall Score = (HouseScore + BroadbandScore + CrimeScore) / 3\n")
cat("  - All scores are on 0-10 scale\n")
cat("  - HouseScore: 10 = most affordable\n")
cat("  - BroadbandScore: 10 = fastest internet\n")
cat("  - CrimeScore: 10 = safest (lowest crime)\n")
cat("\n")