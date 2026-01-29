library(tidyverse)
library(lubridate)
library(stringr)
library(fmsb)

output_dir <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Graph/Crime-Graph"

crime_raw <- read_csv("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/Crime_Cleaned.csv")
population_clean <- read_csv("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/Population_Data_Cleaned.csv")

cat("\n=== STEP 1: CHECKING RAW DATA ===\n")
cat("Sample crime Districts (from lsoa_name):\n")
print(head(crime_raw$lsoa_name, 10))
cat("\nSample population Towns:\n")
print(head(population_clean$Town, 20))

# Map Towns to Districts in population data
population_clean <- population_clean %>%
  mutate(
    District = case_when(
      # Cheshire East towns
      str_detect(Town, regex("CONGLETON|KNUTSFORD|MACCLESFIELD|CREWE|NANTWICH|ALSAGER|SANDBACH|MIDDLEWICH|WILMSLOW|POYNTON", ignore_case = TRUE)) ~ "Cheshire East",
      # Cheshire West towns  
      str_detect(Town, regex("CHESTER|ELLESMERE|PORT|NORTHWICH|WINSFORD|FRODSHAM", ignore_case = TRUE)) ~ "Cheshire West and Chester",
      # Cumberland towns
      str_detect(Town, regex("MILLOM|WORKING|CARLISLE|FRIZINGTC|COCKERM|WHITEHAV|CLEATOR|ST BEES|EGREMONT|KESWICK|PENRITH|WIGTON|MARYPORT|APPLEBY|BARROW", ignore_case = TRUE)) ~ "Cumberland",
      TRUE ~ Town  # Keep original if no match
    ),
    County = case_when(
      str_detect(County, regex("Cheshire|CHESHIRE", ignore_case = TRUE)) ~ "Cheshire",
      str_detect(County, regex("Cumberland|CUMBERL", ignore_case = TRUE)) ~ "Cumberland",
      TRUE ~ County
    )
  )

cat("\n=== STEP 2: AFTER MAPPING ===\n")
cat("Population Districts created:\n")
print(table(population_clean$District))

# Aggregate population by District
district_populations <- population_clean %>%
  group_by(County, District) %>%
  summarise(Pop2023 = sum(Pop2023, na.rm = TRUE), .groups = "drop")

cat("\nDistrict populations:\n")
print(district_populations)

cat("\nUnique Counties in population data:\n")
print(unique(district_populations$County))

# Clean crime data
crime_clean <- crime_raw %>%
  filter(!is.na(Date)) %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = FALSE),
    District = str_remove(lsoa_name, "\\s[0-9]{3}[A-Z]?$"),
    County = case_when(
      str_detect(falls_within, regex("Cheshire", ignore_case = TRUE)) ~ "Cheshire",
      str_detect(falls_within, regex("Cumbria|Cumberland", ignore_case = TRUE)) ~ "Cumberland",
      TRUE ~ "OTHER"
    )
  ) %>%
  filter(County %in% c("Cheshire", "Cumberland"))

cat("\n=== STEP 3: CRIME DATA DISTRICTS ===\n")
cat("Crime Districts (Cheshire only):\n")
cheshire_crime_districts <- crime_clean %>% 
  filter(County == "Cheshire") %>% 
  distinct(District) %>% 
  arrange(District)
print(cheshire_crime_districts)

cat("\nUnique Counties in crime data:\n")
print(unique(crime_clean$County))

# BOX PLOT OF dDRUG OFFENCE RATE IN CHESHIRE AND CUMBERLAND 
cat("\n=== CREATING DRUG BOX PLOTS ===\n")
plot_drug_boxplot <- function(county_name) {
  drugs <- crime_clean %>%
    filter(County == county_name,
           str_detect(crime_type, regex("Drug", ignore_case = TRUE))) %>%
    group_by(District, Date) %>%
    summarise(DrugCount = n(), .groups = "drop")
  cat(paste0("\n", county_name, " drug crimes found: ", nrow(drugs), " rows\n"))
  if(nrow(drugs) == 0) return(NULL)
  districts_with_data <- drugs %>%
    group_by(District) %>%
    summarise(TotalRecords = n(), MaxCount = max(DrugCount), .groups = "drop") %>%
    filter(MaxCount > 0) %>%
    pull(District)
  
  drugs_filtered <- drugs %>% filter(District %in% districts_with_data)
  top_districts <- drugs_filtered %>%
    group_by(District) %>%
    summarise(MedianCount = median(DrugCount), .groups = "drop") %>%
    arrange(desc(MedianCount)) %>%
    head(10) %>%
    pull(District)
  
  drugs_final <- drugs_filtered %>% filter(District %in% top_districts)
  p <- ggplot(drugs_final, aes(x = fct_reorder(District, DrugCount, median), 
                               y = DrugCount, fill = District)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.color = "red") +
    theme_minimal() +
    labs(
      title = paste(county_name, ": Monthly Drug Offense Count by District"),
      x = "District Name",
      y = "Drug Offense Count per Month"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "none"
    )
  print(p)
  plot_drug_boxplot("Cheshire")
  plot_drug_boxplot("Cumberland")
  ggsave(file.path(output_dir, paste0("Drug_Boxplot_", county_name, ".png")), p,
         width = 12, height = 8, dpi = 300)
  cat(paste0("✓ ", county_name, " drug boxplot saved\n"))
}




# RADAR CHART: VEHICLE CRIME RATE (2023)
crime_file <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/Crime_Cleaned.csv"
population_file <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/Population_Data_Cleaned.csv"
output_path <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Graph/Crime-Graph/vehicle_crime_radar_chart.png"

# Read the datasets
crime_data <- read.csv(crime_file, stringsAsFactors = FALSE)
population_data <- read.csv(population_file, stringsAsFactors = FALSE)

# Define Cheshire towns
cheshire_towns <- c("ALTRINCHAM", "CHESTER", "CONGLETON", "CREWE", "ELLESMERE PORT", 
                    "HIGH PEAK", "HYDE", "KNUTSFORD", "LYMM", "MACCLESFIELD", 
                    "MALPAS", "MIDDLEWICH", "NESTON", "SANDBACH", "STOCKPORT", 
                    "WHITCHURCH", "WIRRAL")

# Filter crime data for Vehicle crime in Cheshire towns
vehicle_crime_data <- crime_data %>%
  filter(grepl("Vehicle", crime_type, ignore.case = TRUE))

# Extract town from location and count vehicle crimes
vehicle_counts <- vehicle_crime_data %>%
  mutate(Town = sapply(location, function(loc) {
    loc_upper <- toupper(loc)
    for(town in cheshire_towns) {
      if(grepl(town, loc_upper, fixed = TRUE)) {
        return(town)
      }
    }
    return(NA)
  })) %>%
  filter(!is.na(Town)) %>%
  group_by(Town) %>%
  summarise(vehicle_crime_count = n(), .groups = 'drop')

# Prepare population data
population_data_clean <- population_data %>%
  mutate(Town = toupper(Town)) %>%
  filter(Town %in% cheshire_towns) %>%
  group_by(Town) %>%
  summarise(population = sum(Pop2024, na.rm = TRUE), .groups = 'drop')

# Merge vehicle crime counts with population data
vehicle_rate_data <- vehicle_counts %>%
  left_join(population_data_clean, by = "Town") %>%
  filter(!is.na(population), population > 0) %>%
  mutate(vehicle_crime_rate = (vehicle_crime_count / population) * 1000) %>%  # Rate per 1000 people
  arrange(desc(vehicle_crime_rate))

# Format town names
vehicle_rate_data <- vehicle_rate_data %>%
  mutate(Town_formatted = tools::toTitleCase(tolower(Town)))

# Prepare data for radar chart
# fmsb package requires data in a specific format:
# - First row: maximum values
# - Second row: minimum values
# - Following rows: actual data

# Create radar chart data
radar_data <- data.frame(
  town = vehicle_rate_data$Town_formatted,
  rate = vehicle_rate_data$vehicle_crime_rate
)

# Transpose data for radar chart (towns as columns)
radar_matrix <- t(radar_data$rate)
colnames(radar_matrix) <- radar_data$town

# Add max and min rows (required by fmsb)
max_value <- max(radar_data$rate) * 1.1  # Add 10% buffer
min_value <- 0

radar_chart_data <- rbind(
  rep(max_value, ncol(radar_matrix)),  # Max row
  rep(min_value, ncol(radar_matrix)),  # Min row
  radar_matrix                          # Actual data
)
radar_chart_data <- as.data.frame(radar_chart_data)

# Create radar chart
png(output_path, width = 1400, height = 1400, res = 120, bg = "white")

par(mar = c(1, 1, 3, 1))

# Create the radar chart
radarchart(
  radar_chart_data,
  axistype = 1,
  # Polygon customization
  pcol = "#FF6B6B",
  pfcol = rgb(1, 0.42, 0.42, 0.5),  # Semi-transparent fill
  plwd = 3,
  plty = 1,
  # Grid customization
  cglcol = "grey",
  cglty = 1,
  cglwd = 0.8,
  axislabcol = "grey30",
  # Label customization
  vlcex = 0.9,
  caxislabels = round(seq(0, max_value, length.out = 5), 1),
  title = "Vehicle Crime Rate per 1000 People by Town (Cheshire, 2022-2024)"
)

dev.off()

# Print summary statistics
cat("\n=== Vehicle Crime Rate Summary for Cheshire Towns ===\n\n")
summary_table <- vehicle_rate_data %>% 
  select(Town_formatted, vehicle_crime_count, population, vehicle_crime_rate) %>%
  arrange(desc(vehicle_crime_rate))

names(summary_table) <- c("Town", "Vehicle Crimes", "Population", "Rate per 1000")
print(summary_table)

cat("\n\nTotal vehicle crimes:", sum(vehicle_rate_data$vehicle_crime_count), "\n")
cat("Total population:", sum(vehicle_rate_data$population), "\n")
cat("Overall vehicle crime rate:", round(sum(vehicle_rate_data$vehicle_crime_count) / sum(vehicle_rate_data$population) * 1000, 2), "per 1000 people\n")
cat("\nRadar chart saved to:", output_path, "\n")


# PIE CHART: ROBBERY RATE PER 10,000 PEOPLE (CHESHIRE, JUNE 2023)
# Define Cheshire towns
cheshire_towns <- c("ALTRINCHAM", "CHESTER", "CONGLETON", "CREWE", "ELLESMERE PORT", 
                    "HIGH PEAK", "HYDE", "KNUTSFORD", "LYMM", "MACCLESFIELD", 
                    "MALPAS", "MIDDLEWICH", "NESTON", "SANDBACH", "STOCKPORT", 
                    "WHITCHURCH", "WIRRAL")

# Filter crime data for Robbery in Cheshire towns
robbery_data <- crime_data %>%
  filter(grepl("Robbery", crime_type, ignore.case = TRUE))

# Extract town from location and count robberies
robbery_counts <- robbery_data %>%
  mutate(Town = sapply(location, function(loc) {
    loc_upper <- toupper(loc)
    for(town in cheshire_towns) {
      if(grepl(town, loc_upper, fixed = TRUE)) {
        return(town)
      }
    }
    return(NA)
  })) %>%
  filter(!is.na(Town)) %>%
  # KEY FIX: Group by Town to combine all entries (especially Chester)
  group_by(Town) %>%
  summarise(robbery_count = sum(n()), .groups = 'drop')

# Prepare population data - also consolidate by town name
population_data_clean <- population_data %>%
  mutate(Town = toupper(Town)) %>%
  filter(Town %in% cheshire_towns) %>%
  # Consolidate population data by town (sum or take max)
  group_by(Town) %>%
  summarise(population = sum(Pop2024, na.rm = TRUE), .groups = 'drop')

# Merge robbery counts with population data
robbery_rate_data <- robbery_counts %>%
  left_join(population_data_clean, by = "Town") %>%
  filter(!is.na(population), population > 0) %>%
  mutate(robbery_rate = (robbery_count / population) * 1000) %>%  # Rate per 1000 people
  arrange(desc(robbery_rate))

# Calculate percentages
robbery_rate_data <- robbery_rate_data %>%
  mutate(
    percentage = robbery_rate / sum(robbery_rate) * 100,
    Town_formatted = tools::toTitleCase(tolower(Town))
  )

# Print data to verify Chester is only appearing once
cat("\n=== Towns in the dataset ===\n")
print(robbery_rate_data$Town_formatted)
cat("\nNumber of unique towns:", nrow(robbery_rate_data), "\n\n")

# Create pie chart
png(output_path, width = 1200, height = 800, res = 120, bg = "white")

par(mar = c(2, 2, 4, 2))

# Define distinct colors - one for each unique town
colors <- c("#FF6B6B", "#4ECDC4", "blue", "#FFA07A", "#98D8C8",
            "#F7DC6F", "#BB8FCE", "#85C1E2", "#F8B88B", "#AED6F1",
            "#A9DFBF", "#FAD7A0", "#D7BDE2", "#A3E4D7", "#F9E79F",
            "#FADBD8", "#D5F4E6")

# Take only as many colors as needed
colors <- colors[1:nrow(robbery_rate_data)]

# Create labels with only percentages (to be shown on pie)
pie_labels <- paste0(round(robbery_rate_data$percentage, 1), "%")

# Create the pie chart
pie(robbery_rate_data$robbery_rate, 
    labels = pie_labels,
    col = colors,
    border = "white",
    lwd = 2,
    cex = 1.2,
    font = 2)

# Add title
title(main = "Robbery Rate by Town (Cheshire, 2022-2024)",
      cex.main = 1.8,
      font.main = 2,
      line = 1)

# Add legend on the right side - each town appears only once
legend("right",
       legend = robbery_rate_data$Town_formatted,
       fill = colors,
       border = "white",
       bty = "n",
       cex = 1.0,
       title = "Town",
       title.font = 2,
       x.intersp = 0.5,
       y.intersp = 1.2)

dev.off()

# Print summary statistics
cat("\n=== Robbery Rate Summary for Cheshire Towns ===\n\n")
summary_table <- robbery_rate_data %>% 
  select(Town_formatted, robbery_count, population, robbery_rate, percentage) %>%
  arrange(desc(robbery_rate))

names(summary_table) <- c("Town", "Robberies", "Population", "Rate per 1000", "Percentage")
print(summary_table)

cat("\n\nTotal robberies:", sum(robbery_rate_data$robbery_count), "\n")
cat("Total population:", sum(robbery_rate_data$population), "\n")
cat("Overall robbery rate:", round(sum(robbery_rate_data$robbery_count) / sum(robbery_rate_data$population) * 1000, 2), "per 1000 people\n")
cat("\nPie chart saved to:", output_path, "\n")


# ============================================================================
# LINE CHART: DRUG OFFENSE RATES (ALL YEARS)
# ============================================================================
# Read the datasets
crime_data <- read.csv(crime_file, stringsAsFactors = FALSE)
population_data <- read.csv(population_file, stringsAsFactors = FALSE)

# Define Cheshire and Cumberland towns
cheshire_towns <- c("ALTRINCHAM", "CHESTER", "CONGLETON", "CREWE", "ELLESMERE PORT", 
                    "HIGH PEAK", "HYDE", "KNUTSFORD", "LYMM", "MACCLESFIELD", 
                    "MALPAS", "MIDDLEWICH", "NESTON", "SANDBACH", "STOCKPORT", 
                    "WHITCHURCH", "WIRRAL")

cumberland_towns <- c("CARLISLE", "COCKERMOUTH", "MILLOM", "PENRITH", "WHITEHAVEN", 
                      "WORKINGTON", "WIGTON", "CLEATOR", "MARYPORT", "KESWICK",
                      "BARROW", "ULVERSTON", "KENDAL", "WINDERMERE", "AMBLESIDE")

# Filter crime data for Drug offenses
drug_crime_data <- crime_data %>%
  filter(grepl("Drug|Drugs", crime_type, ignore.case = TRUE))

# Function to classify county based on location
classify_county <- function(location) {
  loc_upper <- toupper(location)
  
  # Check Cheshire towns
  for(town in cheshire_towns) {
    if(grepl(town, loc_upper, fixed = TRUE)) {
      return("Cheshire")
    }
  }
  
  # Check Cumberland towns
  for(town in cumberland_towns) {
    if(grepl(town, loc_upper, fixed = TRUE)) {
      return("Cumberland")
    }
  }
  
  return(NA)
}

# Add county classification
drug_crime_processed <- drug_crime_data %>%
  mutate(County = sapply(location, classify_county)) %>%
  filter(!is.na(County))

# Extract Year from the data
if("Date" %in% names(drug_crime_processed)) {
  drug_crime_processed <- drug_crime_processed %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
           Year = year(Date))
} else if("Year" %in% names(drug_crime_processed)) {
  # Year column already exists
  drug_crime_processed <- drug_crime_processed
} else {
  stop("Year or Date column not found in the dataset")
}

# Count drug crimes by County and Year
drug_counts_yearly <- drug_crime_processed %>%
  group_by(County, Year) %>%
  summarise(drug_count = n(), .groups = 'drop')

# Create a complete grid of all County-Year combinations
all_years <- sort(unique(drug_counts_yearly$Year))
all_counties <- c("Cheshire", "Cumberland")

complete_grid <- expand.grid(
  County = all_counties,
  Year = all_years,
  stringsAsFactors = FALSE
)

# Merge with actual counts, filling missing values with 0
drug_counts_complete <- complete_grid %>%
  left_join(drug_counts_yearly, by = c("County", "Year")) %>%
  mutate(drug_count = ifelse(is.na(drug_count), 0, drug_count))

# Get population data for Cheshire and Cumberland
cheshire_pop <- population_data %>%
  mutate(Town = toupper(Town)) %>%
  filter(Town %in% cheshire_towns) %>%
  summarise(total_pop = sum(Pop2024, na.rm = TRUE)) %>%
  pull(total_pop)

cumberland_pop <- population_data %>%
  mutate(Town = toupper(Town)) %>%
  filter(Town %in% cumberland_towns) %>%
  summarise(total_pop = sum(Pop2024, na.rm = TRUE)) %>%
  pull(total_pop)

# If Cumberland population not available, use an estimate
if(is.na(cumberland_pop) || cumberland_pop == 0) {
  cumberland_pop <- 500000  # Approximate population of Cumberland/Cumbria
  cat("Note: Using estimated population for Cumberland:", cumberland_pop, "\n")
}

# Add population and calculate rates per 10,000 people
drug_rates_yearly <- drug_counts_complete %>%
  mutate(population = ifelse(County == "Cheshire", cheshire_pop, cumberland_pop),
         drug_rate = (drug_count / population) * 10000) %>%
  arrange(County, Year)

# Print data to verify completeness
cat("\n=== Complete Data for Both Counties ===\n\n")
print(drug_rates_yearly)

# Create line chart
png(output_path, width = 1200, height = 800, res = 120, bg = "white")

line_chart <- ggplot(drug_rates_yearly, aes(x = Year, y = drug_rate, color = County, group = County)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Cheshire" = "#FF6B6B", "Cumberland" = "#4ECDC4")) +
  scale_x_continuous(breaks = all_years) +
  labs(
    title = "Drug Offense Rates per 10,000 People (All Years)",
    x = "Year",
    y = "Rate per 10,000 Population",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 20)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(line_chart)

dev.off()

# Print summary statistics
cat("\n=== Drug Offense Rate Summary by Year ===\n\n")

for(year in all_years) {
  cat(paste0("\n", year, ":\n"))
  year_data <- drug_rates_yearly %>% filter(Year == year)
  for(i in 1:nrow(year_data)) {
    cat(sprintf("  %s: %.2f per 10,000 (%d offenses)\n", 
                year_data$County[i], 
                year_data$drug_rate[i],
                year_data$drug_count[i]))
  }
}

cat("\n=== Statistics by County ===\n\n")

summary_by_county <- drug_rates_yearly %>%
  group_by(County) %>%
  summarise(
    total_offenses = sum(drug_count),
    avg_yearly_rate = round(mean(drug_rate), 2),
    max_rate = round(max(drug_rate), 2),
    min_rate = round(min(drug_rate), 2),
    .groups = 'drop'
  )

print(summary_by_county)

cat("\n=== Population Data ===\n")
cat("Cheshire population:", format(cheshire_pop, big.mark = ","), "\n")
cat("Cumberland population:", format(cumberland_pop, big.mark = ","), "\n")

cat("\nLine chart saved to:", output_path, "\n")
cat("\nNote: All years now have data for both counties. Missing data filled with 0.\n")