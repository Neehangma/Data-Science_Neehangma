library(tidyverse)
library(scales)

# 1. Load the two datasets
broadband_data <- read_csv(
  "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/CleanedBroadband_Performance.csv",
  show_col_types = FALSE
)

house_data <- read_csv(
  "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/HousePrices_cleaned.csv",
  show_col_types = FALSE
)

# 2. Create a "Lookup Table" to map shortPostcode to County
county_lookup <- house_data %>%
  select(shortPostcode, County) %>%
  distinct()

# 3. Combine the data and create the 'Parent_County' panel labels
broadband_processed <- broadband_data %>%
  mutate(AvgDownload = as.numeric(AvgDownload)) %>%
  # Join with the lookup table to get the County names
  left_join(county_lookup, by = "shortPostcode") %>%
  # Create the two main groups: CHESHIRE and CUMBERLAND
  mutate(Parent_County = ifelse(grepl("Cheshire", County, ignore.case = TRUE), 
                                "CHESHIRE", "CUMBERLAND")) %>%
  # Remove any rows that don't match our regions of interest
  filter(!is.na(Parent_County))

# ────────────────────────────────────────────────
# 1️⃣ BOXPLOT: Distribution of Download Speed
# ────────────────────────────────────────────────
ggplot(broadband_processed, aes(x = shortPostcode, y = AvgDownload, fill = shortPostcode)) +
  # Black jittered points for the distribution background
  geom_jitter(width = 0.2, alpha = 0.2, size = 1, colour = "black") +
  # Colorful boxplots for the foreground
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +
  
  # Faceting: Creates side-by-side CHESHIRE and CUMBERLAND panels
  facet_wrap(~ Parent_County, scales = "free_x") +
  
  # Formatting
  scale_y_continuous(labels = label_number(suffix = " Mbps", big.mark = ",")) +
  coord_cartesian(ylim = c(0, quantile(broadband_processed$AvgDownload, 0.98, na.rm = TRUE))) +
  
  labs(
    title = "Distribution of Average Download Speed by District",
    subtitle = "Performance in Cheshire and Cumberland Regions",
    x = "District (Short Postcode)",
    y = "Average Download Speed (Mbps)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(), # Names move to the legend
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

# ────────────────────────────────────────────────
# 2️⃣ BAR CHART: Average Download Speed
# ────────────────────────────────────────────────
# Prepare summary data
broadband_summary <- broadband_processed %>%
  group_by(Parent_County, shortPostcode) %>%
  summarise(AvgDownloadTown = mean(AvgDownload, na.rm = TRUE), .groups = "drop")

ggplot(broadband_summary, aes(x = reorder(shortPostcode, -AvgDownloadTown), y = AvgDownloadTown, fill = shortPostcode)) +
  geom_col() +
  facet_wrap(~ Parent_County, scales = "free_x") +
  scale_y_continuous(labels = label_number(suffix = " Mbps")) +
  labs(
    title = "Average Download Speed by Town",
    x = "District (Short Postcode)",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), 
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))