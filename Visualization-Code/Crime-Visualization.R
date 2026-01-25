# ==============================================================================
# CRIME ANALYSIS & VISUALIZATION (RATE PER 10,000 PEOPLE)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(fmsb) # For Radar Chart

# 1. SET PATHS
# ------------------------------------------------------------------------------
crime_file <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/Crime_Cleaned.csv"
pop_file   <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/Population_Data_Cleaned.csv"
output_dir <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Graph/Crime-Graph/"

if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 2. LOAD & CLEAN POPULATION DATA
# ------------------------------------------------------------------------------
pop_data <- read_csv(pop_file, show_col_types = FALSE) %>% clean_names()

# Collapse Cheshire sub-counties into one "CHESHIRE"
county_pop_fixed <- pop_data %>%
  mutate(County = case_when(
    county %in% c("CHESHIRE EAST", "CHESHIRE WEST AND CHESTER") ~ "CHESHIRE",
    county == "CUMBERLAND" ~ "CUMBERLAND",
    TRUE ~ county
  )) %>%
  group_by(County) %>%
  summarise(
    pop2022 = sum(pop2022, na.rm = TRUE),
    pop2023 = sum(pop2023, na.rm = TRUE),
    pop2024 = sum(pop2024, na.rm = TRUE),
    .groups = "drop"
  )

# 3. LOAD & CLEAN CRIME DATA
# ------------------------------------------------------------------------------
crime_raw <- read_csv(crime_file, show_col_types = FALSE) %>% clean_names()

crime_data <- crime_raw %>%
  filter(!is.na(month)) %>%
  mutate(
    Date = ym(month),   # parse "YYYY-MM" safely
    Year = year(Date),
    Month_Name = month(Date, label = TRUE, abbr = FALSE),
    County = case_when(
      grepl("Cheshire", falls_within, ignore.case = TRUE) ~ "CHESHIRE",
      grepl("Cumbria", falls_within, ignore.case = TRUE) ~ "CUMBERLAND",
      TRUE ~ "OTHER"
    ),
    District = str_remove(lsoa_name, "\\s[0-9]{3}[A-Z]$")
  ) %>%
  filter(County != "OTHER")

# 4. PREPARE CRIME RATES PER 10,000 PEOPLE
# ------------------------------------------------------------------------------
crime_rates <- crime_data %>%
  group_by(County, District, Date, Year, Month_Name, crime_type) %>%
  summarise(CrimeCount = n(), .groups = "drop") %>%
  left_join(county_pop_fixed, by = "County") %>%
  mutate(
    CurrentPop = case_when(
      Year == 2022 ~ pop2022,
      Year == 2023 ~ pop2023,
      Year == 2024 ~ pop2024,
      TRUE ~ pop2023
    ),
    Rate = (CrimeCount / CurrentPop) * 10000
  )

# Clean dataset for plotting (remove NA/Inf)
crime_rates_clean <- crime_rates %>%
  filter(!is.na(Rate), is.finite(Rate))

# ==============================================================================
# VISUALIZATIONS
# ==============================================================================

# 1. BOXPLOT: DRUG OFFENCE RATE (CHESHIRE)
p1 <- crime_rates_clean %>%
  filter(grepl("Drug", crime_type, ignore.case = TRUE), County == "CHESHIRE") %>%
  ggplot(aes(x = District, y = Rate, fill = District)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "Cheshire: Drug Offence Rate by District",
       y = "Monthly Rate per 10,000 people", x = "District") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
ggsave(file.path(output_dir, "Drug_Boxplot_Cheshire.png"), p1, width = 10, height = 6)
p1

# 2. BOXPLOT: DRUG OFFENCE RATE (CUMBERLAND)
p2 <- crime_rates_clean %>%
  filter(grepl("Drug", crime_type, ignore.case = TRUE), County == "CUMBERLAND") %>%
  ggplot(aes(x = District, y = Rate, fill = District)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "Cumberland: Drug Offence Rate by District",
       y = "Monthly Rate per 10,000 people", x = "District") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
ggsave(file.path(output_dir, "Drug_Boxplot_Cumberland.png"), p2, width = 10, height = 6)

# 3. RADAR CHART: VEHICLE CRIME RATE (2023)
radar_data <- crime_rates_clean %>%
  filter(crime_type == "Vehicle crime", Year == 2023) %>%
  group_by(District) %>%
  summarise(AvgRate = mean(Rate), .groups = "drop") %>%
  spread(District, AvgRate)

radar_plot_data <- rbind(rep(max(radar_data, na.rm=TRUE), ncol(radar_data)),
                         rep(0, ncol(radar_data)),
                         radar_data)

png(file.path(output_dir, "Vehicle_Crime_Radar_2023.png"), width = 800, height = 800)
radarchart(radar_plot_data, pcol = "blue", pfcol = rgb(0,0,1,0.2), plwd = 3,
           title = "Average Vehicle Crime Rate by District (2023)")
dev.off()

# 4. PIE CHART: ROBBERY RATE (2023)
p4 <- crime_rates_clean %>%
  filter(crime_type == "Robbery", Year == 2023) %>%
  group_by(County) %>%
  summarise(TotalRate = sum(Rate), .groups = "drop") %>%
  ggplot(aes(x = "", y = TotalRate, fill = County)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Proportion of Robbery Rates (2023)",
       subtitle = "Aggregated Rate per 10,000 People") +
  geom_text(aes(label = paste0(round(TotalRate, 2))),
            position = position_stack(vjust = 0.5))
ggsave(file.path(output_dir, "Robbery_Pie_2023.png"), p4)

# 5. LINE GRAPH: DRUG OFFENCE RATE TREND
p5 <- crime_rates_clean %>%
  filter(grepl("Drug", crime_type, ignore.case = TRUE)) %>%
  group_by(County, Date) %>%
  summarise(MeanRate = mean(Rate), .groups = "drop") %>%
  ggplot(aes(x = Date, y = MeanRate, color = County)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal() +
  labs(title = "Monthly Drug Offence Rate Comparison",
       subtitle = "Average District Rate per 10,000 People",
       y = "Mean Rate", x = "Month")
ggsave(file.path(output_dir, "Drug_Trend_Line.png"), p5, width = 10, height = 6)

cat("Success! All visualizations saved to:", output_dir, "\n")
