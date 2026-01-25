library(ggplot2)
library(dplyr)
library(scales)

# 1. Read and Prepare Data
data <- read_csv("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/HousePrices_cleaned.csv")

#Line-Graph 
plot_data <- data %>%
  mutate(Parent_County = ifelse(grepl("Cheshire", County, ignore.case = TRUE), "CHESHIRE", "CUMBERLAND")) %>%
  filter(Year >= 2022 & Year <= 2024)

#Summarise Data by Town
town_data <- plot_data %>%
  group_by(Year, Town, Parent_County) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = 'drop')

# If you want ALL towns, you can remove the next 5 lines of code.
top_towns <- town_data %>%
  group_by(Parent_County, Town) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Parent_County) %>%
  slice_max(order_by = count, n = 5) %>%
  pull(Town)

town_data <- town_data %>% filter(Town %in% top_towns)

# 3. Create the Plot
line_graph <- ggplot(town_data, aes(x = Year, y = Avg_Price, color = Town, group = Town)) +
  # Lines and Points with different styles per town
  geom_line(aes(linetype = Town), size = 1) +
  geom_point(aes(shape = Town), size = 3) +
    facet_wrap(~ Parent_County, scales = "free_y") +
  
  # Formatting
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2021:2024) +
    labs(title = "Average House Prices (2022-2024) by Town and County",
       x = "Year",
       y = "Average Price (£)") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12), # The County headers
    panel.grid.minor = element_blank(),
    
    # Legend at the bottom
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.box = "vertical"
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         shape = guide_legend(nrow = 2, byrow = TRUE),
         linetype = guide_legend(nrow = 2, byrow = TRUE))

print(line_graph)

# BAR CHART: Average House Prices in 2023 by District and County
bar_data_processed <- data %>%
  filter(Year == 2022) %>%
  mutate(Parent_County = ifelse(grepl("Cheshire", County, ignore.case = TRUE), 
                                "CHESHIRE", "CUMBERLAND")) %>%
  group_by(Parent_County, District, Town) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Parent_County) %>%
  slice_max(order_by = Avg_Price, n = 8)

# 2. Create the Bar Chart
bar_chart <- ggplot(bar_data_processed, aes(x = reorder(Town, -Avg_Price), y = Avg_Price, fill = Town)) +
  geom_bar(stat = "identity", width = 0.7) +
  
  # Faceting: Creates the side-by-side CHESHIRE and CUMBERLAND panels
  facet_wrap(~ Parent_County, scales = "free_x") +
  
  # Formatting
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_d(option = "viridis") + 
  
  labs(title = "Average House Prices in 2022 by District and County",
       subtitle = "Cheshire and Cumberland",
       x = "Towns (Organized by District)",
       y = "Average Price (£)",
       fill = "Town Name") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12), # Panel headers
    
    axis.text.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    
    # Legend at the bottom
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

print(bar_chart)



#Box-plot

# 1. Prepare Data
# Group the two Cheshire counties into one "CHESHIRE" panel for a 2-column layout
boxplot_data <- df %>%
  mutate(Parent_County = ifelse(grepl("Cheshire", County, ignore.case = TRUE), 
                                "CHESHIRE", "CUMBERLAND")) %>%
  # Optional: Filter for a subset of towns so the X-axis stays clean like the image
  group_by(Parent_County, Town) %>%
  filter(Year == 2023) %>%
  ungroup()

# 2. Create the Boxplot
ggplot(boxplot_data, aes(x = Town, y = Price, fill = Town)) +
  # Add Jitter (Points) - Background
  geom_jitter(
    width = 0.2, 
    alpha = 0.2, 
    size = 1, 
    colour = "gray40"
  ) +
  # Add Boxplot - Foreground
  geom_boxplot(
    alpha = 0.7, 
    outlier.shape = NA, # Hide outliers because jitter shows them
    color = "black"
  ) +
  # Faceting: Creates the two-panel look (CHESHIRE vs CUMBERLAND)
  facet_wrap(~ Parent_County, scales = "free_x") +
  
  # Formatting Y-Axis
  scale_y_continuous(
    labels = label_number(prefix = "£", big.mark = ",")
  ) +
  # Zoom in to remove extreme outliers for better visibility
  coord_cartesian(
    ylim = c(0, quantile(boxplot_data$Price, 0.98, na.rm = TRUE))
  ) +
  
  # Labels and Titles
  labs(
    title = "Distribution of House Prices by Town and District in the year 2023",
    subtitle = "Cheshire and Cumberland Regions",
    x = "Town",
    y = "House Price (£)"
  ) +
  
  # Theme Customization to match reference image
  theme_minimal(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(), # Hide x-axis text as it's in the legend
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  # Arrange legend in rows like the reference
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))