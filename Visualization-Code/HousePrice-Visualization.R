library(ggplot2)
library(dplyr)
library(scales)
library(readr)

# 1. Read and Prepare Data
data <- read_csv("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data/HousePrices_cleaned.csv")

# -------------------------------
# LINE GRAPH: Average Prices 2022-2024
# -------------------------------

plot_data <- data %>%
  mutate(Parent_County = ifelse(grepl("Cheshire", County, ignore.case = TRUE), "CHESHIRE", "CUMBERLAND")) %>%
  filter(Year >= 2022 & Year <= 2024)

town_data <- plot_data %>%
  group_by(Year, Town, Parent_County) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = 'drop')

# Top towns per county
top_towns <- town_data %>%
  group_by(Parent_County, Town) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Parent_County) %>%
  slice_max(order_by = count, n = 5) %>%
  pull(Town)

town_data <- town_data %>% filter(Town %in% top_towns)

line_graph <- ggplot(town_data, aes(x = Year, y = Avg_Price, color = Town, group = Town)) +
  geom_line(aes(linetype = Town), size = 1) +
  geom_point(aes(shape = Town), size = 3) +
  facet_wrap(~ Parent_County, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2021:2024) +
  labs(title = "Average House Prices (2022-2024) by Town and County",
       x = "Year",
       y = "Average Price (£)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.box = "vertical"
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         shape = guide_legend(nrow = 2, byrow = TRUE),
         linetype = guide_legend(nrow = 2, byrow = TRUE))

# Save Line Graph
ggsave("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Graph/House Price-Graph/line_graph_house_prices.png",
       plot = line_graph, width = 10, height = 7)

# -------------------------------
# BAR CHART: Average Prices in 2022
# -------------------------------

bar_data_processed <- data %>%
  filter(Year == 2022) %>%
  mutate(Parent_County = ifelse(grepl("Cheshire", County, ignore.case = TRUE), 
                                "CHESHIRE", "CUMBERLAND")) %>%
  group_by(Parent_County, District, Town) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Parent_County) %>%
  slice_max(order_by = Avg_Price, n = 8)

bar_chart <- ggplot(bar_data_processed, aes(x = reorder(Town, -Avg_Price), y = Avg_Price, fill = Town)) +
  geom_bar(stat = "identity", width = 0.7) +
  facet_wrap(~ Parent_County, scales = "free_x") +
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
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

# Save Bar Chart
ggsave("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Graph/House Price-Graph/bar_chart_house_prices.png",
       plot = bar_chart, width = 10, height = 7)


# BOX PLOT: Distribution in 2023
boxplot_data <- data %>%
  mutate(Parent_County = ifelse(grepl("Cheshire", County, ignore.case = TRUE), 
                                "CHESHIRE", "CUMBERLAND")) %>%
  filter(Year == 2023)

box_plot <- ggplot(boxplot_data, aes(x = Town, y = Price, fill = Town)) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1, colour = "gray40") +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +
  facet_wrap(~ Parent_County, scales = "free_x") +
  scale_y_continuous(labels = label_number(prefix = "£", big.mark = ",")) +
  coord_cartesian(ylim = c(0, quantile(boxplot_data$Price, 0.98, na.rm = TRUE))) +
  labs(title = "Distribution of House Prices by Town and District in 2023",
       subtitle = "Cheshire and Cumberland Regions",
       x = "Town",
       y = "House Price (£)") +
  theme_minimal(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

# Save Box Plot
ggsave("C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Graph/House Price-Graph/boxplot_house_prices.png",
       plot = box_plot, width = 10, height = 7)
