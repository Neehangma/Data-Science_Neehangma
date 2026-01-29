

library(tidyverse)
library(janitor)

set.seed(123)  # reproducibility

# 1. Simulate data for 4 towns with enough postcode sectors
cheshire_demo <- tibble(
  Town = rep(c("CARLISLE", "CREWE", "WARRINGTON", "WINSFORD"), each = 50),
  AvgSpeed = c(
    rnorm(50, mean = 65, sd = 15),   # CARLISLE
    rnorm(50, mean = 55, sd = 10),   # CREWE
    rnorm(50, mean = 60, sd = 20),   # WARRINGTON
    rnorm(50, mean = 50, sd = 12)    # WINSFORD
  )
)


# 2. Create boxplot with red outliers and diamond median
p_cheshire <- ggplot(cheshire_demo, aes(x = Town, y = AvgSpeed, fill = Town)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2, width = 0.6) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3.5,
               fill = "white", color = "black") +
  scale_fill_manual(values = c("#2471A3", "#5499C7", "#F39C12", "#16A085")) +
  labs(
    title = "Broadband Speed Distribution in CHESHIRE",
    x = "Town",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, color = "#2C3E50"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8)
  )


# 3. Save and print
output_dir <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Graph/Broadband-Graph/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(file.path(output_dir, "Boxplot_Cheshire.png"), p_cheshire, width = 9, height = 7, dpi = 300)
print(p_cheshire)


# BOXPLOT: CUMBERLAND
set.seed(456)  # reproducibility
# 1. Simulate data for 4 towns with enough postcode sectors
cumberland_demo <- tibble(
  Town = rep(c("CARLISLE", "WORKINGTON", "WHITEHAVEN", "COCKERMOUTH"), each = 50),
  AvgSpeed = c(
    rnorm(50, mean = 65, sd = 15),   # CARLISLE
    rnorm(50, mean = 55, sd = 10),   # WORKINGTON
    rnorm(50, mean = 50, sd = 12),   # WHITEHAVEN
    rnorm(50, mean = 60, sd = 18)    # COCKERMOUTH
  )
)


# 2. Create boxplot with red outliers and diamond median
p_cumberland <- ggplot(cumberland_demo, aes(x = Town, y = AvgSpeed, fill = Town)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2, width = 0.6) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3.5,
               fill = "white", color = "black") +
  scale_fill_manual(values = c("#C0392B", "#27AE60", "#F1C40F", "#7F8C8D")) +
  labs(
    title = "Broadband Speed Distribution in CUMBERLAND",
    x = "Town",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, color = "#2C3E50"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8)
  )

# 3. Save and print
output_dir <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Graph/Broadband-Graph/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(file.path(output_dir, "Boxplot_Cumberland.png"), p_cumberland, width = 9, height = 7, dpi = 300)
print(p_cumberland)


# ------------------------------------------------------------------------------
# STACKED BAR: CHESHIRE (Download vs Upload)
# ------------------------------------------------------------------------------
cheshire_summary <- cheshire_data %>%
  group_by(Town) %>%
  summarise(AvgSpeed = mean(AvgSpeed, na.rm = TRUE),
            UploadSpeed = mean(UploadSpeed, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(cols = c(AvgSpeed, UploadSpeed), names_to = "Type", values_to = "Speed") %>%
  mutate(Type = recode(Type, "AvgSpeed" = "Average Download", "UploadSpeed" = "Average Upload"))

p3 <- ggplot(cheshire_summary, aes(x = reorder(Town, Speed), y = Speed, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("Average Download" = "steelblue", "Average Upload" = "coral")) +
  theme_minimal() +
  labs(title = "Cheshire: Average Download vs Upload Speeds",
       x = "Town", y = "Speed (Mbps)", fill = "Speed Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

ggsave(file.path(output_dir, "StackedBar_Cheshire.png"), p3, width = 9, height = 7, dpi = 300)
print(p3)

# ==============================================================================
# BROADBAND STACKED BAR CHARTS - CHESHIRE & CUMBERLAND
# ==============================================================================


set.seed(789)  # reproducibility

# ------------------------------------------------------------------------------
# 1. Simulate summary data for 4 towns each
# ------------------------------------------------------------------------------
cheshire_summary <- tibble(
  Town = c("CHESTER", "CREWE", "WARRINGTON", "WINSFORD"),
  AvgSpeed = c(30.3, 32.9, 54.9, 29.6),
  MaxSpeed = c(80.0, 77.8, 135.5, 80.0)
)

cumberland_summary <- tibble(
  Town = c("CARLISLE", "WORKINGTON", "WHITEHAVEN", "COCKERMOUTH"),
  AvgSpeed = c(35.3, 29.1, 28.3, 30.9),
  MaxSpeed = c(150.2, 80.0, 72.9, 79.8)
)

# ------------------------------------------------------------------------------
# 2. Transform to stacked format
# ------------------------------------------------------------------------------
cheshire_stacked <- cheshire_summary %>%
  mutate(PeakExtra = MaxSpeed - AvgSpeed) %>%
  pivot_longer(cols = c(AvgSpeed, PeakExtra), names_to = "Type", values_to = "Speed") %>%
  mutate(Type = recode(Type, "AvgSpeed" = "Base Speed", "PeakExtra" = "Additional Peak"))

cumberland_stacked <- cumberland_summary %>%
  mutate(PeakExtra = MaxSpeed - AvgSpeed) %>%
  pivot_longer(cols = c(AvgSpeed, PeakExtra), names_to = "Type", values_to = "Speed") %>%
  mutate(Type = recode(Type, "AvgSpeed" = "Base Speed", "PeakExtra" = "Additional Peak"))

# ------------------------------------------------------------------------------
# 3. Plot: CHESHIRE
# ------------------------------------------------------------------------------
p_cheshire_bar <- ggplot(cheshire_stacked, aes(x = reorder(Town, MaxSpeed), y = Speed, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Base Speed" = "pink", "Additional Peak" = "gold")) +
  labs(
    title = "Broadband Speed Distribution in CHESHIRE",
    x = "Town",
    y = "Download Speed (Mbps)",
    fill = "Speed Component"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "top"
  )

# ------------------------------------------------------------------------------
# 4. Plot: CUMBERLAND
# ------------------------------------------------------------------------------
p_cumberland_bar <- ggplot(cumberland_stacked, aes(x = reorder(Town, MaxSpeed), y = Speed, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Base Speed" = "green", "Additional Peak" = "brown")) +
  labs(
    title = "Broadband Speed Distribution in CUMBERLAND",
    x = "Town",
    y = "Download Speed (Mbps)",
    fill = "Speed Component"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "top"
  )

# ------------------------------------------------------------------------------
# 5. Save and print
# ------------------------------------------------------------------------------
output_dir <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Graph/Broadband-Graph/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(file.path(output_dir, "StackedBar_Cheshire.png"), p_cheshire_bar, width = 9, height = 7, dpi = 300)
ggsave(file.path(output_dir, "StackedBar_Cumberland.png"), p_cumberland_bar, width = 9, height = 7, dpi = 300)

print(p_cheshire_bar)
print(p_cumberland_bar)


