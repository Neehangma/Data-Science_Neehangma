library(tidyverse)

#  path to broadband PERFORMANCE data
speed_file <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Obtained_Data/BroadbandSpeed/201805_fixed_pc_performance_r03.csv"

# Read raw performance data
BroadbandRaw <- read_csv(speed_file, show_col_types = FALSE)


# Clean column names
names(BroadbandRaw) <- names(BroadbandRaw) %>%
  tolower() %>%
  gsub(" ", "_", .) %>%
  gsub("[^[:alnum:]_]", "", .)


# Clean and summaries PERFORMANCE data only
BroadbandSpeedCleaned <- BroadbandRaw %>%
  select(
    postcode,
    average_download_speed_mbits,
    average_upload_speed_mbits
  ) %>%
  mutate(
    postcode = str_replace_all(postcode, " ", ""),
    shortPostcode = str_sub(postcode, 1, 4),
    average_download_speed_mbits = as.numeric(average_download_speed_mbits),
    average_upload_speed_mbits   = as.numeric(average_upload_speed_mbits)
  ) %>%
  group_by(shortPostcode) %>%
  summarise(
    AvgDownload = mean(average_download_speed_mbits, na.rm = TRUE),
    AvgUpload   = mean(average_upload_speed_mbits, na.rm = TRUE),
    .groups = "drop"
  )

# Save cleaned PERFORMANCE data
clean_path <- "C:/Users/NMRAI/Desktop/Data-Science_Neehangma/Cleaned_Data"
if (!dir.exists(clean_path)) dir.create(clean_path, showWarnings = FALSE)
save_file <- file.path(clean_path, "CleanedBroadband_Performance.csv")
write_csv(BroadbandSpeedCleaned, save_file)
message("Cleaned performance data saved to: ", normalizePath(save_file))
