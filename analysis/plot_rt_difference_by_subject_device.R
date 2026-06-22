library(dplyr)
library(ggplot2)

data_dir <- file.path("exp", "data_from_lab")
processed_dir <- file.path(data_dir, "extracted_data_processed")
fig_dir <- file.path("analysis", "fig")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

root_csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
root_csv_files <- root_csv_files[grepl("/sub[^/]+/CCRP_subj", root_csv_files, perl = TRUE)]
processed_csv_files <- list.files(processed_dir, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
csv_files <- c(root_csv_files, processed_csv_files)

if (length(csv_files) == 0) {
  stop("No CSV files found in ", data_dir, " or ", processed_dir)
}

read_trial_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)

  if (!("ResponseDevice" %in% names(df))) {
    df$ResponseDevice <- NA_character_
  }

  if (!("RTDifference" %in% names(df))) {
    df$RTDifference <- NA_character_
  }

  df$SourceFile <- basename(path)
  df
}

label_response_device <- function(response_device) {
  response_device_clean <- tolower(trimws(response_device))
  device_label <- gsub("[^A-Za-z0-9]+", "_", response_device)

  device_label[is.na(response_device_clean) | response_device_clean == ""] <- "UnknownDevice"
  device_label[grepl("cedrus|response[_ -]?box|response box", response_device_clean)] <- "RB"
  device_label[grepl("keyboard", response_device_clean)] <- "KB"
  device_label[grepl("self", response_device_clean)] <- "SRB1"

  device_label
}

combined_df <- bind_rows(lapply(csv_files, read_trial_csv))

rt_difference_df <- combined_df %>%
  mutate(
    SubjectNum = suppressWarnings(as.integer(gsub("[^0-9]", "", Subject))),
    SessionNum = suppressWarnings(as.integer(Session)),
    ResponseDevice = if_else(
      SubjectNum == 1 & (is.na(ResponseDevice) | trimws(ResponseDevice) == ""),
      "keyboard",
      ResponseDevice
    ),
    DeviceLabel = label_response_device(ResponseDevice),
    SubjectDevice = paste0("sub", SubjectNum, "_", DeviceLabel),
    RTDifferenceNum = suppressWarnings(as.numeric(RTDifference))
  ) %>%
  filter(
    SubjectNum %in% 1:4,
    SessionNum %in% 6:17
  )

missing_rt_difference_groups <- rt_difference_df %>%
  group_by(SubjectNum, DeviceLabel, SubjectDevice) %>%
  summarize(
    usable_n = sum(!is.na(RTDifferenceNum)),
    .groups = "drop"
  ) %>%
  filter(usable_n == 0)

if (nrow(missing_rt_difference_groups) > 0) {
  message(
    "No numeric RTDifference values for: ",
    paste(missing_rt_difference_groups$SubjectDevice, collapse = ", ")
  )
}

summary_df <- rt_difference_df %>%
  filter(!is.na(RTDifferenceNum)) %>%
  group_by(SubjectNum, DeviceLabel, SubjectDevice) %>%
  summarize(
    mean_rt_difference = mean(RTDifferenceNum),
    sd_rt_difference = sd(RTDifferenceNum),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(SubjectNum, DeviceLabel) %>%
  mutate(SubjectDevice = factor(SubjectDevice, levels = SubjectDevice))

if (nrow(summary_df) == 0) {
  stop("No numeric RTDifference values found for subjects 1-4, sessions 6-17.")
}

rt_difference_plot <- ggplot(summary_df, aes(x = SubjectDevice, y = mean_rt_difference)) +
  geom_hline(yintercept = 0, color = "gray55", linewidth = 0.5) +
  geom_col(fill = "#4C78A8", width = 0.65, alpha = 0.85) +
  geom_errorbar(
    aes(
      ymin = mean_rt_difference - sd_rt_difference,
      ymax = mean_rt_difference + sd_rt_difference
    ),
    width = 0.18,
    linewidth = 0.9
  ) +
  geom_point(size = 3.2, color = "#D95F02") +
  labs(
    title = "Mean RT Difference by Subject and Device",
    subtitle = "Sessions 6-17; error bars show +/- 1 SD",
    x = "Subject and response device",
    y = "RT difference (ms)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 17),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

fig_file <- file.path(fig_dir, "rt_difference_mean_sd_by_subject_device.png")
ggsave(filename = fig_file, plot = rt_difference_plot, width = 10, height = 7, dpi = 300)

message("Saved ", fig_file)
print(summary_df)
