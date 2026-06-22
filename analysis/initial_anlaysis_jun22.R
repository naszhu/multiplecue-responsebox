library(dplyr)
library(ggplot2)

# Resolve project root so this runs from repo root or multiplecue-responsebox/
if (dir.exists("exp/data_from_lab")) {
  project_root <- normalizePath(".")
} else if (dir.exists("multiplecue-responsebox/exp/data_from_lab")) {
  project_root <- normalizePath("multiplecue-responsebox")
} else {
  stop("Could not find exp/data_from_lab. Set working directory to multiplecue-responsebox/ or the repo root.")
}

setwd(project_root)
data_dir <- file.path(project_root, "exp", "data_from_lab")

# Only subject folders named sub1, sub2, sub3, ... (no sub3star, subtest, etc.)
subject_dirs <- list.dirs(data_dir, full.names = FALSE, recursive = FALSE)
subject_dirs <- subject_dirs[grepl("^sub[0-9]+$", subject_dirs)]

trial_csv_files <- unlist(lapply(subject_dirs, function(sub_dir) {
  list.files(
    file.path(data_dir, sub_dir),
    pattern = "_trials\\.csv$",
    full.names = TRUE
  )
}))

if (length(trial_csv_files) == 0) {
  stop("No *_trials.csv files found in subject folders under ", data_dir)
}

read_trial_csv <- function(path) {
  if (file.info(path)$size == 0) {
    return(NULL)
  }
  read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
}

combined_df <- bind_rows(lapply(trial_csv_files, read_trial_csv))

if (nrow(combined_df) == 0 || !("Subject" %in% names(combined_df))) {
  stop(
    "No trial data loaded from ", data_dir,
    ". Found ", length(trial_csv_files), " CSV file(s), but combined_df is empty or missing Subject."
  )
}

combined_df <- combined_df %>%
  filter(!is.na(DeviceName), trimws(DeviceName) != "")

# One row per Subject x DeviceName combination
subject_device <- combined_df %>%
  group_by(Subject, DeviceName) %>%
  summarize(.groups = "drop")

# Count distinct devices used by each subject, ordered by Subject
subject_device_counts <- subject_device %>%
  group_by(Subject) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  arrange(as.integer(Subject))

# Subjects who used more than 2 devices
multi_device_subjects <- subject_device_counts %>%
  filter(n >= 2) %>%
  arrange(as.integer(Subject), DeviceName)

cat("Subjects with 2 or more devices (Subject, DeviceName, n_devices):\n")
print(as.data.frame(multi_device_subjects), row.names = FALSE)

# ------------------------------------------------------------
# RT difference by device for subjects with >= 2 devices
# ------------------------------------------------------------
multi_device_subject_ids <- multi_device_subjects %>%
  distinct(Subject) %>%
  pull(Subject)

rt_difference_df <- combined_df %>%
  mutate(
    SubjectNum = suppressWarnings(as.integer(Subject)),
    SessionNum = suppressWarnings(as.integer(Session)),
    RTDifferenceNum = suppressWarnings(as.numeric(RTDifference))
  ) %>%
  filter(Subject %in% multi_device_subject_ids)

device_session_counts <- rt_difference_df %>%
  group_by(Subject, DeviceName) %>%
  summarize(n_sessions = n_distinct(SessionNum), .groups = "drop")

summary_df <- rt_difference_df %>%
  filter(!is.na(RTDifferenceNum)) %>%
  group_by(Subject, DeviceName) %>%
  summarize(
    mean_rt_difference = mean(RTDifferenceNum),
    sd_rt_difference = sd(RTDifferenceNum),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  left_join(device_session_counts, by = c("Subject", "DeviceName")) %>%
  mutate(
    DeviceLabel = paste0(DeviceName, "(n=", n_sessions, ")"),
    Subject = factor(Subject, levels = sort(unique(as.integer(Subject))))
  ) %>%
  group_by(Subject) %>%
  mutate(DeviceLabel = factor(DeviceLabel, levels = DeviceLabel)) %>%
  ungroup()

if (nrow(summary_df) == 0) {
  stop("No numeric RTDifference values found for subjects with >= 2 devices.")
}

rt_difference_plot <- ggplot(summary_df, aes(x = DeviceLabel, y = mean_rt_difference)) +
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
  geom_point(size = 5, color = "#D95F02") +
  facet_wrap(~Subject, scales = "free_x", labeller = labeller(Subject = function(x) paste0("Subject ", x))) +
  labs(
    title = "Mean RT Difference by Device (Subjects with >= 2 Devices)",
    subtitle = "Error bars show +/- 1 SD; x-axis labels show session count per device",
    x = "Device",
    y = "RT difference (ms)"
  ) +
  theme_minimal(base_size = 28) +
  theme(
    plot.title = element_text(size = 34, face = "bold"),
    plot.subtitle = element_text(size = 26),
    axis.title = element_text(size = 28, face = "bold"),
    axis.text = element_text(size = 24),
    axis.text.x = element_text(angle = 25, hjust = 1, size = 24),
    axis.text.y = element_text(size = 24),
    strip.text = element_text(size = 28, face = "bold"),
    panel.spacing = unit(1.2, "lines")
  )

print(rt_difference_plot)

fig_dir <- file.path(project_root, "analysis", "fig")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
fig_file <- file.path(fig_dir, "rt_difference_by_device_multi_device_subjects.png")
ggsave(filename = fig_file, plot = rt_difference_plot, width = 16, height = 12, dpi = 300)
cat("Saved ", fig_file, "\n", sep = "")
print(as.data.frame(summary_df), row.names = FALSE)
