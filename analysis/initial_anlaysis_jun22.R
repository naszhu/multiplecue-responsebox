library(dplyr)
library(ggplot2)

find_project_root <- function() {
  has_data_dir <- function(dir) {
    dir.exists(file.path(dir, "exp", "data_from_lab"))
  }

  script_dir <- tryCatch({
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) == 0) {
      NA_character_
    } else {
      normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/", mustWork = FALSE)
    }
  }, error = function(e) NA_character_)

  candidates <- c(
    ".",
    "..",
    file.path("..", ".."),
    "multiplecue-responsebox",
    file.path("multiplecue-responsebox"),
    file.path("..", "multiplecue-responsebox"),
    script_dir,
    if (!is.na(script_dir)) dirname(script_dir) else character(0),
    if (!is.na(script_dir)) file.path(script_dir, "..") else character(0),
    if (!is.na(script_dir)) file.path(script_dir, "..", "..") else character(0)
  )

  for (dir in unique(candidates[!is.na(candidates)])) {
    resolved <- tryCatch(normalizePath(dir, winslash = "/", mustWork = FALSE), error = function(e) NA_character_)
    if (!is.na(resolved) && has_data_dir(resolved)) {
      return(resolved)
    }
  }

  stop(
    "Could not find exp/data_from_lab. Run from repo root, multiplecue-responsebox/, analysis/, ",
    "or use: Rscript path/to/initial_anlaysis_jun22.R"
  )
}

project_root <- find_project_root()
setwd(project_root)
data_dir <- file.path(project_root, "exp", "data_from_lab")
fig_dir <- file.path(project_root, "analysis", "fig", "initial_analysis_jun22")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

rt_plot_theme <- theme_minimal(base_size = 28) +
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

summarize_rt_by_subject <- function(df) {
  subject_session_counts <- df %>%
    group_by(Subject) %>%
    summarize(n_sessions = n_distinct(SessionNum), .groups = "drop")

  df %>%
    filter(!is.na(RTDifferenceNum)) %>%
    group_by(Subject) %>%
    summarize(
      mean_rt_difference = mean(RTDifferenceNum),
      sd_rt_difference = sd(RTDifferenceNum),
      n_trials = n(),
      .groups = "drop"
    ) %>%
    left_join(subject_session_counts, by = "Subject") %>%
    mutate(
      SubjectNum = suppressWarnings(as.integer(Subject)),
      SubjectLabel = paste0("Subject ", Subject, "(n=", n_sessions, ")"),
      SubjectLabel = factor(SubjectLabel, levels = SubjectLabel[order(SubjectNum)])
    )
}

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

# lousy participant
combined_df <- combined_df %>%
  filter(Subject != "13")

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
  rt_plot_theme +
  theme(strip.text = element_text(size = 28, face = "bold"))

print(rt_difference_plot)

fig_file <- file.path(fig_dir, "rt_difference_by_device_multi_device_subjects.png")
ggsave(filename = fig_file, plot = rt_difference_plot, width = 16, height = 12, dpi = 300)
cat("Saved ", fig_file, "\n", sep = "")
print(as.data.frame(summary_df), row.names = FALSE)

# ------------------------------------------------------------
# RT difference by device, pooled across all subjects
# ------------------------------------------------------------
all_rt_difference_df <- combined_df %>%
  mutate(
    SubjectNum = suppressWarnings(as.integer(Subject)),
    SessionNum = suppressWarnings(as.integer(Session)),
    RTDifferenceNum = suppressWarnings(as.numeric(RTDifference))
  )

device_counts_all <- all_rt_difference_df %>%
  group_by(DeviceName) %>%
  summarize(
    n_sessions = n_distinct(Subject, SessionNum),
    n_subjects = n_distinct(Subject),
    .groups = "drop"
  )

device_summary_df <- all_rt_difference_df %>%
  filter(!is.na(RTDifferenceNum)) %>%
  group_by(DeviceName) %>%
  summarize(
    mean_rt_difference = mean(RTDifferenceNum),
    sd_rt_difference = sd(RTDifferenceNum),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  left_join(device_counts_all, by = "DeviceName") %>%
  mutate(
    DeviceLabel = paste0(DeviceName, "(n=", n_sessions, ")"),
    DeviceLabel = factor(DeviceLabel, levels = DeviceLabel[order(DeviceName)])
  )

if (nrow(device_summary_df) == 0) {
  stop("No numeric RTDifference values found for all-subjects device summary.")
}

rt_difference_all_devices_plot <- ggplot(device_summary_df, aes(x = DeviceLabel, y = mean_rt_difference)) +
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
  labs(
    title = "Mean RT Difference by Device (All Subjects)",
    subtitle = "Pooled across all subjects; error bars show +/- 1 SD; x-axis labels show session count per device",
    x = "Device",
    y = "RT difference (ms)"
  ) +
  rt_plot_theme

print(rt_difference_all_devices_plot)

all_devices_fig_file <- file.path(fig_dir, "rt_difference_by_device_all_subjects.png")
ggsave(filename = all_devices_fig_file, plot = rt_difference_all_devices_plot, width = 16, height = 10, dpi = 300)
cat("Saved ", all_devices_fig_file, "\n", sep = "")
print(as.data.frame(device_summary_df), row.names = FALSE)

# ------------------------------------------------------------
# RT difference by MonitorName, pooled across all subjects
# ------------------------------------------------------------
monitor_rt_df <- all_rt_difference_df %>%
  filter(!is.na(MonitorName), trimws(MonitorName) != "")

monitor_counts_all <- monitor_rt_df %>%
  group_by(MonitorName) %>%
  summarize(
    n_sessions = n_distinct(Subject, SessionNum),
    n_subjects = n_distinct(Subject),
    .groups = "drop"
  )

monitor_summary_df <- monitor_rt_df %>%
  filter(!is.na(RTDifferenceNum)) %>%
  group_by(MonitorName) %>%
  summarize(
    mean_rt_difference = mean(RTDifferenceNum),
    sd_rt_difference = sd(RTDifferenceNum),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  left_join(monitor_counts_all, by = "MonitorName") %>%
  mutate(
    MonitorLabel = paste0(MonitorName, "(n=", n_sessions, ")"),
    MonitorLabel = factor(MonitorLabel, levels = MonitorLabel[order(MonitorName)])
  )

if (nrow(monitor_summary_df) == 0) {
  stop("No numeric RTDifference values found for MonitorName summary.")
}

rt_difference_by_monitor_plot <- ggplot(monitor_summary_df, aes(x = MonitorLabel, y = mean_rt_difference)) +
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
  labs(
    title = "Mean RT Difference by Monitor (All Subjects)",
    subtitle = "Pooled across all subjects; error bars show +/- 1 SD; x-axis labels show session count per monitor",
    x = "Monitor",
    y = "RT difference (ms)"
  ) +
  rt_plot_theme

print(rt_difference_by_monitor_plot)

monitor_fig_file <- file.path(fig_dir, "rt_difference_by_monitor_all_subjects.png")
ggsave(filename = monitor_fig_file, plot = rt_difference_by_monitor_plot, width = 16, height = 10, dpi = 300)
cat("Saved ", monitor_fig_file, "\n", sep = "")
print(as.data.frame(monitor_summary_df), row.names = FALSE)

# ------------------------------------------------------------
# RT difference by subject for room1_a5 (monitor 5)
# ------------------------------------------------------------
monitor_a5_summary <- summarize_rt_by_subject(
  monitor_rt_df %>% filter(MonitorName == "room1_a5")
)

if (nrow(monitor_a5_summary) == 0) {
  stop("No numeric RTDifference values found for monitor room1_a5.")
}

rt_difference_monitor_a5_by_subject_plot <- ggplot(monitor_a5_summary, aes(x = SubjectLabel, y = mean_rt_difference)) +
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
  labs(
    title = "Mean RT Difference by Subject (room1_a5)",
    subtitle = "Error bars show +/- 1 SD; x-axis labels show session count per subject",
    x = "Subject",
    y = "RT difference (ms)"
  ) +
  rt_plot_theme

print(rt_difference_monitor_a5_by_subject_plot)

monitor_a5_fig_file <- file.path(fig_dir, "rt_difference_by_subject_room1_a5.png")
ggsave(filename = monitor_a5_fig_file, plot = rt_difference_monitor_a5_by_subject_plot, width = 16, height = 10, dpi = 300)
cat("Saved ", monitor_a5_fig_file, "\n", sep = "")
print(as.data.frame(monitor_a5_summary), row.names = FALSE)

# ------------------------------------------------------------
# RT difference by subject for SRB6
# ------------------------------------------------------------
srb6_summary <- summarize_rt_by_subject(
  all_rt_difference_df %>% filter(DeviceName == "SRB6")
)

if (nrow(srb6_summary) == 0) {
  stop("No numeric RTDifference values found for device SRB6.")
}

rt_difference_srb6_by_subject_plot <- ggplot(srb6_summary, aes(x = SubjectLabel, y = mean_rt_difference)) +
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
  labs(
    title = "Mean RT Difference by Subject (SRB6)",
    subtitle = "Error bars show +/- 1 SD; x-axis labels show session count per subject",
    x = "Subject",
    y = "RT difference (ms)"
  ) +
  rt_plot_theme

print(rt_difference_srb6_by_subject_plot)

srb6_fig_file <- file.path(fig_dir, "rt_difference_by_subject_SRB6.png")
ggsave(filename = srb6_fig_file, plot = rt_difference_srb6_by_subject_plot, width = 16, height = 10, dpi = 300)
cat("Saved ", srb6_fig_file, "\n", sep = "")
print(as.data.frame(srb6_summary), row.names = FALSE)
