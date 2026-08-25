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

valid_trial_file <- grepl(
  "^CCRP_subj[0-9]+_ses[0-9]+(_trials)?\\.csv$",
  basename(csv_files)
)
ignored_csv_files <- csv_files[!valid_trial_file]
csv_files <- csv_files[valid_trial_file]

if (length(ignored_csv_files) > 0) {
  message("Ignored non-standard CSV files: ", paste(basename(ignored_csv_files), collapse = ", "))
}

if (length(csv_files) == 0) {
  stop("No CSV files found in ", data_dir, " or ", processed_dir)
}

read_trial_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)

  if (!("ResponseDevice" %in% names(df))) {
    df$ResponseDevice <- NA_character_
  }

  df$SourceFile <- basename(path)
  df$SourceFolder <- if (basename(dirname(path)) == "extracted_data_processed") {
    "extracted_data_processed"
  } else {
    "data_from_lab"
  }

  df
}

nsub <- 20
rt_min_ms <- 100
rt_max_ms <- 3000
histogram_binwidth_ms <- 50

combined_df <- bind_rows(lapply(csv_files, read_trial_csv))

reward_sets <- list(
  "1" = c(1),
  "2" = c(2),
  "3" = c(3),
  "4" = c(4),
  "1,2" = c(1, 2),
  "1,3" = c(1, 3),
  "1,4" = c(1, 4),
  "2,3" = c(2, 3),
  "2,4" = c(2, 4),
  "3,4" = c(3, 4)
)

make_pattern_strings <- function(values) {
  all_digits <- c(values, rep(0, 4 - length(values)))

  permute_unique <- function(x) {
    if (length(x) == 1) return(list(x))

    out <- list()
    used <- c()
    for (i in seq_along(x)) {
      if (x[i] %in% used) next

      used <- c(used, x[i])
      rest_perms <- permute_unique(x[-i])
      for (p in rest_perms) out[[length(out) + 1]] <- c(x[i], p)
    }
    out
  }

  perms <- permute_unique(all_digits)
  unique(vapply(perms, function(p) paste0(p, collapse = ""), character(1)))
}

pattern_map <- lapply(reward_sets, make_pattern_strings)

get_condition <- function(cue_values_string) {
  digits <- gsub("[^0-9]", "", cue_values_string)
  if (nchar(digits) != 4) return(NA_character_)

  vals <- as.integer(strsplit(digits, "")[[1]])
  nonzero_sorted <- sort(vals[vals != 0])
  key <- paste(nonzero_sorted, collapse = ",")

  if (!(key %in% names(reward_sets))) return(NA_character_)
  if (!(digits %in% pattern_map[[key]])) return(NA_character_)

  key
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

condition_levels <- names(reward_sets)

plot_df <- combined_df %>%
  mutate(
    SubjectNum = suppressWarnings(as.integer(gsub("[^0-9]", "", Subject))),
    SessionNum = suppressWarnings(as.integer(Session)),
    ResponseDevice = if_else(
      SubjectNum == 1 & (is.na(ResponseDevice) | trimws(ResponseDevice) == ""),
      "keyboard",
      ResponseDevice
    ),
    DeviceLabel = label_response_device(ResponseDevice),
    Condition = vapply(CueValues, get_condition, character(1)),
    WarmUpFlag = tolower(trimws(WarmUpTrial)),
    RT_num = suppressWarnings(as.numeric(RT))
  ) %>%
  filter(
    SubjectNum %in% 1:nsub,
    SessionNum %in% 7:17,
    !(WarmUpFlag %in% c("1", "true", "t", "yes", "y")),
    !grepl("timeout", Response, ignore.case = TRUE),
    !is.na(RT_num),
    RT_num >= rt_min_ms,
    RT_num <= rt_max_ms,
    !is.na(Condition)
  ) %>%
  mutate(
    SubjectNum = factor(SubjectNum, levels = 1:nsub),
    Condition = factor(Condition, levels = condition_levels)
  )

if (nrow(plot_df) == 0) {
  stop("No trials remained after filtering subjects, sessions 7-17, and RT range.")
}

plot_groups <- plot_df %>%
  distinct(SubjectNum, DeviceLabel) %>%
  arrange(SubjectNum, DeviceLabel)

for (i in seq_len(nrow(plot_groups))) {
  subject_num <- as.character(plot_groups$SubjectNum[i])
  device_label <- plot_groups$DeviceLabel[i]

  subject_plot_df <- plot_df %>%
    filter(SubjectNum == subject_num, DeviceLabel == device_label)

  rt_plot <- ggplot(subject_plot_df, aes(x = RT_num)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = histogram_binwidth_ms, boundary = 0, fill = "#4C78A8", color = "white", alpha = 0.7) +
    geom_density(color = "#D95F02", linewidth = 1.2, adjust = 1) +
    facet_wrap(~Condition, scales = "free_y") +
    labs(
      title = paste0(
        "Subject ", subject_num,
        " RT Distribution by Condition (", device_label, ")"
      ),
      subtitle = paste0("Sessions 7-17, warmup, timeout, and RT outside ", rt_min_ms, "-", rt_max_ms, " ms removed"),
      x = "Reaction Time (ms)",
      y = "Density"
    ) +
    coord_cartesian(xlim = c(0, rt_max_ms)) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 17),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 16, face = "bold")
    )

  fig_file <- file.path(
    fig_dir,
    paste0("sub", subject_num, "_rt_by_condition_", device_label, ".png")
  )

  ggsave(filename = fig_file, plot = rt_plot, width = 14, height = 10, dpi = 300)
  message("Saved ", fig_file, " with ", nrow(subject_plot_df), " trials.")
}
