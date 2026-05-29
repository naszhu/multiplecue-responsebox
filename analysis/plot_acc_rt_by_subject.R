library(dplyr)
library(ggplot2)
library(gridExtra)

data_dir <- file.path("exp", "data_from_lab")
processed_dir <- file.path(data_dir, "extracted_data_processed")
fig_dir <- file.path("analysis", "fig")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

root_csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
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
  if (!("ACC" %in% names(df))) {
    df$ACC <- NA_character_
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
session_range <- 6:17
rt_min_ms <- 0
rt_max_ms <- 4000
rt_qcond_max_ms <- 3000
plot_base_size <- 28
title_size <- 32
subtitle_size <- 24
axis_title_size <- 28
axis_text_size <- 24
strip_text_size <- 24
bold_axes_theme <- theme(
  axis.title = element_text(face = "bold"),
  axis.text = element_text(face = "bold"),
  axis.text.x = element_text(face = "bold"),
  axis.text.y = element_text(face = "bold"),
  plot.caption = element_text(face = "bold")
)

ordinal_suffix <- function(n) {
  if (is.na(n)) return(NA_character_)
  if (n %% 100 %in% c(11, 12, 13)) return(paste0(n, "th"))
  last_digit <- n %% 10
  suffix <- if (last_digit == 1) {
    "st"
  } else if (last_digit == 2) {
    "nd"
  } else if (last_digit == 3) {
    "rd"
  } else {
    "th"
  }
  paste0(n, suffix)
}

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

get_reward_diff_from_condition <- function(condition_string) {
  if (is.na(condition_string) || !grepl(",", condition_string, fixed = TRUE)) return(NA_real_)
  parts <- as.integer(strsplit(condition_string, ",", fixed = TRUE)[[1]])
  if (length(parts) != 2 || any(is.na(parts))) return(NA_real_)
  abs(parts[2] - parts[1])
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

cue_color_map <- c("1" = "red", "2" = "green", "3" = "blue", "4" = "yellow")

get_highest_reward_color <- function(cues_string, cue_ranks_string) {
  cue_digits <- gsub("[^0-9]", "", cues_string)
  rank_digits <- gsub("[^0-9]", "", cue_ranks_string)

  if (nchar(cue_digits) != 4 || nchar(rank_digits) != 4) return(NA_character_)

  cue_vals <- as.integer(strsplit(cue_digits, "")[[1]])
  rank_vals <- as.integer(strsplit(rank_digits, "")[[1]])

  highest_idx <- which(rank_vals == 1 & cue_vals %in% 1:4)
  if (length(highest_idx) == 0) return(NA_character_)

  cue_color_map[as.character(cue_vals[highest_idx[1]])]
}

condition_levels <- names(reward_sets)

combined_df <- bind_rows(lapply(csv_files, read_trial_csv))

plot_df <- combined_df %>%
  mutate(
    SubjectNum = suppressWarnings(as.integer(gsub("[^0-9]", "", Subject))),
    SubjectNumInt = SubjectNum,
    SessionNumInt = suppressWarnings(as.integer(Session)),
    TrialNumInt = suppressWarnings(as.integer(Trial)),
    ResponseDevice = if_else(
      SubjectNum == 1 & (is.na(ResponseDevice) | trimws(ResponseDevice) == ""),
      "keyboard",
      ResponseDevice
    ),
    DeviceLabel = label_response_device(ResponseDevice),
    Condition = vapply(CueValues, get_condition, character(1)),
    RewardDiff = vapply(Condition, get_reward_diff_from_condition, numeric(1)),
    HighestRewardColor = mapply(get_highest_reward_color, Cues, CueRanks, USE.NAMES = FALSE),
    CueCount = if_else(Condition %in% c("1", "2", "3", "4"), "1-cue", "2-cue"),
    ResponseKey = tolower(trimws(Response)),
    TrialDate = as.Date(substr(TrialWallClockTime, 1, 10), format = "%Y-%m-%d"),
    WarmUpFlag = tolower(trimws(WarmUpTrial)),
    RT_num = suppressWarnings(as.numeric(RT)),
    ACC_num = suppressWarnings(as.numeric(ACC)),
    RTDifference_num = suppressWarnings(as.numeric(RTDifference))
  ) %>%
  filter(
    SubjectNum %in% 1:nsub,
    SessionNumInt %in% session_range,
    !(WarmUpFlag %in% c("1", "true", "t", "yes", "y")),
    !grepl("timeout", Response, ignore.case = TRUE),
    !is.na(RT_num),
    RT_num >= rt_min_ms,
    RT_num <= rt_max_ms,
    !is.na(Condition),
    !is.na(ACC_num)
  ) %>%
  mutate(
    SubjectNum = factor(SubjectNum, levels = 1:nsub),
    SessionNum = factor(SessionNumInt, levels = session_range),
    SessionHalfGroup = if_else(SessionNumInt <= 11, "group1", "group2"),
    Condition = factor(Condition, levels = condition_levels),
    CueCount = factor(CueCount, levels = c("1-cue", "2-cue")),
    RewardDiff = factor(RewardDiff, levels = c(1, 2, 3))
  )

if (nrow(plot_df) == 0) {
  stop("No trials remained after filtering.")
}

sub6_total_trials <- plot_df %>%
  filter(SubjectNumInt == 6) %>%
  nrow()

sub6_filtered_trials <- plot_df %>%
  filter(SubjectNumInt == 6, !is.na(RTDifference_num), RTDifference_num > 500) %>%
  nrow()

sub6_filtered_prop <- if (sub6_total_trials > 0) {
  sub6_filtered_trials / sub6_total_trials
} else {
  NA_real_
}

message(
  "Subject 6 RTDifference filter (>500 ms): filtered ",
  sub6_filtered_trials, " of ", sub6_total_trials,
  " trials (", sprintf("%.2f%%", 100 * sub6_filtered_prop), ")."
)

rt_plot_df <- plot_df %>%
  filter(!(SubjectNumInt == 6 & !is.na(RTDifference_num) & RTDifference_num > 500))

device_order <- c("KB", "RB", "SRB1", "UnknownDevice")
subject_device_order <- plot_df %>%
  distinct(SubjectNum, DeviceLabel) %>%
  mutate(
    SubjectInt = as.integer(as.character(SubjectNum)),
    DeviceRank = match(DeviceLabel, device_order)
  ) %>%
  arrange(DeviceRank, SubjectInt)

subject_facet_levels <- paste0(
  "sub", as.character(subject_device_order$SubjectNum), "_", subject_device_order$DeviceLabel
)

plot_df <- plot_df %>%
  mutate(
    SubjectFacet = factor(
      paste0("sub", as.character(SubjectNum), "_", DeviceLabel),
      levels = subject_facet_levels
    )
  )

subject_day_map <- plot_df %>%
  distinct(SubjectNum, TrialDate) %>%
  arrange(SubjectNum, TrialDate) %>%
  group_by(SubjectNum) %>%
  mutate(
    DayIndex = row_number(),
    DayLabel = paste0("day", DayIndex)
  ) %>%
  ungroup()

plot_df <- plot_df %>%
  left_join(subject_day_map, by = c("SubjectNum", "TrialDate"))

day_levels <- plot_df %>%
  distinct(DayIndex) %>%
  filter(!is.na(DayIndex)) %>%
  arrange(DayIndex) %>%
  pull(DayIndex)

plot_df <- plot_df %>%
  mutate(
    DayLabel = factor(paste0("day", DayIndex), levels = paste0("day", day_levels))
  )

subject_day_session_map <- plot_df %>%
  distinct(SubjectNum, DayLabel, SessionNumInt) %>%
  arrange(SubjectNum, DayLabel, SessionNumInt) %>%
  group_by(SubjectNum, DayLabel) %>%
  mutate(SessionOrderInDay = row_number()) %>%
  ungroup() %>%
  mutate(
    SessionOrderLabel = vapply(SessionOrderInDay, ordinal_suffix, character(1)),
    SessionOrderLabel = factor(
      SessionOrderLabel,
      levels = vapply(1:6, ordinal_suffix, character(1))
    )
  )

plot_df <- plot_df %>%
  left_join(
    subject_day_session_map %>%
      select(SubjectNum, DayLabel, SessionNumInt, SessionOrderInDay, SessionOrderLabel),
    by = c("SubjectNum", "DayLabel", "SessionNumInt")
  )

rt_plot_df <- plot_df %>%
  filter(!(SubjectNumInt == 6 & !is.na(RTDifference_num) & RTDifference_num > 500))

rt_qcond_test_df <- rt_plot_df %>%
  group_by(SubjectFacet) %>%
  summarize(
    n_1cue = sum(CueCount == "1-cue", na.rm = TRUE),
    n_2cue = sum(CueCount == "2-cue", na.rm = TRUE),
    p_value = if (n_1cue >= 2 && n_2cue >= 2) {
      tryCatch(
        t.test(RT_num ~ CueCount)$p.value,
        error = function(e) NA_real_
      )
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(
    sig_label = case_when(
      !is.na(p_value) & p_value < 0.001 ~ "***",
      !is.na(p_value) & p_value < 0.01 ~ "**",
      !is.na(p_value) & p_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    x_pos = 1.5,
    y_pos = rt_qcond_max_ms * 0.97
  )

rt_qcond_plot <- ggplot(rt_plot_df, aes(x = CueCount, y = RT_num)) +
  geom_violin(fill = "#B07AA1", alpha = 0.7, color = "gray25", trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.size = 0.45, alpha = 0.25, color = "black") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "red3", linewidth = 2.0) +
  stat_summary(fun = mean, geom = "point", color = "red3", size = 3.4) +
  geom_text(
    data = rt_qcond_test_df %>% filter(sig_label != ""),
    aes(x = x_pos, y = y_pos, label = sig_label),
    inherit.aes = FALSE,
    size = 10,
    fontface = "bold"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "free_y") +
  labs(
    title = "RT by 1-cue vs 2-cue Condition (All Subjects)",
    subtitle = "Facets ordered by device groups (KB first), RT filtered to 0-4000 ms",
    x = "Q Condition",
    y = "Reaction Time (ms)"
  ) +
  coord_cartesian(ylim = c(rt_min_ms, rt_qcond_max_ms)) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

rt_qcond_file <- file.path(fig_dir, "subAll_RTQCond_AllDev.png")
ggsave(filename = rt_qcond_file, plot = rt_qcond_plot + bold_axes_theme, width = 18, height = 14, dpi = 300)

reward_diff_condition_counts <- plot_df %>%
  filter(CueCount == "2-cue", !is.na(RewardDiff)) %>%
  distinct(Condition, RewardDiff) %>%
  count(RewardDiff, name = "ConditionCount") %>%
  mutate(RewardDiffNum = as.integer(as.character(RewardDiff))) %>%
  arrange(RewardDiffNum)

reward_diff_caption <- paste0(
  "Condition-types per reward-difference level: ",
  paste0(
    "diff ", reward_diff_condition_counts$RewardDiffNum,
    " = ", reward_diff_condition_counts$ConditionCount,
    collapse = "; "
  ),
  ". Unequal counts across levels."
)

rt_diff_plot_df <- rt_plot_df %>%
  filter(CueCount == "2-cue", !is.na(RewardDiff))

rt_diff_summary_df <- rt_diff_plot_df %>%
  group_by(SubjectFacet, RewardDiff) %>%
  summarize(
    MeanRT = mean(RT_num, na.rm = TRUE),
    Trials = n(),
    RTSD = sd(RT_num, na.rm = TRUE),
    RTSE = if_else(Trials > 1, RTSD / sqrt(Trials), 0),
    .groups = "drop"
  )

rt_diff_plot_all <- ggplot(rt_diff_plot_df, aes(x = RewardDiff, y = RT_num)) +
  geom_violin(fill = "#A0CBE8", alpha = 0.65, color = "gray25", trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.size = 0.45, alpha = 0.25, color = "black") +
  geom_ribbon(
    data = rt_diff_summary_df,
    aes(
      x = RewardDiff,
      y = MeanRT,
      group = 1,
      ymin = pmax(0, MeanRT - RTSE),
      ymax = pmin(3000, MeanRT + RTSE)
    ),
    inherit.aes = FALSE,
    fill = "red3",
    alpha = 0.16
  ) +
  geom_line(
    data = rt_diff_summary_df,
    aes(x = RewardDiff, y = MeanRT, group = 1),
    color = "red3",
    linewidth = 1.5,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = rt_diff_summary_df,
    aes(x = RewardDiff, y = MeanRT),
    color = "red3",
    size = 2.8,
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    data = rt_diff_summary_df,
    aes(
      x = RewardDiff,
      y = MeanRT,
      group = 1,
      ymin = pmax(0, MeanRT - RTSE),
      ymax = pmin(3000, MeanRT + RTSE)
    ),
    inherit.aes = FALSE,
    width = 0.15,
    linewidth = 0.75,
    color = "#A81F1F"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "RT by Reward Difference (2-cue Conditions, All Subjects)",
    subtitle = "Reward difference = absolute difference between the two cue values",
    x = "Reward Difference",
    y = "Reaction Time (ms)",
    caption = reward_diff_caption
  ) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold"),
    plot.caption = element_text(size = axis_text_size * 0.7, hjust = 0)
  )

rt_diff_all_file <- file.path(fig_dir, "subAll_RTRewardDiff_AllDev.png")
ggsave(filename = rt_diff_all_file, plot = rt_diff_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

acc_diff_df <- plot_df %>%
  filter(CueCount == "2-cue", !is.na(RewardDiff)) %>%
  group_by(SubjectFacet, RewardDiff) %>%
  summarize(
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    AccuracySD = sd(ACC_num, na.rm = TRUE),
    AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
    .groups = "drop"
  )

acc_diff_ymin <- max(0, min(acc_diff_df$Accuracy, na.rm = TRUE) - 0.08)

acc_diff_plot_all <- ggplot(acc_diff_df, aes(x = RewardDiff, y = Accuracy, group = 1)) +
  geom_ribbon(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    fill = "#59A14F",
    alpha = 0.18
  ) +
  geom_line(color = "#59A14F", linewidth = 1.1) +
  geom_point(color = "#59A14F", size = 2.4) +
  geom_errorbar(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    width = 0.15,
    linewidth = 0.7,
    color = "#2F6B34"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Accuracy by Reward Difference (2-cue Conditions, All Subjects)",
    subtitle = "Reward difference = absolute difference between the two cue values",
    x = "Reward Difference",
    y = "Accuracy",
    caption = reward_diff_caption
  ) +
  scale_y_continuous(limits = c(acc_diff_ymin, 1), expand = expansion(mult = c(0, 0.06))) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold"),
    plot.caption = element_text(size = axis_text_size * 0.7, hjust = 0)
  )

acc_diff_all_file <- file.path(fig_dir, "subAll_ACCRewardDiff_AllDev.png")
ggsave(filename = acc_diff_all_file, plot = acc_diff_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

rt_cond_plot_all <- ggplot(rt_plot_df, aes(x = Condition, y = RT_num)) +
  geom_violin(fill = "#4C78A8", alpha = 0.65, color = "gray25", trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.size = 0.5, alpha = 0.25, color = "black") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "red3", linewidth = 1.15) +
  stat_summary(fun = mean, geom = "point", color = "red3", size = 2.5) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "RT Violin by Condition (All Subjects)",
    subtitle = "Facets ordered by device groups (KB first), RT filtered to 0-4000 ms",
    x = "Condition",
    y = "Reaction Time (ms)"
  ) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    axis.text.x = element_text(size = axis_text_size, angle = 30, hjust = 1),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

rt_cond_all_file <- file.path(fig_dir, "subAll_RTCondVln_AllDev.png")
ggsave(filename = rt_cond_all_file, plot = rt_cond_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

acc_condition_all_df <- plot_df %>%
  group_by(SubjectFacet, Condition) %>%
  summarize(
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    AccuracySD = sd(ACC_num, na.rm = TRUE),
    AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
    .groups = "drop"
  )

acc_cond_global_ymin <- max(0, min(acc_condition_all_df$Accuracy, na.rm = TRUE) - 0.08)

acc_cond_plot_all <- ggplot(acc_condition_all_df, aes(x = Condition, y = Accuracy, group = 1)) +
  geom_ribbon(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    fill = "#59A14F",
    alpha = 0.18
  ) +
  geom_line(color = "#59A14F", linewidth = 1.1) +
  geom_point(color = "#59A14F", size = 2.5) +
  geom_errorbar(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    width = 0.15,
    linewidth = 0.7,
    color = "#2F6B34"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Accuracy by Condition (All Subjects)",
    subtitle = "Facets ordered by device groups (KB first)",
    x = "Condition",
    y = "Accuracy"
  ) +
  scale_y_continuous(limits = c(acc_cond_global_ymin, 1), expand = expansion(mult = c(0, 0.06))) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    axis.text.x = element_text(size = axis_text_size, angle = 30, hjust = 1),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

acc_cond_all_file <- file.path(fig_dir, "subAll_ACCCond_AllDev.png")
ggsave(filename = acc_cond_all_file, plot = acc_cond_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

rt_day_plot_all <- ggplot(rt_plot_df, aes(x = DayLabel, y = RT_num)) +
  geom_violin(fill = "#F28E2B", alpha = 0.7, color = "gray25", trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.size = 0.45, alpha = 0.25, color = "black") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "red3", linewidth = 2.0) +
  stat_summary(fun = mean, geom = "point", color = "red3", size = 3.2) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "RT by Experiment Day (All Subjects)",
    subtitle = "Facets ordered by device groups (KB first)",
    x = "Day",
    y = "Reaction Time (ms)"
  ) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

rt_day_all_file <- file.path(fig_dir, "subAll_RTDayVln_AllDev.png")
ggsave(filename = rt_day_all_file, plot = rt_day_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

acc_day_all_df <- plot_df %>%
  group_by(SubjectFacet, DayLabel) %>%
  summarize(
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    AccuracySD = sd(ACC_num, na.rm = TRUE),
    AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
    .groups = "drop"
  )

acc_day_global_ymin <- max(0, min(acc_day_all_df$Accuracy, na.rm = TRUE) - 0.08)

acc_day_plot_all <- ggplot(acc_day_all_df, aes(x = DayLabel, y = Accuracy, group = 1)) +
  geom_ribbon(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    fill = "#59A14F",
    alpha = 0.18
  ) +
  geom_line(color = "#59A14F", linewidth = 1.1) +
  geom_point(color = "#59A14F", size = 2.5) +
  geom_errorbar(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    width = 0.15,
    linewidth = 0.7,
    color = "#2F6B34"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Accuracy by Experiment Day (All Subjects)",
    subtitle = "Facets ordered by device groups (KB first)",
    x = "Day",
    y = "Accuracy"
  ) +
  scale_y_continuous(limits = c(acc_day_global_ymin, 1), expand = expansion(mult = c(0, 0.06))) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

acc_day_all_file <- file.path(fig_dir, "subAll_ACCDay_AllDev.png")
ggsave(filename = acc_day_all_file, plot = acc_day_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

rt_session_plot_all <- ggplot(rt_plot_df, aes(x = SessionNum, y = RT_num)) +
  geom_violin(fill = "#F28E2B", alpha = 0.68, color = "gray25", trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.size = 0.45, alpha = 0.25, color = "black") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "red3", linewidth = 1.6) +
  stat_summary(fun = mean, geom = "point", color = "red3", size = 2.8) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "RT Violin by Session (All Subjects)",
    subtitle = "Facets ordered by device groups (KB first)",
    x = "Session",
    y = "Reaction Time (ms)"
  ) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

rt_session_all_file <- file.path(fig_dir, "subAll_RTSessionVln_AllDev.png")
ggsave(filename = rt_session_all_file, plot = rt_session_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

acc_session_all_df <- plot_df %>%
  group_by(SubjectFacet, SessionNum) %>%
  summarize(
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    AccuracySD = sd(ACC_num, na.rm = TRUE),
    AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
    .groups = "drop"
  )

acc_session_global_ymin <- max(0, min(acc_session_all_df$Accuracy, na.rm = TRUE) - 0.08)

acc_session_plot_all <- ggplot(acc_session_all_df, aes(x = SessionNum, y = Accuracy, group = 1)) +
  geom_ribbon(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    fill = "#59A14F",
    alpha = 0.18
  ) +
  geom_line(color = "#59A14F", linewidth = 1.1) +
  geom_point(color = "#59A14F", size = 2.4) +
  geom_errorbar(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    width = 0.15,
    linewidth = 0.7,
    color = "#2F6B34"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Accuracy by Session (All Subjects)",
    subtitle = "Facets ordered by device groups (KB first)",
    x = "Session",
    y = "Accuracy"
  ) +
  scale_y_continuous(limits = c(acc_session_global_ymin, 1), expand = expansion(mult = c(0, 0.06))) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

acc_session_all_file <- file.path(fig_dir, "subAll_ACCSession_AllDev.png")
ggsave(filename = acc_session_all_file, plot = acc_session_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

key_levels <- sort(unique(plot_df$ResponseKey[!is.na(plot_df$ResponseKey) & plot_df$ResponseKey != ""]))

rt_key_all_df <- rt_plot_df %>%
  filter(!is.na(ResponseKey), ResponseKey != "") %>%
  group_by(SubjectFacet, ResponseKey) %>%
  summarize(
    MeanRT = mean(RT_num, na.rm = TRUE),
    Trials = n(),
    RTSD = sd(RT_num, na.rm = TRUE),
    RTSE = if_else(Trials > 1, RTSD / sqrt(Trials), 0),
    .groups = "drop"
  ) %>%
  mutate(ResponseKey = factor(ResponseKey, levels = key_levels))

rt_key_plot_all <- ggplot(
  rt_plot_df %>%
    filter(!is.na(ResponseKey), ResponseKey != "") %>%
    mutate(ResponseKey = factor(ResponseKey, levels = key_levels)),
  aes(x = ResponseKey, y = RT_num)
) +
  geom_violin(fill = "#B07AA1", alpha = 0.6, color = "gray25", trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.size = 0.45, alpha = 0.25, color = "black") +
  geom_ribbon(
    data = rt_key_all_df,
    aes(
      x = ResponseKey,
      y = MeanRT,
      group = 1,
      ymin = pmax(0, MeanRT - RTSE),
      ymax = pmin(3000, MeanRT + RTSE)
    ),
    inherit.aes = FALSE,
    fill = "red3",
    alpha = 0.16
  ) +
  geom_line(
    data = rt_key_all_df,
    aes(x = ResponseKey, y = MeanRT, group = 1),
    color = "red3",
    linewidth = 1.5,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = rt_key_all_df,
    aes(x = ResponseKey, y = MeanRT),
    color = "red3",
    size = 2.8,
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    data = rt_key_all_df,
    aes(
      x = ResponseKey,
      y = MeanRT,
      group = 1,
      ymin = pmax(0, MeanRT - RTSE),
      ymax = pmin(3000, MeanRT + RTSE)
    ),
    inherit.aes = FALSE,
    width = 0.15,
    linewidth = 0.75,
    color = "#A81F1F"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "RT by Response Key (All Subjects)",
    subtitle = "Facets ordered by device groups (KB first)",
    x = "Response Key",
    y = "Reaction Time (ms)"
  ) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

rt_key_all_file <- file.path(fig_dir, "subAll_RTKey_AllDev.png")
ggsave(filename = rt_key_all_file, plot = rt_key_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

acc_key_all_df <- plot_df %>%
  filter(!is.na(ResponseKey), ResponseKey != "") %>%
  group_by(SubjectFacet, ResponseKey) %>%
  summarize(
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    AccuracySD = sd(ACC_num, na.rm = TRUE),
    AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
    .groups = "drop"
  ) %>%
  mutate(ResponseKey = factor(ResponseKey, levels = key_levels))

acc_key_global_ymin <- max(0, min(acc_key_all_df$Accuracy, na.rm = TRUE) - 0.08)

acc_key_plot_all <- ggplot(acc_key_all_df, aes(x = ResponseKey, y = Accuracy, group = 1)) +
  geom_ribbon(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    fill = "#59A14F",
    alpha = 0.18
  ) +
  geom_line(color = "#59A14F", linewidth = 1.1) +
  geom_point(color = "#59A14F", size = 2.4) +
  geom_errorbar(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    width = 0.15,
    linewidth = 0.7,
    color = "#2F6B34"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Accuracy by Response Key (All Subjects)",
    subtitle = "Facets ordered by device groups (KB first)",
    x = "Response Key",
    y = "Accuracy"
  ) +
  scale_y_continuous(limits = c(acc_key_global_ymin, 1), expand = expansion(mult = c(0, 0.06))) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

acc_key_all_file <- file.path(fig_dir, "subAll_ACCKey_AllDev.png")
ggsave(filename = acc_key_all_file, plot = acc_key_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

rt_key_g2_df <- rt_plot_df %>%
  filter(SessionHalfGroup == "group2", !is.na(ResponseKey), ResponseKey != "") %>%
  mutate(ResponseKey = factor(ResponseKey, levels = key_levels))

rt_key_g2_summary <- rt_key_g2_df %>%
  group_by(SubjectFacet, ResponseKey) %>%
  summarize(
    MeanRT = mean(RT_num, na.rm = TRUE),
    Trials = n(),
    RTSD = sd(RT_num, na.rm = TRUE),
    RTSE = if_else(Trials > 1, RTSD / sqrt(Trials), 0),
    .groups = "drop"
  )

rt_key_g2_plot_all <- ggplot(rt_key_g2_df, aes(x = ResponseKey, y = RT_num)) +
  geom_violin(fill = "#B07AA1", alpha = 0.6, color = "gray25", trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.size = 0.45, alpha = 0.25, color = "black") +
  geom_ribbon(
    data = rt_key_g2_summary,
    aes(
      x = ResponseKey,
      y = MeanRT,
      group = 1,
      ymin = pmax(0, MeanRT - RTSE),
      ymax = pmin(3000, MeanRT + RTSE)
    ),
    inherit.aes = FALSE,
    fill = "red3",
    alpha = 0.16
  ) +
  geom_line(data = rt_key_g2_summary, aes(x = ResponseKey, y = MeanRT, group = 1), color = "red3", linewidth = 1.5, inherit.aes = FALSE) +
  geom_point(data = rt_key_g2_summary, aes(x = ResponseKey, y = MeanRT), color = "red3", size = 2.8, inherit.aes = FALSE) +
  geom_errorbar(
    data = rt_key_g2_summary,
    aes(
      x = ResponseKey,
      y = MeanRT,
      group = 1,
      ymin = pmax(0, MeanRT - RTSE),
      ymax = pmin(3000, MeanRT + RTSE)
    ),
    inherit.aes = FALSE,
    width = 0.15,
    linewidth = 0.75,
    color = "#A81F1F"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "RT by Response Key (Group 2 Sessions, All Subjects)",
    subtitle = "Group 2 = sessions 12-17; facets ordered by device groups (KB first)",
    x = "Response Key",
    y = "Reaction Time (ms)"
  ) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

rt_key_g2_all_file <- file.path(fig_dir, "subAll_RTKey_G2_AllDev.png")
ggsave(filename = rt_key_g2_all_file, plot = rt_key_g2_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

acc_key_g2_df <- plot_df %>%
  filter(SessionHalfGroup == "group2", !is.na(ResponseKey), ResponseKey != "") %>%
  group_by(SubjectFacet, ResponseKey) %>%
  summarize(
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    AccuracySD = sd(ACC_num, na.rm = TRUE),
    AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
    .groups = "drop"
  ) %>%
  mutate(ResponseKey = factor(ResponseKey, levels = key_levels))

acc_key_g2_ymin <- max(0, min(acc_key_g2_df$Accuracy, na.rm = TRUE) - 0.08)

acc_key_g2_plot_all <- ggplot(acc_key_g2_df, aes(x = ResponseKey, y = Accuracy, group = 1)) +
  geom_ribbon(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    fill = "#59A14F",
    alpha = 0.18
  ) +
  geom_line(color = "#59A14F", linewidth = 1.1) +
  geom_point(color = "#59A14F", size = 2.4) +
  geom_errorbar(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    width = 0.15,
    linewidth = 0.7,
    color = "#2F6B34"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Accuracy by Response Key (Group 2 Sessions, All Subjects)",
    subtitle = "Group 2 = sessions 12-17; facets ordered by device groups (KB first)",
    x = "Response Key",
    y = "Accuracy"
  ) +
  scale_y_continuous(limits = c(acc_key_g2_ymin, 1), expand = expansion(mult = c(0, 0.06))) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

acc_key_g2_all_file <- file.path(fig_dir, "subAll_ACCKey_G2_AllDev.png")
ggsave(filename = acc_key_g2_all_file, plot = acc_key_g2_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

color_levels <- c("red", "green", "blue", "yellow")

rt_color_all_df <- rt_plot_df %>%
  filter(!is.na(HighestRewardColor), HighestRewardColor %in% color_levels) %>%
  group_by(SubjectFacet, HighestRewardColor) %>%
  summarize(
    MeanRT = mean(RT_num, na.rm = TRUE),
    Trials = n(),
    RTSD = sd(RT_num, na.rm = TRUE),
    RTSE = if_else(Trials > 1, RTSD / sqrt(Trials), 0),
    .groups = "drop"
  ) %>%
  mutate(HighestRewardColor = factor(HighestRewardColor, levels = color_levels))

rt_color_plot_all <- ggplot(
  rt_plot_df %>%
    filter(!is.na(HighestRewardColor), HighestRewardColor %in% color_levels) %>%
    mutate(HighestRewardColor = factor(HighestRewardColor, levels = color_levels)),
  aes(x = HighestRewardColor, y = RT_num)
) +
  geom_violin(fill = "#76B7B2", alpha = 0.6, color = "gray25", trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.size = 0.45, alpha = 0.25, color = "black") +
  geom_ribbon(
    data = rt_color_all_df,
    aes(
      x = HighestRewardColor,
      y = MeanRT,
      group = 1,
      ymin = pmax(0, MeanRT - RTSE),
      ymax = pmin(3000, MeanRT + RTSE)
    ),
    inherit.aes = FALSE,
    fill = "red3",
    alpha = 0.16
  ) +
  geom_line(
    data = rt_color_all_df,
    aes(x = HighestRewardColor, y = MeanRT, group = 1),
    color = "red3",
    linewidth = 1.5,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = rt_color_all_df,
    aes(x = HighestRewardColor, y = MeanRT),
    color = "red3",
    size = 2.8,
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    data = rt_color_all_df,
    aes(
      x = HighestRewardColor,
      y = MeanRT,
      group = 1,
      ymin = pmax(0, MeanRT - RTSE),
      ymax = pmin(3000, MeanRT + RTSE)
    ),
    inherit.aes = FALSE,
    width = 0.15,
    linewidth = 0.75,
    color = "#A81F1F"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "RT by Highest-Reward Color (All Subjects)",
    subtitle = "Facets ordered by device groups (KB first)",
    x = "Highest-Reward Color",
    y = "Reaction Time (ms)"
  ) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

rt_color_all_file <- file.path(fig_dir, "subAll_RTTopColor_AllDev.png")
ggsave(filename = rt_color_all_file, plot = rt_color_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

acc_color_all_df <- plot_df %>%
  filter(!is.na(HighestRewardColor), HighestRewardColor %in% color_levels) %>%
  group_by(SubjectFacet, HighestRewardColor) %>%
  summarize(
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    AccuracySD = sd(ACC_num, na.rm = TRUE),
    AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
    .groups = "drop"
  ) %>%
  mutate(HighestRewardColor = factor(HighestRewardColor, levels = color_levels))

acc_color_global_ymin <- max(0, min(acc_color_all_df$Accuracy, na.rm = TRUE) - 0.08)

acc_color_plot_all <- ggplot(acc_color_all_df, aes(x = HighestRewardColor, y = Accuracy, group = 1)) +
  geom_ribbon(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    fill = "#59A14F",
    alpha = 0.18
  ) +
  geom_line(color = "#59A14F", linewidth = 1.1) +
  geom_point(color = "#59A14F", size = 2.4) +
  geom_errorbar(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    width = 0.15,
    linewidth = 0.7,
    color = "#2F6B34"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Accuracy by Highest-Reward Color (All Subjects)",
    subtitle = "Facets ordered by device groups (KB first)",
    x = "Highest-Reward Color",
    y = "Accuracy"
  ) +
  scale_y_continuous(limits = c(acc_color_global_ymin, 1), expand = expansion(mult = c(0, 0.06))) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

acc_color_all_file <- file.path(fig_dir, "subAll_ACCTopColor_AllDev.png")
ggsave(filename = acc_color_all_file, plot = acc_color_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

rt_color_g2_df <- rt_plot_df %>%
  filter(SessionHalfGroup == "group2", !is.na(HighestRewardColor), HighestRewardColor %in% color_levels) %>%
  mutate(HighestRewardColor = factor(HighestRewardColor, levels = color_levels))

rt_color_g2_summary <- rt_color_g2_df %>%
  group_by(SubjectFacet, HighestRewardColor) %>%
  summarize(
    MeanRT = mean(RT_num, na.rm = TRUE),
    Trials = n(),
    RTSD = sd(RT_num, na.rm = TRUE),
    RTSE = if_else(Trials > 1, RTSD / sqrt(Trials), 0),
    .groups = "drop"
  )

rt_color_g2_plot_all <- ggplot(rt_color_g2_df, aes(x = HighestRewardColor, y = RT_num)) +
  geom_violin(fill = "#76B7B2", alpha = 0.6, color = "gray25", trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.size = 0.45, alpha = 0.25, color = "black") +
  geom_ribbon(
    data = rt_color_g2_summary,
    aes(
      x = HighestRewardColor,
      y = MeanRT,
      group = 1,
      ymin = pmax(0, MeanRT - RTSE),
      ymax = pmin(3000, MeanRT + RTSE)
    ),
    inherit.aes = FALSE,
    fill = "red3",
    alpha = 0.16
  ) +
  geom_line(
    data = rt_color_g2_summary,
    aes(x = HighestRewardColor, y = MeanRT, group = 1),
    color = "red3",
    linewidth = 1.5,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = rt_color_g2_summary,
    aes(x = HighestRewardColor, y = MeanRT),
    color = "red3",
    size = 2.8,
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    data = rt_color_g2_summary,
    aes(
      x = HighestRewardColor,
      y = MeanRT,
      group = 1,
      ymin = pmax(0, MeanRT - RTSE),
      ymax = pmin(3000, MeanRT + RTSE)
    ),
    inherit.aes = FALSE,
    width = 0.15,
    linewidth = 0.75,
    color = "#A81F1F"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "RT by Highest-Reward Color (Group 2 Sessions, All Subjects)",
    subtitle = "Group 2 = sessions 12-17; facets ordered by device groups (KB first)",
    x = "Highest-Reward Color",
    y = "Reaction Time (ms)"
  ) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

rt_color_g2_all_file <- file.path(fig_dir, "subAll_RTTopColor_G2_AllDev.png")
ggsave(filename = rt_color_g2_all_file, plot = rt_color_g2_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

acc_color_g2_df <- plot_df %>%
  filter(SessionHalfGroup == "group2", !is.na(HighestRewardColor), HighestRewardColor %in% color_levels) %>%
  group_by(SubjectFacet, HighestRewardColor) %>%
  summarize(
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    AccuracySD = sd(ACC_num, na.rm = TRUE),
    AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
    .groups = "drop"
  ) %>%
  mutate(HighestRewardColor = factor(HighestRewardColor, levels = color_levels))

acc_color_g2_ymin <- max(0, min(acc_color_g2_df$Accuracy, na.rm = TRUE) - 0.08)

acc_color_g2_plot_all <- ggplot(acc_color_g2_df, aes(x = HighestRewardColor, y = Accuracy, group = 1)) +
  geom_ribbon(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    fill = "#59A14F",
    alpha = 0.18
  ) +
  geom_line(color = "#59A14F", linewidth = 1.1) +
  geom_point(color = "#59A14F", size = 2.4) +
  geom_errorbar(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    width = 0.15,
    linewidth = 0.7,
    color = "#2F6B34"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Accuracy by Highest-Reward Color (Group 2 Sessions, All Subjects)",
    subtitle = "Group 2 = sessions 12-17; facets ordered by device groups (KB first)",
    x = "Highest-Reward Color",
    y = "Accuracy"
  ) +
  scale_y_continuous(limits = c(acc_color_g2_ymin, 1), expand = expansion(mult = c(0, 0.06))) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

acc_color_g2_all_file <- file.path(fig_dir, "subAll_ACCTopColor_G2_AllDev.png")
ggsave(filename = acc_color_g2_all_file, plot = acc_color_g2_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

tradeoff_df <- rt_plot_df %>%
  group_by(SubjectFacet, Condition) %>%
  summarize(
    MeanRT = mean(RT_num, na.rm = TRUE),
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    CueCount = dplyr::first(CueCount),
    RewardDiff = dplyr::first(RewardDiff),
    .groups = "drop"
  )

tradeoff_df <- tradeoff_df %>%
  mutate(
    ConditionLabel = paste0("(", as.character(Condition), ")"),
    SingleValue = suppressWarnings(as.integer(as.character(Condition))),
    TradeoffColor = case_when(
      CueCount == "1-cue" & SingleValue == 1 ~ "#08519C",
      CueCount == "1-cue" & SingleValue == 2 ~ "#3182BD",
      CueCount == "1-cue" & SingleValue == 3 ~ "#6BAED6",
      CueCount == "1-cue" & SingleValue == 4 ~ "#BDD7E7",
      CueCount == "2-cue" & as.integer(as.character(RewardDiff)) == 1 ~ "#A50F15",
      CueCount == "2-cue" & as.integer(as.character(RewardDiff)) == 2 ~ "#DE2D26",
      CueCount == "2-cue" & as.integer(as.character(RewardDiff)) == 3 ~ "#FCAE91",
      TRUE ~ "#777777"
    )
  )

tradeoff_ymin <- max(0, min(tradeoff_df$Accuracy, na.rm = TRUE) - 0.06)
tradeoff_ymax <- min(1, max(tradeoff_df$Accuracy, na.rm = TRUE) + 0.02)

tradeoff_plot_all <- ggplot(tradeoff_df, aes(x = MeanRT, y = Accuracy, group = 1)) +
  geom_point(aes(color = TradeoffColor), size = 6.2) +
  geom_text(aes(label = ConditionLabel, color = TradeoffColor), vjust = -1.0, size = 10.5, fontface = "bold") +
  scale_color_identity() +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Speed-Accuracy Tradeoff by Condition (All Subjects)",
    subtitle = "Each point is one condition; labels are condition names",
    x = "Mean RT (ms)",
    y = "Accuracy"
  ) +
  scale_y_continuous(limits = c(tradeoff_ymin, tradeoff_ymax), expand = expansion(mult = c(0, 0.06))) +
  coord_cartesian(xlim = c(500, 1500)) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size + 4, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size + 2),
    axis.title = element_text(size = axis_title_size + 2, face = "bold"),
    axis.text = element_text(size = axis_text_size + 2, face = "bold"),
    strip.text = element_text(size = strip_text_size + 2, face = "bold")
  )

tradeoff_all_file <- file.path(fig_dir, "subAll_SpeedAccuracyTradeoff_AllDev.png")
ggsave(filename = tradeoff_all_file, plot = tradeoff_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

tradeoff_plot_relative_all <- ggplot(tradeoff_df, aes(x = MeanRT, y = Accuracy, group = 1)) +
  geom_point(aes(color = TradeoffColor), size = 6.2) +
  geom_text(aes(label = ConditionLabel, color = TradeoffColor), vjust = -1.0, size = 10.5, fontface = "bold") +
  scale_color_identity() +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "free") +
  labs(
    title = "Speed-Accuracy Tradeoff by Condition (Relative Scales)",
    subtitle = "Per-subject axes are auto-scaled to improve within-subject visibility",
    x = "Mean RT (ms)",
    y = "Accuracy"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.08))) +
  scale_y_continuous(expand = expansion(mult = c(0.06, 0.20))) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size + 4, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size + 2),
    axis.title = element_text(size = axis_title_size + 2, face = "bold"),
    axis.text = element_text(size = axis_text_size + 2, face = "bold"),
    strip.text = element_text(size = strip_text_size + 2, face = "bold")
  )

tradeoff_relative_all_file <- file.path(fig_dir, "subAll_SpeedAccuracyTradeoff_RelativeScale_AllDev.png")
ggsave(filename = tradeoff_relative_all_file, plot = tradeoff_plot_relative_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

tradeoff_qcount_df <- rt_plot_df %>%
  group_by(SubjectFacet, CueCount) %>%
  summarize(
    MeanRT = mean(RT_num, na.rm = TRUE),
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    .groups = "drop"
  ) %>%
  mutate(CueCount = factor(CueCount, levels = c("1-cue", "2-cue")))

tradeoff_qcount_ymin <- max(0, min(tradeoff_qcount_df$Accuracy, na.rm = TRUE) - 0.06)
tradeoff_qcount_ymax <- min(1, max(tradeoff_qcount_df$Accuracy, na.rm = TRUE) + 0.02)

tradeoff_qcount_plot_all <- ggplot(tradeoff_qcount_df, aes(x = MeanRT, y = Accuracy, group = 1)) +
  geom_path(color = "#6B6B6B", linewidth = 1.8, alpha = 0.9) +
  geom_point(aes(color = CueCount), size = 6.2) +
  geom_text(aes(label = as.character(CueCount), color = CueCount), vjust = -1.0, size = 8.2, fontface = "bold") +
  scale_color_manual(values = c("1-cue" = "#1F77B4", "2-cue" = "#D62728")) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Speed-Accuracy Tradeoff by Cue Number (All Subjects)",
    subtitle = "Two points per subject: 1-cue vs 2-cue",
    x = "Mean RT (ms)",
    y = "Accuracy"
  ) +
  scale_y_continuous(limits = c(tradeoff_qcount_ymin, tradeoff_qcount_ymax), expand = expansion(mult = c(0, 0.06))) +
  coord_cartesian(xlim = c(500, 1500)) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size + 4, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size + 2),
    axis.title = element_text(size = axis_title_size + 2, face = "bold"),
    axis.text = element_text(size = axis_text_size + 2, face = "bold"),
    strip.text = element_text(size = strip_text_size + 2, face = "bold")
  )

tradeoff_qcount_all_file <- file.path(fig_dir, "subAll_SpeedAccuracyTradeoff_QCount_AllDev.png")
ggsave(filename = tradeoff_qcount_all_file, plot = tradeoff_qcount_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

caf_n_bins <- 5
caf_df <- rt_plot_df %>%
  filter(!is.na(RT_num), !is.na(ACC_num)) %>%
  group_by(SubjectFacet) %>%
  mutate(RTBin = ntile(RT_num, caf_n_bins)) %>%
  ungroup() %>%
  group_by(SubjectFacet, RTBin) %>%
  summarize(
    MeanRT = mean(RT_num, na.rm = TRUE),
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    AccuracySD = sd(ACC_num, na.rm = TRUE),
    AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
    .groups = "drop"
  ) %>%
  mutate(RTBinLabel = factor(paste0("Q", RTBin), levels = paste0("Q", 1:caf_n_bins)))

caf_plot_all <- ggplot(caf_df, aes(x = RTBinLabel, y = Accuracy, group = 1)) +
  geom_ribbon(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    fill = "#59A14F",
    alpha = 0.18
  ) +
  geom_line(color = "#59A14F", linewidth = 1.2) +
  geom_point(color = "#59A14F", size = 2.6) +
  geom_errorbar(
    aes(
      ymin = pmax(0, Accuracy - AccuracySE),
      ymax = pmin(1, Accuracy + AccuracySE)
    ),
    width = 0.15,
    linewidth = 0.7,
    color = "#2F6B34"
  ) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Conditional Accuracy Function (CAF) (All Subjects)",
    subtitle = "RT quantiles: Q1 fastest to Q5 slowest",
    x = "RT Quantile Bin",
    y = "Accuracy"
  ) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.08))) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

caf_all_file <- file.path(fig_dir, "subAll_CAF_AllDev.png")
ggsave(filename = caf_all_file, plot = caf_plot_all + bold_axes_theme, width = 20, height = 24, dpi = 300)

interference_df <- rt_plot_df %>%
  filter(
    !is.na(ResponseKey), ResponseKey != "",
    !is.na(HighestRewardColor), HighestRewardColor %in% color_levels
  ) %>%
  mutate(
    ResponseKey = factor(ResponseKey, levels = key_levels),
    HighestRewardColor = factor(HighestRewardColor, levels = color_levels)
  ) %>%
  group_by(SubjectFacet, ResponseKey, HighestRewardColor) %>%
  summarize(
    MeanRT = mean(RT_num, na.rm = TRUE),
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    .groups = "drop"
  )

interference_rt_plot <- ggplot(interference_df, aes(x = ResponseKey, y = HighestRewardColor, fill = MeanRT)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.0f", MeanRT)), size = 4.2, color = "black", fontface = "bold") +
  facet_wrap(~SubjectFacet, ncol = 2) +
  scale_fill_gradient(low = "#E0F3F8", high = "#D7301F") +
  labs(
    title = "Interference Heatmap: RT by Response Key x Highest-Reward Color",
    subtitle = "Cell text shows mean RT (ms)",
    x = "Response Key",
    y = "Highest-Reward Color",
    fill = "Mean RT"
  ) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold"),
    panel.grid = element_blank()
  )

interference_rt_file <- file.path(fig_dir, "subAll_InterferenceRTHeatmap_AllDev.png")
ggsave(filename = interference_rt_file, plot = interference_rt_plot + bold_axes_theme, width = 20, height = 24, dpi = 300)

interference_acc_plot <- ggplot(interference_df, aes(x = ResponseKey, y = HighestRewardColor, fill = Accuracy)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Accuracy)), size = 4.2, color = "black", fontface = "bold") +
  facet_wrap(~SubjectFacet, ncol = 2) +
  scale_fill_gradient(low = "#F0F9E8", high = "#238443", limits = c(0, 1)) +
  labs(
    title = "Interference Heatmap: Accuracy by Response Key x Highest-Reward Color",
    subtitle = "Cell text shows mean accuracy",
    x = "Response Key",
    y = "Highest-Reward Color",
    fill = "Accuracy"
  ) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold"),
    panel.grid = element_blank()
  )

interference_acc_file <- file.path(fig_dir, "subAll_InterferenceACCHeatmap_AllDev.png")
ggsave(filename = interference_acc_file, plot = interference_acc_plot + bold_axes_theme, width = 20, height = 24, dpi = 300)

rtdiff_qc_df <- plot_df %>%
  filter(DeviceLabel != "KB", !is.na(RTDifference_num), !is.na(SubjectFacet))

rtdiff_by_session_df <- rtdiff_qc_df %>%
  group_by(SubjectFacet, SessionNum) %>%
  summarize(
    MeanRTDiff = mean(RTDifference_num, na.rm = TRUE),
    .groups = "drop"
  )

rtdiff_session_plot <- ggplot(rtdiff_by_session_df, aes(x = SessionNum, y = MeanRTDiff, group = 1)) +
  geom_line(color = "#7B3294", linewidth = 1.2) +
  geom_point(color = "#7B3294", size = 2.4) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Device Timing QC: RTDifference by Session (No KB)",
    x = "Session",
    y = "Mean RTDifference (ms)"
  ) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

rtdiff_session_file <- file.path(fig_dir, "subAll_RTDiffTrendSession_NoKB.png")
ggsave(filename = rtdiff_session_file, plot = rtdiff_session_plot + bold_axes_theme, width = 20, height = 24, dpi = 300)

rtdiff_by_day_df <- rtdiff_qc_df %>%
  group_by(SubjectFacet, DayLabel) %>%
  summarize(
    MeanRTDiff = mean(RTDifference_num, na.rm = TRUE),
    .groups = "drop"
  )

rtdiff_day_plot <- ggplot(rtdiff_by_day_df, aes(x = DayLabel, y = MeanRTDiff, group = 1)) +
  geom_line(color = "#7B3294", linewidth = 1.2) +
  geom_point(color = "#7B3294", size = 2.4) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Device Timing QC: RTDifference by Day (No KB)",
    x = "Day",
    y = "Mean RTDifference (ms)"
  ) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

rtdiff_day_file <- file.path(fig_dir, "subAll_RTDiffTrendDay_NoKB.png")
ggsave(filename = rtdiff_day_file, plot = rtdiff_day_plot + bold_axes_theme, width = 20, height = 24, dpi = 300)

rtdiff_by_trial_df <- rtdiff_qc_df %>%
  filter(!is.na(TrialNumInt)) %>%
  group_by(SubjectFacet, TrialNumInt) %>%
  summarize(
    MeanRTDiff = mean(RTDifference_num, na.rm = TRUE),
    .groups = "drop"
  )

rtdiff_trial_plot <- ggplot(rtdiff_by_trial_df, aes(x = TrialNumInt, y = MeanRTDiff)) +
  geom_line(color = "#7B3294", linewidth = 0.9, alpha = 0.85) +
  geom_smooth(method = "loess", se = FALSE, color = "#D01C8B", linewidth = 1.1, span = 0.3) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = "Device Timing QC: RTDifference by Trial Index (No KB)",
    subtitle = "Per-trial mean trend with LOESS smooth",
    x = "Trial Index",
    y = "Mean RTDifference (ms)"
  ) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

rtdiff_trial_file <- file.path(fig_dir, "subAll_RTDiffTrendTrial_NoKB.png")
ggsave(filename = rtdiff_trial_file, plot = rtdiff_trial_plot + bold_axes_theme, width = 20, height = 24, dpi = 300)

rt_sesday_plot_list <- lapply(subject_facet_levels, function(subject_facet_name) {
  subject_plot_df <- rt_plot_df %>%
    filter(as.character(SubjectFacet) == subject_facet_name)

  ggplot(subject_plot_df, aes(x = SessionOrderLabel, y = RT_num)) +
    geom_violin(fill = "#76B7B2", alpha = 0.7, color = "gray25", trim = FALSE) +
    geom_boxplot(width = 0.12, outlier.size = 0.45, alpha = 0.25, color = "black") +
    stat_summary(aes(group = 1), fun = mean, geom = "line", color = "red3", linewidth = 2.2) +
    stat_summary(fun = mean, geom = "point", color = "red3", size = 3.4) +
    scale_x_discrete(drop = FALSE) +
    facet_wrap(~DayLabel, ncol = 1, scales = "fixed") +
    labs(
      title = paste0(subject_facet_name, " RT by Session-Order Within Day"),
      x = "Session in Day",
      y = "Reaction Time (ms)"
    ) +
    coord_cartesian(ylim = c(0, 3000)) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(size = axis_title_size, face = "bold"),
      axis.title = element_text(size = axis_title_size, face = "bold"),
      axis.text = element_text(size = axis_text_size, face = "bold"),
      strip.text = element_text(size = axis_text_size, face = "bold")
    )
})

rt_sesday_all <- do.call(gridExtra::arrangeGrob, c(rt_sesday_plot_list, ncol = 2))
rt_sesday_all_file <- file.path(fig_dir, "subAll_RTSesDayVln_AllDev.png")
ggsave(filename = rt_sesday_all_file, plot = rt_sesday_all, width = 24, height = 36, dpi = 300)

acc_sesday_plot_list <- lapply(subject_facet_levels, function(subject_facet_name) {
  subject_acc_df <- plot_df %>%
    filter(as.character(SubjectFacet) == subject_facet_name) %>%
    group_by(DayLabel, SessionOrderInDay, SessionOrderLabel) %>%
    summarize(
      Accuracy = mean(ACC_num, na.rm = TRUE),
      Trials = n(),
      AccuracySD = sd(ACC_num, na.rm = TRUE),
      AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
      .groups = "drop"
    )

  subject_acc_ymin <- max(0, min(subject_acc_df$Accuracy, na.rm = TRUE) - 0.08)

  ggplot(subject_acc_df, aes(x = SessionOrderLabel, y = Accuracy, group = 1)) +
    geom_ribbon(
      aes(
        ymin = pmax(0, Accuracy - AccuracySE),
        ymax = pmin(1, Accuracy + AccuracySE)
      ),
      fill = "#59A14F",
      alpha = 0.18
    ) +
    geom_line(color = "#59A14F", linewidth = 1.1) +
    geom_point(color = "#59A14F", size = 2.4) +
    geom_errorbar(
      aes(
        ymin = pmax(0, Accuracy - AccuracySE),
        ymax = pmin(1, Accuracy + AccuracySE)
      ),
      width = 0.15,
      linewidth = 0.7,
      color = "#2F6B34"
    ) +
    scale_x_discrete(drop = FALSE) +
    facet_wrap(~DayLabel, ncol = 1, scales = "fixed") +
    labs(
      title = paste0(subject_facet_name, " Accuracy by Session-Order Within Day"),
      x = "Session in Day",
      y = "Accuracy"
    ) +
    scale_y_continuous(limits = c(subject_acc_ymin, 1), expand = expansion(mult = c(0, 0.06))) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(size = axis_title_size, face = "bold"),
      axis.title = element_text(size = axis_title_size, face = "bold"),
      axis.text = element_text(size = axis_text_size, face = "bold"),
      strip.text = element_text(size = axis_text_size, face = "bold")
    )
})

acc_sesday_all <- do.call(gridExtra::arrangeGrob, c(acc_sesday_plot_list, ncol = 2))
acc_sesday_all_file <- file.path(fig_dir, "subAll_ACCSesDay_AllDev.png")
ggsave(filename = acc_sesday_all_file, plot = acc_sesday_all, width = 24, height = 36, dpi = 300)

plot_groups <- plot_df %>%
  distinct(SubjectNum, DeviceLabel) %>%
  arrange(SubjectNum, DeviceLabel)

for (i in seq_len(nrow(plot_groups))) {
  subject_num <- as.character(plot_groups$SubjectNum[i])
  device_label <- plot_groups$DeviceLabel[i]

  subject_plot_df <- plot_df %>%
    filter(SubjectNum == subject_num, DeviceLabel == device_label)

  subject_rt_plot_df <- rt_plot_df %>%
    filter(SubjectNum == subject_num, DeviceLabel == device_label)

  if (nrow(subject_plot_df) == 0 || nrow(subject_rt_plot_df) == 0) next

  acc_session_df <- subject_plot_df %>%
    group_by(SessionNum) %>%
    summarize(
      Accuracy = mean(ACC_num, na.rm = TRUE),
      Trials = n(),
      AccuracySD = sd(ACC_num, na.rm = TRUE),
      AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
      .groups = "drop"
    )

  acc_session_ymin <- max(0, min(acc_session_df$Accuracy, na.rm = TRUE) - 0.08)

  rt_session_violin_plot <- ggplot(subject_rt_plot_df, aes(x = SessionNum, y = RT_num)) +
    geom_violin(fill = "#F28E2B", alpha = 0.65, color = "gray25", trim = FALSE) +
    geom_boxplot(width = 0.12, outlier.size = 0.5, alpha = 0.25, color = "black") +
    stat_summary(aes(group = 1), fun = mean, geom = "line", color = "red3", linewidth = 1.1) +
    stat_summary(fun = mean, geom = "point", color = "red3", size = 2.4) +
    labs(
      title = paste0("Subject ", subject_num, " RT Violin by Session (", device_label, ")"),
      subtitle = "Sessions 6-17",
      x = "Session",
      y = "Reaction Time (ms)"
    ) +
    coord_cartesian(ylim = c(0, 3000)) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = subtitle_size),
      axis.title = element_text(size = axis_title_size, face = "bold"),
      axis.text = element_text(size = axis_text_size, face = "bold")
    )

  acc_session_plot <- ggplot(acc_session_df, aes(x = SessionNum, y = Accuracy, group = 1)) +
    geom_ribbon(
      aes(
        ymin = pmax(0, Accuracy - AccuracySE),
        ymax = pmin(1, Accuracy + AccuracySE)
      ),
      fill = "#59A14F",
      alpha = 0.18
    ) +
    geom_line(color = "#59A14F", linewidth = 1.1) +
    geom_point(color = "#59A14F", size = 2.3) +
    geom_errorbar(
      aes(
        ymin = pmax(0, Accuracy - AccuracySE),
        ymax = pmin(1, Accuracy + AccuracySE)
      ),
      width = 0.15,
      linewidth = 0.7,
      color = "#2F6B34"
    ) +
    geom_text(aes(label = sprintf("n=%d", Trials)), vjust = -0.6, size = 3.2) +
    labs(
      title = paste0("Subject ", subject_num, " Accuracy by Session (", device_label, ")"),
      subtitle = "Sessions 6-17",
      x = "Session",
      y = "Accuracy"
    ) +
    scale_y_continuous(limits = c(acc_session_ymin, 1), expand = expansion(mult = c(0, 0.08))) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = subtitle_size),
      axis.title = element_text(size = axis_title_size, face = "bold"),
      axis.text = element_text(size = axis_text_size, face = "bold")
    )

  session_combined <- gridExtra::grid.arrange(rt_session_violin_plot, acc_session_plot, ncol = 1, nrow = 2)

  session_file <- file.path(
    fig_dir,
    paste0("sub", subject_num, "_RTACCSession_", device_label, ".png")
  )
  ggsave(filename = session_file, plot = session_combined, width = 12, height = 14, dpi = 300)

  message("Saved plots for sub", subject_num, " (", device_label, ").")
}
