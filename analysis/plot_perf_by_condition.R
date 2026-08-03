# Performance (RT + accuracy) by cue condition, per subject.
# Reads trial CSVs from exp/data_from_lab/sub{N}/ only.
#
# Run from multiplecue-responsebox/ or analysis/:
#   Rscript analysis/plot_perf_by_condition.R
#   Rscript plot_perf_by_condition.R

library(dplyr)
library(ggplot2)

# ---- CONFIG ----
subject_range <- 9:(9+11)          # participant IDs to plot, e.g. 4:7 or c(4, 6, 12)
session_range <- 6:17         # experimental sessions (1-5 = practice)
rt_min_ms <- 0
rt_max_ms <- 4000
rt_plot_ylim_ms <- 3000
exclude_timeouts <- TRUE      # drop Response containing "timeout" / empty / "none"
exclude_warmup <- TRUE        # drop WarmUpTrial == 1
exclude_burnin <- TRUE        # drop BurnInBlock == 1 (and Block == 0 when flagged)
# sub6 device timing QC used in the full analysis script
exclude_sub6_rtdiff_gt_ms <- 500
# ----------------

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
  axis.text.y = element_text(face = "bold")
)

# Resolve project root so the script works from either cwd.
if (dir.exists(file.path("exp", "data_from_lab"))) {
  proj_root <- normalizePath(".")
} else if (dir.exists(file.path("..", "exp", "data_from_lab"))) {
  proj_root <- normalizePath("..")
} else {
  stop("Cannot find exp/data_from_lab. Run from multiplecue-responsebox/ or analysis/.")
}

data_dir <- file.path(proj_root, "exp", "data_from_lab")
fig_dir <- file.path(proj_root, "analysis", "fig", "fig_aug2")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

subject_ids <- sort(unique(as.integer(subject_range)))
session_ids <- sort(unique(as.integer(session_range)))

# Match current lab naming, including retries and multi-computer tags:
#   CCRP_subj12_ses6_trials.csv
#   CCRP_subj19_ses11(2)_trials.csv
#   CCRP_subj7_ses6_trials_a4.csv
trial_file_regex <- paste0(
  "^CCRP_subj(\\d+)_ses(\\d+)",
  "(?:\\(([^)]+)\\))?",
  "_trials",
  "(?:_(a\\d+))?",
  "\\.csv$"
)

discover_subject_files <- function(data_dir, subject_ids, session_ids) {
  out <- list()

  for (sid in subject_ids) {
    sub_dir <- file.path(data_dir, paste0("sub", sid))
    if (!dir.exists(sub_dir)) {
      warning("sub", sid, ": folder missing (", sub_dir, "); skipping subject.", call. = FALSE)
      next
    }

    found_sessions <- integer(0)
    unusable_sessions <- integer(0)
    csv_paths <- list.files(sub_dir, pattern = "\\.csv$", full.names = TRUE)
    for (path in csv_paths) {
      m <- regexec(trial_file_regex, basename(path), perl = TRUE, ignore.case = TRUE)
      hit <- regmatches(basename(path), m)[[1]]
      if (length(hit) == 0) next

      file_subj <- as.integer(hit[2])
      file_ses <- as.integer(hit[3])
      ses_repeat <- if (length(hit) >= 4 && !is.na(hit[4]) && nzchar(hit[4])) hit[4] else ""
      computer_tag <- if (length(hit) >= 5 && !is.na(hit[5]) && nzchar(hit[5])) hit[5] else ""

      # Exact numeric subject token only (skip sub4star, sub6eeeee, etc.)
      if (is.na(file_subj) || file_subj != sid) next
      if (!(file_ses %in% session_ids)) next

      # Ignore empty / header-only placeholders (e.g. 0-byte ses17 for sub11)
      file_bytes <- file.info(path)$size
      if (is.na(file_bytes) || file_bytes == 0) {
        unusable_sessions <- c(unusable_sessions, file_ses)
        warning(
          "sub", sid, " ses", file_ses,
          ": empty file ignored (", basename(path), ").",
          call. = FALSE
        )
        next
      }
      n_rows <- length(readLines(path, warn = FALSE)) - 1L
      if (is.na(n_rows) || n_rows <= 0) {
        unusable_sessions <- c(unusable_sessions, file_ses)
        warning(
          "sub", sid, " ses", file_ses,
          ": no trial rows ignored (", basename(path), ").",
          call. = FALSE
        )
        next
      }

      found_sessions <- c(found_sessions, file_ses)
      out[[length(out) + 1L]] <- data.frame(
        path = path,
        subject_id = sid,
        session = file_ses,
        session_repeat = ses_repeat,
        computer_tag = computer_tag,
        n_rows = n_rows,
        stringsAsFactors = FALSE
      )
    }

    missing_ses <- setdiff(session_ids, unique(c(found_sessions, unusable_sessions)))
    if (length(missing_ses) > 0) {
      warning(
        "sub", sid, ": missing session(s) ",
        paste(sort(missing_ses), collapse = ", "),
        " in range ", min(session_ids), "-", max(session_ids),
        "; ignored.",
        call. = FALSE
      )
    }
  }

  if (length(out) == 0) {
    return(data.frame(
      path = character(),
      subject_id = integer(),
      session = integer(),
      session_repeat = character(),
      computer_tag = character(),
      n_rows = integer(),
      stringsAsFactors = FALSE
    ))
  }

  bind_rows(out)
}

# When the same session has multiple computer tags (e.g. incomplete a3 + full a4),
# keep the file with the most rows. Session retries sesN(2) are kept separately.
resolve_duplicate_sessions <- function(file_df) {
  if (nrow(file_df) == 0) return(file_df)

  file_df <- file_df %>%
    arrange(subject_id, session, session_repeat, desc(n_rows), computer_tag)

  keys <- paste(file_df$subject_id, file_df$session, file_df$session_repeat, sep = "|")
  keep_idx <- !duplicated(keys)

  dup_keys <- unique(keys[duplicated(keys)])
  for (key in dup_keys) {
    rows <- which(keys == key)
    keep <- rows[1]
    drop <- rows[-1]
    message(
      "Duplicate session files for sub", file_df$subject_id[keep],
      " ses", file_df$session[keep],
      if (nzchar(file_df$session_repeat[keep])) {
        paste0("(", file_df$session_repeat[keep], ")")
      } else {
        ""
      },
      ": keeping ", basename(file_df$path[keep]),
      " (n=", file_df$n_rows[keep], "); dropping ",
      paste(basename(file_df$path[drop]), collapse = ", ")
    )
  }

  file_df[keep_idx, , drop = FALSE]
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
condition_levels <- names(reward_sets)

normalize_condition_key <- function(values) {
  values <- as.integer(values)
  values <- sort(values[!is.na(values) & values != 0])
  if (length(values) == 0) return(NA_character_)
  paste(values, collapse = ",")
}

# Prefer CueCondition column (current lab format: "(4,2)"); fall back to CueValues.
get_condition <- function(cue_condition, cue_values) {
  if (!is.na(cue_condition) && nzchar(trimws(cue_condition))) {
    digits <- gsub("[^0-9,]", "", cue_condition)
    parts <- strsplit(digits, ",", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    key <- normalize_condition_key(parts)
    if (!is.na(key) && key %in% names(reward_sets)) return(key)
  }

  digits <- gsub("[^0-9]", "", cue_values)
  if (nchar(digits) != 4) return(NA_character_)
  vals <- as.integer(strsplit(digits, "")[[1]])
  key <- normalize_condition_key(vals)
  if (is.na(key) || !(key %in% names(reward_sets))) return(NA_character_)
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

is_true_flag <- function(x) {
  tolower(trimws(as.character(x))) %in% c("1", "true", "t", "yes", "y")
}

is_timeout_response <- function(response) {
  r <- tolower(trimws(as.character(response)))
  is.na(r) | r == "" | r %in% c("timeout", "none")
}

file_df <- discover_subject_files(data_dir, subject_ids, session_ids)
file_df <- resolve_duplicate_sessions(file_df)

if (nrow(file_df) == 0) {
  stop(
    "No trial CSVs found for subjects ", paste(subject_ids, collapse = ","),
    " sessions ", paste(range(session_ids), collapse = "-"),
    " under ", data_dir
  )
}

message(
  "Reading ", nrow(file_df), " session file(s) for subjects ",
  paste(subject_ids, collapse = ","), " / sessions ",
  paste(range(session_ids), collapse = "-")
)

read_trial_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)

  needed <- c(
    "Subject", "Session", "Block", "Trial", "WarmUpTrial", "Response", "RT", "ACC",
    "CueValues", "CueCondition", "ResponseDevice", "BurnInBlock", "RTDifference",
    "CueRankResponse", "CueResponseValue", "NumCues"
  )
  for (col in needed) {
    if (!(col %in% names(df))) df[[col]] <- NA_character_
  }

  df$SourceFile <- basename(path)
  df
}

combined_df <- bind_rows(lapply(file_df$path, read_trial_csv))

n_raw <- nrow(combined_df)

plot_df <- combined_df %>%
  mutate(
    SubjectNumInt = suppressWarnings(as.integer(gsub("[^0-9]", "", Subject))),
    SessionNumInt = suppressWarnings(as.integer(Session)),
    ResponseDevice = if_else(
      SubjectNumInt == 1 & (is.na(ResponseDevice) | trimws(ResponseDevice) == ""),
      "keyboard",
      ResponseDevice
    ),
    DeviceLabel = label_response_device(ResponseDevice),
    Condition = mapply(get_condition, CueCondition, CueValues, USE.NAMES = FALSE),
    WarmUpFlag = is_true_flag(WarmUpTrial),
    # Burn-in: BurnInBlock==1, or Block==0 when BurnInBlock column is present (0/1).
    BurnInFlag = is_true_flag(BurnInBlock) |
      (
        !is.na(BurnInBlock) & trimws(BurnInBlock) != "" &
          !is.na(suppressWarnings(as.integer(Block))) &
          suppressWarnings(as.integer(Block)) == 0
      ),
    TimeoutFlag = is_timeout_response(Response),
    RT_num = suppressWarnings(as.numeric(RT)),
    ACC_num = suppressWarnings(as.numeric(ACC)),
    RTDifference_num = suppressWarnings(as.numeric(RTDifference))
  )

n_warmup <- sum(plot_df$WarmUpFlag, na.rm = TRUE)
n_burnin <- sum(plot_df$BurnInFlag, na.rm = TRUE)
n_timeout <- sum(plot_df$TimeoutFlag, na.rm = TRUE)

plot_df <- plot_df %>%
  filter(SubjectNumInt %in% subject_ids, SessionNumInt %in% session_ids)

if (exclude_warmup) plot_df <- plot_df %>% filter(!WarmUpFlag)
if (exclude_burnin) plot_df <- plot_df %>% filter(!BurnInFlag)
if (exclude_timeouts) plot_df <- plot_df %>% filter(!TimeoutFlag)

plot_df <- plot_df %>%
  filter(
    !is.na(RT_num),
    RT_num >= rt_min_ms,
    RT_num <= rt_max_ms,
    !is.na(Condition),
    !is.na(ACC_num)
  )

n_sub6_rtdiff <- 0L
if (!is.na(exclude_sub6_rtdiff_gt_ms) && 6L %in% subject_ids) {
  sub6_drop <- plot_df$SubjectNumInt == 6 &
    !is.na(plot_df$RTDifference_num) &
    plot_df$RTDifference_num > exclude_sub6_rtdiff_gt_ms
  n_sub6_rtdiff <- sum(sub6_drop, na.rm = TRUE)
  plot_df <- plot_df %>% filter(!sub6_drop)
}

if (nrow(plot_df) == 0) {
  stop("No trials remained after filtering.")
}

message(
  "Filter summary: raw=", n_raw,
  "; warmup_flagged=", n_warmup,
  "; burnin_flagged=", n_burnin,
  "; timeout_flagged=", n_timeout,
  "; sub6_rtdiff_dropped=", n_sub6_rtdiff,
  "; kept=", nrow(plot_df)
)

device_order <- c("KB", "RB", "SRB1", "UnknownDevice")
subject_device_order <- plot_df %>%
  distinct(SubjectNumInt, DeviceLabel) %>%
  mutate(DeviceRank = match(DeviceLabel, device_order)) %>%
  arrange(DeviceRank, SubjectNumInt)

subject_facet_levels <- paste0(
  "sub", subject_device_order$SubjectNumInt, "_", subject_device_order$DeviceLabel
)

plot_df <- plot_df %>%
  mutate(
    SubjectFacet = factor(
      paste0("sub", SubjectNumInt, "_", DeviceLabel),
      levels = subject_facet_levels
    ),
    Condition = factor(Condition, levels = condition_levels)
  )

rt_plot_df <- plot_df

sub_tag <- if (length(subject_ids) == 1) {
  paste0("sub", subject_ids)
} else if (length(subject_ids) == diff(range(subject_ids)) + 1 &&
           all(subject_ids == seq.int(min(subject_ids), max(subject_ids)))) {
  paste0("sub", min(subject_ids), "-", max(subject_ids))
} else {
  paste0("sub", paste(subject_ids, collapse = "-"))
}
ses_tag <- paste0("ses", min(session_ids), "-", max(session_ids))
setting_tag <- paste(sub_tag, ses_tag, sep = "_")

# ---- RT by condition ----
rt_cond_plot <- ggplot(rt_plot_df, aes(x = Condition, y = RT_num)) +
  geom_violin(fill = "#4C78A8", alpha = 0.65, color = "gray25", trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.size = 0.5, alpha = 0.25, color = "black") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "red3", linewidth = 1.15) +
  stat_summary(fun = mean, geom = "point", color = "red3", size = 2.5) +
  facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
  labs(
    title = paste0("RT by Cue Condition (", sub_tag, ", ", ses_tag, ")"),
    subtitle = paste0(
      "Warmup/burn-in/timeouts excluded; RT ", rt_min_ms, "-", rt_max_ms, " ms"
    ),
    x = "Condition",
    y = "Reaction Time (ms)"
  ) +
  coord_cartesian(ylim = c(0, rt_plot_ylim_ms)) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    axis.text.x = element_text(size = axis_text_size, angle = 30, hjust = 1),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

rt_file <- file.path(fig_dir, paste0(setting_tag, "_RTbyCond.png"))
ggsave(filename = rt_file, plot = rt_cond_plot + bold_axes_theme, width = 20, height = 24, dpi = 300)
message("Saved: ", rt_file)

# ---- Accuracy by condition ----
acc_condition_df <- plot_df %>%
  group_by(SubjectFacet, Condition) %>%
  summarize(
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    AccuracySD = sd(ACC_num, na.rm = TRUE),
    AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
    .groups = "drop"
  )

acc_ymin <- max(0, min(acc_condition_df$Accuracy, na.rm = TRUE) - 0.08)

acc_cond_plot <- ggplot(acc_condition_df, aes(x = Condition, y = Accuracy, group = 1)) +
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
    title = paste0("Accuracy by Cue Condition (", sub_tag, ", ", ses_tag, ")"),
    subtitle = paste0(
      "Warmup/burn-in/timeouts excluded; RT ", rt_min_ms, "-", rt_max_ms, " ms"
    ),
    x = "Condition",
    y = "Accuracy"
  ) +
  scale_y_continuous(limits = c(acc_ymin, 1), expand = expansion(mult = c(0, 0.06))) +
  theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = axis_text_size),
    axis.text.x = element_text(size = axis_text_size, angle = 30, hjust = 1),
    strip.text = element_text(size = strip_text_size, face = "bold")
  )

acc_file <- file.path(fig_dir, paste0(setting_tag, "_ACCbyCond.png"))
ggsave(filename = acc_file, plot = acc_cond_plot + bold_axes_theme, width = 20, height = 24, dpi = 300)
message("Saved: ", acc_file)

# ---- Two-cue choice composition: P(high), P(low), P(uncued) ----
# CueRankResponse coding (from experiment):
#   1 = highest-value cued response
#   2 = lower-value cued response
#   4 = uncued (no reward at chosen location)
#   0 = no valid chosen location (treated as uncued here)
two_cue_levels <- c("1,2", "1,3", "1,4", "2,3", "2,4", "3,4")
choice_levels <- c("high", "low", "uncued")
choice_colors <- c(high = "#1B9E77", low = "#D95F02", uncued = "#7570B3")

classify_two_cue_choice <- function(cue_rank_response) {
  rank <- suppressWarnings(as.integer(trimws(as.character(cue_rank_response))))
  dplyr::case_when(
    rank == 1L ~ "high",
    rank == 2L ~ "low",
    rank %in% c(0L, 4L) ~ "uncued",
    TRUE ~ NA_character_
  )
}

two_cue_all_df <- plot_df %>%
  filter(as.character(Condition) %in% two_cue_levels) %>%
  mutate(
    Condition2 = factor(as.character(Condition), levels = two_cue_levels),
    RewardDiff = vapply(as.character(Condition), function(cond) {
      parts <- as.integer(strsplit(cond, ",", fixed = TRUE)[[1]])
      if (length(parts) != 2 || any(is.na(parts))) return(NA_real_)
      abs(parts[2] - parts[1])
    }, numeric(1)),
    RewardDiff = factor(RewardDiff, levels = c(1, 2, 3)),
    ChoiceType = classify_two_cue_choice(CueRankResponse)
  ) %>%
  filter(!is.na(Condition2), !is.na(RewardDiff))

two_cue_df <- two_cue_all_df %>%
  filter(!is.na(ChoiceType))

reward_diff_caption <- paste0(
  "ΔR=1: (1,2)(2,3)(3,4); ΔR=2: (1,3)(2,4); ΔR=3: (1,4)"
)

if (nrow(two_cue_all_df) == 0) {
  warning("No two-cue trials available for two-cue descriptive plots.", call. = FALSE)
} else {
  n_unclassified <- sum(is.na(two_cue_all_df$ChoiceType))
  if (n_unclassified > 0) {
    warning(
      n_unclassified,
      " two-cue trial(s) had unrecognized CueRankResponse and were dropped from choice/RT-by-winner plots.",
      call. = FALSE
    )
  }

  if (nrow(two_cue_df) > 0) {
    two_cue_prop_df <- two_cue_df %>%
      group_by(SubjectFacet, Condition2, ChoiceType) %>%
      summarize(Count = n(), .groups = "drop") %>%
      group_by(SubjectFacet, Condition2) %>%
      mutate(
        Trials = sum(Count),
        Probability = Count / Trials,
        ProbSE = sqrt(Probability * (1 - Probability) / Trials),
        ChoiceType = factor(ChoiceType, levels = choice_levels)
      ) %>%
      ungroup()

    # Ensure all three choice types appear for every subject x condition
    two_cue_prop_df <- tidyr::complete(
      two_cue_prop_df,
      SubjectFacet,
      Condition2,
      ChoiceType = factor(choice_levels, levels = choice_levels),
      fill = list(Count = 0L, Trials = 0L, Probability = 0, ProbSE = 0)
    ) %>%
      group_by(SubjectFacet, Condition2) %>%
      mutate(
        Trials = max(Trials),
        Probability = if_else(Trials > 0, Count / Trials, 0),
        ProbSE = if_else(Trials > 1, sqrt(Probability * (1 - Probability) / Trials), 0)
      ) %>%
      ungroup()

    two_cue_choice_plot <- ggplot(
      two_cue_prop_df,
      aes(x = Condition2, y = Probability, color = ChoiceType, group = ChoiceType)
    ) +
      geom_hline(yintercept = 0, color = "gray80", linewidth = 0.4) +
      geom_line(linewidth = 1.15) +
      geom_point(size = 2.8) +
      geom_errorbar(
        aes(ymin = pmax(0, Probability - ProbSE), ymax = pmin(1, Probability + ProbSE)),
        width = 0.12,
        linewidth = 0.65
      ) +
      scale_color_manual(
        values = choice_colors,
        breaks = choice_levels,
        labels = c("P(high)", "P(low)", "P(uncued)"),
        name = NULL
      ) +
      facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
      labs(
        title = paste0("Two-cue choice: P(high) / P(low) / P(uncued) (", sub_tag, ", ", ses_tag, ")"),
        subtitle = "CueRankResponse: 1=high-value cued, 2=low-value cued, 4=uncued; timeouts/warmup excluded",
        x = "Two-cue condition",
        y = "Choice probability"
      ) +
      scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
      theme_minimal(base_size = plot_base_size) +
      theme(
        plot.title = element_text(size = title_size, face = "bold"),
        plot.subtitle = element_text(size = subtitle_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        axis.text.x = element_text(size = axis_text_size, angle = 30, hjust = 1),
        strip.text = element_text(size = strip_text_size, face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = axis_text_size, face = "bold")
      )

    two_cue_file <- file.path(fig_dir, paste0(setting_tag, "_TwoCueChoiceProb.png"))
    ggsave(
      filename = two_cue_file,
      plot = two_cue_choice_plot + bold_axes_theme,
      width = 20,
      height = 24,
      dpi = 300
    )
    message("Saved: ", two_cue_file)
  }

  # ---- Accuracy by reward difference (ΔR) ----
  acc_rwd_df <- two_cue_all_df %>%
    group_by(SubjectFacet, RewardDiff) %>%
    summarize(
      Accuracy = mean(ACC_num, na.rm = TRUE),
      Trials = n(),
      AccuracySD = sd(ACC_num, na.rm = TRUE),
      AccuracySE = if_else(Trials > 1, AccuracySD / sqrt(Trials), 0),
      .groups = "drop"
    )

  acc_rwd_ymin <- max(0, min(acc_rwd_df$Accuracy, na.rm = TRUE) - 0.08)

  acc_rwd_plot <- ggplot(acc_rwd_df, aes(x = RewardDiff, y = Accuracy, group = 1)) +
    geom_ribbon(
      aes(
        ymin = pmax(0, Accuracy - AccuracySE),
        ymax = pmin(1, Accuracy + AccuracySE)
      ),
      fill = "#59A14F",
      alpha = 0.18
    ) +
    geom_line(color = "#59A14F", linewidth = 1.15) +
    geom_point(color = "#59A14F", size = 2.8) +
    geom_errorbar(
      aes(
        ymin = pmax(0, Accuracy - AccuracySE),
        ymax = pmin(1, Accuracy + AccuracySE)
      ),
      width = 0.12,
      linewidth = 0.7,
      color = "#2F6B34"
    ) +
    facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
    labs(
      title = paste0("Two-cue accuracy by reward difference (", sub_tag, ", ", ses_tag, ")"),
      subtitle = paste0("ACC vs ΔR = R_H - R_L; ", reward_diff_caption),
      x = "Reward difference (ΔR)",
      y = "Accuracy",
      caption = reward_diff_caption
    ) +
    scale_y_continuous(limits = c(acc_rwd_ymin, 1), expand = expansion(mult = c(0, 0.06))) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = subtitle_size),
      axis.title = element_text(size = axis_title_size),
      axis.text = element_text(size = axis_text_size),
      strip.text = element_text(size = strip_text_size, face = "bold"),
      plot.caption = element_text(size = axis_text_size * 0.7, hjust = 0)
    )

  acc_rwd_file <- file.path(fig_dir, paste0(setting_tag, "_ACCbyRewardDiff.png"))
  ggsave(
    filename = acc_rwd_file,
    plot = acc_rwd_plot + bold_axes_theme,
    width = 20,
    height = 24,
    dpi = 300
  )
  message("Saved: ", acc_rwd_file)

  # ---- RT by reward difference, conditional on choice winner ----
  if (nrow(two_cue_df) > 0) {
    rt_rwd_choice_df <- two_cue_df %>%
      mutate(ChoiceType = factor(ChoiceType, levels = choice_levels)) %>%
      group_by(SubjectFacet, RewardDiff, ChoiceType) %>%
      summarize(
        MeanRT = mean(RT_num, na.rm = TRUE),
        Trials = n(),
        RTSD = sd(RT_num, na.rm = TRUE),
        RTSE = if_else(Trials > 1, RTSD / sqrt(Trials), 0),
        .groups = "drop"
      )

    rt_rwd_choice_plot <- ggplot(
      rt_rwd_choice_df,
      aes(x = RewardDiff, y = MeanRT, color = ChoiceType, group = ChoiceType)
    ) +
      geom_line(linewidth = 1.15) +
      geom_point(size = 2.8) +
      geom_errorbar(
        aes(
          ymin = pmax(0, MeanRT - RTSE),
          ymax = MeanRT + RTSE
        ),
        width = 0.12,
        linewidth = 0.65
      ) +
      scale_color_manual(
        values = choice_colors,
        breaks = choice_levels,
        labels = c(expression(RT[H]), expression(RT[L]), expression(RT[uncued])),
        name = NULL
      ) +
      facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
      labs(
        title = paste0("Two-cue RT by reward difference × choice (", sub_tag, ", ", ses_tag, ")"),
        subtitle = paste0("Mean RT conditional on high / low / uncued winner; ", reward_diff_caption),
        x = "Reward difference (ΔR)",
        y = "Mean RT (ms)",
        caption = reward_diff_caption
      ) +
      coord_cartesian(ylim = c(0, rt_plot_ylim_ms)) +
      theme_minimal(base_size = plot_base_size) +
      theme(
        plot.title = element_text(size = title_size, face = "bold"),
        plot.subtitle = element_text(size = subtitle_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        strip.text = element_text(size = strip_text_size, face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = axis_text_size, face = "bold"),
        plot.caption = element_text(size = axis_text_size * 0.7, hjust = 0)
      )

    rt_rwd_file <- file.path(fig_dir, paste0(setting_tag, "_RTbyRewardDiffChoice.png"))
    ggsave(
      filename = rt_rwd_file,
      plot = rt_rwd_choice_plot + bold_axes_theme,
      width = 20,
      height = 24,
      dpi = 300
    )
    message("Saved: ", rt_rwd_file)
  }

  # ---- Competitor-cost plots (mean ΔRT + quantile ΔRT) ----
  # Triangle of contrasts (weaker competitor vs single-cue focal baseline):
  #   FOCAL 4: (1,4), (2,4), (3,4)
  #   FOCAL 3: (1,3), (2,3)
  #   FOCAL 2: (1,2)
  # Each cell: RT(pair | chose focal) - RT(single focal | chose focal)
  competitor_triangle <- list(
    "4" = c(1L, 2L, 3L),
    "3" = c(1L, 2L),
    "2" = c(1L)
  )
  focal_levels <- c("4", "3", "2")
  competitor_levels <- c(1L, 2L, 3L)
  quantile_probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  focal_colors <- c("4" = "#E15759", "3" = "#F28E2B", "2" = "#4E79A7")

  contrast_map <- bind_rows(lapply(names(competitor_triangle), function(focal) {
    comps <- competitor_triangle[[focal]]
    data.frame(
      Focal = focal,
      Competitor = comps,
      SingleCond = focal,
      PairCond = paste0(comps, ",", focal),
      stringsAsFactors = FALSE
    )
  }))

  relevant_conds <- unique(c(contrast_map$SingleCond, contrast_map$PairCond))

  choose_focal_rt_df <- plot_df %>%
    mutate(
      CondChr = as.character(Condition),
      CueRespVal = suppressWarnings(as.integer(trimws(CueResponseValue))),
      CueRank = suppressWarnings(as.integer(trimws(CueRankResponse)))
    ) %>%
    filter(CondChr %in% relevant_conds) %>%
    mutate(
      IsSingle = CondChr %in% unique(contrast_map$SingleCond),
      IsPair = CondChr %in% unique(contrast_map$PairCond),
      FocalFromCond = dplyr::case_when(
        IsSingle ~ CondChr,
        IsPair ~ sub("^.*,", "", CondChr),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(
      !is.na(FocalFromCond),
      CueRespVal == as.integer(FocalFromCond),
      (IsSingle & (CueRank == 1L | CueRespVal == as.integer(FocalFromCond))) |
        (IsPair & CueRank == 1L)
    )

  if (nrow(choose_focal_rt_df) == 0) {
    warning("No choose-focal trials for competitor-cost plots.", call. = FALSE)
  } else {
    # Mean RT by subject × condition (only trials choosing the focal action)
    rt_by_cond <- choose_focal_rt_df %>%
      group_by(SubjectFacet, CondChr) %>%
      summarize(
        MeanRT = mean(RT_num, na.rm = TRUE),
        Trials = n(),
        RTSD = sd(RT_num, na.rm = TRUE),
        RTSE = if_else(Trials > 1, RTSD / sqrt(Trials), 0),
        .groups = "drop"
      )

    delta_rt_df <- contrast_map %>%
      tidyr::crossing(SubjectFacet = unique(rt_by_cond$SubjectFacet)) %>%
      left_join(
        rt_by_cond %>%
          select(SubjectFacet, PairCond = CondChr, PairRT = MeanRT, PairN = Trials, PairSE = RTSE),
        by = c("SubjectFacet", "PairCond")
      ) %>%
      left_join(
        rt_by_cond %>%
          select(
            SubjectFacet,
            SingleCond = CondChr,
            BaselineRT = MeanRT,
            BaselineN = Trials,
            BaselineSE = RTSE
          ),
        by = c("SubjectFacet", "SingleCond")
      ) %>%
      filter(!is.na(PairRT), !is.na(BaselineRT)) %>%
      mutate(
        DeltaRT = PairRT - BaselineRT,
        DeltaSE = sqrt(PairSE^2 + BaselineSE^2),
        Focal = factor(Focal, levels = focal_levels),
        Competitor = factor(Competitor, levels = competitor_levels),
        ContrastLabel = paste0("(", PairCond, ")-(", SingleCond, ")")
      )

    if (nrow(delta_rt_df) == 0) {
      warning("Could not form mean competitor-cost ΔRT contrasts.", call. = FALSE)
    } else {
      # Plot 1: mean competitor-cost triangle
      delta_rt_plot <- ggplot(
        delta_rt_df,
        aes(x = Competitor, y = DeltaRT, color = Focal, group = Focal)
      ) +
        geom_hline(yintercept = 0, color = "gray50", linewidth = 0.7, linetype = "dashed") +
        geom_line(linewidth = 1.15) +
        geom_point(size = 3.0) +
        geom_errorbar(
          aes(ymin = DeltaRT - DeltaSE, ymax = DeltaRT + DeltaSE),
          width = 0.12,
          linewidth = 0.65
        ) +
        scale_color_manual(
          values = focal_colors,
          breaks = focal_levels,
          labels = paste0("focal ", focal_levels),
          name = NULL
        ) +
        facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
        labs(
          title = paste0("Competitor cost: ΔRT when focal still wins (", sub_tag, ", ", ses_tag, ")"),
          subtitle = "ΔRT = RT(c,focal | chose focal) − RT(focal | chose focal); triangle: focal4×{1,2,3}, focal3×{1,2}, focal2×{1}",
          x = "Competitor value c",
          y = "Competitor cost ΔRT (ms)"
        ) +
        theme_minimal(base_size = plot_base_size) +
        theme(
          plot.title = element_text(size = title_size, face = "bold"),
          plot.subtitle = element_text(size = subtitle_size),
          axis.title = element_text(size = axis_title_size),
          axis.text = element_text(size = axis_text_size),
          strip.text = element_text(size = strip_text_size, face = "bold"),
          legend.position = "top",
          legend.text = element_text(size = axis_text_size, face = "bold")
        )

      delta_rt_file <- file.path(fig_dir, paste0(setting_tag, "_CompetitorCost_MeanDeltaRT.png"))
      ggsave(
        filename = delta_rt_file,
        plot = delta_rt_plot + bold_axes_theme,
        width = 20,
        height = 24,
        dpi = 300
      )
      message("Saved: ", delta_rt_file)
    }

    # Plot 2: quantile competitor-cost (same contrasts, ΔQ at matched percentiles)
    min_n_quantile <- 10L
    quantile_delta_list <- list()

    subjects_for_q <- unique(as.character(choose_focal_rt_df$SubjectFacet))
    for (subj in subjects_for_q) {
      subj_df <- choose_focal_rt_df %>%
        filter(as.character(SubjectFacet) == subj)

      for (i in seq_len(nrow(contrast_map))) {
        focal <- contrast_map$Focal[i]
        comp <- contrast_map$Competitor[i]
        single_cond <- contrast_map$SingleCond[i]
        pair_cond <- contrast_map$PairCond[i]

        base_rt <- subj_df %>%
          filter(CondChr == single_cond) %>%
          pull(RT_num)
        pair_rt <- subj_df %>%
          filter(CondChr == pair_cond) %>%
          pull(RT_num)

        if (length(base_rt) < min_n_quantile || length(pair_rt) < min_n_quantile) next

        base_q <- as.numeric(stats::quantile(base_rt, probs = quantile_probs, names = FALSE, type = 7))
        pair_q <- as.numeric(stats::quantile(pair_rt, probs = quantile_probs, names = FALSE, type = 7))

        quantile_delta_list[[length(quantile_delta_list) + 1L]] <- data.frame(
          SubjectFacet = subj,
          Focal = focal,
          Competitor = comp,
          PairCond = pair_cond,
          Quantile = quantile_probs,
          BaselineQ = base_q,
          PairQ = pair_q,
          DeltaQ = pair_q - base_q,
          BaselineN = length(base_rt),
          PairN = length(pair_rt),
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(quantile_delta_list) == 0) {
      warning(
        "Could not form quantile competitor-cost contrasts (need >= ",
        min_n_quantile, " trials per cell).",
        call. = FALSE
      )
    } else {
      quantile_delta_df <- bind_rows(quantile_delta_list) %>%
        mutate(
          SubjectFacet = factor(SubjectFacet, levels = levels(plot_df$SubjectFacet)),
          Focal = factor(
            Focal,
            levels = focal_levels,
            labels = c(
              "focal 4  (pairs vs single 4)",
              "focal 3  (pairs vs single 3)",
              "focal 2  (pairs vs single 2)"
            )
          ),
          Competitor = factor(Competitor, levels = competitor_levels),
          QuantileLabel = factor(
            paste0("Q", sprintf("%.0f", 100 * Quantile)),
            levels = paste0("Q", sprintf("%.0f", 100 * quantile_probs))
          )
        )

      # Columns = the 3 focals; rows = subjects. Legend = weaker-cue value in the pair.
      quantile_delta_plot <- ggplot(
        quantile_delta_df,
        aes(x = QuantileLabel, y = DeltaQ, color = Competitor, group = Competitor)
      ) +
        geom_hline(yintercept = 0, color = "gray50", linewidth = 0.7, linetype = "dashed") +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2.6) +
        scale_color_manual(
          values = c("1" = "#1B9E77", "2" = "#D95F02", "3" = "#7570B3"),
          breaks = as.character(competitor_levels),
          labels = c(
            "1 = weaker cue 1  →  (1,focal) − (focal)",
            "2 = weaker cue 2  →  (2,focal) − (focal)",
            "3 = weaker cue 3  →  (3,focal) − (focal)"
          ),
          name = "Competitor (weaker cue value)"
        ) +
        facet_grid(SubjectFacet ~ Focal, scales = "free_y") +
        labs(
          title = paste0("Competitor cost by RT quantile (", sub_tag, ", ", ses_tag, ")"),
          subtitle = paste0(
            "Columns = focal reward still chosen; ΔQ = Q(pair|chose focal) − Q(single focal|chose focal). ",
            "Example: focal4 + competitor3 means RT((3,4)|chose4) − RT(4|chose4)."
          ),
          x = "RT quantile",
          y = "Competitor cost ΔRT at quantile (ms)"
        ) +
        theme_minimal(base_size = plot_base_size) +
        theme(
          plot.title = element_text(size = title_size, face = "bold"),
          plot.subtitle = element_text(size = subtitle_size * 0.9),
          axis.title = element_text(size = axis_title_size),
          axis.text = element_text(size = axis_text_size * 0.75),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text.x = element_text(size = strip_text_size * 0.75, face = "bold"),
          strip.text.y = element_text(size = strip_text_size * 0.65, face = "bold"),
          legend.position = "top",
          legend.title = element_text(size = axis_text_size, face = "bold"),
          legend.text = element_text(size = axis_text_size * 0.85, face = "bold")
        )

      n_subj_q <- n_distinct(quantile_delta_df$SubjectFacet)
      quantile_file <- file.path(fig_dir, paste0(setting_tag, "_CompetitorCost_QuantileDeltaRT.png"))
      ggsave(
        filename = quantile_file,
        plot = quantile_delta_plot + bold_axes_theme,
        width = 22,
        height = max(14, 2.8 * n_subj_q),
        dpi = 300,
        limitsize = FALSE
      )
      message("Saved: ", quantile_file)

      # Equal-n RT-bin competitor cost (companion to ACC RT-bin plot):
      # Within chose-high trials, split each condition into 5 equal-n RT bins,
      # then ΔRT(bin) = meanRT(pair, bin) − meanRT(single, bin).
      n_rt_bins_cost <- 5L
      min_trials_rtbin_cost <- n_rt_bins_cost * 5L

      rtbin_cost_df <- choose_focal_rt_df %>%
        group_by(SubjectFacet, CondChr) %>%
        filter(n() >= min_trials_rtbin_cost) %>%
        mutate(
          RTBin = ntile(RT_num, n_rt_bins_cost),
          RTBin = factor(
            RTBin,
            levels = 1:n_rt_bins_cost,
            labels = c("Fastest 20%", "20–40%", "40–60%", "60–80%", "Slowest 20%")
          )
        ) %>%
        ungroup() %>%
        group_by(SubjectFacet, CondChr, FocalFromCond, IsPair, IsSingle, RTBin) %>%
        summarize(
          MeanRT = mean(RT_num, na.rm = TRUE),
          Trials = n(),
          RTSE = if_else(Trials > 1, sd(RT_num, na.rm = TRUE) / sqrt(Trials), 0),
          .groups = "drop"
        )

      pair_rtbin_cost <- rtbin_cost_df %>%
        filter(IsPair) %>%
        mutate(
          Competitor = as.integer(sub(",.*$", "", CondChr)),
          HighReward = FocalFromCond
        ) %>%
        filter(Competitor %in% competitor_levels, HighReward %in% focal_levels) %>%
        mutate(
          SubjectFacet = factor(SubjectFacet, levels = levels(plot_df$SubjectFacet)),
          HighReward = factor(
            HighReward,
            levels = focal_levels,
            labels = c(
              "high=4  (pairs vs single 4)",
              "high=3  (pairs vs single 3)",
              "high=2  (pairs vs single 2)"
            )
          ),
          Competitor = factor(Competitor, levels = competitor_levels)
        )

      single_rtbin_cost <- rtbin_cost_df %>%
        filter(IsSingle) %>%
        mutate(HighRewardRaw = FocalFromCond) %>%
        select(
          SubjectFacet,
          HighRewardRaw,
          RTBin,
          BaselineRT = MeanRT,
          BaselineN = Trials,
          BaselineSE = RTSE
        )

      delta_rtbin_cost_df <- pair_rtbin_cost %>%
        mutate(HighRewardRaw = as.character(FocalFromCond)) %>%
        left_join(
          single_rtbin_cost %>%
            mutate(SubjectFacet = factor(SubjectFacet, levels = levels(plot_df$SubjectFacet))),
          by = c("SubjectFacet", "HighRewardRaw", "RTBin")
        ) %>%
        filter(!is.na(BaselineRT)) %>%
        mutate(
          DeltaRT = MeanRT - BaselineRT,
          DeltaSE = sqrt(RTSE^2 + BaselineSE^2),
          ymin = DeltaRT - DeltaSE,
          ymax = DeltaRT + DeltaSE
        )

      if (nrow(delta_rtbin_cost_df) == 0) {
        warning("Could not form equal-n RT-bin competitor-cost ΔRT.", call. = FALSE)
      } else {
        rtbin_cost_plot <- ggplot(
          delta_rtbin_cost_df,
          aes(x = RTBin, y = DeltaRT, color = Competitor, fill = Competitor, group = Competitor)
        ) +
          geom_hline(yintercept = 0, color = "gray50", linewidth = 0.7, linetype = "dashed") +
          geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.12, color = NA) +
          geom_line(linewidth = 1.1) +
          geom_point(size = 2.6) +
          scale_color_manual(
            values = c("1" = "#1B9E77", "2" = "#D95F02", "3" = "#7570B3"),
            breaks = as.character(competitor_levels),
            labels = c(
              "1 = weaker cue 1  →  (1,high) − (high)",
              "2 = weaker cue 2  →  (2,high) − (high)",
              "3 = weaker cue 3  →  (3,high) − (high)"
            ),
            name = "Competitor (weaker cue value)"
          ) +
          scale_fill_manual(
            values = c("1" = "#1B9E77", "2" = "#D95F02", "3" = "#7570B3"),
            breaks = as.character(competitor_levels),
            labels = c(
              "1 = weaker cue 1  →  (1,high) − (high)",
              "2 = weaker cue 2  →  (2,high) − (high)",
              "3 = weaker cue 3  →  (3,high) − (high)"
            ),
            name = "Competitor (weaker cue value)"
          ) +
          facet_grid(SubjectFacet ~ HighReward, scales = "free_y") +
          labs(
            title = paste0("Competitor cost ΔRT by equal-n RT bin (", sub_tag, ", ", ses_tag, ")"),
            subtitle = paste0(
              "Chose higher reward only. Each condition split into 5 equal-n RT bins (fast→slow). ",
              "ΔRT = meanRT(pair, bin) − meanRT(single, same relative bin). ",
              "Example high=4, competitor 3: meanRT((3,4)|chose4) − meanRT(4|chose4) within each bin."
            ),
            x = "RT bin (equal-n within condition)",
            y = "Competitor cost ΔRT (ms)"
          ) +
          theme_minimal(base_size = plot_base_size) +
          theme(
            plot.title = element_text(size = title_size, face = "bold"),
            plot.subtitle = element_text(size = subtitle_size * 0.9),
            axis.title = element_text(size = axis_title_size),
            axis.text = element_text(size = axis_text_size * 0.7),
            axis.text.x = element_text(angle = 35, hjust = 1),
            strip.text.x = element_text(size = strip_text_size * 0.75, face = "bold"),
            strip.text.y = element_text(size = strip_text_size * 0.65, face = "bold"),
            legend.position = "top",
            legend.title = element_text(size = axis_text_size, face = "bold"),
            legend.text = element_text(size = axis_text_size * 0.85, face = "bold")
          )

        rtbin_cost_file <- file.path(
          fig_dir,
          paste0(setting_tag, "_CompetitorCost_RTbinDeltaRT.png")
        )
        ggsave(
          filename = rtbin_cost_file,
          plot = rtbin_cost_plot + bold_axes_theme,
          width = 22,
          height = max(14, 2.8 * n_distinct(delta_rtbin_cost_df$SubjectFacet)),
          dpi = 300,
          limitsize = FALSE
        )
        message("Saved: ", rtbin_cost_file)
      }
    }
  }

  # ---- Competitor-cost plots for ACCURACY (mean ΔACC + RT-bin ACC) ----
  # Same triangle, but outcome is P(chose high) = mean(ACC).
  # Mean: ΔACC = P(chose high | pair) − P(chose high | single).
  # RT-bin: equal-n bins within condition; P(chose high | bin) and ΔACC by bin.
  acc_cond_df <- plot_df %>%
    mutate(
      CondChr = as.character(Condition),
      CueRespVal = suppressWarnings(as.integer(trimws(CueResponseValue))),
      CueRank = suppressWarnings(as.integer(trimws(CueRankResponse)))
    ) %>%
    filter(CondChr %in% relevant_conds) %>%
    mutate(
      IsSingle = CondChr %in% unique(contrast_map$SingleCond),
      IsPair = CondChr %in% unique(contrast_map$PairCond),
      FocalFromCond = dplyr::case_when(
        IsSingle ~ CondChr,
        IsPair ~ sub("^.*,", "", CondChr),
        TRUE ~ NA_character_
      ),
      # ACC already marks max-reward / high choice; keep explicit high-reward check as backup
      ChoseFocal = as.integer(
        (!is.na(ACC_num) & ACC_num == 1) |
          (!is.na(CueRank) & CueRank == 1L &
             !is.na(CueRespVal) & CueRespVal == as.integer(FocalFromCond))
      )
    ) %>%
    filter(!is.na(FocalFromCond), !is.na(ChoseFocal))

  if (nrow(acc_cond_df) == 0) {
    warning("No trials for accuracy competitor-cost plots.", call. = FALSE)
  } else {
    acc_by_cond <- acc_cond_df %>%
      group_by(SubjectFacet, CondChr) %>%
      summarize(
        Accuracy = mean(ChoseFocal, na.rm = TRUE),
        Trials = n(),
        AccuracySE = if_else(
          Trials > 1,
          sqrt(Accuracy * (1 - Accuracy) / Trials),
          0
        ),
        .groups = "drop"
      )

    delta_acc_df <- contrast_map %>%
      tidyr::crossing(SubjectFacet = unique(acc_by_cond$SubjectFacet)) %>%
      left_join(
        acc_by_cond %>%
          select(
            SubjectFacet,
            PairCond = CondChr,
            PairACC = Accuracy,
            PairN = Trials,
            PairSE = AccuracySE
          ),
        by = c("SubjectFacet", "PairCond")
      ) %>%
      left_join(
        acc_by_cond %>%
          select(
            SubjectFacet,
            SingleCond = CondChr,
            BaselineACC = Accuracy,
            BaselineN = Trials,
            BaselineSE = AccuracySE
          ),
        by = c("SubjectFacet", "SingleCond")
      ) %>%
      filter(!is.na(PairACC), !is.na(BaselineACC)) %>%
      mutate(
        DeltaACC = PairACC - BaselineACC,
        DeltaSE = sqrt(PairSE^2 + BaselineSE^2),
        Focal = factor(Focal, levels = focal_levels),
        Competitor = factor(Competitor, levels = competitor_levels)
      )

    if (nrow(delta_acc_df) == 0) {
      warning("Could not form mean competitor-cost ΔACC contrasts.", call. = FALSE)
    } else {
      delta_acc_plot <- ggplot(
        delta_acc_df,
        aes(x = Competitor, y = DeltaACC, color = Focal, group = Focal)
      ) +
        geom_hline(yintercept = 0, color = "gray50", linewidth = 0.7, linetype = "dashed") +
        geom_line(linewidth = 1.15) +
        geom_point(size = 3.0) +
        geom_errorbar(
          aes(ymin = DeltaACC - DeltaSE, ymax = DeltaACC + DeltaSE),
          width = 0.12,
          linewidth = 0.65
        ) +
        scale_color_manual(
          values = focal_colors,
          breaks = focal_levels,
          labels = paste0("focal ", focal_levels),
          name = NULL
        ) +
        facet_wrap(~SubjectFacet, ncol = 2, scales = "fixed") +
        labs(
          title = paste0("Competitor cost: ΔACC when focal is available (", sub_tag, ", ", ses_tag, ")"),
          subtitle = "ΔACC = P(chose focal | pair) − P(chose focal | single); triangle: focal4×{1,2,3}, focal3×{1,2}, focal2×{1}",
          x = "Competitor value c",
          y = "Competitor cost ΔACC (probability)"
        ) +
        theme_minimal(base_size = plot_base_size) +
        theme(
          plot.title = element_text(size = title_size, face = "bold"),
          plot.subtitle = element_text(size = subtitle_size),
          axis.title = element_text(size = axis_title_size),
          axis.text = element_text(size = axis_text_size),
          strip.text = element_text(size = strip_text_size, face = "bold"),
          legend.position = "top",
          legend.text = element_text(size = axis_text_size, face = "bold")
        )

      delta_acc_file <- file.path(fig_dir, paste0(setting_tag, "_CompetitorCost_MeanDeltaACC.png"))
      ggsave(
        filename = delta_acc_file,
        plot = delta_acc_plot + bold_axes_theme,
        width = 20,
        height = 24,
        dpi = 300
      )
      message("Saved: ", delta_acc_file)
    }

    # ---- Equal-n RT-bin accuracy plots (layout like QuantileDelta) ----
    # Split each condition's trials into 5 equal-sized RT groups (fast→slow),
    # then plot P(chose focal | RT bin). This puts similar N in each bin.
    n_rt_bins <- 5L
    min_trials_for_bins <- n_rt_bins * 5L  # >=5 trials per bin on average

    acc_rtbin_df <- acc_cond_df %>%
      group_by(SubjectFacet, CondChr) %>%
      filter(n() >= min_trials_for_bins) %>%
      mutate(
        RTBin = ntile(RT_num, n_rt_bins),
        RTBin = factor(
          RTBin,
          levels = 1:n_rt_bins,
          labels = c("Fastest 20%", "20–40%", "40–60%", "60–80%", "Slowest 20%")
        )
      ) %>%
      ungroup() %>%
      group_by(SubjectFacet, CondChr, FocalFromCond, IsPair, IsSingle, RTBin) %>%
      summarize(
        P_high = mean(ChoseFocal, na.rm = TRUE),
        Trials = n(),
        PSE = if_else(Trials > 1, sqrt(P_high * (1 - P_high) / Trials), 0),
        .groups = "drop"
      )

    pair_rtbin_df <- acc_rtbin_df %>%
      filter(IsPair) %>%
      mutate(
        Competitor = as.integer(sub(",.*$", "", CondChr)),
        FocalRaw = FocalFromCond
      ) %>%
      filter(Competitor %in% competitor_levels, FocalRaw %in% focal_levels) %>%
      mutate(
        SubjectFacet = factor(SubjectFacet, levels = levels(plot_df$SubjectFacet)),
        Focal = factor(
          FocalRaw,
          levels = focal_levels,
          labels = c(
            "high=4  (pairs vs single 4)",
            "high=3  (pairs vs single 3)",
            "high=2  (pairs vs single 2)"
          )
        ),
        Competitor = factor(Competitor, levels = competitor_levels),
        ymin = pmax(0, P_high - PSE),
        ymax = pmin(1, P_high + PSE)
      )

    single_rtbin_df <- acc_rtbin_df %>%
      filter(IsSingle) %>%
      mutate(Focal = FocalFromCond) %>%
      select(SubjectFacet, Focal, RTBin, BaselineP = P_high, BaselineN = Trials, BaselineSE = PSE)

    if (nrow(pair_rtbin_df) == 0) {
      warning("No equal-n RT-bin accuracy data for pair conditions.", call. = FALSE)
    } else {
      # Plot 1: absolute P(chose high | RT bin) — pairs only (not a difference)
      rtbin_acc_plot <- ggplot(
        pair_rtbin_df,
        aes(x = RTBin, y = P_high, color = Competitor, fill = Competitor, group = Competitor)
      ) +
        geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.12, color = NA) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2.6) +
        scale_color_manual(
          values = c("1" = "#1B9E77", "2" = "#D95F02", "3" = "#7570B3"),
          breaks = as.character(competitor_levels),
          labels = c(
            "1 = weaker cue 1  →  (1,high)",
            "2 = weaker cue 2  →  (2,high)",
            "3 = weaker cue 3  →  (3,high)"
          ),
          name = "Competitor (weaker cue value)"
        ) +
        scale_fill_manual(
          values = c("1" = "#1B9E77", "2" = "#D95F02", "3" = "#7570B3"),
          breaks = as.character(competitor_levels),
          labels = c(
            "1 = weaker cue 1  →  (1,high)",
            "2 = weaker cue 2  →  (2,high)",
            "3 = weaker cue 3  →  (3,high)"
          ),
          name = "Competitor (weaker cue value)"
        ) +
        facet_grid(SubjectFacet ~ Focal, scales = "free_y") +
        labs(
          title = paste0("P(chose high | RT bin) by competitor (", sub_tag, ", ", ses_tag, ")"),
          subtitle = paste0(
            "Within each two-cue condition, trials split into 5 equal-n RT bins (fast→slow). ",
            "Y = absolute P(chose higher reward), NOT pair−single. Example high=4: (1,4)/(2,4)/(3,4)."
          ),
          x = "RT bin (equal-n within condition)",
          y = "P(chose higher reward)"
        ) +
        theme_minimal(base_size = plot_base_size) +
        theme(
          plot.title = element_text(size = title_size, face = "bold"),
          plot.subtitle = element_text(size = subtitle_size * 0.9),
          axis.title = element_text(size = axis_title_size),
          axis.text = element_text(size = axis_text_size * 0.7),
          axis.text.x = element_text(angle = 35, hjust = 1),
          strip.text.x = element_text(size = strip_text_size * 0.75, face = "bold"),
          strip.text.y = element_text(size = strip_text_size * 0.65, face = "bold"),
          legend.position = "top",
          legend.title = element_text(size = axis_text_size, face = "bold"),
          legend.text = element_text(size = axis_text_size * 0.85, face = "bold")
        )

      n_subj_rtbin <- n_distinct(pair_rtbin_df$SubjectFacet)
      rtbin_acc_file <- file.path(
        fig_dir,
        paste0(setting_tag, "_CompetitorCost_RTbinPChooseHigh.png")
      )
      ggsave(
        filename = rtbin_acc_file,
        plot = rtbin_acc_plot + bold_axes_theme,
        width = 22,
        height = max(14, 2.8 * n_subj_rtbin),
        dpi = 300,
        limitsize = FALSE
      )
      message("Saved: ", rtbin_acc_file)

      # Plot 2: ΔP(h) at matched equal-n RT bins (pair − single) — like RTbinDeltaRT
      delta_rtbin_df <- pair_rtbin_df %>%
        left_join(
          single_rtbin_df %>%
            mutate(
              SubjectFacet = factor(SubjectFacet, levels = levels(plot_df$SubjectFacet)),
              FocalRaw = as.character(Focal)
            ) %>%
            select(SubjectFacet, FocalRaw, RTBin, BaselineP, BaselineN, BaselineSE),
          by = c("SubjectFacet", "FocalRaw", "RTBin")
        ) %>%
        filter(!is.na(BaselineP)) %>%
        mutate(
          DeltaACC = P_high - BaselineP,
          DeltaSE = sqrt(PSE^2 + BaselineSE^2),
          ymin = DeltaACC - DeltaSE,
          ymax = DeltaACC + DeltaSE
        )

      if (nrow(delta_rtbin_df) == 0) {
        warning("Could not form equal-n RT-bin ΔP(h) contrasts.", call. = FALSE)
      } else {
        delta_rtbin_plot <- ggplot(
          delta_rtbin_df,
          aes(x = RTBin, y = DeltaACC, color = Competitor, fill = Competitor, group = Competitor)
        ) +
          geom_hline(yintercept = 0, color = "gray50", linewidth = 0.7, linetype = "dashed") +
          geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.12, color = NA) +
          geom_line(linewidth = 1.1) +
          geom_point(size = 2.6) +
          scale_color_manual(
            values = c("1" = "#1B9E77", "2" = "#D95F02", "3" = "#7570B3"),
            breaks = as.character(competitor_levels),
            labels = c(
              "1 = weaker cue 1  →  (1,high) − (high)",
              "2 = weaker cue 2  →  (2,high) − (high)",
              "3 = weaker cue 3  →  (3,high) − (high)"
            ),
            name = "Competitor (weaker cue value)"
          ) +
          scale_fill_manual(
            values = c("1" = "#1B9E77", "2" = "#D95F02", "3" = "#7570B3"),
            breaks = as.character(competitor_levels),
            labels = c(
              "1 = weaker cue 1  →  (1,high) − (high)",
              "2 = weaker cue 2  →  (2,high) − (high)",
              "3 = weaker cue 3  →  (3,high) − (high)"
            ),
            name = "Competitor (weaker cue value)"
          ) +
          facet_grid(SubjectFacet ~ Focal, scales = "free_y") +
          labs(
            title = paste0("Competitor cost ΔP(chose high) by equal-n RT bin (", sub_tag, ", ", ses_tag, ")"),
            subtitle = paste0(
              "Same structure as RTbinDeltaRT. ",
              "ΔP(h) = P(chose high|pair, bin) − P(chose high|single, same relative bin). ",
              "Example high=4, competitor 3: P(chose4|(3,4)) − P(chose4|(4)) within each RT bin. ",
              "Near 0 if accuracy is at ceiling."
            ),
            x = "RT bin (equal-n within condition)",
            y = "Competitor cost ΔP(h) (pair − single)"
          ) +
          theme_minimal(base_size = plot_base_size) +
          theme(
            plot.title = element_text(size = title_size, face = "bold"),
            plot.subtitle = element_text(size = subtitle_size * 0.9),
            axis.title = element_text(size = axis_title_size),
            axis.text = element_text(size = axis_text_size * 0.7),
            axis.text.x = element_text(angle = 35, hjust = 1),
            strip.text.x = element_text(size = strip_text_size * 0.75, face = "bold"),
            strip.text.y = element_text(size = strip_text_size * 0.65, face = "bold"),
            legend.position = "top",
            legend.title = element_text(size = axis_text_size, face = "bold"),
            legend.text = element_text(size = axis_text_size * 0.85, face = "bold")
          )

        rtbin_delta_acc_file <- file.path(
          fig_dir,
          paste0(setting_tag, "_CompetitorCost_RTbinDeltaACC.png")
        )
        ggsave(
          filename = rtbin_delta_acc_file,
          plot = delta_rtbin_plot + bold_axes_theme,
          width = 22,
          height = max(14, 2.8 * n_distinct(delta_rtbin_df$SubjectFacet)),
          dpi = 300,
          limitsize = FALSE
        )
        message("Saved: ", rtbin_delta_acc_file)

        # keep old name as a copy for continuity
        quantile_acc_file <- file.path(
          fig_dir,
          paste0(setting_tag, "_CompetitorCost_QuantileDeltaACC.png")
        )
        file.copy(rtbin_delta_acc_file, quantile_acc_file, overwrite = TRUE)
        message("Saved: ", quantile_acc_file)
      }
    }
  }
}

message("Done.")
