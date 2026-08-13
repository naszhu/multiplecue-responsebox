# WHY THIS SCRIPT
# Descriptive check: does accuracy differ across the 10 cue conditions, for each
# participant separately? Plots accuracy (y) vs condition (x) in two panels
# (subjects 1-16 and 17-32; missing subjects skipped).
#
# Also: per-subject one-way ANOVA on the four SINGLE-CUE conditions (1,2,3,4) —
# trial-level ACC ~ Condition within each subject — to test whether that subject's
# accuracy differs across reward levels 1 vs 2 vs 3 vs 4 (not a pooled RM-ANOVA).
#
# Run from multiplecue-responsebox/ or analysis/:
#   Rscript analysis/aug_12_acc_by_condition_sub1-32.R

library(dplyr)
library(ggplot2)

# ---- CONFIG ----
subject_range <- 1:32
session_range <- 6:17
plot_panels <- list(
  list(min = 1L, max = 16L, tag = "sub1-16"),
  list(min = 17L, max = 32L, tag = "sub17-32")
)
single_cue_levels <- c("1", "2", "3", "4")
rt_min_ms <- 0
rt_max_ms <- 4000
exclude_timeouts <- TRUE
exclude_warmup <- TRUE
exclude_burnin <- TRUE
exclude_sub6_rtdiff_gt_ms <- 500
# ----------------

plot_base_size <- 24
title_size <- 28
subtitle_size <- 20
axis_title_size <- 24
axis_text_size <- 20
strip_text_size <- 20
bold_axes_theme <- theme(
  axis.title = element_text(face = "bold"),
  axis.text = element_text(face = "bold"),
  axis.text.x = element_text(face = "bold"),
  axis.text.y = element_text(face = "bold")
)

if (dir.exists(file.path("exp", "data_from_lab"))) {
  proj_root <- normalizePath(".")
} else if (dir.exists(file.path("..", "exp", "data_from_lab"))) {
  proj_root <- normalizePath("..")
} else {
  stop("Cannot find exp/data_from_lab. Run from multiplecue-responsebox/ or analysis/.")
}

data_dir <- file.path(proj_root, "exp", "data_from_lab")
fig_dir <- file.path(proj_root, "analysis", "fig", "fig_aug12")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

subject_ids <- sort(unique(as.integer(subject_range)))
session_ids <- sort(unique(as.integer(session_range)))
ses_tag <- paste0("ses", min(session_ids), "-", max(session_ids))

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
      warning("sub", sid, ": folder missing; skipping.", call. = FALSE)
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

      if (is.na(file_subj) || file_subj != sid) next
      if (!(file_ses %in% session_ids)) next

      file_bytes <- file.info(path)$size
      if (is.na(file_bytes) || file_bytes == 0) {
        unusable_sessions <- c(unusable_sessions, file_ses)
        next
      }
      n_rows <- length(readLines(path, warn = FALSE)) - 1L
      if (is.na(n_rows) || n_rows <= 0) {
        unusable_sessions <- c(unusable_sessions, file_ses)
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

resolve_duplicate_sessions <- function(file_df) {
  if (nrow(file_df) == 0) return(file_df)

  file_df <- file_df %>%
    arrange(subject_id, session, session_repeat, desc(n_rows), computer_tag)

  keys <- paste(file_df$subject_id, file_df$session, file_df$session_repeat, sep = "|")
  file_df[!duplicated(keys), , drop = FALSE]
}

reward_sets <- list(
  "1" = c(1), "2" = c(2), "3" = c(3), "4" = c(4),
  "1,2" = c(1, 2), "1,3" = c(1, 3), "1,4" = c(1, 4),
  "2,3" = c(2, 3), "2,4" = c(2, 4), "3,4" = c(3, 4)
)
condition_levels <- names(reward_sets)

normalize_condition_key <- function(values) {
  values <- as.integer(values)
  values <- sort(values[!is.na(values) & values != 0])
  if (length(values) == 0) return(NA_character_)
  paste(values, collapse = ",")
}

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
  key <- normalize_condition_key(vals[vals != 0])
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
  stop("No trial CSVs found for subjects ", paste(subject_ids, collapse = ","))
}

message(
  "Reading ", nrow(file_df), " session file(s) for subjects ",
  paste(sort(unique(file_df$subject_id)), collapse = ","),
  " / ", ses_tag
)

read_trial_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
  needed <- c(
    "Subject", "Session", "Block", "WarmUpTrial", "Response", "RT", "ACC",
    "CueValues", "CueCondition", "ResponseDevice", "BurnInBlock", "RTDifference"
  )
  for (col in needed) {
    if (!(col %in% names(df))) df[[col]] <- NA_character_
  }
  df
}

plot_df <- bind_rows(lapply(file_df$path, read_trial_csv)) %>%
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
  ) %>%
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

if (!is.na(exclude_sub6_rtdiff_gt_ms)) {
  plot_df <- plot_df %>%
    filter(
      !(SubjectNumInt == 6 & !is.na(RTDifference_num) & RTDifference_num > exclude_sub6_rtdiff_gt_ms)
    )
}

if (nrow(plot_df) == 0) stop("No trials remained after filtering.")

plot_df <- plot_df %>%
  mutate(
    Condition = factor(Condition, levels = condition_levels),
    SubjectFacet = paste0("sub", SubjectNumInt, "_", DeviceLabel)
  )

acc_condition_df <- plot_df %>%
  group_by(SubjectNumInt, SubjectFacet, Condition) %>%
  summarize(
    Accuracy = mean(ACC_num, na.rm = TRUE),
    Trials = n(),
    AccuracySE = if_else(Trials > 1, sd(ACC_num, na.rm = TRUE) / sqrt(Trials), 0),
    .groups = "drop"
  )

make_acc_plot <- function(acc_df, panel_tag, subjects_present) {
  acc_ymin <- max(0, min(acc_df$Accuracy, na.rm = TRUE) - 0.08)
  n_facets <- length(subjects_present)
  facet_levels <- acc_df %>%
    distinct(SubjectFacet, SubjectNumInt) %>%
    arrange(SubjectNumInt) %>%
    pull(SubjectFacet)

  acc_df <- acc_df %>%
    mutate(SubjectFacet = factor(SubjectFacet, levels = facet_levels))

  ggplot(acc_df, aes(x = Condition, y = Accuracy, group = 1)) +
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
      title = paste0("Accuracy by Cue Condition (", panel_tag, ", ", ses_tag, ")"),
      subtitle = paste0(
        "Subjects with data in ", panel_tag, ": ",
        paste(subjects_present, collapse = ", ")
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
      axis.text.x = element_text(angle = 30, hjust = 1),
      strip.text = element_text(size = strip_text_size, face = "bold")
    )
}

for (panel in plot_panels) {
  subjects_in_panel <- sort(unique(
    acc_condition_df$SubjectNumInt[
      acc_condition_df$SubjectNumInt >= panel$min &
        acc_condition_df$SubjectNumInt <= panel$max
    ]
  ))

  if (length(subjects_in_panel) == 0) {
    warning(panel$tag, ": no subjects with data; skipping plot.", call. = FALSE)
    next
  }

  panel_df <- acc_condition_df %>%
    filter(SubjectNumInt %in% subjects_in_panel)

  panel_plot <- make_acc_plot(panel_df, panel$tag, subjects_in_panel)
  panel_file <- file.path(fig_dir, paste0(panel$tag, "_", ses_tag, "_ACCbyCond.png"))
  ggsave(
    filename = panel_file,
    plot = panel_plot + bold_axes_theme,
    width = 20,
    height = max(14, 3.2 * ceiling(length(subjects_in_panel) / 2)),
    dpi = 300,
    limitsize = FALSE
  )
  message("Saved: ", panel_file)
}

# ---- Per-subject one-way ANOVA: single-cue conditions 1–4 ----
single_trial_df <- plot_df %>%
  filter(as.character(Condition) %in% single_cue_levels) %>%
  mutate(Condition = factor(Condition, levels = single_cue_levels))

subjects_with_single <- sort(unique(single_trial_df$SubjectNumInt))

run_subject_single_anova <- function(subj_id) {
  subj_df <- single_trial_df %>% filter(SubjectNumInt == subj_id)
  cond_present <- sort(unique(as.character(subj_df$Condition)))
  n_conds <- length(cond_present)
  n_trials <- nrow(subj_df)

  if (n_conds < 2) {
    return(data.frame(
      SubjectNumInt = subj_id,
      n_trials = n_trials,
      n_conditions = n_conds,
      conditions = paste(cond_present, collapse = ","),
      F_value = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_,
      p_value = NA_real_,
      status = "skipped (<2 conditions)",
      stringsAsFactors = FALSE
    ))
  }

  subj_df <- subj_df %>% mutate(Condition = droplevels(Condition))
  fit <- tryCatch(
    aov(ACC_num ~ Condition, data = subj_df),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(data.frame(
      SubjectNumInt = subj_id,
      n_trials = n_trials,
      n_conditions = n_conds,
      conditions = paste(cond_present, collapse = ","),
      F_value = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_,
      p_value = NA_real_,
      status = "error",
      stringsAsFactors = FALSE
    ))
  }

  tab <- summary(fit)[[1]]
  data.frame(
    SubjectNumInt = subj_id,
    n_trials = n_trials,
    n_conditions = n_conds,
    conditions = paste(cond_present, collapse = ","),
    F_value = tab[["F value"]][1],
    df1 = tab[["Df"]][1],
    df2 = tab[["Df"]][2],
    p_value = tab[["Pr(>F)"]][1],
    status = if (n_conds < length(single_cue_levels)) "partial (not all 4 cues)" else "ok",
    stringsAsFactors = FALSE
  )
}

subject_anova_df <- bind_rows(lapply(subjects_with_single, run_subject_single_anova)) %>%
  arrange(SubjectNumInt)

cat("\n--- Per-subject one-way ANOVA (single-cue conditions 1–4) ---\n")
cat("Trial-level ACC ~ Condition within each subject.\n\n")

for (i in seq_len(nrow(subject_anova_df))) {
  row <- subject_anova_df[i, ]
  if (is.na(row$p_value)) {
    cat(sprintf(
      "sub%-2d  %s  (n=%d, conds=%s)\n",
      row$SubjectNumInt, row$status, row$n_trials, row$conditions
    ))
  } else {
    sig <- if (row$p_value < 0.05) "*" else ""
    cat(sprintf(
      "sub%-2d  F(%.0f,%.0f)=%.3f  p=%.4g%s  n=%d  [%s]\n",
      row$SubjectNumInt, row$df1, row$df2, row$F_value, row$p_value, sig,
      row$n_trials, row$status
    ))
  }
}

sig_subs <- subject_anova_df %>%
  filter(!is.na(p_value), p_value < 0.05) %>%
  pull(SubjectNumInt)

cat("\nSignificant (p < .05): ")
if (length(sig_subs) == 0) {
  cat("none\n")
} else {
  cat(paste0("sub", sig_subs, collapse = ", "), "\n")
}

n_sig <- length(sig_subs)
n_tested <- sum(!is.na(subject_anova_df$p_value))
cat(sprintf("Summary: %d/%d subjects with p < .05\n", n_sig, n_tested))

cat("Done.\n")
