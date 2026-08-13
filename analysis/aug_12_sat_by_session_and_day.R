# WHY THIS SCRIPT
# Speed-accuracy tradeoff (SAT) by condition, same style as the relative-scale
# AllDev plot (Mean RT vs Accuracy, one point per condition, free axes), but
# sliced within a subject so you can see how the tradeoff pattern evolves:
#   1) by experimental session (ses 6..17 as separate facets)
#   2) by participation day (pool all valid sessions on day 1, day 2, ...)
#
# Config: subjects_to_plot <- c(10, 12)  → one figure per subject per view.
# Filenames start with plot meaning: SpeedAccuracyTradeoff_bySession_..._subN.png
#
# Run from multiplecue-responsebox/ or analysis/:
#   Rscript analysis/aug_12_sat_by_session_and_day.R

library(dplyr)
library(ggplot2)

# ---- CONFIG ----
subjects_to_plot <- c(10, 12)   # change here; one figure per subject
session_range <- 6:17
rt_min_ms <- 0
rt_max_ms <- 4000
exclude_timeouts <- TRUE
exclude_warmup <- TRUE
exclude_burnin <- TRUE
exclude_sub6_rtdiff_gt_ms <- 500
# layout for many session facets
session_facet_ncol <- 3
day_facet_ncol <- 2
point_size <- 4.5
label_size <- 5.5
# ----------------

plot_base_size <- 18
title_size <- 24
subtitle_size <- 16
axis_title_size <- 18
axis_text_size <- 14
strip_text_size <- 16
bold_axes_theme <- theme(
  axis.title = element_text(face = "bold"),
  axis.text = element_text(face = "bold")
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

subject_ids <- sort(unique(as.integer(subjects_to_plot)))
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
    csv_paths <- list.files(sub_dir, pattern = "\\.csv$", full.names = TRUE)
    for (path in csv_paths) {
      m <- regexec(trial_file_regex, basename(path), perl = TRUE, ignore.case = TRUE)
      hit <- regmatches(basename(path), m)[[1]]
      if (length(hit) == 0) next
      file_subj <- as.integer(hit[2])
      file_ses <- as.integer(hit[3])
      if (is.na(file_subj) || file_subj != sid) next
      if (!(file_ses %in% session_ids)) next
      file_bytes <- file.info(path)$size
      if (is.na(file_bytes) || file_bytes == 0) next
      n_rows <- length(readLines(path, warn = FALSE)) - 1L
      if (is.na(n_rows) || n_rows <= 0) next
      found_sessions <- c(found_sessions, file_ses)
      out[[length(out) + 1L]] <- data.frame(
        path = path, subject_id = sid, session = file_ses, n_rows = n_rows,
        stringsAsFactors = FALSE
      )
    }
    missing_ses <- setdiff(session_ids, unique(found_sessions))
    if (length(missing_ses) > 0) {
      warning(
        "sub", sid, ": missing session(s) ",
        paste(sort(missing_ses), collapse = ", "), "; ignored.",
        call. = FALSE
      )
    }
  }
  if (length(out) == 0) {
    return(data.frame(
      path = character(), subject_id = integer(),
      session = integer(), n_rows = integer()
    ))
  }
  bind_rows(out)
}

resolve_duplicate_sessions <- function(file_df) {
  if (nrow(file_df) == 0) return(file_df)
  file_df <- file_df %>%
    arrange(subject_id, session, desc(n_rows))
  file_df[!duplicated(paste(file_df$subject_id, file_df$session)), , drop = FALSE]
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

get_reward_diff_from_condition <- function(condition_string) {
  if (is.na(condition_string) || !grepl(",", condition_string, fixed = TRUE)) return(NA_real_)
  parts <- as.integer(strsplit(condition_string, ",", fixed = TRUE)[[1]])
  if (length(parts) != 2 || any(is.na(parts))) return(NA_real_)
  abs(parts[2] - parts[1])
}

label_response_device <- function(response_device) {
  rd <- tolower(trimws(response_device))
  lbl <- gsub("[^A-Za-z0-9]+", "_", response_device)
  lbl[is.na(rd) | rd == ""] <- "UnknownDevice"
  lbl[grepl("cedrus|response[_ -]?box|response box", rd)] <- "RB"
  lbl[grepl("keyboard", rd)] <- "KB"
  lbl[grepl("self", rd)] <- "SRB1"
  lbl
}

is_true_flag <- function(x) {
  tolower(trimws(as.character(x))) %in% c("1", "true", "t", "yes", "y")
}

is_timeout_response <- function(response) {
  r <- tolower(trimws(as.character(response)))
  is.na(r) | r == "" | r %in% c("timeout", "none")
}

ordinal_suffix <- function(n) {
  if (is.na(n)) return(NA_character_)
  if (n %% 100 %in% c(11, 12, 13)) return(paste0(n, "th"))
  last_digit <- n %% 10
  suffix <- if (last_digit == 1) "st" else if (last_digit == 2) "nd" else if (last_digit == 3) "rd" else "th"
  paste0(n, suffix)
}

assign_tradeoff_color <- function(cue_count, condition_chr, reward_diff) {
  single_val <- suppressWarnings(as.integer(condition_chr))
  dplyr::case_when(
    cue_count == "1-cue" & single_val == 1 ~ "#08519C",
    cue_count == "1-cue" & single_val == 2 ~ "#3182BD",
    cue_count == "1-cue" & single_val == 3 ~ "#6BAED6",
    cue_count == "1-cue" & single_val == 4 ~ "#BDD7E7",
    cue_count == "2-cue" & as.integer(reward_diff) == 1 ~ "#A50F15",
    cue_count == "2-cue" & as.integer(reward_diff) == 2 ~ "#DE2D26",
    cue_count == "2-cue" & as.integer(reward_diff) == 3 ~ "#FCAE91",
    TRUE ~ "#777777"
  )
}

file_df <- resolve_duplicate_sessions(
  discover_subject_files(data_dir, subject_ids, session_ids)
)
if (nrow(file_df) == 0) {
  stop("No trial CSVs found for subjects ", paste(subject_ids, collapse = ","))
}

message(
  "Reading ", nrow(file_df), " session file(s) for subjects ",
  paste(sort(unique(file_df$subject_id)), collapse = ","), " / ", ses_tag
)

read_trial_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
  needed <- c(
    "Subject", "Session", "Block", "WarmUpTrial", "Response", "RT", "ACC",
    "CueValues", "CueCondition", "ResponseDevice", "BurnInBlock", "RTDifference",
    "ParticipationDay", "TrialWallClockTime"
  )
  for (col in needed) if (!(col %in% names(df))) df[[col]] <- NA_character_
  df
}

plot_df <- bind_rows(lapply(file_df$path, read_trial_csv)) %>%
  mutate(
    SubjectNumInt = suppressWarnings(as.integer(gsub("[^0-9]", "", Subject))),
    SessionNumInt = suppressWarnings(as.integer(Session)),
    DeviceLabel = label_response_device(ResponseDevice),
    Condition = mapply(get_condition, CueCondition, CueValues, USE.NAMES = FALSE),
    RewardDiff = vapply(Condition, get_reward_diff_from_condition, numeric(1)),
    CueCount = if_else(Condition %in% c("1", "2", "3", "4"), "1-cue", "2-cue"),
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
    RTDifference_num = suppressWarnings(as.numeric(RTDifference)),
    ParticipationDayNum = suppressWarnings(as.integer(ParticipationDay)),
    TrialDate = as.Date(substr(TrialWallClockTime, 1, 10), format = "%Y-%m-%d")
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
      !(SubjectNumInt == 6 & !is.na(RTDifference_num) &
          RTDifference_num > exclude_sub6_rtdiff_gt_ms)
    )
}

if (nrow(plot_df) == 0) stop("No trials remained after filtering.")

# Participation day: prefer logged ParticipationDay; else rank unique TrialDates.
day_from_date <- plot_df %>%
  filter(!is.na(TrialDate)) %>%
  distinct(SubjectNumInt, TrialDate) %>%
  arrange(SubjectNumInt, TrialDate) %>%
  group_by(SubjectNumInt) %>%
  mutate(DayFromDate = row_number()) %>%
  ungroup()

plot_df <- plot_df %>%
  left_join(day_from_date, by = c("SubjectNumInt", "TrialDate")) %>%
  mutate(
    DayIndex = if_else(
      !is.na(ParticipationDayNum) & ParticipationDayNum > 0,
      ParticipationDayNum,
      DayFromDate
    ),
    Condition = factor(Condition, levels = condition_levels),
    CueCount = factor(CueCount, levels = c("1-cue", "2-cue")),
    RewardDiff = factor(RewardDiff, levels = c(1, 2, 3)),
    SessionFacet = factor(
      paste0("ses", SessionNumInt),
      levels = paste0("ses", session_ids)
    )
  )

make_tradeoff_df <- function(df, facet_col) {
  df %>%
    group_by(.data[[facet_col]], Condition) %>%
    summarize(
      MeanRT = mean(RT_num, na.rm = TRUE),
      Accuracy = mean(ACC_num, na.rm = TRUE),
      Trials = n(),
      CueCount = dplyr::first(CueCount),
      RewardDiff = dplyr::first(RewardDiff),
      SessionsInFacet = paste(sort(unique(SessionNumInt)), collapse = ","),
      NSessions = n_distinct(SessionNumInt),
      .groups = "drop"
    ) %>%
    mutate(
      ConditionLabel = paste0("(", as.character(Condition), ")"),
      TradeoffColor = assign_tradeoff_color(
        as.character(CueCount),
        as.character(Condition),
        RewardDiff
      )
    )
}

make_sat_plot <- function(tradeoff_df, facet_col, title, subtitle, ncol) {
  ggplot(tradeoff_df, aes(x = MeanRT, y = Accuracy, group = 1)) +
    geom_point(aes(color = TradeoffColor), size = point_size) +
    geom_text(
      aes(label = ConditionLabel, color = TradeoffColor),
      vjust = -0.9,
      size = label_size,
      fontface = "bold"
    ) +
    scale_color_identity() +
    facet_wrap(as.formula(paste0("~", facet_col)), ncol = ncol, scales = "free") +
    labs(title = title, subtitle = subtitle, x = "Mean RT (ms)", y = "Accuracy") +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.12))) +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.22))) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = subtitle_size),
      axis.title = element_text(size = axis_title_size, face = "bold"),
      axis.text = element_text(size = axis_text_size, face = "bold"),
      strip.text = element_text(size = strip_text_size, face = "bold"),
      panel.spacing = unit(1.1, "lines")
    )
}

for (sid in subject_ids) {
  subj_df <- plot_df %>% filter(SubjectNumInt == sid)
  if (nrow(subj_df) == 0) {
    warning("sub", sid, ": no usable trials; skipping.", call. = FALSE)
    next
  }

  device_lab <- unique(subj_df$DeviceLabel)[1]
  sessions_present <- sort(unique(subj_df$SessionNumInt))

  # ---- 1) By session ----
  by_ses_df <- make_tradeoff_df(subj_df, "SessionFacet") %>%
    filter(as.integer(gsub("ses", "", as.character(SessionFacet))) %in% sessions_present)

  n_ses_facets <- n_distinct(by_ses_df$SessionFacet)
  ses_ncol <- min(session_facet_ncol, n_ses_facets)
  ses_nrow <- ceiling(n_ses_facets / ses_ncol)

  ses_plot <- make_sat_plot(
    by_ses_df,
    "SessionFacet",
    title = paste0(
      "Speed-Accuracy Tradeoff by Session (Relative Scales; sub", sid, ", ", ses_tag, ")"
    ),
    subtitle = paste0(
      "Device=", device_lab, ". One facet per session with data: ",
      paste(sessions_present, collapse = ", "),
      ". Axes free per session."
    ),
    ncol = ses_ncol
  )

  ses_file <- file.path(
    fig_dir,
    paste0("SpeedAccuracyTradeoff_bySession_RelativeScale_sub", sid, ".png")
  )
  ggsave(
    filename = ses_file,
    plot = ses_plot + bold_axes_theme,
    width = max(14, 5.5 * ses_ncol),
    height = max(8, 4.8 * ses_nrow),
    dpi = 300,
    limitsize = FALSE
  )
  message("Saved: ", ses_file)

  # ---- 2) By participation day (pool sessions within day) ----
  day_levels_subj <- sort(unique(subj_df$DayIndex[!is.na(subj_df$DayIndex)]))
  if (length(day_levels_subj) == 0) {
    warning("sub", sid, ": no participation-day info; skipping day plot.", call. = FALSE)
    next
  }

  day_session_summary <- subj_df %>%
    filter(!is.na(DayIndex)) %>%
    group_by(DayIndex) %>%
    summarize(
      Sessions = paste(sort(unique(SessionNumInt)), collapse = ","),
      NSessions = n_distinct(SessionNumInt),
      .groups = "drop"
    ) %>%
    arrange(DayIndex) %>%
    mutate(
      DayFacetLabel = paste0(
        "day", DayIndex, " (n_ses=", NSessions, "; ses ", Sessions, ")"
      ),
      DayFacet = factor(DayFacetLabel, levels = DayFacetLabel)
    )

  subj_day_df <- subj_df %>%
    filter(!is.na(DayIndex)) %>%
    left_join(day_session_summary %>% select(DayIndex, DayFacet), by = "DayIndex")

  by_day_df <- make_tradeoff_df(subj_day_df, "DayFacet")

  n_day_facets <- n_distinct(by_day_df$DayFacet)
  day_ncol <- min(day_facet_ncol, n_day_facets)
  day_nrow <- ceiling(n_day_facets / day_ncol)

  day_plot <- make_sat_plot(
    by_day_df,
    "DayFacet",
    title = paste0(
      "Speed-Accuracy Tradeoff by Participation Day (Relative Scales; sub", sid, ")"
    ),
    subtitle = paste0(
      "Device=", device_lab, ". Each facet pools all valid sessions on that day. ",
      "Axes free per day."
    ),
    ncol = day_ncol
  )

  day_file <- file.path(
    fig_dir,
    paste0("SpeedAccuracyTradeoff_byParticipationDay_RelativeScale_sub", sid, ".png")
  )
  ggsave(
    filename = day_file,
    plot = day_plot + bold_axes_theme,
    width = max(12, 7 * day_ncol),
    height = max(8, 5.5 * day_nrow),
    dpi = 300,
    limitsize = FALSE
  )
  message("Saved: ", day_file)

  cat(sprintf(
    "sub%d: sessions=%s; days=%s\n",
    sid,
    paste(sessions_present, collapse = ","),
    paste(day_levels_subj, collapse = ",")
  ))
}

cat("Done.\n")
