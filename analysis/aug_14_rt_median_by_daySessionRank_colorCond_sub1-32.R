# Median RT by session order within participation day, colored by condition.
# Each subject = one column of participation-day panels (Day 1, Day 2, ... stacked
# vertically). Subject blocks arranged in a 3x3 grid per page.
#
# Run from multiplecue-responsebox/ or analysis/:
#   Rscript analysis/aug_14_rt_median_by_daySessionRank_colorCond_sub1-32.R

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

# ---- CONFIG ----
subject_range <- 1:32
session_range <- 6:17
subjects_per_page <- 9L
rt_min_ms <- 0
rt_max_ms <- 4000
exclude_timeouts <- TRUE
exclude_warmup <- TRUE
exclude_burnin <- TRUE
exclude_sub6_rtdiff_gt_ms <- 500
subject_row_fills <- c("#f8f8f8", "#eef3fa")
subject_blocks_ncol <- 3L
output_name_tag <- "RTmedian_byDaySessionRank_colorCond_subjectDayCol"
if (!exists("apply_session_exclusions")) apply_session_exclusions <- FALSE
if (!exists("exclusion_tier")) exclusion_tier <- "auto"
if (!exists("output_file_suffix")) output_file_suffix <- ""
# ----------------

plot_base_size <- 13
title_size <- 17
subtitle_size <- 11
axis_title_size <- 13
axis_text_size <- 10
strip_text_size <- 11
point_size <- 2.2
line_width <- 0.65

rank_label <- function(rank) {
  rank <- as.integer(rank)
  suffix <- dplyr::case_when(
    rank %% 100 %in% 11:13 ~ "th",
    rank %% 10 == 1 ~ "st",
    rank %% 10 == 2 ~ "nd",
    rank %% 10 == 3 ~ "rd",
    TRUE ~ "th"
  )
  paste0(rank, suffix, " in day")
}

bold_axes_theme <- theme(
  axis.title = element_text(face = "bold"),
  axis.text = element_text(face = "bold")
)

build_session_participation_day <- function(trial_df) {
  day_from_date <- trial_df %>%
    filter(!is.na(TrialDate)) %>%
    distinct(SubjectNumInt, TrialDate) %>%
    arrange(SubjectNumInt, TrialDate) %>%
    group_by(SubjectNumInt) %>%
    mutate(DayFromDate = row_number()) %>%
    ungroup()

  trial_df %>%
    group_by(SubjectNumInt, SessionNumInt) %>%
    summarize(
      ParticipationDayLogged = {
        x <- ParticipationDayNum[!is.na(ParticipationDayNum) & ParticipationDayNum > 0]
        if (length(x) == 0) NA_integer_ else min(x)
      },
      TrialDate = min(TrialDate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(day_from_date, by = c("SubjectNumInt", "TrialDate")) %>%
    mutate(
      ParticipationDay = if_else(
        !is.na(ParticipationDayLogged) & ParticipationDayLogged > 0,
        ParticipationDayLogged,
        DayFromDate
      )
    ) %>%
    filter(!is.na(ParticipationDay)) %>%
    group_by(SubjectNumInt, ParticipationDay) %>%
    arrange(SessionNumInt, .by_group = TRUE) %>%
    mutate(
      SessionRankInDay = row_number(),
      NSessionsInDay = n()
    ) %>%
    ungroup()
}

if (dir.exists(file.path("exp", "data_from_lab"))) {
  proj_root <- normalizePath(".")
} else if (dir.exists(file.path("..", "exp", "data_from_lab"))) {
  proj_root <- normalizePath("..")
} else {
  stop("Cannot find exp/data_from_lab. Run from multiplecue-responsebox/ or analysis/.")
}

data_dir <- file.path(proj_root, "exp", "data_from_lab")
fig_dir <- file.path(proj_root, "analysis", "fig", "fig_aug14")
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
    csv_paths <- list.files(sub_dir, pattern = "\\.csv$", full.names = TRUE)
    for (path in csv_paths) {
      m <- regexec(trial_file_regex, basename(path), perl = TRUE, ignore.case = TRUE)
      hit <- regmatches(basename(path), m)[[1]]
      if (length(hit) == 0) next
      file_subj <- as.integer(hit[2])
      file_ses <- as.integer(hit[3])
      if (is.na(file_subj) || file_subj != sid || !(file_ses %in% session_ids)) next
      if (is.na(file.info(path)$size) || file.info(path)$size == 0) next
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
  file_df <- file_df %>% arrange(subject_id, session, desc(n_rows))
  file_df[!duplicated(paste(file_df$subject_id, file_df$session)), , drop = FALSE]
}

reward_sets <- list(
  "1" = c(1), "2" = c(2), "3" = c(3), "4" = c(4),
  "1,2" = c(1, 2), "1,3" = c(1, 3), "1,4" = c(1, 4),
  "2,3" = c(2, 3), "2,4" = c(2, 4), "3,4" = c(3, 4)
)
condition_levels <- names(reward_sets)
condition_labels <- paste0("(", condition_levels, ")")

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

assign_condition_color <- function(condition_chr, reward_diff) {
  single_val <- suppressWarnings(as.integer(condition_chr))
  is_single <- !grepl(",", condition_chr, fixed = TRUE)
  dplyr::case_when(
    is_single & single_val == 1 ~ "#08519C",
    is_single & single_val == 2 ~ "#3182BD",
    is_single & single_val == 3 ~ "#6BAED6",
    is_single & single_val == 4 ~ "#BDD7E7",
    !is_single & as.integer(reward_diff) == 1 ~ "#A50F15",
    !is_single & as.integer(reward_diff) == 2 ~ "#DE2D26",
    !is_single & as.integer(reward_diff) == 3 ~ "#FCAE91",
    TRUE ~ "#777777"
  )
}

is_true_flag <- function(x) {
  tolower(trimws(as.character(x))) %in% c("1", "true", "t", "yes", "y")
}

is_timeout_response <- function(response) {
  r <- tolower(trimws(as.character(response)))
  is.na(r) | r == "" | r %in% c("timeout", "none")
}

file_df <- resolve_duplicate_sessions(
  discover_subject_files(data_dir, subject_ids, session_ids)
)
if (nrow(file_df) == 0) stop("No trial CSVs found.")

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
    WarmUpFlag = is_true_flag(WarmUpTrial),
    BurnInFlag = is_true_flag(BurnInBlock),
    TimeoutFlag = is_timeout_response(Response),
    RT_num = suppressWarnings(as.numeric(RT)),
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
    !is.na(RT_num), RT_num >= rt_min_ms, RT_num <= rt_max_ms, !is.na(Condition)
  )

if (!is.na(exclude_sub6_rtdiff_gt_ms)) {
  plot_df <- plot_df %>%
    filter(
      !(SubjectNumInt == 6 & !is.na(RTDifference_num) &
          RTDifference_num > exclude_sub6_rtdiff_gt_ms)
    )
}

if (apply_session_exclusions) {
  tiers_path <- file.path(proj_root, "analysis", "session_exclusion_tiers.R")
  if (!file.exists(tiers_path)) stop("Missing session_exclusion_tiers.R")
  source(tiers_path, local = TRUE)
  plot_df <- apply_session_exclusion_filter(plot_df, exclusion_tier, proj_root)
}

if (nrow(plot_df) == 0) stop("No trials remained after filtering.")

session_day_df <- build_session_participation_day(plot_df)

rt_summary_df <- plot_df %>%
  group_by(SubjectNumInt, DeviceLabel, SessionNumInt, Condition) %>%
  summarize(
    MedianRT = median(RT_num, na.rm = TRUE),
    Trials = n(),
    .groups = "drop"
  ) %>%
  left_join(session_day_df, by = c("SubjectNumInt", "SessionNumInt")) %>%
  filter(!is.na(ParticipationDay)) %>%
  mutate(
    Condition = factor(Condition, levels = condition_levels),
    ConditionLabel = factor(
      paste0("(", as.character(Condition), ")"),
      levels = condition_labels
    ),
    SubjectFacet = paste0("sub", SubjectNumInt, "_", DeviceLabel),
    ParticipationDayFacet = factor(
      paste0("Day ", ParticipationDay),
      levels = paste0("Day ", sort(unique(ParticipationDay)))
    ),
    RewardDiff = vapply(as.character(Condition), get_reward_diff_from_condition, numeric(1)),
    ConditionColor = assign_condition_color(as.character(Condition), RewardDiff)
  )

condition_color_map <- rt_summary_df %>%
  distinct(ConditionLabel, ConditionColor) %>%
  { setNames(.$ConditionColor, as.character(.$ConditionLabel)) }

subjects_with_data <- sort(unique(rt_summary_df$SubjectNumInt))
message("Subjects with data: ", paste(subjects_with_data, collapse = ", "))

page_starts <- seq(min(subject_ids), max(subject_ids), by = subjects_per_page)

subject_y_limits <- function(values) {
  y_vals <- values[is.finite(values)]
  if (length(y_vals) == 0) return(c(0, 1))
  y_pad <- diff(range(y_vals))
  if (!is.finite(y_pad) || y_pad == 0) y_pad <- max(abs(y_vals), na.rm = TRUE) * 0.12
  c(min(y_vals) - 0.08 * y_pad, max(y_vals) + 0.08 * y_pad)
}

make_subject_day_col_plot <- function(
    sub_df, subject_label, rank_levels, block_fill, show_legend = FALSE) {
  day_levels <- sub_df %>%
    distinct(ParticipationDay) %>%
    arrange(ParticipationDay) %>%
    mutate(DayLabel = paste0("Part. day ", ParticipationDay)) %>%
    pull(DayLabel)

  sub_df <- sub_df %>%
    mutate(
      DayLabel = factor(
        paste0("Part. day ", ParticipationDay),
        levels = day_levels
      ),
      SessionRankLabel = factor(
        vapply(SessionRankInDay, rank_label, character(1)),
        levels = rank_levels
      )
    )

  y_lim <- subject_y_limits(sub_df$MedianRT)
  n_days <- length(day_levels)

  p <- ggplot(sub_df, aes(
    x = SessionRankLabel, y = MedianRT,
    color = ConditionLabel, group = ConditionLabel
  )) +
    geom_line(linewidth = line_width, alpha = 0.85) +
    geom_point(size = point_size) +
    scale_color_manual(values = condition_color_map, name = "Condition") +
    facet_wrap(~DayLabel, ncol = 1, nrow = n_days, scales = "fixed") +
    coord_cartesian(ylim = y_lim) +
    labs(
      title = subject_label,
      x = "Session in day",
      y = "Median RT (ms)"
    ) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(
        size = strip_text_size + 2, face = "bold", hjust = 0.5,
        margin = margin(b = 8, t = 4)
      ),
      plot.background = element_rect(fill = block_fill, color = "gray40", linewidth = 1),
      plot.margin = margin(t = 8, r = 8, b = 8, l = 8),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
      panel.spacing.y = unit(0.55, "lines"),
      strip.background = element_rect(fill = "gray85", color = "gray55", linewidth = 0.5),
      strip.text = element_text(size = strip_text_size, face = "bold"),
      axis.title = element_text(size = axis_title_size - 1, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.text = element_text(size = axis_text_size - 1, face = "bold"),
      axis.text.x = element_text(angle = 40, hjust = 1),
      legend.position = if (show_legend) "bottom" else "none",
      legend.key.width = unit(0.75, "cm")
    )

  p + bold_axes_theme
}

make_day_session_rank_plot <- function(page_df, page_tag, subjects_on_page) {
  present <- intersect(subjects_on_page, unique(page_df$SubjectNumInt))
  if (length(present) == 0) {
    warning(page_tag, ": no subjects with data on this page.", call. = FALSE)
    return(NULL)
  }

  subject_levels <- page_df %>%
    filter(SubjectNumInt %in% present) %>%
    distinct(SubjectNumInt, SubjectFacet) %>%
    arrange(SubjectNumInt) %>%
    pull(SubjectFacet)

  max_rank <- max(page_df$SessionRankInDay[page_df$SubjectNumInt %in% present], na.rm = TRUE)
  rank_levels <- vapply(seq_len(max_rank), rank_label, character(1))

  subject_grobs <- lapply(seq_along(subject_levels), function(i) {
    sf <- subject_levels[[i]]
    sub_df <- page_df %>% filter(SubjectFacet == sf)
    block_fill <- subject_row_fills[((i - 1L) %% length(subject_row_fills)) + 1L]
    ggplotGrob(make_subject_day_col_plot(
      sub_df, sf, rank_levels, block_fill, show_legend = FALSE
    ))
  })

  while (length(subject_grobs) %% subject_blocks_ncol != 0L) {
    subject_grobs <- c(subject_grobs, list(nullGrob()))
  }

  page_title <- paste0(
    "Median RT by session order within participation day (", page_tag, ", ", ses_tag, ")"
  )
  page_subtitle <- paste0(
    "Each block = one subject; participation days stacked vertically (Day 1, Day 2, ...). ",
    "X = session order within day (lowest session number = 1st). ",
    "Y scale shared across days within each subject. ",
    if (apply_session_exclusions) {
      paste0("Tier-1 excluded sessions removed (tier=", exclusion_tier, "). ")
    } else {
      ""
    },
    "Subjects: ",
    paste(subjects_on_page, collapse = ", ")
  )

  legend_plot <- ggplot(
    data.frame(
      ConditionLabel = factor(condition_labels, levels = condition_labels),
      x = 1,
      y = 1
    ),
    aes(x = x, y = y, color = ConditionLabel)
  ) +
    geom_point(size = 3) +
    scale_color_manual(values = condition_color_map, name = "Condition") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 3))) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = axis_text_size, face = "bold"),
      legend.title = element_text(size = axis_title_size, face = "bold")
    )

  legend_grob <- ggplotGrob(legend_plot)
  legend_idx <- which(legend_grob$layout$name == "guide-box")
  legend_only <- if (length(legend_idx) > 0) legend_grob$grobs[[legend_idx[[1]]]] else nullGrob()

  arrangeGrob(
    grobs = subject_grobs,
    ncol = subject_blocks_ncol,
    top = textGrob(
      page_title,
      gp = gpar(fontface = "bold", fontsize = title_size),
      x = 0.02, hjust = 0
    ),
    bottom = gridExtra::arrangeGrob(
      textGrob(
        page_subtitle,
        gp = gpar(fontsize = subtitle_size),
        x = 0.02, hjust = 0
      ),
      legend_only,
      ncol = 1,
      heights = c(unit(1.5, "lines"), unit(1, "null"))
    )
  )
}

for (start_id in page_starts) {
  end_id <- min(start_id + subjects_per_page - 1L, max(subject_ids))
  subjects_on_page <- start_id:end_id
  page_tag <- paste0("sub", start_id, "-", end_id)

  page_df <- rt_summary_df %>% filter(SubjectNumInt %in% subjects_on_page)
  page_grob <- make_day_session_rank_plot(page_df, page_tag, subjects_on_page)
  if (is.null(page_grob)) next

  n_subj <- length(intersect(subjects_on_page, subjects_with_data))
  max_days_page <- page_df %>%
    filter(SubjectNumInt %in% intersect(subjects_on_page, subjects_with_data)) %>%
    group_by(SubjectNumInt) %>%
    summarize(n_days = n_distinct(ParticipationDay), .groups = "drop") %>%
    pull(n_days) %>%
    max(na.rm = TRUE)
  max_days_page <- max(max_days_page, 1L)
  n_subj_rows <- ceiling(n_subj / subject_blocks_ncol)

  out_file <- file.path(
    fig_dir,
    paste0(output_name_tag, "_", page_tag, "_", ses_tag, output_file_suffix, ".png")
  )
  ggsave(
    filename = out_file,
    plot = page_grob,
    width = max(16, 5.4 * subject_blocks_ncol + 2),
    height = max(14, 2.75 * max_days_page * n_subj_rows + 5.5),
    dpi = 300,
    limitsize = FALSE
  )
  message("Saved: ", out_file)
}

cat("Done.\n")
