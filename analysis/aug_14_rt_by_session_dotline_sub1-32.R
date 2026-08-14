# Median RT by condition across sessions (dot + line), subjects 1-32.
# Each page: 9 subject panels (3x3). Y = ordered cue condition; X = median RT (ms);
# color = session; lines connect the same condition across sessions.
#
# Run from multiplecue-responsebox/ or analysis/:
#   Rscript analysis/aug_14_rt_by_session_dotline_sub1-32.R

library(dplyr)
library(ggplot2)

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
facet_ncol <- 3L
# ----------------

plot_base_size <- 14
title_size <- 18
subtitle_size <- 12
axis_title_size <- 14
axis_text_size <- 11
strip_text_size <- 13
point_size <- 2.2
line_width <- 0.65

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
    "CueValues", "CueCondition", "ResponseDevice", "BurnInBlock", "RTDifference"
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
    RTDifference_num = suppressWarnings(as.numeric(RTDifference))
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

if (nrow(plot_df) == 0) stop("No trials remained after filtering.")

rt_summary_df <- plot_df %>%
  group_by(SubjectNumInt, DeviceLabel, SessionNumInt, Condition) %>%
  summarize(
    MedianRT = median(RT_num, na.rm = TRUE),
    Trials = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Condition = factor(Condition, levels = condition_levels),
    ConditionLabel = factor(
      paste0("(", as.character(Condition), ")"),
      levels = condition_labels
    ),
    Session = factor(SessionNumInt, levels = session_ids),
    SubjectFacet = paste0("sub", SubjectNumInt, "_", DeviceLabel)
  )

subjects_with_data <- sort(unique(rt_summary_df$SubjectNumInt))
message("Subjects with data: ", paste(subjects_with_data, collapse = ", "))

# Pages of 9 subjects in numeric order (include slots even if no data)
page_starts <- seq(min(subject_ids), max(subject_ids), by = subjects_per_page)

session_colors <- grDevices::colorRampPalette(c(
  "#440154", "#31688e", "#35b779", "#fde725"
))(length(session_ids))
names(session_colors) <- as.character(session_ids)

make_session_dotline_plot <- function(page_df, page_tag, subjects_on_page) {
  present <- intersect(subjects_on_page, unique(page_df$SubjectNumInt))
  if (length(present) == 0) {
    warning(page_tag, ": no subjects with data on this page.", call. = FALSE)
    return(NULL)
  }

  facet_levels <- page_df %>%
    filter(SubjectNumInt %in% present) %>%
    distinct(SubjectNumInt, SubjectFacet) %>%
    arrange(SubjectNumInt) %>%
    pull(SubjectFacet)

  page_df <- page_df %>%
    filter(SubjectNumInt %in% present) %>%
    mutate(SubjectFacet = factor(as.character(SubjectFacet), levels = facet_levels))

  n_facets <- length(facet_levels)
  n_row <- ceiling(n_facets / facet_ncol)

  ggplot(page_df, aes(x = MedianRT, y = ConditionLabel, color = Session, group = Condition)) +
    geom_line(linewidth = line_width, alpha = 0.85) +
    geom_point(size = point_size) +
    scale_color_manual(values = session_colors, name = "Session") +
    facet_wrap(~SubjectFacet, ncol = facet_ncol, scales = "free_x") +
    labs(
      title = paste0(
        "Median RT by condition across sessions (", page_tag, ", ", ses_tag, ")"
      ),
      subtitle = paste0(
        "Lines connect the same condition across sessions. Subjects on page: ",
        paste(subjects_on_page, collapse = ", ")
      ),
      x = "Median RT (ms)",
      y = "Condition"
    ) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = subtitle_size),
      axis.title = element_text(size = axis_title_size, face = "bold"),
      axis.text = element_text(size = axis_text_size),
      axis.text.y = element_text(size = axis_text_size * 0.95),
      strip.text = element_text(size = strip_text_size, face = "bold"),
      legend.position = "bottom",
      legend.key.width = unit(0.8, "cm"),
      panel.spacing = unit(1.0, "lines")
    )
}

for (start_id in page_starts) {
  end_id <- min(start_id + subjects_per_page - 1L, max(subject_ids))
  subjects_on_page <- start_id:end_id
  page_tag <- paste0("sub", start_id, "-", end_id)

  page_df <- rt_summary_df %>% filter(SubjectNumInt %in% subjects_on_page)
  p <- make_session_dotline_plot(page_df, page_tag, subjects_on_page)
  if (is.null(p)) next

  n_on_page <- length(intersect(subjects_on_page, subjects_with_data))
  n_row <- ceiling(max(1, n_on_page) / facet_ncol)

  out_file <- file.path(
    fig_dir,
    paste0("RTbySession_dotline_", page_tag, "_", ses_tag, ".png")
  )
  ggsave(
    filename = out_file,
    plot = p + bold_axes_theme,
    width = max(16, 5.8 * facet_ncol),
    height = max(10, 4.2 * n_row + 1.2),
    dpi = 300,
    limitsize = FALSE
  )
  message("Saved: ", out_file)
}

cat("Done.\n")
