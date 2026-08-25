# Conditional Accuracy Function (CAF) by session, subjects 1-32.
#
# Standard CAF method (Ridderinkhof et al., 2004):
#   For each subject x session:
#     1. Rank-order trials by RT
#     2. Divide into N equal-count quantile bins
#     3. Compute mean RT and proportion correct per bin
#     4. Plot points + smoothed line: X = mean RT, Y = P(correct)
#
# One curve per session (colored by session), one panel per subject.
# 3x3 subject grid per page (9 subjects/page).
#
# Run from multiplecue-responsebox/ or analysis/:
#   Rscript analysis/aug_19_sat_cumulative_bySession_sub1-32.R

library(dplyr)
library(ggplot2)

# ---- CONFIG ----
subject_range  <- 1:32
session_range  <- 6:17
subjects_per_page <- 9L
rt_min_ms      <- 130
rt_max_ms      <- 4000
exclude_timeouts <- TRUE
exclude_warmup   <- TRUE
exclude_burnin   <- TRUE
exclude_sub6_rtdiff_gt_ms <- 500
facet_ncol <- 3L
# Number of RT quantile bins for the CAF.
# Breakpoints are defined from the POOLED distribution across all sessions
# of that subject, so every session curve shares the same x-axis positions.
# More bins = smoother curve; 20 works well with ~400 trials/session.
n_rt_bins <- 30L
# ----------------

plot_base_size  <- 14
title_size      <- 17
subtitle_size   <- 11
axis_title_size <- 13
axis_text_size  <- 10
strip_text_size <- 12
line_width      <- 0.75

# Session colour palette: cool (ses 6) → warm (ses 17)
session_ids_all <- sort(unique(as.integer(session_range)))
n_ses <- length(session_ids_all)
session_palette <- colorRampPalette(
  c("#084594", "#2171B5", "#6BAED6", "#C6DBEF",
    "#FDAE6B", "#F16913", "#D94801", "#7F2704")
)(n_ses)
session_color_map <- setNames(session_palette, as.character(session_ids_all))

bold_axes_theme <- theme(
  axis.title = element_text(face = "bold"),
  axis.text  = element_text(face = "bold")
)

if (dir.exists(file.path("exp", "data_from_lab"))) {
  proj_root <- normalizePath(".")
} else if (dir.exists(file.path("..", "exp", "data_from_lab"))) {
  proj_root <- normalizePath("..")
} else {
  stop("Cannot find exp/data_from_lab. Run from multiplecue-responsebox/ or analysis/.")
}

data_dir <- file.path(proj_root, "exp", "data_from_lab")
fig_dir  <- file.path(proj_root, "analysis", "fig", "fig_aug19")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

subject_ids <- sort(unique(as.integer(subject_range)))
session_ids <- sort(unique(as.integer(session_range)))
ses_tag     <- paste0("ses", min(session_ids), "-", max(session_ids))

# ---- File discovery ----
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
      warning("sub", sid, ": folder missing; skipping.", call. = FALSE); next
    }
    found_sessions <- integer(0)
    csv_paths <- list.files(sub_dir, pattern = "\\.csv$", full.names = TRUE)
    for (path in csv_paths) {
      m   <- regexec(trial_file_regex, basename(path), perl = TRUE, ignore.case = TRUE)
      hit <- regmatches(basename(path), m)[[1]]
      if (length(hit) == 0) next
      file_subj <- as.integer(hit[2]); file_ses <- as.integer(hit[3])
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
      warning("sub", sid, ": missing session(s) ",
              paste(sort(missing_ses), collapse = ", "), "; ignored.", call. = FALSE)
    }
  }
  if (length(out) == 0) return(data.frame(
    path = character(), subject_id = integer(), session = integer(), n_rows = integer()
  ))
  bind_rows(out)
}

resolve_duplicate_sessions <- function(file_df) {
  if (nrow(file_df) == 0) return(file_df)
  file_df <- file_df %>% arrange(subject_id, session, desc(n_rows))
  file_df[!duplicated(paste(file_df$subject_id, file_df$session)), , drop = FALSE]
}

is_true_flag <- function(x) tolower(trimws(as.character(x))) %in% c("1","true","t","yes","y")
is_timeout_response <- function(r) { r <- tolower(trimws(as.character(r))); is.na(r)|r==""|r%in%c("timeout","none") }

label_response_device <- function(rd) {
  rd_lc <- tolower(trimws(rd)); lbl <- gsub("[^A-Za-z0-9]+","_",rd)
  lbl[is.na(rd_lc)|rd_lc==""] <- "UnknownDevice"
  lbl[grepl("cedrus|response[_ -]?box|response box", rd_lc)] <- "RB"
  lbl[grepl("keyboard", rd_lc)] <- "KB"
  lbl[grepl("self", rd_lc)] <- "SRB1"
  lbl
}

normalize_condition_key <- function(values) {
  values <- sort(as.integer(values[!is.na(values) & values != 0]))
  if (length(values) == 0) return(NA_character_)
  paste(values, collapse = ",")
}

get_condition <- function(cue_condition, cue_values) {
  reward_keys <- c("1","2","3","4","1,2","1,3","1,4","2,3","2,4","3,4")
  if (!is.na(cue_condition) && nzchar(trimws(cue_condition))) {
    digits <- gsub("[^0-9,]","",cue_condition)
    parts  <- strsplit(digits,",",fixed=TRUE)[[1]]; parts <- parts[nzchar(parts)]
    key <- normalize_condition_key(parts)
    if (!is.na(key) && key %in% reward_keys) return(key)
  }
  digits <- gsub("[^0-9]","",cue_values)
  if (nchar(digits) != 4) return(NA_character_)
  vals <- as.integer(strsplit(digits,"")[[1]])
  key  <- normalize_condition_key(vals[vals != 0])
  if (is.na(key) || !(key %in% reward_keys)) return(NA_character_)
  key
}

read_trial_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
  needed <- c("Subject","Session","Block","WarmUpTrial","Response","RT","ACC",
              "CueValues","CueCondition","ResponseDevice","BurnInBlock","RTDifference",
              "TrialWallClockTime","CueRankResponse","ExpectedReward")
  for (col in needed) if (!(col %in% names(df))) df[[col]] <- NA_character_
  df
}

# ---- Read & filter ----
file_df <- resolve_duplicate_sessions(discover_subject_files(data_dir, subject_ids, session_ids))
if (nrow(file_df) == 0) stop("No trial CSVs found.")
message("Reading ", nrow(file_df), " session file(s) / ", ses_tag)

plot_df <- bind_rows(lapply(file_df$path, read_trial_csv)) %>%
  mutate(
    SubjectNumInt    = suppressWarnings(as.integer(gsub("[^0-9]","",Subject))),
    SessionNumInt    = suppressWarnings(as.integer(Session)),
    DeviceLabel      = label_response_device(ResponseDevice),
    Condition        = mapply(get_condition, CueCondition, CueValues, USE.NAMES = FALSE),
    WarmUpFlag       = is_true_flag(WarmUpTrial),
    BurnInFlag       = is_true_flag(BurnInBlock),
    TimeoutFlag      = is_timeout_response(Response),
    RT_num           = suppressWarnings(as.numeric(RT)),
    ACC_num          = suppressWarnings(as.numeric(ACC)),
    RTDifference_num = suppressWarnings(as.numeric(RTDifference))
  ) %>%
  filter(SubjectNumInt %in% subject_ids, SessionNumInt %in% session_ids)

if (exclude_warmup)   plot_df <- plot_df %>% filter(!WarmUpFlag)
if (exclude_burnin)   plot_df <- plot_df %>% filter(!BurnInFlag)
if (exclude_timeouts) plot_df <- plot_df %>% filter(!TimeoutFlag)

plot_df <- plot_df %>%
  filter(!is.na(RT_num), RT_num >= rt_min_ms, RT_num <= rt_max_ms, !is.na(ACC_num))

if (!is.na(exclude_sub6_rtdiff_gt_ms)) {
  plot_df <- plot_df %>%
    filter(!(SubjectNumInt == 6 & !is.na(RTDifference_num) &
               RTDifference_num > exclude_sub6_rtdiff_gt_ms))
}

if (nrow(plot_df) == 0) stop("No trials remained after filtering.")

# ---- Prepare trial-level data for smooth CAF ----
# We plot geom_smooth(method="glm", family=binomial) directly on the raw
# binary ACC trials — this fits a logistic curve P(correct | RT) per session,
# giving the proper smooth S-shaped CAF. No binning needed.

sat_curves <- plot_df %>%
  mutate(
    SubjectFacet = paste0("sub", SubjectNumInt, "_", DeviceLabel),
    SessionLabel = factor(SessionNumInt, levels = session_ids)
  )

subjects_with_data <- sort(unique(sat_curves$SubjectNumInt))
message("Subjects with curves: ", paste(subjects_with_data, collapse = ", "))

# ---- Plotting ----
page_starts <- seq(min(subject_ids), max(subject_ids), by = subjects_per_page)

make_cumsat_page <- function(page_df, page_tag, subjects_on_page) {
  present <- intersect(subjects_on_page, unique(page_df$SubjectNumInt))
  if (length(present) == 0) { warning(page_tag, ": no data.", call. = FALSE); return(NULL) }

  facet_levels <- page_df %>%
    filter(SubjectNumInt %in% present) %>%
    distinct(SubjectNumInt, SubjectFacet) %>%
    arrange(SubjectNumInt) %>%
    pull(SubjectFacet)

  pdata <- page_df %>%
    filter(SubjectNumInt %in% present) %>%
    mutate(SubjectFacet = factor(SubjectFacet, levels = facet_levels))

  ggplot(pdata, aes(x = RT_num, y = ACC_num,
                    color = SessionLabel, group = SessionLabel)) +
    # Logistic regression fit: P(correct | RT) per session — the proper smooth CAF
    geom_smooth(method = "glm", method.args = list(family = binomial),
                formula = y ~ x, se = FALSE,
                linewidth = line_width + 0.15, alpha = 0.85) +
    scale_color_manual(
      values = session_color_map,
      name   = "Session",
      guide  = guide_legend(nrow = 1, override.aes = list(linewidth = 1.8, size = 2))
    ) +
    scale_x_continuous(labels = function(x) paste0(x)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(~SubjectFacet, ncol = facet_ncol, scales = "free") +
    labs(
      title    = paste0("CAF: Conditional Accuracy Function by session (", page_tag, ", ", ses_tag, ")"),
      subtitle = paste0(
        "Logistic CAF: fitted P(correct | RT) per session via logistic regression on raw trials. ",
        "One smooth curve per session, colored by session (blue=early, red=late). ",
        "Subjects on page: ", paste(subjects_on_page, collapse = ", ")
      ),
      x = "Reaction Time (ms)",
      y = "Proportion correct"
    ) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title    = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = subtitle_size),
      axis.title    = element_text(size = axis_title_size, face = "bold"),
      axis.text     = element_text(size = axis_text_size, face = "bold"),
      axis.text.x   = element_text(angle = 45, hjust = 1),
      strip.text    = element_text(size = strip_text_size, face = "bold"),
      legend.position  = "bottom",
      legend.key.width = unit(0.8, "cm"),
      panel.spacing    = unit(1.0, "lines")
    )
}

for (start_id in page_starts) {
  end_id           <- min(start_id + subjects_per_page - 1L, max(subject_ids))
  subjects_on_page <- start_id:end_id
  page_tag         <- paste0("sub", start_id, "-", end_id)

  page_df <- sat_curves %>% filter(SubjectNumInt %in% subjects_on_page)
  p <- make_cumsat_page(page_df, page_tag, subjects_on_page)
  if (is.null(p)) next

  n_on_page <- length(intersect(subjects_on_page, subjects_with_data))
  n_row     <- ceiling(max(1, n_on_page) / facet_ncol)

  out_file <- file.path(fig_dir,
    paste0("CAF_bySession_", page_tag, "_", ses_tag, ".png"))
  ggsave(filename = out_file, plot = p + bold_axes_theme,
         width = max(16, 5.8 * facet_ncol),
         height = max(10, 4.2 * n_row + 2.5),
         dpi = 300, limitsize = FALSE)
  message("Saved: ", out_file)
}

cat("Done.\n")
