# SAT (Speed-Accuracy Tradeoff) plots by session, subjects 1-32.
#
# Two plot types saved per page (9 subjects, 3x3 grid):
#
#  1. "SAT scatter" — per subject panel:
#     X = median RT (ms) across all trials in that session,
#     Y = mean accuracy (proportion correct),
#     point = one session, colored and labeled by session number.
#     A light grey reference curve (loess or identity-of-no-SAT line) is added.
#
#  2. "Standard SAT plot" — per subject panel:
#     X = session (ordered), Y = mean accuracy (left axis, proportion);
#     overlaid as secondary: median RT (right axis, ms).
#     Color = session. Lines connect across sessions.
#     This makes the within-subject session trend legible.
#
# Output saved in analysis/fig/fig_aug19/
# Run from multiplecue-responsebox/ or analysis/:
#   Rscript analysis/aug_19_sat_by_session_sub1-32.R

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

# ---- CONFIG ----
subject_range  <- 1:32
session_range  <- 6:17
subjects_per_page <- 9L
rt_min_ms     <- 0
rt_max_ms     <- 4000
exclude_timeouts <- TRUE
exclude_warmup   <- TRUE
exclude_burnin   <- TRUE
exclude_sub6_rtdiff_gt_ms <- 500
facet_ncol <- 3L
# ----------------

plot_base_size <- 14
title_size     <- 17
subtitle_size  <- 11
axis_title_size <- 13
axis_text_size  <- 10
strip_text_size <- 12
point_size <- 2.8
line_width <- 0.7

session_palette <- colorRampPalette(
  c("#084594", "#2171B5", "#6BAED6", "#BDD7E7",
    "#FDAE6B", "#F16913", "#D94801", "#7F2704")
)(12)

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
ses_tag <- paste0("ses", min(session_ids), "-", max(session_ids))

# ---- File discovery (same as other scripts) ----
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
      m   <- regexec(trial_file_regex, basename(path), perl = TRUE, ignore.case = TRUE)
      hit <- regmatches(basename(path), m)[[1]]
      if (length(hit) == 0) next
      file_subj <- as.integer(hit[2])
      file_ses  <- as.integer(hit[3])
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

is_true_flag <- function(x) {
  tolower(trimws(as.character(x))) %in% c("1", "true", "t", "yes", "y")
}
is_timeout_response <- function(response) {
  r <- tolower(trimws(as.character(response)))
  is.na(r) | r == "" | r %in% c("timeout", "none")
}

read_trial_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                 check.names = FALSE)
  needed <- c(
    "Subject", "Session", "Block", "WarmUpTrial", "Response", "RT", "ACC",
    "ResponseDevice", "BurnInBlock", "RTDifference", "TrialWallClockTime"
  )
  for (col in needed) if (!(col %in% names(df))) df[[col]] <- NA_character_
  df
}

# ---- Read & filter ----
file_df <- resolve_duplicate_sessions(
  discover_subject_files(data_dir, subject_ids, session_ids)
)
if (nrow(file_df) == 0) stop("No trial CSVs found.")

message(
  "Reading ", nrow(file_df), " session file(s) for subjects ",
  paste(sort(unique(file_df$subject_id)), collapse = ","), " / ", ses_tag
)

label_response_device <- function(rd) {
  rd_lc <- tolower(trimws(rd))
  lbl   <- gsub("[^A-Za-z0-9]+", "_", rd)
  lbl[is.na(rd_lc) | rd_lc == ""]                               <- "UnknownDevice"
  lbl[grepl("cedrus|response[_ -]?box|response box", rd_lc)]    <- "RB"
  lbl[grepl("keyboard", rd_lc)]                                  <- "KB"
  lbl[grepl("self", rd_lc)]                                      <- "SRB1"
  lbl
}

plot_df <- bind_rows(lapply(file_df$path, read_trial_csv)) %>%
  mutate(
    SubjectNumInt    = suppressWarnings(as.integer(gsub("[^0-9]", "", Subject))),
    SessionNumInt    = suppressWarnings(as.integer(Session)),
    DeviceLabel      = label_response_device(ResponseDevice),
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
  filter(!is.na(RT_num), RT_num >= rt_min_ms, RT_num <= rt_max_ms)

if (!is.na(exclude_sub6_rtdiff_gt_ms)) {
  plot_df <- plot_df %>%
    filter(
      !(SubjectNumInt == 6 & !is.na(RTDifference_num) &
          RTDifference_num > exclude_sub6_rtdiff_gt_ms)
    )
}

if (nrow(plot_df) == 0) stop("No trials remained after filtering.")

# ---- Session-level SAT summary ----
sat_df <- plot_df %>%
  group_by(SubjectNumInt, DeviceLabel, SessionNumInt) %>%
  summarize(
    MedianRT = median(RT_num, na.rm = TRUE),
    MeanACC  = mean(ACC_num, na.rm = TRUE),
    Trials   = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    SubjectFacet  = paste0("sub", SubjectNumInt, "_", DeviceLabel),
    SessionLabel  = factor(SessionNumInt, levels = session_ids),
    SessionColor  = session_palette[match(SessionNumInt, session_ids)]
  )

# named colour vector for scale_color_manual
session_color_map <- setNames(session_palette, as.character(session_ids))

subjects_with_data <- sort(unique(sat_df$SubjectNumInt))
message("Subjects with data: ", paste(subjects_with_data, collapse = ", "))

page_starts <- seq(min(subject_ids), max(subject_ids), by = subjects_per_page)

# ============================================================
# PLOT TYPE 1 — SAT scatter (RT vs ACC, colored by session)
# ============================================================
make_sat_scatter_page <- function(page_df, page_tag, subjects_on_page) {
  present <- intersect(subjects_on_page, unique(page_df$SubjectNumInt))
  if (length(present) == 0) {
    warning(page_tag, ": no data.", call. = FALSE)
    return(NULL)
  }

  facet_levels <- page_df %>%
    filter(SubjectNumInt %in% present) %>%
    distinct(SubjectNumInt, SubjectFacet) %>%
    arrange(SubjectNumInt) %>%
    pull(SubjectFacet)

  pdata <- page_df %>%
    filter(SubjectNumInt %in% present) %>%
    mutate(SubjectFacet = factor(SubjectFacet, levels = facet_levels))

  ggplot(pdata, aes(x = MedianRT, y = MeanACC,
                    color = SessionLabel, label = as.character(SessionNumInt))) +
    geom_point(size = point_size) +
    geom_text(size = 3, vjust = -0.6, show.legend = FALSE) +
    scale_color_manual(
      values = session_color_map,
      name   = "Session",
      guide  = guide_legend(nrow = 1, override.aes = list(size = 3))
    ) +
    scale_y_continuous(limits = c(NA, 1), labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(~SubjectFacet, ncol = facet_ncol, scales = "free") +
    labs(
      title    = paste0("SAT: Median RT vs Accuracy by session (", page_tag, ", ", ses_tag, ")"),
      subtitle = paste0(
        "Each point = one session. X = median RT (ms); Y = mean accuracy. ",
        "Point labels = session number. Subjects on page: ",
        paste(subjects_on_page, collapse = ", ")
      ),
      x = "Median RT (ms)",
      y = "Mean accuracy"
    ) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title    = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = subtitle_size),
      axis.title    = element_text(size = axis_title_size, face = "bold"),
      axis.text     = element_text(size = axis_text_size, face = "bold"),
      strip.text    = element_text(size = strip_text_size, face = "bold"),
      legend.position = "bottom",
      legend.key.width = unit(0.6, "cm"),
      panel.spacing = unit(1.0, "lines")
    )
}

# ============================================================
# PLOT TYPE 2 — Standard SAT: dual-axis RT + ACC across sessions
# ============================================================
make_sat_standard_page <- function(page_df, page_tag, subjects_on_page) {
  present <- intersect(subjects_on_page, unique(page_df$SubjectNumInt))
  if (length(present) == 0) return(NULL)

  facet_levels <- page_df %>%
    filter(SubjectNumInt %in% present) %>%
    distinct(SubjectNumInt, SubjectFacet) %>%
    arrange(SubjectNumInt) %>%
    pull(SubjectFacet)

  pdata <- page_df %>%
    filter(SubjectNumInt %in% present) %>%
    mutate(SubjectFacet = factor(SubjectFacet, levels = facet_levels))

  # Scale accuracy to RT range for dual-axis overlay.
  # We map [0, 1] acc -> [rt_min, rt_max] of subject's data,
  # then use sec_axis to show the real acc scale.
  # Because facet_wrap with free scales + sec_axis is complex,
  # we instead draw both on a single normalized axis per panel.
  # Approach: two geom layers sharing x=Session.
  #   - geom_line + geom_point in red for ACC (left y, 0-1)
  #   - geom_line + geom_point in blue for RT/1000 scaled to [0,1] per subject
  # Both shown on proportion [0,1]; RT labeled by annotation.
  # This keeps dual info visible without sec_axis facet issues.

  # Normalize RT to [0,1] within each subject (min-max)
  pdata <- pdata %>%
    group_by(SubjectNumInt) %>%
    mutate(
      RT_min = min(MedianRT, na.rm = TRUE),
      RT_max = max(MedianRT, na.rm = TRUE),
      RT_norm = dplyr::if_else(RT_max > RT_min, (MedianRT - RT_min) / (RT_max - RT_min), 0.5)
    ) %>%
    ungroup()

  # Long format for plotting
  long_df <- bind_rows(
    pdata %>% transmute(
      SubjectFacet, SubjectNumInt, SessionNumInt, SessionLabel,
      Metric = "Accuracy", Value = MeanACC
    ),
    pdata %>% transmute(
      SubjectFacet, SubjectNumInt, SessionNumInt, SessionLabel,
      Metric = "RT (norm)", Value = RT_norm
    )
  ) %>%
    mutate(Metric = factor(Metric, levels = c("Accuracy", "RT (norm)")))

  acc_df <- long_df %>% filter(Metric == "Accuracy")
  rt_df  <- long_df %>% filter(Metric == "RT (norm)")

  # Lines must have constant colour per geom_line call (ggplot2 restriction for
  # non-solid linetypes). Use fixed grey lines; session colour shown via points.
  ggplot(mapping = aes(x = SessionNumInt, y = Value, group = SubjectFacet)) +
    geom_line(data = acc_df, colour = "#333333",
              linewidth = line_width, linetype = "solid",  alpha = 0.55) +
    geom_line(data = rt_df,  colour = "#999999",
              linewidth = line_width, linetype = "dashed", alpha = 0.55) +
    geom_point(data = acc_df, aes(color = SessionLabel), size = point_size, shape = 16) +
    geom_point(data = rt_df,  aes(color = SessionLabel), size = point_size, shape = 17) +
    scale_color_manual(
      values = session_color_map, name = "Session",
      guide  = guide_legend(nrow = 1, override.aes = list(size = 3))
    ) +
    scale_x_continuous(breaks = session_ids) +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1),
      name   = "Accuracy  |  RT (min–max normalized)"
    ) +
    facet_wrap(~SubjectFacet, ncol = facet_ncol, scales = "fixed") +
    labs(
      title    = paste0("SAT by session: accuracy & RT trajectory (", page_tag, ", ", ses_tag, ")"),
      subtitle = paste0(
        "Solid line = accuracy (left axis, 0–100%); ",
        "Dashed line = median RT, min-max normalized within subject. ",
        "Both colored by session. Subjects: ",
        paste(subjects_on_page, collapse = ", ")
      ),
      x = "Session"
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
      legend.key.width = unit(0.6, "cm"),
      panel.spacing    = unit(1.0, "lines")
    )
}

# ============================================================
# Save pages
# ============================================================
for (start_id in page_starts) {
  end_id          <- min(start_id + subjects_per_page - 1L, max(subject_ids))
  subjects_on_page <- start_id:end_id
  page_tag        <- paste0("sub", start_id, "-", end_id)

  page_df <- sat_df %>% filter(SubjectNumInt %in% subjects_on_page)

  n_on_page <- length(intersect(subjects_on_page, subjects_with_data))
  n_row     <- ceiling(max(1, n_on_page) / facet_ncol)
  pw        <- max(16, 5.8 * facet_ncol)
  ph        <- max(10, 4.2 * n_row + 2.5)

  # --- Plot 1: SAT scatter ---
  p1 <- make_sat_scatter_page(page_df, page_tag, subjects_on_page)
  if (!is.null(p1)) {
    out1 <- file.path(fig_dir,
      paste0("SAT_scatter_bySession_", page_tag, "_", ses_tag, ".png"))
    ggsave(filename = out1, plot = p1 + bold_axes_theme,
           width = pw, height = ph, dpi = 300, limitsize = FALSE)
    message("Saved: ", out1)
  }

  # --- Plot 2: Standard SAT (dual-axis trajectory) ---
  p2 <- make_sat_standard_page(page_df, page_tag, subjects_on_page)
  if (!is.null(p2)) {
    out2 <- file.path(fig_dir,
      paste0("SAT_standard_bySession_", page_tag, "_", ses_tag, ".png"))
    ggsave(filename = out2, plot = p2 + bold_axes_theme,
           width = pw, height = ph, dpi = 300, limitsize = FALSE)
    message("Saved: ", out2)
  }
}

cat("Done.\n")
