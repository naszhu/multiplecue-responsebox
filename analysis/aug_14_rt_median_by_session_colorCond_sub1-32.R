# Median RT by session (dot + line), subjects 1-32.
# Each page: 9 subject panels (3x3). X = session (ordered); Y = median RT (ms);
# color = cue condition; lines connect the same condition across sessions.
# Black vertical lines mark the first session of each calendar day (from TrialWallClockTime).
# Also writes an emphasized variant: (1,2), (2,3), (3,4) in green shades; others unchanged.
# Also writes per-page 9-subject plots with 3 condition-group rows per subject
# (adjacent Δ=1, single cue, non-adjacent pairs).
#
# Run from multiplecue-responsebox/ or analysis/:
#   Rscript analysis/aug_14_rt_median_by_session_colorCond_sub1-32.R

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
facet_ncol <- 3L
if (!exists("apply_session_exclusions")) apply_session_exclusions <- FALSE
if (!exists("exclusion_tier")) exclusion_tier <- "auto"
if (!exists("output_file_suffix")) output_file_suffix <- ""
# ----------------

plot_base_size <- 14
title_size <- 18
subtitle_size <- 12
axis_title_size <- 14
axis_text_size <- 11
strip_text_size <- 13
point_size <- 2.4
line_width <- 0.7

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

emph_adjacent_condition_keys <- c("1,2", "2,3", "3,4")
emph_adjacent_green_colors <- c(
  "1,2" = "#006D2C",
  "2,3" = "#41AB5D",
  "3,4" = "#74C476"
)

assign_condition_color_emph_adjacent <- function(condition_chr, reward_diff) {
  if (condition_chr %in% emph_adjacent_condition_keys) {
    return(emph_adjacent_green_colors[[condition_chr]])
  }
  assign_condition_color(condition_chr, reward_diff)
}

condition_group_defs <- list(
  list(
    key = "adjacent_diff1",
    label = "Adjacent pairs (reward diff = 1)",
    conditions = c("1,2", "2,3", "3,4"),
    colors = c(
      "(1,2)" = "#004529",
      "(2,3)" = "#238B45",
      "(3,4)" = "#78C679"
    )
  ),
  list(
    key = "single",
    label = "Single-cue conditions",
    conditions = c("1", "2", "3", "4"),
    colors = c(
      "(1)" = "#053061",
      "(2)" = "#2166AC",
      "(3)" = "#4393C3",
      "(4)" = "#92C5DE"
    )
  ),
  list(
    key = "far_pairs",
    label = "Non-adjacent pairs",
    conditions = c("1,3", "2,4", "1,4"),
    colors = c(
      "(1,3)" = "#7F2704",
      "(2,4)" = "#E6550D",
      "(1,4)" = "#FDAE6B"
    )
  )
)
condition_group_order <- vapply(condition_group_defs, `[[`, character(1), "label")
condition_to_group_label <- setNames(
  rep(condition_group_order, vapply(condition_group_defs, function(g) length(g$conditions), integer(1))),
  unlist(lapply(condition_group_defs, `[[`, "conditions"))
)
condition_group_color_map <- unlist(lapply(condition_group_defs, `[[`, "colors"))
condition_group_color_map <- condition_group_color_map[!duplicated(names(condition_group_color_map))]

assign_condition_group_label <- function(condition_chr) {
  lbl <- condition_to_group_label[[condition_chr]]
  if (is.null(lbl)) NA_character_ else lbl
}

is_true_flag <- function(x) {
  tolower(trimws(as.character(x))) %in% c("1", "true", "t", "yes", "y")
}

is_timeout_response <- function(response) {
  r <- tolower(trimws(as.character(response)))
  is.na(r) | r == "" | r %in% c("timeout", "none")
}

build_first_session_of_day_from_timestamp <- function(trial_df, session_ids) {
  trial_df %>%
    filter(!is.na(TrialWallClockTime), nzchar(trimws(TrialWallClockTime))) %>%
    mutate(
      TrialDate = as.Date(substr(TrialWallClockTime, 1, 10), format = "%Y-%m-%d")
    ) %>%
    filter(!is.na(TrialDate)) %>%
    group_by(SubjectNumInt, DeviceLabel, SessionNumInt) %>%
    summarize(SessionDate = min(TrialDate, na.rm = TRUE), .groups = "drop") %>%
    group_by(SubjectNumInt, DeviceLabel, SessionDate) %>%
    slice_min(order_by = SessionNumInt, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      SubjectFacet = paste0("sub", SubjectNumInt, "_", DeviceLabel),
      Session = factor(SessionNumInt, levels = session_ids),
      VlineX = match(SessionNumInt, session_ids)
    )
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
    "TrialWallClockTime"
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

if (apply_session_exclusions) {
  tiers_path <- file.path(proj_root, "analysis", "session_exclusion_tiers.R")
  if (!file.exists(tiers_path)) stop("Missing session_exclusion_tiers.R")
  source(tiers_path, local = TRUE)
  plot_df <- apply_session_exclusion_filter(plot_df, exclusion_tier, proj_root)
}

if (nrow(plot_df) == 0) stop("No trials remained after filtering.")

day_start_vlines_df <- build_first_session_of_day_from_timestamp(plot_df, session_ids)

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
    SubjectFacet = paste0("sub", SubjectNumInt, "_", DeviceLabel),
    RewardDiff = vapply(as.character(Condition), get_reward_diff_from_condition, numeric(1)),
    ConditionColor = assign_condition_color(as.character(Condition), RewardDiff)
  )

condition_color_map <- rt_summary_df %>%
  distinct(ConditionLabel, ConditionColor) %>%
  { setNames(.$ConditionColor, as.character(.$ConditionLabel)) }

rt_summary_df <- rt_summary_df %>%
  mutate(
    ConditionColorEmphAdjacent = mapply(
      assign_condition_color_emph_adjacent,
      as.character(Condition),
      RewardDiff,
      USE.NAMES = FALSE
    )
  )

condition_color_map_emph_adjacent <- rt_summary_df %>%
  distinct(ConditionLabel, ConditionColorEmphAdjacent) %>%
  { setNames(.$ConditionColorEmphAdjacent, as.character(.$ConditionLabel)) }

rt_summary_df <- rt_summary_df %>%
  mutate(
    ConditionGroup = factor(
      vapply(as.character(Condition), assign_condition_group_label, character(1)),
      levels = condition_group_order
    ),
    ConditionGroupColor = condition_group_color_map[as.character(ConditionLabel)]
  )

condition_group_color_map_present <- rt_summary_df %>%
  distinct(ConditionLabel, ConditionGroupColor) %>%
  filter(!is.na(ConditionGroupColor)) %>%
  { setNames(.$ConditionGroupColor, as.character(.$ConditionLabel)) }

subjects_with_data <- sort(unique(rt_summary_df$SubjectNumInt))
message("Subjects with data: ", paste(subjects_with_data, collapse = ", "))

page_starts <- seq(min(subject_ids), max(subject_ids), by = subjects_per_page)

make_session_condition_plot <- function(
  page_df,
  day_start_vlines,
  page_tag,
  subjects_on_page,
  color_map = condition_color_map,
  title_suffix = "",
  subtitle_suffix = ""
) {
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

  page_day_starts <- day_start_vlines %>%
    filter(SubjectNumInt %in% present) %>%
    mutate(SubjectFacet = factor(as.character(SubjectFacet), levels = facet_levels))

  ggplot(page_df, aes(x = Session, y = MedianRT, color = ConditionLabel, group = ConditionLabel)) +
    geom_vline(
      data = page_day_starts,
      aes(xintercept = VlineX),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.5
    ) +
    geom_line(linewidth = line_width, alpha = 0.85) +
    geom_point(size = point_size) +
    scale_color_manual(values = color_map, name = "Condition") +
    facet_wrap(~SubjectFacet, ncol = facet_ncol, scales = "free_y") +
    labs(
      title = paste0(
        "Median RT by session, colored by condition",
        if (nzchar(title_suffix)) paste0(" (", title_suffix, ")") else "",
        " (", page_tag, ", ", ses_tag, ")"
      ),
      subtitle = paste0(
        "Lines connect the same condition across sessions. ",
        "Black vertical lines = first session of each calendar day (from TrialWallClockTime). ",
        if (nzchar(subtitle_suffix)) paste0(subtitle_suffix, " ") else "",
        if (apply_session_exclusions) {
          paste0("Tier-1 excluded sessions removed (tier=", exclusion_tier, "). ")
        } else {
          ""
        },
        "Subjects on page: ",
        paste(subjects_on_page, collapse = ", ")
      ),
      x = "Session",
      y = "Median RT (ms)"
    ) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = subtitle_size),
      axis.title = element_text(size = axis_title_size, face = "bold"),
      axis.text = element_text(size = axis_text_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = strip_text_size, face = "bold"),
      legend.position = "bottom",
      legend.key.width = unit(0.7, "cm"),
      panel.spacing = unit(1.0, "lines")
    )
}

extract_legend_grob <- function(plot) {
  gtable <- ggplotGrob(plot)
  legend_idx <- which(gtable$layout$name == "guide-box")
  if (length(legend_idx) == 0) return(grid::nullGrob())
  gtable$grobs[[legend_idx]]
}

make_page_condgroup_plot <- function(page_df, day_start_vlines, page_tag, subjects_on_page) {
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
    filter(SubjectNumInt %in% present, !is.na(ConditionGroup)) %>%
    mutate(
      SubjectFacet = factor(as.character(SubjectFacet), levels = facet_levels),
      ConditionGroup = factor(as.character(ConditionGroup), levels = condition_group_order)
    )

  if (nrow(page_df) == 0) return(NULL)

  subject_grid_info <- data.frame(
    SubjectFacet = facet_levels,
    GridIdx = seq_along(facet_levels),
    GridRow = ceiling(seq_along(facet_levels) / facet_ncol),
    GridCol = (seq_along(facet_levels) - 1L) %% facet_ncol + 1L,
    stringsAsFactors = FALSE
  )

  y_lim <- range(page_df$MedianRT, na.rm = TRUE)

  make_subject_condgroup_mini_plot <- function(
    subject_facet, show_legend = FALSE, show_x_title = FALSE, show_y_title = FALSE
  ) {
    subject_df <- page_df %>% filter(SubjectFacet == subject_facet)
    if (nrow(subject_df) == 0) {
      return(ggplot() + theme_void())
    }

    subject_day_starts <- day_start_vlines %>%
      filter(SubjectFacet == subject_facet) %>%
      tidyr::crossing(
        ConditionGroup = factor(condition_group_order, levels = condition_group_order)
      )

    ggplot(subject_df, aes(x = Session, y = MedianRT, color = ConditionLabel, group = ConditionLabel)) +
      geom_vline(
        data = subject_day_starts,
        aes(xintercept = VlineX),
        inherit.aes = FALSE,
        color = "black",
        linewidth = 0.5
      ) +
      geom_line(linewidth = line_width, alpha = 0.9) +
      geom_point(size = point_size - 0.2) +
      scale_color_manual(values = condition_group_color_map_present, name = "Condition") +
      coord_cartesian(ylim = y_lim) +
      facet_wrap(~ConditionGroup, ncol = 1, scales = "fixed") +
      labs(
        title = as.character(subject_facet),
        x = if (show_x_title) "Session" else NULL,
        y = if (show_y_title) "Median RT (ms)" else NULL
      ) +
      theme_minimal(base_size = plot_base_size - 2) +
      theme(
        plot.title = element_text(size = strip_text_size, face = "bold", hjust = 0.5),
        plot.margin = margin(2, 4, 2, 4),
        axis.title = element_text(size = axis_title_size - 1, face = "bold"),
        axis.text = element_text(size = axis_text_size - 2, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = strip_text_size - 2, face = "bold"),
        legend.position = if (show_legend) "bottom" else "none",
        legend.key.width = unit(0.7, "cm"),
        panel.spacing = unit(0.35, "lines")
      )
  }

  max_grid_row <- max(subject_grid_info$GridRow)
  subject_grobs <- lapply(facet_levels, function(subject_facet) {
    grid_row <- subject_grid_info$GridRow[subject_grid_info$SubjectFacet == subject_facet]
    grid_col <- subject_grid_info$GridCol[subject_grid_info$SubjectFacet == subject_facet]
    p <- make_subject_condgroup_mini_plot(
      subject_facet = subject_facet,
      show_x_title = grid_row == max_grid_row,
      show_y_title = grid_col == 1L
    )
    ggplotGrob(p + bold_axes_theme)
  })

  while (length(subject_grobs) < subjects_per_page) {
    subject_grobs[[length(subject_grobs) + 1L]] <- grid::nullGrob()
  }

  layout_matrix <- matrix(seq_len(subjects_per_page), nrow = 3, ncol = 3, byrow = TRUE)

  legend_plot <- make_subject_condgroup_mini_plot(
    subject_facet = facet_levels[[1]],
    show_legend = TRUE
  )
  legend_grob <- extract_legend_grob(legend_plot + bold_axes_theme)

  title_grob <- grid::textGrob(
    paste0(
      "Median RT by session, grouped by condition kind (", page_tag, ", ", ses_tag, ")"
    ),
    gp = grid::gpar(fontsize = title_size, fontface = "bold")
  )
  subtitle_grob <- grid::textGrob(
    paste0(
      "3x3 subjects; each subject = 3 rows (adjacent green, single blue, non-adjacent orange). ",
      "Black vertical lines = first session of each calendar day. ",
      if (apply_session_exclusions) {
        paste0("Tier-1 excluded sessions removed (tier=", exclusion_tier, "). ")
      } else {
        ""
      },
      "Subjects on page: ",
      paste(subjects_on_page, collapse = ", ")
    ),
    gp = grid::gpar(fontsize = subtitle_size)
  )

  gridExtra::arrangeGrob(
    grobs = c(subject_grobs, list(legend_grob)),
    layout_matrix = rbind(
      layout_matrix,
      rep(subjects_per_page + 1L, 3)
    ),
    heights = c(1, 1, 1, 0.18),
    top = gridExtra::arrangeGrob(title_grob, subtitle_grob, heights = c(0.55, 0.45))
  )
}

save_page_condgroup_plot <- function(p, page_tag, subjects_on_page) {
  out_file <- file.path(
    fig_dir,
    paste0("RTmedian_bySession_condGroup3row_", page_tag, "_", ses_tag, output_file_suffix, ".png")
  )
  ggsave(
    filename = out_file,
    plot = p,
    width = max(16, 5.8 * facet_ncol),
    height = max(18, 6.2 * facet_ncol + 2.5),
    dpi = 300,
    limitsize = FALSE
  )
  message("Saved: ", out_file)
}

plot_variants <- list(
  list(
    color_map = condition_color_map,
    title_suffix = "",
    subtitle_suffix = "",
    file_name_tag = "RTmedian_bySession_colorCond"
  ),
  list(
    color_map = condition_color_map_emph_adjacent,
    title_suffix = "emph (1,2), (2,3), (3,4) in green",
    subtitle_suffix = paste0(
      "(1,2), (2,3), (3,4) shown in green shades; other conditions keep original colors. "
    ),
    file_name_tag = "RTmedian_bySession_colorCond_emph12_23_34"
  )
)

save_session_condition_page <- function(p, page_tag, subjects_on_page, file_name_tag) {
  n_on_page <- length(intersect(subjects_on_page, subjects_with_data))
  n_row <- ceiling(max(1, n_on_page) / facet_ncol)

  out_file <- file.path(
    fig_dir,
    paste0(file_name_tag, "_", page_tag, "_", ses_tag, output_file_suffix, ".png")
  )
  ggsave(
    filename = out_file,
    plot = p + bold_axes_theme,
    width = max(16, 5.8 * facet_ncol),
    height = max(10, 4.2 * n_row + 1.8),
    dpi = 300,
    limitsize = FALSE
  )
  message("Saved: ", out_file)
}

for (start_id in page_starts) {
  end_id <- min(start_id + subjects_per_page - 1L, max(subject_ids))
  subjects_on_page <- start_id:end_id
  page_tag <- paste0("sub", start_id, "-", end_id)

  page_df <- rt_summary_df %>% filter(SubjectNumInt %in% subjects_on_page)

  for (variant in plot_variants) {
    p <- make_session_condition_plot(
      page_df,
      day_start_vlines_df,
      page_tag,
      subjects_on_page,
      color_map = variant$color_map,
      title_suffix = variant$title_suffix,
      subtitle_suffix = variant$subtitle_suffix
    )
    if (is.null(p)) next

    save_session_condition_page(p, page_tag, subjects_on_page, variant$file_name_tag)
  }

  p_group <- make_page_condgroup_plot(
    page_df, day_start_vlines_df, page_tag, subjects_on_page
  )
  if (!is.null(p_group)) {
    save_page_condgroup_plot(p_group, page_tag, subjects_on_page)
  }
}

cat("Done.\n")
