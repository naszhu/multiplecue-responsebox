# WHY THIS SCRIPT
# Put uncertainty on speed–accuracy (SAT) points without arbitrary error circles.
# For each condition within a session (and within a participation day):
#   - block-bootstrap resample trials (preferred over iid trial resampling because
#     trials are sequential / temporally dependent)
#   - recalculate (median RT, accuracy)
#   - repeat n_boot times
# Then plot a faint bootstrap cloud and a 95% ellipse around each SAT point so
# you can see whether e.g. (2,3) substantially overlaps (3,4), (1,2), etc.
#
# Also prints a within-participant ANOVA / logistic analysis:
#   RT  ~ Condition + Session + Condition:Session
#   ACC ~ Condition + Session + Condition:Session  (binomial GLM)
# Goal: is (2,3) consistently harder, or does its difficulty mostly change
# session-to-session? Compare Condition vs Condition×Session variance/SS.
#
# Bootstrap CSVs are cached in fig_aug12/. If cache (+ plot) already exist,
# bootstrap is skipped and the CSV is reused (set force_rebootstrap=TRUE to redo).
#
# Run from multiplecue-responsebox/ or analysis/:
#   Rscript analysis/aug_13_sat_bootstrap_uncertainty_sub12.R

library(dplyr)
library(ggplot2)

# ---- CONFIG ----
subjects_to_plot <- c(12)          # change to add more subjects
session_range <- 6:17
n_boot <- 1000L
boot_block_size <- 10L             # consecutive-trial blocks if Block is unavailable
force_rebootstrap <- FALSE         # TRUE = ignore existing bootstrap CSVs
rt_min_ms <- 0
rt_max_ms <- 4000
exclude_timeouts <- TRUE
exclude_warmup <- TRUE
exclude_burnin <- TRUE
session_facet_ncol <- 3
day_facet_ncol <- 2
ellipse_level <- 0.95
# ----------------

plot_base_size <- 16
title_size <- 22
subtitle_size <- 14
axis_title_size <- 16
axis_text_size <- 12
strip_text_size <- 14
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
      out[[length(out) + 1L]] <- data.frame(
        path = path, subject_id = sid, session = file_ses, n_rows = n_rows,
        stringsAsFactors = FALSE
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

# 95% covariance ellipse around bootstrap cloud points
cov_ellipse_df <- function(x, y, level = 0.95, n = 80) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 10) return(NULL)
  mu <- c(mean(x), mean(y))
  Sigma <- stats::cov(cbind(x, y))
  if (any(!is.finite(Sigma)) || det(Sigma) <= 1e-12) return(NULL)
  eig <- eigen(Sigma, symmetric = TRUE)
  if (any(eig$values <= 0)) return(NULL)
  r <- sqrt(stats::qchisq(level, df = 2))
  angles <- seq(0, 2 * pi, length.out = n)
  circle <- rbind(cos(angles), sin(angles))
  pts <- t(mu + r * eig$vectors %*% diag(sqrt(eig$values), 2) %*% circle)
  data.frame(x = pts[, 1], y = pts[, 2])
}

# Block bootstrap of (median RT, accuracy) for one condition cell
block_bootstrap_sat <- function(rt, acc, block_ids, n_boot) {
  n <- length(rt)
  if (n < 5) {
    return(data.frame(
      Boot = integer(), MedianRT = numeric(), Accuracy = numeric()
    ))
  }
  if (is.null(block_ids) || all(is.na(block_ids))) {
    block_ids <- ceiling(seq_len(n) / boot_block_size)
  }
  uniq_blocks <- sort(unique(block_ids))
  block_list <- lapply(uniq_blocks, function(b) which(block_ids == b))
  n_blocks <- length(block_list)
  if (n_blocks < 2) {
    # fall back to trial resampling if only one block
    out_rt <- numeric(n_boot)
    out_acc <- numeric(n_boot)
    for (b in seq_len(n_boot)) {
      idx <- sample.int(n, n, replace = TRUE)
      out_rt[b] <- median(rt[idx], na.rm = TRUE)
      out_acc[b] <- mean(acc[idx], na.rm = TRUE)
    }
    return(data.frame(Boot = seq_len(n_boot), MedianRT = out_rt, Accuracy = out_acc))
  }

  out_rt <- numeric(n_boot)
  out_acc <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    chosen <- sample.int(n_blocks, n_blocks, replace = TRUE)
    idx <- unlist(block_list[chosen], use.names = FALSE)
    out_rt[b] <- median(rt[idx], na.rm = TRUE)
    out_acc[b] <- mean(acc[idx], na.rm = TRUE)
  }
  data.frame(Boot = seq_len(n_boot), MedianRT = out_rt, Accuracy = out_acc)
}

# ---- Load data ----
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
    "Subject", "Session", "Block", "Trial", "WarmUpTrial", "Response", "RT", "ACC",
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
    BlockNum = suppressWarnings(as.integer(Block)),
    TrialNum = suppressWarnings(as.integer(Trial)),
    DeviceLabel = label_response_device(ResponseDevice),
    Condition = mapply(get_condition, CueCondition, CueValues, USE.NAMES = FALSE),
    RewardDiff = vapply(Condition, get_reward_diff_from_condition, numeric(1)),
    CueCount = if_else(Condition %in% c("1", "2", "3", "4"), "1-cue", "2-cue"),
    WarmUpFlag = is_true_flag(WarmUpTrial),
    BurnInFlag = is_true_flag(BurnInBlock),
    TimeoutFlag = is_timeout_response(Response),
    RT_num = suppressWarnings(as.numeric(RT)),
    ACC_num = suppressWarnings(as.numeric(ACC)),
    ParticipationDayNum = suppressWarnings(as.integer(ParticipationDay)),
    TrialDate = as.Date(substr(TrialWallClockTime, 1, 10), format = "%Y-%m-%d")
  ) %>%
  filter(SubjectNumInt %in% subject_ids, SessionNumInt %in% session_ids)

if (exclude_warmup) plot_df <- plot_df %>% filter(!WarmUpFlag)
if (exclude_burnin) plot_df <- plot_df %>% filter(!BurnInFlag)
if (exclude_timeouts) plot_df <- plot_df %>% filter(!TimeoutFlag)

plot_df <- plot_df %>%
  filter(
    !is.na(RT_num), RT_num >= rt_min_ms, RT_num <= rt_max_ms,
    !is.na(Condition), !is.na(ACC_num)
  )

if (nrow(plot_df) == 0) stop("No trials remained after filtering.")

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
  ) %>%
  arrange(SubjectNumInt, SessionNumInt, TrialNum)

run_bootstrap_for_facet <- function(df, facet_col, facet_val, condition_chr) {
  cell <- df %>%
    filter(
      as.character(.data[[facet_col]]) == facet_val,
      as.character(Condition) == condition_chr
    ) %>%
    arrange(SessionNumInt, TrialNum)

  if (nrow(cell) < 5) return(NULL)

  block_ids <- cell$BlockNum
  if (all(is.na(block_ids))) {
    block_ids <- ceiling(seq_len(nrow(cell)) / boot_block_size)
  }

  boot <- block_bootstrap_sat(cell$RT_num, cell$ACC_num, block_ids, n_boot)
  if (nrow(boot) == 0) return(NULL)

  boot %>%
    mutate(
      Facet = facet_val,
      Condition = condition_chr,
      ObsMedianRT = median(cell$RT_num, na.rm = TRUE),
      ObsAccuracy = mean(cell$ACC_num, na.rm = TRUE),
      NTrials = nrow(cell),
      CueCount = as.character(cell$CueCount[1]),
      RewardDiff = as.character(cell$RewardDiff[1])
    )
}

build_or_load_bootstrap <- function(subj_df, sid, facet_col, view_tag) {
  csv_path <- file.path(
    fig_dir,
    paste0("SAT_bootstrap_", view_tag, "_sub", sid, ".csv")
  )

  if (!force_rebootstrap && file.exists(csv_path)) {
    message("Reusing bootstrap cache: ", csv_path)
    return(read.csv(csv_path, stringsAsFactors = FALSE))
  }

  message("Running block bootstrap (n=", n_boot, ") for sub", sid, " / ", view_tag, " ...")
  facets <- sort(unique(as.character(subj_df[[facet_col]])))
  conds <- condition_levels
  pieces <- list()

  for (fv in facets) {
    for (cc in conds) {
      res <- run_bootstrap_for_facet(subj_df, facet_col, fv, cc)
      if (!is.null(res)) pieces[[length(pieces) + 1L]] <- res
    }
  }

  if (length(pieces) == 0) stop("No bootstrap cells for sub", sid, " / ", view_tag)
  boot_df <- bind_rows(pieces)
  write.csv(boot_df, csv_path, row.names = FALSE)
  message("Saved bootstrap CSV: ", csv_path)
  boot_df
}

make_point_ellipse_layers <- function(boot_df) {
  point_df <- boot_df %>%
    distinct(Facet, Condition, ObsMedianRT, ObsAccuracy, CueCount, RewardDiff, NTrials) %>%
    mutate(
      ConditionLabel = paste0("(", Condition, ")"),
      TradeoffColor = assign_tradeoff_color(CueCount, Condition, RewardDiff),
      Facet = factor(Facet, levels = unique(Facet))
    )

  # subsample bootstrap cloud for plotting (max 200 per cell)
  cloud_df <- boot_df %>%
    group_by(Facet, Condition) %>%
    group_modify(function(.x, .y) {
      n_keep <- min(200L, nrow(.x))
      .x[sample.int(nrow(.x), n_keep), , drop = FALSE]
    }) %>%
    ungroup() %>%
    left_join(
      point_df %>% select(Facet, Condition, TradeoffColor),
      by = c("Facet", "Condition")
    )

  ellipse_list <- list()
  keys <- distinct(boot_df, Facet, Condition)
  for (i in seq_len(nrow(keys))) {
    cell <- boot_df %>%
      filter(Facet == keys$Facet[i], Condition == keys$Condition[i])
    ell <- cov_ellipse_df(cell$MedianRT, cell$Accuracy, level = ellipse_level)
    if (is.null(ell)) next
    col <- assign_tradeoff_color(
      as.character(cell$CueCount[1]),
      as.character(keys$Condition[i]),
      as.character(cell$RewardDiff[1])
    )
    ellipse_list[[length(ellipse_list) + 1L]] <- ell %>%
      mutate(
        Facet = keys$Facet[i],
        Condition = keys$Condition[i],
        TradeoffColor = col
      )
  }
  ellipse_df <- if (length(ellipse_list) == 0) {
    data.frame(
      x = numeric(), y = numeric(), Facet = character(),
      Condition = character(), TradeoffColor = character()
    )
  } else {
    bind_rows(ellipse_list)
  }

  list(point = point_df, cloud = cloud_df, ellipse = ellipse_df)
}

make_sat_boot_plot <- function(layers, title, subtitle, ncol) {
  point_df <- layers$point
  cloud_df <- layers$cloud
  ellipse_df <- layers$ellipse

  facet_levels <- levels(point_df$Facet)
  if (is.null(facet_levels)) facet_levels <- unique(as.character(point_df$Facet))
  point_df$Facet <- factor(as.character(point_df$Facet), levels = facet_levels)
  cloud_df$Facet <- factor(as.character(cloud_df$Facet), levels = facet_levels)
  if (nrow(ellipse_df) > 0) {
    ellipse_df$Facet <- factor(as.character(ellipse_df$Facet), levels = facet_levels)
  }

  p <- ggplot() +
    geom_point(
      data = cloud_df,
      aes(x = MedianRT, y = Accuracy, color = TradeoffColor),
      alpha = 0.04, size = 0.7, show.legend = FALSE
    )

  if (nrow(ellipse_df) > 0) {
    p <- p +
      geom_path(
        data = ellipse_df,
        aes(x = x, y = y, group = interaction(Facet, Condition), color = TradeoffColor),
        linewidth = 0.7, alpha = 0.85, show.legend = FALSE
      )
  }

  p +
    geom_point(
      data = point_df,
      aes(x = ObsMedianRT, y = ObsAccuracy, color = TradeoffColor),
      size = 3.2
    ) +
    geom_text(
      data = point_df,
      aes(x = ObsMedianRT, y = ObsAccuracy, label = ConditionLabel, color = TradeoffColor),
      vjust = -0.9, size = 3.8, fontface = "bold", show.legend = FALSE
    ) +
    scale_color_identity() +
    facet_wrap(~Facet, ncol = ncol, scales = "free") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Median RT (ms)",
      y = "Accuracy"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.12))) +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.18))) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = subtitle_size),
      axis.title = element_text(size = axis_title_size, face = "bold"),
      axis.text = element_text(size = axis_text_size, face = "bold"),
      strip.text = element_text(size = strip_text_size, face = "bold"),
      panel.spacing = unit(1.0, "lines")
    )
}

print_condition_session_stats <- function(subj_df, sid) {
  cat("\n=== Within-subject Condition × Session analysis (sub", sid, ") ===\n", sep = "")

  dat <- subj_df %>%
    mutate(
      Condition = droplevels(Condition),
      Session = factor(SessionNumInt)
    )

  # RT: linear model / ANOVA (Type I via anova(lm); safer row lookup)
  rt_fit <- lm(RT_num ~ Condition * Session, data = dat)
  rt_tab <- as.data.frame(anova(rt_fit))
  pick_exact <- function(tab, name) {
    i <- which(rownames(tab) == name)[1]
    if (is.na(i)) return(c(SS = NA_real_, F = NA_real_, p = NA_real_, Df = NA_real_))
    c(
      SS = tab$`Sum Sq`[i],
      F = tab$`F value`[i],
      p = tab$`Pr(>F)`[i],
      Df = tab$Df[i]
    )
  }
  t_cond <- pick_exact(rt_tab, "Condition")
  t_ses <- pick_exact(rt_tab, "Session")
  t_int <- pick_exact(rt_tab, "Condition:Session")

  cat("RT ~ Condition * Session (ANOVA)\n")
  cat(sprintf(
    "  Condition:           SS=%.0f  F=%.2f  p=%.4g\n",
    t_cond["SS"], t_cond["F"], t_cond["p"]
  ))
  cat(sprintf(
    "  Session:             SS=%.0f  F=%.2f  p=%.4g\n",
    t_ses["SS"], t_ses["F"], t_ses["p"]
  ))
  cat(sprintf(
    "  Condition × Session: SS=%.0f  F=%.2f  p=%.4g\n",
    t_int["SS"], t_int["F"], t_int["p"]
  ))
  if (is.finite(t_cond["SS"]) && t_cond["SS"] > 0) {
    cat(sprintf(
      "  SS_int / SS_Condition = %.3f  (interaction relative to stable condition effect)\n",
      t_int["SS"] / t_cond["SS"]
    ))
    cat(sprintf(
      "  SS_int / (SS_Condition+SS_int) = %.3f\n",
      t_int["SS"] / (t_cond["SS"] + t_int["SS"])
    ))
  }

  # Accuracy: binomial GLM
  acc_fit <- glm(
    ACC_num ~ Condition * Session,
    data = dat,
    family = binomial()
  )
  acc_aov <- anova(acc_fit, test = "Chisq")
  # rows: NULL, Condition, Session, Condition:Session
  get_row <- function(nm) {
    rn <- rownames(acc_aov)
    hit <- which(rn == nm)
    if (length(hit) == 0) return(c(NA, NA, NA))
    c(
      Dev = acc_aov$Deviance[hit],
      Df = acc_aov$Df[hit],
      p = acc_aov$`Pr(>Chi)`[hit]
    )
  }
  r_cond <- get_row("Condition")
  r_ses <- get_row("Session")
  r_int <- get_row("Condition:Session")

  cat("ACC ~ Condition * Session (binomial GLM, LR tests)\n")
  cat(sprintf(
    "  Condition:           Dev=%.2f  df=%.0f  p=%.4g\n",
    r_cond["Dev"], r_cond["Df"], r_cond["p"]
  ))
  cat(sprintf(
    "  Session:             Dev=%.2f  df=%.0f  p=%.4g\n",
    r_ses["Dev"], r_ses["Df"], r_ses["p"]
  ))
  cat(sprintf(
    "  Condition × Session: Dev=%.2f  df=%.0f  p=%.4g\n",
    r_int["Dev"], r_int["Df"], r_int["p"]
  ))
  if (is.finite(r_cond["Dev"]) && r_cond["Dev"] > 0) {
    cat(sprintf(
      "  Dev_int / Dev_Condition = %.3f\n",
      r_int["Dev"] / r_cond["Dev"]
    ))
  }

  # Per two-cue condition: mean accuracy & median RT across sessions (stability snapshot)
  two_cue <- dat %>%
    filter(as.character(Condition) %in% c("1,2", "1,3", "1,4", "2,3", "2,4", "3,4")) %>%
    group_by(Condition, Session) %>%
    summarize(
      MedRT = median(RT_num, na.rm = TRUE),
      Acc = mean(ACC_num, na.rm = TRUE),
      .groups = "drop"
    )

  stab <- two_cue %>%
    group_by(Condition) %>%
    summarize(
      mean_MedRT = mean(MedRT),
      sd_MedRT = sd(MedRT),
      mean_Acc = mean(Acc),
      sd_Acc = sd(Acc),
      n_ses = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(sd_MedRT))

  cat("Two-cue condition stability across sessions (higher sd => more session-to-session change):\n")
  for (i in seq_len(nrow(stab))) {
    cat(sprintf(
      "  (%s)  MedRT mean=%.0f sd=%.1f | Acc mean=%.3f sd=%.3f  (n_ses=%d)\n",
      as.character(stab$Condition[i]),
      stab$mean_MedRT[i], stab$sd_MedRT[i],
      stab$mean_Acc[i], stab$sd_Acc[i],
      stab$n_ses[i]
    ))
  }
  cat("\n")
}

for (sid in subject_ids) {
  subj_df <- plot_df %>% filter(SubjectNumInt == sid)
  if (nrow(subj_df) == 0) {
    warning("sub", sid, ": no usable trials; skipping.", call. = FALSE)
    next
  }

  device_lab <- unique(subj_df$DeviceLabel)[1]
  sessions_present <- sort(unique(subj_df$SessionNumInt))

  # ---- Stats (always print; cheap) ----
  print_condition_session_stats(subj_df, sid)

  # ---- By session: bootstrap + plot ----
  ses_png <- file.path(
    fig_dir,
    paste0("SAT_bootstrapCloud_bySession_RelativeScale_sub", sid, ".png")
  )
  ses_boot <- build_or_load_bootstrap(subj_df, sid, "SessionFacet", "bySession")

  if (force_rebootstrap || !file.exists(ses_png)) {
    layers_ses <- make_point_ellipse_layers(ses_boot)
    n_facets <- n_distinct(layers_ses$point$Facet)
    ncol <- min(session_facet_ncol, n_facets)
    nrow_f <- ceiling(n_facets / ncol)

    ses_plot <- make_sat_boot_plot(
      layers_ses,
      title = paste0(
        "SAT with block-bootstrap 95% ellipses by Session (sub", sid, ", ", ses_tag, ")"
      ),
      subtitle = paste0(
        "Device=", device_lab, ". Faint points = bootstrap (median RT, Acc); ",
        "ellipse = 95% cov. n_boot=", n_boot, ", block≈", boot_block_size,
        " trials (or experiment Block)."
      ),
      ncol = ncol
    )
    ggsave(
      ses_png, ses_plot + bold_axes_theme,
      width = max(14, 5.5 * ncol),
      height = max(8, 4.8 * nrow_f),
      dpi = 300, limitsize = FALSE
    )
    message("Saved: ", ses_png)
  } else {
    message("Plot already exists, skip redraw: ", ses_png)
  }

  # ---- By participation day ----
  day_levels_subj <- sort(unique(subj_df$DayIndex[!is.na(subj_df$DayIndex)]))
  if (length(day_levels_subj) == 0) {
    warning("sub", sid, ": no day info; skipping day bootstrap/plot.", call. = FALSE)
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

  day_png <- file.path(
    fig_dir,
    paste0("SAT_bootstrapCloud_byParticipationDay_RelativeScale_sub", sid, ".png")
  )
  day_boot <- build_or_load_bootstrap(subj_day_df, sid, "DayFacet", "byParticipationDay")

  if (force_rebootstrap || !file.exists(day_png)) {
    layers_day <- make_point_ellipse_layers(day_boot)
    n_facets <- n_distinct(layers_day$point$Facet)
    ncol <- min(day_facet_ncol, n_facets)
    nrow_f <- ceiling(n_facets / ncol)

    day_plot <- make_sat_boot_plot(
      layers_day,
      title = paste0(
        "SAT with block-bootstrap 95% ellipses by Participation Day (sub", sid, ")"
      ),
      subtitle = paste0(
        "Device=", device_lab, ". Sessions pooled within day. ",
        "n_boot=", n_boot, "."
      ),
      ncol = ncol
    )
    ggsave(
      day_png, day_plot + bold_axes_theme,
      width = max(12, 7 * ncol),
      height = max(8, 5.5 * nrow_f),
      dpi = 300, limitsize = FALSE
    )
    message("Saved: ", day_png)
  } else {
    message("Plot already exists, skip redraw: ", day_png)
  }
}

cat("Done.\n")
