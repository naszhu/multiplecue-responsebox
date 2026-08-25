# WHY THIS SCRIPT
# Before fitting free weights (freeW), check whether the exponential MIS weight
# rule is plausible. Uncued responses are ~0, so use high-vs-low choice on
# two-cue pairs only.
#
# Under Luce + exponential weights, log(P(L)/P(H)) ≈ log(ω_L/ω_H) should be
# ~linear in (H−L). So plot per participant:
#   x = H − L,  y = log(P(L)/P(H))
# Pairs sharing the same difference should line up, e.g.
#   (1,2) ≈ (2,3) ≈ (3,4)  and  (1,3) ≈ (2,4).
# Raw empirical log-odds are unstable when P ≈ 0/1, so also fit four Luce weights
# ω1..ω4 (ω1 fixed) by MLE and plot log(ω_r) vs r — exponential MIS predicts
# log(ω_r) = a + b·r (straight line). Non-exponential spacing (e.g. g(ω3)−g(ω2)
# much smaller than g(ω2)−g(ω1)) would explain why pair (2,3) is especially hard.
#
# Panel A (empirical): x = H - L, y = log(P(L)/P(H)) on two-cue trials
#   (cued high/low only; uncued trials excluded from odds).
# Panel B (fitted Luce weights): fit ω1..ω4 (ω1=1 fixed) by MLE on two-cue trials,
#   then plot log(ω_r) vs reward r. Exponential rule ⇒ straight line.
#
# Run from multiplecue-responsebox/ or analysis/:
#   Rscript analysis/aug_12_exponential_weight_diagnostic.R

library(dplyr)
library(ggplot2)

# ---- CONFIG ----
subject_range <- 1:32
session_range <- 6:17
plot_panels <- list(
  list(min = 1L, max = 16L, tag = "sub1-16"),
  list(min = 17L, max = 32L, tag = "sub17-32")
)
two_cue_levels <- c("1,2", "1,3", "1,4", "2,3", "2,4", "3,4")
log_odds_pseudocount <- 0.5
rt_min_ms <- 0
rt_max_ms <- 4000
exclude_timeouts <- TRUE
exclude_warmup <- TRUE
exclude_burnin <- TRUE
exclude_sub6_rtdiff_gt_ms <- 500
# ----------------

pair_colors <- c(
  "1,2" = "#4E79A7", "2,3" = "#59A14F", "3,4" = "#E15759",
  "1,3" = "#F28E2B", "2,4" = "#B07AA1", "1,4" = "#76B7B2"
)

plot_base_size <- 22
title_size <- 26
subtitle_size <- 18
axis_title_size <- 22
axis_text_size <- 18
strip_text_size <- 18
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
    if (!dir.exists(sub_dir)) next
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
        path = path, subject_id = sid, session = file_ses,
        n_rows = n_rows,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) == 0) {
    return(data.frame(
      path = character(), subject_id = integer(), session = integer(), n_rows = integer()
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

classify_two_cue_choice <- function(cue_rank_response) {
  rank <- suppressWarnings(as.integer(trimws(as.character(cue_rank_response))))
  dplyr::case_when(
    rank == 1L ~ "high",
    rank == 2L ~ "low",
    TRUE ~ NA_character_
  )
}

parse_pair_rewards <- function(cond_chr) {
  parts <- as.integer(strsplit(cond_chr, ",", fixed = TRUE)[[1]])
  if (length(parts) != 2 || any(is.na(parts))) {
    return(list(H = NA_integer_, L = NA_integer_, delta = NA_real_))
  }
  list(H = max(parts), L = min(parts), delta = max(parts) - min(parts))
}

file_df <- resolve_duplicate_sessions(discover_subject_files(data_dir, subject_ids, session_ids))
if (nrow(file_df) == 0) stop("No trial CSVs found.")

read_trial_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
  needed <- c(
    "Subject", "Session", "Block", "WarmUpTrial", "Response", "RT", "ACC",
    "CueValues", "CueCondition", "ResponseDevice", "BurnInBlock", "RTDifference",
    "CueRankResponse"
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
    ChoiceType = classify_two_cue_choice(CueRankResponse)
  ) %>%
  filter(SubjectNumInt %in% subject_ids, SessionNumInt %in% session_ids)

if (exclude_warmup) plot_df <- plot_df %>% filter(!WarmUpFlag)
if (exclude_burnin) plot_df <- plot_df %>% filter(!BurnInFlag)
if (exclude_timeouts) plot_df <- plot_df %>% filter(!TimeoutFlag)
plot_df <- plot_df %>%
  filter(!is.na(RT_num), RT_num >= rt_min_ms, RT_num <= rt_max_ms, !is.na(Condition))

if (!is.na(exclude_sub6_rtdiff_gt_ms)) {
  plot_df <- plot_df %>%
    filter(!(SubjectNumInt == 6 & !is.na(RTDifference_num) & RTDifference_num > exclude_sub6_rtdiff_gt_ms))
}

two_cue_df <- plot_df %>%
  filter(as.character(Condition) %in% two_cue_levels, !is.na(ChoiceType)) %>%
  mutate(
    ConditionChr = as.character(Condition),
    SubjectFacet = paste0("sub", SubjectNumInt, "_", DeviceLabel)
  )

pair_meta <- lapply(two_cue_levels, function(c) {
  p <- parse_pair_rewards(c)
  data.frame(
    ConditionChr = c,
    HighReward = p$H,
    LowReward = p$L,
    RewardDiff = p$delta,
    stringsAsFactors = FALSE
  )
})
pair_meta_df <- bind_rows(pair_meta)

two_cue_df <- two_cue_df %>%
  left_join(pair_meta_df, by = "ConditionChr")

# ---- Empirical log-odds by pair ----
empirical_df <- two_cue_df %>%
  group_by(SubjectNumInt, SubjectFacet, ConditionChr, RewardDiff, HighReward, LowReward) %>%
  summarize(
    n_high = sum(ChoiceType == "high"),
    n_low = sum(ChoiceType == "low"),
    n_total = n(),
    .groups = "drop"
  ) %>%
  mutate(
    P_high = (n_high + log_odds_pseudocount) / (n_high + n_low + 2 * log_odds_pseudocount),
    P_low = (n_low + log_odds_pseudocount) / (n_high + n_low + 2 * log_odds_pseudocount),
    LogOdds_LH = log(P_low / P_high),
    PairLabel = paste0("(", gsub(",", ",", ConditionChr), ")")
  )

# ---- Fit Luce weights ω1..ω4 (ω1=1 fixed) per subject ----
fit_luce_weights <- function(subj_df) {
  if (nrow(subj_df) < 5) return(rep(NA_real_, 4))

  negll <- function(log_w234) {
    w <- c(1, exp(log_w234[1]), exp(log_w234[2]), exp(log_w234[3]))
    ll <- 0
    for (i in seq_len(nrow(subj_df))) {
      H <- subj_df$HighReward[i]
      L <- subj_df$LowReward[i]
      wH <- w[H]
      wL <- w[L]
      if (subj_df$ChoiceType[i] == "high") {
        ll <- ll + log(wH / (wH + wL))
      } else {
        ll <- ll + log(wL / (wH + wL))
      }
    }
    -ll
  }

  opt <- tryCatch(
    optim(c(0, 0, 0), negll, method = "BFGS"),
    error = function(e) NULL
  )
  if (is.null(opt) || opt$convergence != 0) return(rep(NA_real_, 4))
  c(1, exp(opt$par))
}

weight_list <- list()
for (sid in sort(unique(two_cue_df$SubjectNumInt))) {
  subj_df <- two_cue_df %>% filter(SubjectNumInt == sid)
  facet <- unique(subj_df$SubjectFacet)[1]
  w <- fit_luce_weights(subj_df)
  for (r in 1:4) {
    weight_list[[length(weight_list) + 1L]] <- data.frame(
      SubjectNumInt = sid,
      SubjectFacet = facet,
      Reward = r,
      Omega = w[r],
      LogOmega = if (is.na(w[r]) || w[r] <= 0) NA_real_ else log(w[r]),
      stringsAsFactors = FALSE
    )
  }
}
weight_df <- bind_rows(weight_list)

make_logodds_plot <- function(emp_df, panel_tag, subjects_present) {
  facet_levels <- emp_df %>%
    distinct(SubjectFacet, SubjectNumInt) %>%
    arrange(SubjectNumInt) %>%
    pull(SubjectFacet)

  emp_df <- emp_df %>%
    mutate(
      SubjectFacet = factor(SubjectFacet, levels = facet_levels),
      ConditionChr = factor(ConditionChr, levels = two_cue_levels)
    )

  ggplot(emp_df, aes(x = RewardDiff, y = LogOdds_LH, color = ConditionChr)) +
    geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
    geom_point(aes(shape = ConditionChr), size = 3.2) +
    geom_text(aes(label = ConditionChr), vjust = -0.9, size = 3.2, show.legend = FALSE) +
    geom_smooth(
      aes(group = 1),
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "black",
      linewidth = 0.9,
      linetype = "dashed"
    ) +
    scale_color_manual(values = pair_colors, name = "Pair") +
    scale_shape_manual(values = c(16, 17, 15, 18, 8, 4), name = "Pair") +
    scale_x_continuous(breaks = c(1, 2, 3)) +
    facet_wrap(~SubjectFacet, ncol = 2, scales = "free_y") +
    labs(
      title = paste0("Empirical log(P(L)/P(H)) vs H−L (", panel_tag, ", ", ses_tag, ")"),
      subtitle = paste0(
        "Two-cue trials; uncued excluded. Pseudocount=", log_odds_pseudocount,
        ". Same ΔR should align: (1,2)≈(2,3)≈(3,4), (1,3)≈(2,4). Dashed = linear fit."
      ),
      x = expression(H - L),
      y = expression(log(P(L)/P(H)))
    ) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = subtitle_size),
      strip.text = element_text(size = strip_text_size, face = "bold"),
      legend.position = "bottom"
    )
}

make_logomega_plot <- function(w_df, panel_tag) {
  facet_levels <- w_df %>%
    distinct(SubjectFacet, SubjectNumInt) %>%
    arrange(SubjectNumInt) %>%
    pull(SubjectFacet)

  w_df <- w_df %>%
    mutate(SubjectFacet = factor(SubjectFacet, levels = facet_levels))

  ggplot(w_df, aes(x = Reward, y = LogOmega)) +
    geom_point(size = 3.2, color = "#4E79A7") +
    geom_line(color = "#4E79A7", linewidth = 1) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", linetype = "dashed") +
    scale_x_continuous(breaks = 1:4) +
    facet_wrap(~SubjectFacet, ncol = 2, scales = "free_y") +
    labs(
      title = paste0("Fitted log(ω_r) vs reward (", panel_tag, ", ", ses_tag, ")"),
      subtitle = paste0(
        "MLE Luce weights on two-cue trials (ω1=1 fixed). ",
        "Exponential MIS ⇒ log(ω_r) ≈ a + b·r (dashed line)."
      ),
      x = "Reward value r",
      y = expression(log(omega[r]))
    ) +
    theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      plot.subtitle = element_text(size = subtitle_size),
      strip.text = element_text(size = strip_text_size, face = "bold")
    )
}

for (panel in plot_panels) {
  subjects_in_panel <- sort(unique(
    empirical_df$SubjectNumInt[
      empirical_df$SubjectNumInt >= panel$min &
        empirical_df$SubjectNumInt <= panel$max
    ]
  ))
  if (length(subjects_in_panel) == 0) next

  emp_panel <- empirical_df %>% filter(SubjectNumInt %in% subjects_in_panel)
  w_panel <- weight_df %>% filter(SubjectNumInt %in% subjects_in_panel)

  n_facets <- length(subjects_in_panel)
  fig_h <- max(14, 3.2 * ceiling(n_facets / 2))

  logodds_file <- file.path(
    fig_dir, paste0(panel$tag, "_", ses_tag, "_LogOddsLH_vs_rewardDiff.png")
  )
  ggsave(
    logodds_file,
    make_logodds_plot(emp_panel, panel$tag, subjects_in_panel) + bold_axes_theme,
    width = 22, height = fig_h, dpi = 300, limitsize = FALSE
  )
  message("Saved: ", logodds_file)

  logomega_file <- file.path(
    fig_dir, paste0(panel$tag, "_", ses_tag, "_LogOmega_vs_reward.png")
  )
  ggsave(
    logomega_file,
    make_logomega_plot(w_panel, panel$tag) + bold_axes_theme,
    width = 22, height = fig_h, dpi = 300, limitsize = FALSE
  )
  message("Saved: ", logomega_file)
}

# ---- Concise console summary: linearity of log(ω_r) ----
cat("\n--- Exponential weight diagnostic (fitted log ω vs r) ---\n")
for (sid in sort(unique(weight_df$SubjectNumInt))) {
  sub_w <- weight_df %>% filter(SubjectNumInt == sid, !is.na(LogOmega))
  if (nrow(sub_w) < 4) {
    cat(sprintf("sub%-2d  skipped (fit failed)\n", sid))
    next
  }
  fit <- lm(LogOmega ~ Reward, data = sub_w)
  r2 <- summary(fit)$r.squared
  slope <- coef(fit)[2]
  cat(sprintf("sub%-2d  log(ω)~r: slope=%.3f  R²=%.3f  ω=(%.2f,%.2f,%.2f,%.2f)\n",
              sid, slope, r2,
              sub_w$Omega[1], sub_w$Omega[2], sub_w$Omega[3], sub_w$Omega[4]))
}

cat("Done.\n")
