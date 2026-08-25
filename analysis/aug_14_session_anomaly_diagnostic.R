# Session anomaly diagnostic — flagged pairs + full sub×session screen.
# Writes CSV summary to fig_aug14.
#
# Run: Rscript analysis/aug_14_session_anomaly_diagnostic.R

library(dplyr)

if (dir.exists("exp/data_from_lab")) {
  proj_root <- normalizePath(".")
} else if (dir.exists("../exp/data_from_lab")) {
  proj_root <- normalizePath("..")
} else {
  stop("Cannot find data.")
}

data_dir <- file.path(proj_root, "exp", "data_from_lab")

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

is_true_flag <- function(x) {
  tolower(trimws(as.character(x))) %in% c("1", "true", "t", "yes", "y")
}

is_timeout_response <- function(response) {
  r <- tolower(trimws(as.character(response)))
  is.na(r) | r == "" | r %in% c("timeout", "none")
}

read_session_trials <- function(sid, ses) {
  sub_dir <- file.path(data_dir, paste0("sub", sid))
  pat <- paste0("^CCRP_subj", sid, "_ses", ses, ".*_trials.*\\.csv$")
  paths <- list.files(sub_dir, pattern = pat, full.names = TRUE, ignore.case = TRUE)
  if (length(paths) == 0) return(NULL)
  path <- paths[which.max(file.info(paths)$size)]
  if (is.na(file.info(path)$size) || file.info(path)$size == 0) return(NULL)
  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)
  needed <- c(
    "Subject", "Session", "Block", "Trial", "WarmUpTrial", "Response", "RT", "ACC",
    "CueValues", "CueCondition", "BurnInBlock", "RTDifference", "CueRankResponse"
  )
  for (col in needed) if (!(col %in% names(df))) df[[col]] <- NA_character_
  df$source_file <- basename(path)
  df
}

filter_trials <- function(df, sid) {
  df %>%
    mutate(
      Condition = mapply(get_condition, CueCondition, CueValues, USE.NAMES = FALSE),
      RT_num = suppressWarnings(as.numeric(RT)),
      ACC_num = suppressWarnings(as.numeric(ACC)),
      WarmUpFlag = is_true_flag(WarmUpTrial),
      BurnInFlag = is_true_flag(BurnInBlock),
      TimeoutFlag = is_timeout_response(Response),
      RTDifference_num = suppressWarnings(as.numeric(RTDifference))
    ) %>%
    filter(!WarmUpFlag, !BurnInFlag, !TimeoutFlag) %>%
    filter(!is.na(RT_num), RT_num >= 0, RT_num <= 4000, !is.na(Condition)) %>%
    filter(!(sid == 6 & !is.na(RTDifference_num) & RTDifference_num > 500))
}

summarize_condition <- function(df) {
  df %>%
    group_by(Condition) %>%
    summarize(
      n = n(),
      median_rt = median(RT_num),
      mean_rt = mean(RT_num),
      sd_rt = sd(RT_num),
      q25 = quantile(RT_num, 0.25),
      q75 = quantile(RT_num, 0.75),
      acc = mean(ACC_num, na.rm = TRUE),
      max_rt = max(RT_num),
      min_rt = min(RT_num),
      .groups = "drop"
    ) %>%
    arrange(Condition)
}

outlier_influence <- function(df) {
  df %>%
    group_by(Condition) %>%
    group_modify(function(d, ...) {
      med <- median(d$RT_num)
      trimmed <- median(d$RT_num[abs(d$RT_num - med) <= 2.5 * mad(d$RT_num, constant = 1.4826)])
      top <- d %>% arrange(desc(RT_num)) %>% slice_head(n = 3)
      data.frame(
        n = nrow(d),
        median_rt = med,
        trimmed_median_rt = trimmed,
        median_minus_trimmed = med - trimmed,
        top3_rt = paste(round(top$RT_num, 1), collapse = ","),
        top3_acc = paste(top$ACC_num, collapse = ","),
        n_slow_gt_p90_session = sum(d$RT_num > quantile(d$RT_num, 0.9)),
        .groups = "drop"
      )
    }) %>%
    ungroup()
}

session_dispersion <- function(cond_sum) {
  if (nrow(cond_sum) < 2) return(list(iqr_range = NA, sd_across_medians = NA, cv = NA))
  list(
    iqr_range = max(cond_sum$median_rt, na.rm = TRUE) - min(cond_sum$median_rt, na.rm = TRUE),
    sd_across_medians = sd(cond_sum$median_rt, na.rm = TRUE),
    cv = sd(cond_sum$median_rt, na.rm = TRUE) / mean(cond_sum$median_rt, na.rm = TRUE)
  )
}

compare_to_other_sessions <- function(sid, target_ses, all_sessions = 6:17) {
  rows <- list()
  for (ses in all_sessions) {
    raw <- read_session_trials(sid, ses)
    if (is.null(raw)) next
    f <- filter_trials(raw, sid)
    cs <- summarize_condition(f)
    disp <- session_dispersion(cs)
    rows[[length(rows) + 1]] <- data.frame(
      session = ses,
      n_trials = nrow(f),
      mean_median_rt = mean(cs$median_rt),
      condition_spread = disp$sd_across_medians,
      condition_iqr_range = disp$iqr_range,
      mean_acc = mean(f$ACC_num, na.rm = TRUE)
    )
  }
  all_df <- bind_rows(rows)
  tgt <- all_df %>% filter(session == target_ses)
  if (nrow(tgt) == 0) return(NULL)
  all_df <- all_df %>%
    mutate(
      is_target = session == target_ses,
      z_spread = (condition_spread - mean(condition_spread)) / sd(condition_spread),
      z_mean_rt = (mean_median_rt - mean(mean_median_rt)) / sd(mean_median_rt)
    )
  list(target = tgt, all = all_df)
}

summarize_one_session <- function(sid, ses) {
  raw <- read_session_trials(sid, ses)
  if (is.null(raw)) return(NULL)
  filt <- filter_trials(raw, sid)
  if (nrow(filt) == 0) return(NULL)
  cond <- summarize_condition(filt)
  infl <- outlier_influence(filt)
  disp <- session_dispersion(cond)

  non_high_rate <- NA_real_
  error_fast_rate <- NA_real_
  if ("CueRankResponse" %in% names(filt)) {
    cr <- filt$CueRankResponse
    non_high_rate <- mean(cr != "1", na.rm = TRUE)
    err <- filt %>% filter(ACC_num == 0 | CueRankResponse != "1")
    if (nrow(err) > 0) {
      fast_thr <- quantile(filt$RT_num, 0.25, na.rm = TRUE)
      error_fast_rate <- mean(err$RT_num <= fast_thr, na.rm = TRUE)
    } else {
      error_fast_rate <- 0
    }
  }

  data.frame(
    subject = sid,
    session = ses,
    n_trials = nrow(filt),
    mean_median_rt = mean(cond$median_rt),
    condition_spread = disp$sd_across_medians,
    mean_acc = mean(filt$ACC_num, na.rm = TRUE),
    mean_outlier_pull = mean(infl$median_minus_trimmed, na.rm = TRUE),
    max_outlier_pull = max(infl$median_minus_trimmed, na.rm = TRUE),
    non_high_response_rate = non_high_rate,
    error_fast_rate = error_fast_rate,
    stringsAsFactors = FALSE
  )
}

recommend_exclusion <- function(row) {
  fast_guess <- !is.na(row$z_mean_rt) && row$z_mean_rt < -1.4 &&
    !is.na(row$z_spread) && row$z_spread < -1.0 &&
    !is.na(row$z_acc) && row$z_acc < -1.0
  low_acc <- !is.na(row$z_acc) && row$z_acc < -1.5
  low_acc_abs <- !is.na(row$mean_acc) && row$mean_acc < 0.85
  high_spread <- !is.na(row$z_spread) && row$z_spread > 2.0
  outlier_driven <- !is.na(row$mean_outlier_pull) && row$mean_outlier_pull > 80

  action <- "KEEP"
  reason <- "within normal range for this subject"

  if (fast_guess) {
    action <- "EXCLUDE_SESSION"
    reason <- "session-wide fast responding + collapsed condition spread + low accuracy (strategy shift)"
  } else if (low_acc || low_acc_abs) {
    action <- "EXCLUDE_SESSION"
    reason <- "low accuracy / non-high cue responding (engagement or strategy)"
  } else if (high_spread && !is.na(row$z_mean_rt) && row$z_mean_rt > 1.4) {
    action <- "SENSITIVITY"
    reason <- "slow session with unusually large condition spread; check fatigue"
  } else if (!is.na(row$z_mean_rt) && abs(row$z_mean_rt) > 1.8) {
    action <- "SENSITIVITY"
    reason <- "extreme session RT vs subject's other sessions"
  } else if (outlier_driven) {
    action <- "TRIAL_TRIM_CANDIDATE"
    reason <- "session pattern partly driven by extreme trials; try robust RT (trimmed mean/MAD filter)"
  }

  list(action = action, reason = reason)
}

screen_all_subjects <- function(subject_ids = 1:32, session_ids = 6:17) {
  rows <- list()
  for (sid in subject_ids) {
    for (ses in session_ids) {
      row <- summarize_one_session(sid, ses)
      if (!is.null(row)) rows[[length(rows) + 1]] <- row
    }
  }
  all_df <- bind_rows(rows)
  if (nrow(all_df) == 0) return(all_df)

  all_df %>%
    group_by(subject) %>%
    mutate(
      z_mean_rt = (mean_median_rt - mean(mean_median_rt)) / sd(mean_median_rt),
      z_spread = (condition_spread - mean(condition_spread)) / sd(condition_spread),
      z_acc = (mean_acc - mean(mean_acc)) / sd(mean_acc)
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      rec = list(recommend_exclusion(cur_data())),
      action = rec$action,
      reason = rec$reason
    ) %>%
    ungroup() %>%
    select(-rec)
}

fig_dir <- file.path(proj_root, "analysis", "fig", "fig_aug14")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

message("Screening all subject×session cells ...")
screen_df <- screen_all_subjects(1:32, 6:17)

out_csv <- file.path(fig_dir, "session_anomaly_screen_sub1-32_ses6-17.csv")
write.csv(screen_df, out_csv, row.names = FALSE)
message("Saved: ", out_csv)

flagged_actions <- screen_df %>%
  filter(action != "KEEP") %>%
  arrange(subject, session)

cat("\n=== Sessions flagged for action ===\n")
print(flagged_actions %>%
  select(subject, session, action, mean_acc, z_mean_rt, z_spread, reason))

cat("Diagnostic complete.\n")
