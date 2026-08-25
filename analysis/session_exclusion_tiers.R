# Session exclusion tiers for RT plotting pipelines.
# Tiers:
#   auto         — all EXCLUDE_SESSION rows from anomaly screen CSV (34 sessions)
#   conservative — hand-reviewed high-confidence exclusions (9 sessions)

load_excluded_sessions <- function(tier = c("auto", "conservative"), proj_root = NULL) {
  tier <- match.arg(tier)
  if (is.null(proj_root)) {
    if (dir.exists(file.path("exp", "data_from_lab"))) {
      proj_root <- normalizePath(".")
    } else if (dir.exists(file.path("..", "exp", "data_from_lab"))) {
      proj_root <- normalizePath("..")
    } else {
      stop("Cannot find project root for session exclusions.")
    }
  }

  if (tier == "conservative") {
    return(data.frame(
      subject = c(6L, rep(13L, 6L), 19L),
      session = c(11L, 12:17L, 16L),
      stringsAsFactors = FALSE
    ))
  }

  csv_path <- file.path(
    proj_root, "analysis", "fig", "fig_aug14",
    "session_anomaly_screen_sub1-32_ses6-17.csv"
  )
  if (!file.exists(csv_path)) {
    stop("Missing anomaly screen CSV: ", csv_path)
  }
  screen_df <- read.csv(csv_path, stringsAsFactors = FALSE)
  flagged <- screen_df[screen_df$action == "EXCLUDE_SESSION", c("subject", "session"), drop = FALSE]
  data.frame(
    subject = as.integer(flagged$subject),
    session = as.integer(flagged$session),
    stringsAsFactors = FALSE
  )
}

apply_session_exclusion_filter <- function(plot_df, tier = "auto", proj_root = NULL) {
  excl_df <- load_excluded_sessions(tier, proj_root)
  n_before <- nrow(plot_df)
  is_excluded <- paste(plot_df$SubjectNumInt, plot_df$SessionNumInt) %in%
    paste(excl_df$subject, excl_df$session)
  filtered <- plot_df[!is_excluded, , drop = FALSE]
  n_after <- nrow(filtered)

  message(
    "Session exclusion (tier=", tier, "): removed ", n_before - n_after,
    " trials from ", nrow(excl_df), " session(s)."
  )
  filtered
}
