library(dplyr)

csv_dir <- "exp/data_from_lab/extracted data"
csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)

df_list <- lapply(csv_files, function(f) read.csv(f, stringsAsFactors = FALSE, colClasses = "character"))
combined_df <- bind_rows(df_list)

# Keep these fields as strings
target_cols <- c("Cues", "CueValues", "CueRanks", "RespLoc")
combined_df[target_cols] <- lapply(combined_df[target_cols], as.character)

# ------------------------------------------------------------
# 1) Separate experiment sessions vs non-experiment sessions
#    Experiment sessions: Session > 6
# ------------------------------------------------------------
combined_df <- combined_df %>%
  mutate(
    SessionNum = suppressWarnings(as.integer(Session)),
    SessionType = if_else(!is.na(SessionNum) & SessionNum > 6, "Experiment", "NonExperiment")
  )

experiment_df <- combined_df %>% filter(SessionType == "Experiment")
nonexperiment_df <- combined_df %>% filter(SessionType == "NonExperiment")

# ------------------------------------------------------------
# 2) Add Condition from CueValues (10 reward conditions)
#    Safe play #1: sorted nonzero rewards must match one of 10 sets
#    Safe play #2: original CueValues must match allowed slot patterns
# -----------------------------------------------------------
reward_sets <- list(
  "1"   = c(1),
  "2"   = c(2),
  "3"   = c(3),
  "4"   = c(4),
  "1,2" = c(1, 2),
  "1,3" = c(1, 3),
  "1,4" = c(1, 4),
  "2,3" = c(2, 3),
  "2,4" = c(2, 4),
  "3,4" = c(3, 4)
)

make_pattern_strings <- function(values) {
  all_digits <- c(values, rep(0, 4 - length(values)))

  permute_unique <- function(x) {
    if (length(x) == 1) return(list(x))
    out <- list()
    used <- c()
    for (i in seq_along(x)) {
      if (x[i] %in% used) next
      used <- c(used, x[i])
      rest <- x[-i]
      rest_perms <- permute_unique(rest)
      for (p in rest_perms) out[[length(out) + 1]] <- c(x[i], p)
    }
    out
  }

  perms <- permute_unique(all_digits)
  unique(vapply(perms, function(p) paste0(p, collapse = ""), character(1)))
}

pattern_map <- lapply(reward_sets, make_pattern_strings)

get_condition <- function(cue_values_string) {
  digits <- gsub("[^0-9]", "", cue_values_string)
  if (nchar(digits) != 4) return(NA_character_)

  vals <- as.integer(strsplit(digits, "")[[1]])
  nonzero_sorted <- sort(vals[vals != 0])
  key <- paste(nonzero_sorted, collapse = ",")

  # Safe play #1
  if (!(key %in% names(reward_sets))) return(NA_character_)
  # Safe play #2
  if (!(digits %in% pattern_map[[key]])) return(NA_character_)

  key
}

combined_df <- combined_df %>%
  mutate(Condition = vapply(CueValues, get_condition, character(1)))

# ------------------------------------------------------------
# 3) Exclude warmup trials, then summarize average n trials per
#    session for each Condition 
# ------------------------------------------------------------
analysis_df <- combined_df %>%
  mutate(WarmUpFlag = tolower(trimws(WarmUpTrial))) %>%
  filter(!(WarmUpFlag %in% c("1", "true", "t", "yes", "y")))

session_counts <- analysis_df %>%
  group_by(SessionNum, Condition) %>%
  summarize(n = n(), .groups = "drop")

avg_trials_per_session_by_condition <- session_counts %>%
  group_by(Condition) %>%
  summarize(avg_n_per_session = mean(n), .groups = "drop")
