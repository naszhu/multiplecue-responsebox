library(dplyr)
library(ggplot2)

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
    SessionType = if_else(!is.na(SessionNum) & SessionNum >= 6 , "Experiment", "NonExperiment")
  ) %>% as_tibble()

experiment_df <- combined_df %>% filter(SessionType == "Experiment")
nonexperiment_df <- combined_df %>% filter(SessionType == "NonExperiment")

# ------------------------------------------------------------
# 2) Add Condition from CueValues (10 reward conditions)
#    Safe play #1: sorted nonzero rewards must match one of 10 set
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

# ------------------------------------------------------------
# RT distribution analysis by Condition
# ------------------------------------------------------------
rt_base_df <- combined_df %>%
  filter(SessionType == "Experiment") %>%
  filter(WarmUpTrial == "0") %>%
  filter(!is.na(Condition))

timeout_flag <- grepl("timeout", rt_base_df$Response, ignore.case = TRUE)
timeout_n <- sum(timeout_flag, na.rm = TRUE)
timeout_prop <- timeout_n / nrow(rt_base_df)

cat(sprintf("Timeout trials removed: %d / %d (%.2f%%)\n",
            timeout_n, nrow(rt_base_df), 100 * timeout_prop))

condition_levels <- names(reward_sets)

rt_no_timeout_df <- rt_base_df %>%
  filter(!grepl("timeout", Response, ignore.case = TRUE)) %>%
  mutate(RT_num = suppressWarnings(as.numeric(RT))) %>%
  filter(!is.na(RT_num)) %>% mutate(
    Condition = factor(Condition, levels = condition_levels),
    # DataGroup = factor(DataGroup, levels = names(group_definitions))
  )

rt_plot_df <- rt_no_timeout_df 

rt_distribution_plot <- ggplot(rt_plot_df, aes(x = RT_num)) +
  geom_histogram(bins = 30, fill = "#4C78A8", color = "white") +
  facet_wrap(~Condition, scales = "free_y") +
  labs(
    title = "RT Distribution by Cue Reward Condition",
    x = "Reaction Time (RT)",
    y = "Count"
  ) +
  coord_cartesian(xlim = c(0, 2000)) +
  theme_minimal()

# print(rt_distribution_plot)

fig_dir <- file.path("analysis", "fig")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
fig_file <- file.path(
  fig_dir,
  paste0("rt_distribution_by_condition_", format(Sys.Date(), "%Y-%m-%d"), ".png")
)
ggsave(filename = fig_file, plot = rt_distribution_plot, width = 12, height = 8, dpi = 300)

rt_density_plot <- ggplot(rt_plot_df, aes(x = RT_num)) +
  geom_density( alpha = 0.5, aes(color = Condition), linewidth = 0.6) +
  # facet_wrap(~Condition, scales = "free_y") +
  labs(
    title = "RT Density by Cue Reward Condition",
    x = "Reaction Time (RT)",
    y = "Density"
  ) +
  # coord_cartesian(xlim = c(0, 2000)) +
  theme_minimal()

# print(rt_density_plot)

density_fig_file <- file.path(
  fig_dir,
  paste0("rt_density_by_condition_ex", format(Sys.Date(), "%Y-%m-%d"), ".png")
)
ggsave(filename = density_fig_file, plot = rt_density_plot, width = 12, height = 8, dpi = 300)

# ------------------------------------------------------------
# Cumulative session-group RT density comparison
# ------------------------------------------------------------
condition_levels <- names(reward_sets)

group_definitions <- list(
  "Data1_6to8" = 6:8,
  "Data2_6to11" = 6:11,
  "Data3_6to14" = 11:14,
  "Data4_6to17" = 6:17
)

rt_no_timeout_df=rt_no_timeout_df%>% filter(SessionNum != 6)
data1 <- rt_no_timeout_df %>%
  filter(SessionNum %in% group_definitions[["Data1_6to8"]]) %>%
  transmute(RT_num, Condition, DataGroup = "Data1_6to8")

data2 <- rt_no_timeout_df %>%
  filter(SessionNum %in% group_definitions[["Data2_6to11"]]) %>%
  transmute(RT_num, Condition, DataGroup = "Data2_6to11")

data3 <- rt_no_timeout_df %>%
  filter(SessionNum %in% group_definitions[["Data3_6to14"]]) %>%
  transmute(RT_num, Condition, DataGroup = "Data3_6to14")

data4 <- rt_no_timeout_df %>%
  filter(SessionNum %in% group_definitions[["Data4_6to17"]]) %>%
  transmute(RT_num, Condition, DataGroup = "Data4_6to17")

cumulative_group_df <- bind_rows(data1, data2, data3, data4)

cumulative_group_df <- cumulative_group_df %>%
  mutate(
    Condition = factor(Condition, levels = condition_levels),
    DataGroup = factor(DataGroup, levels = names(group_definitions))
  )

rt_density_group_plot <- ggplot(
  cumulative_group_df,
  aes(
    x = RT_num,
    color = DataGroup,
    linetype = DataGroup,
    group = interaction(Condition, DataGroup)
  )
) +
  geom_density(linewidth = 0.9, adjust = 1) +
  facet_wrap(~Condition, scales = "free_y") +
  labs(
    title = "RT Density by Condition Across Cumulative Session Groups",
    x = "Reaction Time (RT)",
    y = "Density",
    color = "Cumulative Group"
  ) +
  scale_color_brewer(palette = "Dark2") +
  coord_cartesian(xlim = c(0, 2000)) +
  guides(linetype = guide_legend(title = "Cumulative Group")) +
  theme_minimal()

cumulative_density_fig_file <- file.path(
  fig_dir,
  paste0("rt_density_cumulative_groups_by_condition_3", format(Sys.Date(), "%Y-%m-%d"), ".png")
)
ggsave(filename = cumulative_density_fig_file, plot = rt_density_group_plot, width = 13, height = 9, dpi = 300)
