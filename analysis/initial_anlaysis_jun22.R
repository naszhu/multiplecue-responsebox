library(dplyr)

# Resolve project root so this runs from repo root or multiplecue-responsebox/
if (dir.exists("exp/data_from_lab")) {
  project_root <- normalizePath(".")
} else if (dir.exists("multiplecue-responsebox/exp/data_from_lab")) {
  project_root <- normalizePath("multiplecue-responsebox")
} else {
  stop("Could not find exp/data_from_lab. Set working directory to multiplecue-responsebox/ or the repo root.")
}

data_dir <- file.path(project_root, "exp", "data_from_lab")

# Only subject folders named sub1, sub2, sub3, ... (no sub3star, subtest, etc.)
subject_dirs <- list.dirs(data_dir, full.names = FALSE, recursive = FALSE)
subject_dirs <- subject_dirs[grepl("^sub[0-9]+$", subject_dirs)]

trial_csv_files <- unlist(lapply(subject_dirs, function(sub_dir) {
  list.files(
    file.path(data_dir, sub_dir),
    pattern = "_trials\\.csv$",
    full.names = TRUE
  )
}))

if (length(trial_csv_files) == 0) {
  stop("No *_trials.csv files found in subject folders under ", data_dir)
}

read_trial_csv <- function(path) {
  if (file.info(path)$size == 0) {
    return(NULL)
  }
  read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
}

combined_df <- bind_rows(lapply(trial_csv_files, read_trial_csv))

# One row per Subject x DeviceName combination
subject_device <- combined_df %>%
  group_by(Subject, DeviceName) %>%
  summarize(.groups = "drop")

# Count distinct devices used by each subject, ordered by Subject
subject_device_counts <- subject_device %>%
  group_by(Subject) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  arrange(as.integer(Subject))

# Subjects who used more than 2 devices
multi_device_subjects <- subject_device_counts %>%
  filter(n >= 2) %>%
  arrange(as.integer(Subject), DeviceName)

cat("Subjects with more than 2 devices (Subject, DeviceName, n_devices):\n")
print(as.data.frame(multi_device_subjects), row.names = FALSE)
