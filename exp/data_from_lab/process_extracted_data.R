#!/usr/bin/env Rscript

suppressWarnings(suppressMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Install it with install.packages('jsonlite').")
  }
}))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- NULL
for (a in args) {
  if (startsWith(a, file_arg)) {
    script_path <- substring(a, nchar(file_arg) + 1L)
    break
  }
}
if (!is.null(script_path)) {
  script_path <- normalizePath(script_path, winslash = "/", mustWork = FALSE)
  script_dir <- dirname(script_path)
} else {
  script_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_dir_cwd <- file.path(cwd, "extracted data")
data_dir_repo <- file.path(cwd, "exp", "data_from_lab", "extracted data")
data_dir_script <- file.path(script_dir, "extracted data")

if (dir.exists(data_dir_cwd)) {
  data_from_lab_dir <- cwd
} else if (dir.exists(data_dir_repo)) {
  data_from_lab_dir <- file.path(cwd, "exp", "data_from_lab")
} else if (dir.exists(data_dir_script)) {
  data_from_lab_dir <- script_dir
} else {
  stop("Cannot locate 'extracted data'. Run from repo root or exp/data_from_lab, or place script in exp/data_from_lab.")
}

repo_root <- normalizePath(file.path(data_from_lab_dir, "..", ".."), winslash = "/", mustWork = TRUE)
input_dir <- file.path(data_from_lab_dir, "extracted data")
output_dir <- file.path(data_from_lab_dir, "extracted_data_processed")
metadata_template <- file.path(repo_root, "exp", "data_written", "CCRP_subj1ttt_ses1_metadata.json")
metadata_out <- file.path(output_dir, "xx_metadata.json")

if (!dir.exists(input_dir)) {
  stop(sprintf("Input folder not found: %s", input_dir))
}
if (!file.exists(metadata_template)) {
  stop(sprintf("Metadata template not found: %s", metadata_template))
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

allowed_conditions <- c(
  "1", "2", "3", "4",
  "2,1", "3,1", "4,1", "3,2", "4,2", "4,3"
)

to_condition_label <- function(cue_values) {
  digits <- as.integer(strsplit(as.character(cue_values), "", fixed = TRUE)[[1]])
  non_zero <- digits[digits > 0]
  if (length(non_zero) == 0) {
    return(NA_character_)
  }
  uniq_desc <- sort(unique(non_zero), decreasing = TRUE)
  cond <- paste(uniq_desc, collapse = ",")
  if (!(cond %in% allowed_conditions)) {
    return(NA_character_)
  }
  sprintf("(%s)", cond)
}

to_response_location <- function(cue_values, reward_value) {
  reward_value <- suppressWarnings(as.integer(reward_value))
  if (is.na(reward_value) || reward_value <= 0) {
    return(0L)
  }
  digits <- as.integer(strsplit(as.character(cue_values), "", fixed = TRUE)[[1]])
  idx <- which(digits == reward_value)
  if (length(idx) == 0) {
    return(0L)
  }
  as.integer(idx[1])
}

to_expected_reward <- function(cue_values) {
  digits <- as.integer(strsplit(as.character(cue_values), "", fixed = TRUE)[[1]])
  non_zero <- digits[digits > 0]
  if (length(non_zero) == 0) {
    return(0L)
  }
  as.integer(max(non_zero))
}

to_one_hot_location <- function(location_index) {
  idx <- suppressWarnings(as.integer(location_index))
  if (is.na(idx) || idx < 1L || idx > 4L) {
    return("0000")
  }
  bits <- c("0", "0", "0", "0")
  bits[idx] <- "1"
  paste(bits, collapse = "")
}

csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(csv_files) == 0) {
  stop(sprintf("No CSV files found in %s", input_dir))
}

for (f in csv_files) {
  # Read all columns as character so leading zeros in string codes are preserved.
  dat <- read.csv(
    f,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character"
  )

  required_cols <- c("CueCondition", "CueValues", "Reward", "PointTargetResponse", "ExpectedReward")
  missing_cols <- setdiff(required_cols, names(dat))
  if (length(missing_cols) > 0) {
    stop(sprintf("File %s is missing columns: %s", basename(f), paste(missing_cols, collapse = ", ")))
  }

  dat$CueCondition <- vapply(dat$CueValues, to_condition_label, character(1))
  dat$PointTargetResponse <- mapply(
    FUN = to_response_location,
    cue_values = dat$CueValues,
    reward_value = dat$Reward
  )
  dat$ExpectedReward <- vapply(dat$CueValues, to_expected_reward, integer(1))
  dat$RespLoc <- vapply(dat$PointTargetResponse, to_one_hot_location, character(1))
  dat$CueValues <- as.character(dat$CueValues)
  dat$RespLoc <- as.character(dat$RespLoc)

  out_file <- file.path(output_dir, basename(f))
  # Keep quoting enabled so values like "(2,1)" stay in one CSV column.
  write.csv(dat, out_file, row.names = FALSE, quote = TRUE)
}

meta <- jsonlite::fromJSON(metadata_template, simplifyVector = FALSE)
if (is.null(meta$column_definitions)) {
  stop("Template metadata does not contain 'column_definitions'.")
}

meta$column_definitions$CueCondition <- "** Label for reward-value combination among non-zero CueValues in this trial. Allowed conditions: (1), (2), (3), (4), (2,1), (3,1), (4,1), (3,2), (4,2), (4,3). Order is descending reward value and ignores location order."
meta$column_definitions$RespLoc <- "** Four-digit one-hot location code derived from PointTargetResponse (1-4): 1000/0100/0010/0001. Uses 0000 if no valid chosen location."
meta$column_definitions$PointTargetResponse <- "** Response location code (1-4) inferred from CueValues and Reward: the first slot index where CueValues equals Reward; 0 if timeout/Reward=0 or no match."
meta$column_definitions$ExpectedReward <- "** Highest possible reward in this trial, computed as max non-zero digit in CueValues (0 if no non-zero CueValues)."

jsonlite::write_json(meta, metadata_out, pretty = TRUE, auto_unbox = TRUE)

message(sprintf("Processed %d files into: %s", length(csv_files), output_dir))
message(sprintf("Wrote updated metadata to: %s", metadata_out))