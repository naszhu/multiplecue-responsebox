# Regenerate day-session-rank plots with Tier-1 (auto EXCLUDE_SESSION) exclusions.
# Run from multiplecue-responsebox/:
#   Rscript analysis/aug_14_rt_median_by_daySessionRank_colorCond_exclTier1_sub1-32.R

apply_session_exclusions <- TRUE
exclusion_tier <- "auto"
output_file_suffix <- "_exclTier1"

script_dir <- if (dir.exists("analysis")) "analysis" else "."
source(file.path(script_dir, "aug_14_rt_median_by_daySessionRank_colorCond_sub1-32.R"), local = FALSE)
