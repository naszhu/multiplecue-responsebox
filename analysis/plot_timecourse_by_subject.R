library(dplyr)
library(ggplot2)
library(gridExtra)

data_dir <- file.path("exp", "data_from_lab")
processed_dir <- file.path(data_dir, "extracted_data_processed")
fig_dir <- file.path("analysis", "fig")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

root_csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
root_csv_files <- root_csv_files[grepl("/sub[^/]+/CCRP_subj", root_csv_files, perl = TRUE)]
processed_csv_files <- list.files(processed_dir, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
csv_files <- c(root_csv_files, processed_csv_files)

valid_trial_file <- grepl("^CCRP_subj[0-9]+_ses[0-9]+(_trials)?\\.csv$", basename(csv_files))
csv_files <- csv_files[valid_trial_file]

if (length(csv_files) == 0) {
  stop("No trial CSV files found.")
}

read_trial_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
  missing_cols <- setdiff(
    c("Subject", "Session", "Block", "Trial", "WarmUpTrial", "Response", "RT", "ACC", "RTDifference", "ResponseDevice"),
    names(df)
  )
  for (col in missing_cols) df[[col]] <- NA_character_
  df
}

label_response_device <- function(response_device) {
  response_device_clean <- tolower(trimws(response_device))
  out <- gsub("[^A-Za-z0-9]+", "_", response_device)
  out[is.na(response_device_clean) | response_device_clean == ""] <- "UnknownDevice"
  out[grepl("cedrus|response[_ -]?box|response box", response_device_clean)] <- "RB"
  out[grepl("keyboard", response_device_clean)] <- "KB"
  out[grepl("self", response_device_clean)] <- "SRB1"
  out
}

window_stats_one_session <- function(df_session, window_size = 50, step = 10) {
  n <- nrow(df_session)
  if (n == 0) return(data.frame())

  if (n < window_size) {
    starts <- 1
  } else {
    starts <- seq(1, n - window_size + 1, by = step)
  }

  out <- lapply(seq_along(starts), function(i) {
    s <- starts[i]
    e <- min(n, s + window_size - 1)
    w <- df_session[s:e, , drop = FALSE]
    data.frame(
      WindowIdx = i,
      WindowStart = s,
      WindowEnd = e,
      WindowMidTrial = median(w$TrialNumInt, na.rm = TRUE),
      MeanRT = if (all(is.na(w$RT_num[w$ValidRT]))) NA_real_ else mean(w$RT_num[w$ValidRT], na.rm = TRUE),
      Accuracy = mean(w$ACC_num, na.rm = TRUE),
      OmissionRate = mean(w$IsTimeout, na.rm = TRUE)
    )
  })

  bind_rows(out)
}

combined_df <- bind_rows(lapply(csv_files, read_trial_csv)) %>%
  mutate(
    SubjectNum = suppressWarnings(as.integer(gsub("[^0-9]", "", Subject))),
    SessionNum = suppressWarnings(as.integer(Session)),
    BlockNum = suppressWarnings(as.integer(Block)),
    TrialNumInt = suppressWarnings(as.integer(Trial)),
    WarmUpFlag = tolower(trimws(WarmUpTrial)),
    ResponseDevice = if_else(
      SubjectNum == 1 & (is.na(ResponseDevice) | trimws(ResponseDevice) == ""),
      "keyboard",
      ResponseDevice
    ),
    DeviceLabel = label_response_device(ResponseDevice),
    ResponseClean = tolower(trimws(Response)),
    IsTimeout = is.na(ResponseClean) | ResponseClean == "" | grepl("timeout", ResponseClean),
    RT_num = suppressWarnings(as.numeric(RT)),
    ACC_num = suppressWarnings(as.numeric(ACC)),
    RTDifference_num = suppressWarnings(as.numeric(RTDifference))
  ) %>%
  filter(
    SubjectNum %in% 1:20,
    SessionNum %in% 6:17,
    !(WarmUpFlag %in% c("1", "true", "t", "yes", "y"))
  ) %>%
  mutate(
    RTInRange = !is.na(RT_num) & RT_num >= 0 & RT_num <= 4000,
    Sub6BadTiming = SubjectNum == 6 & !is.na(RTDifference_num) & RTDifference_num > 500,
    ValidRT = RTInRange & !IsTimeout & !Sub6BadTiming,
    SubjectFacet = paste0("sub", SubjectNum, "_", DeviceLabel)
  )

subject_order <- combined_df %>%
  distinct(SubjectNum, DeviceLabel) %>%
  mutate(DeviceRank = match(DeviceLabel, c("KB", "RB", "SRB1", "UnknownDevice"))) %>%
  arrange(DeviceRank, SubjectNum) %>%
  pull(SubjectNum)

subjects <- unique(subject_order)

for (sub in subjects) {
  sdat <- combined_df %>%
    filter(SubjectNum == sub) %>%
    arrange(SessionNum, TrialNumInt)

  if (nrow(sdat) == 0) next

  # 1) Block-wise performance (RT, accuracy, omission) with sessions faceted.
  block_df <- sdat %>%
    group_by(SessionNum, BlockNum) %>%
    summarize(
      MeanRT = if (all(is.na(RT_num[ValidRT]))) NA_real_ else mean(RT_num[ValidRT], na.rm = TRUE),
      Accuracy = mean(ACC_num, na.rm = TRUE),
      OmissionRate = mean(IsTimeout, na.rm = TRUE),
      .groups = "drop"
    )

  p_block_rt <- ggplot(block_df, aes(x = BlockNum, y = MeanRT, group = 1)) +
    geom_line(color = "#D62728", linewidth = 1.1) +
    geom_point(color = "#D62728", size = 2.2) +
    facet_wrap(~SessionNum, ncol = 2, scales = "free_y") +
    labs(title = paste0("Subject ", sub, " Block-wise RT"), x = "Block", y = "RT (ms)") +
    theme_minimal(base_size = 16) +
    theme(axis.title = element_text(face = "bold"), axis.text = element_text(face = "bold"), strip.text = element_text(face = "bold"))

  p_block_acc <- ggplot(block_df, aes(x = BlockNum, y = Accuracy, group = 1)) +
    geom_line(color = "#1F77B4", linewidth = 1.1) +
    geom_point(color = "#1F77B4", size = 2.2) +
    facet_wrap(~SessionNum, ncol = 2, scales = "free_y") +
    labs(title = paste0("Subject ", sub, " Block-wise Accuracy"), x = "Block", y = "Accuracy") +
    theme_minimal(base_size = 16) +
    theme(axis.title = element_text(face = "bold"), axis.text = element_text(face = "bold"), strip.text = element_text(face = "bold"))

  p_block_omit <- ggplot(block_df, aes(x = BlockNum, y = OmissionRate, group = 1)) +
    geom_line(color = "#2CA02C", linewidth = 1.1) +
    geom_point(color = "#2CA02C", size = 2.2) +
    facet_wrap(~SessionNum, ncol = 2, scales = "free_y") +
    labs(title = paste0("Subject ", sub, " Block-wise Omission"), x = "Block", y = "Omission Rate") +
    theme_minimal(base_size = 16) +
    theme(axis.title = element_text(face = "bold"), axis.text = element_text(face = "bold"), strip.text = element_text(face = "bold"))

  block_combined <- gridExtra::arrangeGrob(p_block_rt, p_block_acc, p_block_omit, ncol = 1)
  ggsave(
    filename = file.path(fig_dir, paste0("block-wise_sub", sub, ".png")),
    plot = block_combined,
    width = 16,
    height = 24,
    dpi = 300
  )

  # 2) Moving-window analysis (RT, accuracy, omission) by session.
  win_df <- sdat %>%
    group_by(SessionNum) %>%
    group_split() %>%
    lapply(function(x) {
      x <- x %>% arrange(TrialNumInt)
      out <- window_stats_one_session(x, window_size = 50, step = 10)
      out$SessionNum <- unique(x$SessionNum)
      out
    }) %>%
    bind_rows()

  if (nrow(win_df) > 0) {
    p_win_rt <- ggplot(win_df, aes(x = WindowMidTrial, y = MeanRT, group = 1)) +
      geom_line(color = "#D62728", linewidth = 1.1) +
      geom_point(color = "#D62728", size = 2.0) +
      facet_wrap(~SessionNum, ncol = 2, scales = "free_y") +
      labs(title = paste0("Subject ", sub, " Moving-window RT"), x = "Trial (window midpoint)", y = "RT (ms)") +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(face = "bold"), axis.text = element_text(face = "bold"), strip.text = element_text(face = "bold"))

    p_win_acc <- ggplot(win_df, aes(x = WindowMidTrial, y = Accuracy, group = 1)) +
      geom_line(color = "#1F77B4", linewidth = 1.1) +
      geom_point(color = "#1F77B4", size = 2.0) +
      facet_wrap(~SessionNum, ncol = 2, scales = "free_y") +
      labs(title = paste0("Subject ", sub, " Moving-window Accuracy"), x = "Trial (window midpoint)", y = "Accuracy") +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(face = "bold"), axis.text = element_text(face = "bold"), strip.text = element_text(face = "bold"))

    p_win_omit <- ggplot(win_df, aes(x = WindowMidTrial, y = OmissionRate, group = 1)) +
      geom_line(color = "#2CA02C", linewidth = 1.1) +
      geom_point(color = "#2CA02C", size = 2.0) +
      facet_wrap(~SessionNum, ncol = 2, scales = "free_y") +
      labs(title = paste0("Subject ", sub, " Moving-window Omission"), x = "Trial (window midpoint)", y = "Omission Rate") +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(face = "bold"), axis.text = element_text(face = "bold"), strip.text = element_text(face = "bold"))

    win_combined <- gridExtra::arrangeGrob(p_win_rt, p_win_acc, p_win_omit, ncol = 1)
    ggsave(
      filename = file.path(fig_dir, paste0("movingwin_sub", sub, ".png")),
      plot = win_combined,
      width = 16,
      height = 24,
      dpi = 300
    )

    # 3) SAT evolution over time (moving-window RT-Accuracy, colored by session progression).
    sat_df <- win_df %>%
      mutate(SessionColorNum = as.numeric(SessionNum)) %>%
      filter(!is.na(MeanRT), !is.na(Accuracy))

    sat_plot <- ggplot(sat_df, aes(x = MeanRT, y = Accuracy, color = SessionColorNum)) +
      geom_path(aes(group = SessionNum), linewidth = 1.0, alpha = 0.5) +
      geom_point(size = 2.3, alpha = 0.9) +
      scale_color_gradient(low = "#A6CEE3", high = "#08306B") +
      labs(
        title = paste0("Subject ", sub, " SAT Evolution (Moving-window)"),
        subtitle = "Color gradient follows increasing session number",
        x = "Mean RT (ms)",
        y = "Accuracy",
        color = "Session"
      ) +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(face = "bold"), axis.text = element_text(face = "bold"))

    ggsave(
      filename = file.path(fig_dir, paste0("sat-evo_sub", sub, ".png")),
      plot = sat_plot,
      width = 12,
      height = 8,
      dpi = 300
    )
  }

  message("Saved timecourse plots for subject ", sub)
}
