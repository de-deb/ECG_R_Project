
project_dir <- getwd()

data_file <- file.path(
  project_dir, "data", "processed", "all_records_features.csv"
)

results_dir <- file.path(project_dir, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_file)) {
  stop("Combined feature dataset not found. Run Script 12 first.")
}

data <- read.csv(data_file, stringsAsFactors = FALSE)

data$record_id <- as.character(data$record_id)
data$sample <- as.numeric(data$sample)
data$annotation <- as.character(data$annotation)

record_ids <- unique(data$record_id)
x_records <- record_ids[grepl("^x_", record_ids)]

checks <- list()

for (x_id in x_records) {
  original_id <- sub("^x_", "", x_id)

  if (!original_id %in% record_ids) next

  x_data <- data[data$record_id == x_id, ]
  original_data <- data[data$record_id == original_id, ]

  names(x_data)[names(x_data) == "annotation"] <- "annotation_x"
  names(original_data)[names(original_data) == "annotation"] <-
    "annotation_original"

  names(x_data)[names(x_data) == "RMS"] <- "RMS_x"
  names(original_data)[names(original_data) == "RMS"] <-
    "RMS_original"

  matched <- merge(
    x_data[, c("sample", "annotation_x", "RMS_x")],
    original_data[, c("sample", "annotation_original", "RMS_original")],
    by = "sample"
  )

  same_label <- if (nrow(matched) > 0) {
    sum(matched$annotation_x == matched$annotation_original)
  } else {
    0
  }

  rms_correlation <- if (
    nrow(matched) >= 3 &&
    sd(matched$RMS_x) > 0 &&
    sd(matched$RMS_original) > 0
  ) {
    cor(matched$RMS_x, matched$RMS_original)
  } else {
    NA_real_
  }

  checks[[length(checks) + 1]] <- data.frame(
    x_record = x_id,
    original_record = original_id,
    x_beats = nrow(x_data),
    original_beats = nrow(original_data),
    shared_sample_count = nrow(matched),
    same_annotation_count = same_label,
    annotation_match_percentage = if (nrow(matched) > 0) {
      100 * same_label / nrow(matched)
    } else {
      NA_real_
    },
    RMS_correlation = rms_correlation
  )
}

if (length(checks) > 0) {
  result <- do.call(rbind, checks)

  write.csv(
    result,
    file.path(results_dir, "paired_record_check.csv"),
    row.names = FALSE
  )

  cat("\n========== PAIRED RECORD CHECK ==========\n")
  print(result, row.names = FALSE, digits = 4)

  cat("\nSaved:", file.path(results_dir, "paired_record_check.csv"), "\n")
} else {
  cat("No x_ records with matching original record IDs were found.\n")
}
