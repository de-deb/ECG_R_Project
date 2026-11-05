
project_dir <- getwd()

data_file <- file.path(
  project_dir, "data", "processed", "all_records_features.csv"
)

results_dir <- file.path(project_dir, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_file)) {
  stop("Combined feature CSV not found. Run Script 12 first.")
}

data <- read.csv(data_file, stringsAsFactors = FALSE)

required_columns <- c(
  "record_id", "sample", "time_seconds", "annotation", "channel",
  "baseline", "mean", "median", "sd", "variance", "min", "max",
  "range", "q1", "q3", "IQR", "RMS", "energy", "skewness", "kurtosis"
)

missing_columns <- setdiff(required_columns, names(data))

if (length(missing_columns) > 0) {
  stop(paste("Missing columns:", paste(missing_columns, collapse = ", ")))
}

numeric_columns <- c(
  "sample", "time_seconds", "baseline", "mean", "median", "sd",
  "variance", "min", "max", "range", "q1", "q3", "IQR", "RMS",
  "energy", "skewness", "kurtosis"
)

missing_summary <- data.frame(
  column = names(data),
  missing_count = vapply(data, function(x) sum(is.na(x)), integer(1)),
  stringsAsFactors = FALSE
)

write.csv(
  missing_summary,
  file.path(results_dir, "missing_values.csv"),
  row.names = FALSE
)

nonfinite_summary <- data.frame(
  column = numeric_columns,
  nonfinite_count = vapply(
    data[numeric_columns],
    function(x) sum(!is.finite(x)),
    integer(1)
  )
)

duplicate_count <- sum(duplicated(data[c("record_id", "sample")]))
record_count <- length(unique(data$record_id))
beat_count <- nrow(data)

negative_sd <- sum(data$sd < 0, na.rm = TRUE)
negative_variance <- sum(data$variance < 0, na.rm = TRUE)
negative_energy <- sum(data$energy < 0, na.rm = TRUE)
negative_rms <- sum(data$RMS < 0, na.rm = TRUE)

record_counts <- as.data.frame(table(data$record_id))
names(record_counts) <- c("record_id", "beat_count")

annotation_counts <- as.data.frame(table(data$annotation))
names(annotation_counts) <- c("annotation", "beat_count")

write.csv(
  record_counts,
  file.path(results_dir, "beats_per_record.csv"),
  row.names = FALSE
)

write.csv(
  annotation_counts,
  file.path(results_dir, "annotations_in_validation.csv"),
  row.names = FALSE
)

report_file <- file.path(results_dir, "feature_validation.txt")

report <- c(
  "ECG FEATURE DATASET VALIDATION",
  "==============================",
  paste("Total beats:", beat_count),
  paste("Feature columns:", ncol(data)),
  paste("Unique records:", record_count),
  paste("Duplicate record/sample pairs:", duplicate_count),
  "",
  "MISSING VALUES BY COLUMN",
  capture.output(print(missing_summary, row.names = FALSE)),
  "",
  "NON-FINITE VALUES IN NUMERIC COLUMNS",
  capture.output(print(nonfinite_summary, row.names = FALSE)),
  "",
  "BASIC FEATURE CHECKS",
  paste("Negative standard deviations:", negative_sd),
  paste("Negative variances:", negative_variance),
  paste("Negative RMS values:", negative_rms),
  paste("Negative energy values:", negative_energy),
  "",
  "ANNOTATION COUNTS",
  capture.output(print(annotation_counts, row.names = FALSE))
)

writeLines(report, report_file)

cat("\n========== DATASET VALIDATION ==========\n")
cat("Total beats:", beat_count, "\n")
cat("Feature columns:", ncol(data), "\n")
cat("Unique records:", record_count, "\n")
cat("Duplicate record/sample pairs:", duplicate_count, "\n")
cat("Missing values:", sum(missing_summary$missing_count), "\n")
cat("Non-finite numeric values:", sum(nonfinite_summary$nonfinite_count), "\n")
cat("Negative standard deviations:", negative_sd, "\n")
cat("Negative variances:", negative_variance, "\n")
cat("Negative RMS values:", negative_rms, "\n")
cat("Negative energy values:", negative_energy, "\n")
cat("\nSaved validation report:", report_file, "\n")
cat("Saved missing-value summary:", file.path(results_dir, "missing_values.csv"), "\n")
