
# Script 18: Prepare and audit the primary dataset for machine learning

project_dir <- getwd()

input_file <- file.path(
  project_dir, "data", "processed", "primary_records_features.csv"
)

results_dir <- file.path(project_dir, "results")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Primary dataset not found. Run Script 17 first.")
}

data <- read.csv(input_file, stringsAsFactors = FALSE)

features <- c(
  "mean", "median", "sd", "variance", "range",
  "IQR", "RMS", "energy", "skewness", "kurtosis"
)

required_columns <- c("record_id", "annotation", features)
missing_columns <- setdiff(required_columns, names(data))

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

data$record_id <- as.character(data$record_id)
data$annotation <- as.character(data$annotation)

# 1. Count beats and distinct records for every class.
classes <- sort(unique(data$annotation))

class_audit <- do.call(
  rbind,
  lapply(classes, function(label) {
    subset_data <- data[data$annotation == label, , drop = FALSE]

    data.frame(
      annotation = label,
      beat_count = nrow(subset_data),
      record_count = length(unique(subset_data$record_id)),
      percentage = 100 * nrow(subset_data) / nrow(data)
    )
  })
)

class_audit <- class_audit[
  order(-class_audit$beat_count),
]

write.csv(
  class_audit,
  file.path(results_dir, "ml_class_audit.csv"),
  row.names = FALSE
)

# 2. Check missing and non-finite feature values.
feature_data <- data[, features, drop = FALSE]

missing_counts <- sapply(
  feature_data,
  function(x) sum(is.na(x))
)

nonfinite_counts <- sapply(
  feature_data,
  function(x) sum(!is.finite(x[!is.na(x)]))
)

feature_audit <- data.frame(
  feature = features,
  missing_values = as.integer(missing_counts),
  nonfinite_values = as.integer(nonfinite_counts),
  unique_values = sapply(
    feature_data,
    function(x) length(unique(x[is.finite(x)]))
  ),
  standard_deviation = sapply(
    feature_data,
    function(x) sd(x, na.rm = TRUE)
  )
)

write.csv(
  feature_audit,
  file.path(results_dir, "ml_feature_audit.csv"),
  row.names = FALSE
)

# 3. Check feature correlations.
correlation_matrix <- cor(
  feature_data,
  use = "pairwise.complete.obs"
)

write.csv(
  correlation_matrix,
  file.path(results_dir, "ml_feature_correlations.csv")
)

# 4. Summarize whether each class occurs in enough records.
# This is a diagnostic, not an automatic inclusion rule.
class_audit$records_with_class <- class_audit$record_count

cat("\n========== MACHINE LEARNING DATA AUDIT ==========\n")
cat("Total beats:", nrow(data), "\n")
cat("Unique records:", length(unique(data$record_id)), "\n")
cat("Feature count:", length(features), "\n")
cat("Annotation classes:", length(classes), "\n")

cat("\nClass counts and record coverage:\n")
print(class_audit, row.names = FALSE)

cat("\nFeature quality:\n")
print(feature_audit, row.names = FALSE)

cat("\nSaved:\n")
cat("- results/ml_class_audit.csv\n")
cat("- results/ml_feature_audit.csv\n")
cat("- results/ml_feature_correlations.csv\n")
