
# Script 19: Create an N-versus-V dataset with grouped train/test split

project_dir <- getwd()

input_file <- file.path(
  project_dir, "data", "processed", "primary_records_features.csv"
)

processed_dir <- file.path(project_dir, "data", "processed")
results_dir <- file.path(project_dir, "results")

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Primary dataset not found. Run Script 17 first.")
}

data <- read.csv(input_file, stringsAsFactors = FALSE)

features <- c(
  "mean", "median", "sd", "variance", "range",
  "IQR", "RMS", "energy", "skewness", "kurtosis"
)

required <- c("record_id", "annotation", features)

if (!all(required %in% names(data))) {
  stop("The dataset is missing required columns.")
}

data$record_id <- as.character(data$record_id)
data$annotation <- as.character(data$annotation)

# Keep only normal and ventricular beats.
data <- data[data$annotation %in% c("N", "V"), , drop = FALSE]

# Create readable binary class labels.
data$class <- ifelse(
  data$annotation == "N",
  "Normal",
  "Ventricular"
)

# Group records 201 and 202 because they belong to the same subject.
# Other records are grouped by their record ID.
data$group_id <- ifelse(
  data$record_id %in% c("201", "202"),
  "subject_201_202",
  paste0("record_", data$record_id)
)

# Ensure the selected features are usable.
if (any(!is.finite(as.matrix(data[, features])))) {
  stop("Missing or non-finite feature values found. Investigate before modeling.")
}

# Find a reproducible group split with both classes represented
# across multiple groups in both training and testing sets.
groups <- unique(data$group_id)
test_size <- max(1, round(0.20 * length(groups)))

valid_split <- FALSE

for (seed in 1:500) {
  set.seed(seed)

  test_groups <- sample(groups, size = test_size)
  train_groups <- setdiff(groups, test_groups)

  train_data <- data[data$group_id %in% train_groups, ]
  test_data <- data[data$group_id %in% test_groups, ]

  train_class_groups <- sapply(
    c("Normal", "Ventricular"),
    function(label) {
      length(unique(
        train_data$group_id[train_data$class == label]
      ))
    }
  )

  test_class_groups <- sapply(
    c("Normal", "Ventricular"),
    function(label) {
      length(unique(
        test_data$group_id[test_data$class == label]
      ))
    }
  )

  if (
    all(train_class_groups >= 5) &&
    all(test_class_groups >= 3)
  ) {
    valid_split <- TRUE
    break
  }
}

if (!valid_split) {
  stop("Could not find a suitable grouped split. Review record coverage.")
}

# Label every beat according to its record group's assignment.
data$split <- ifelse(
  data$group_id %in% test_groups,
  "test",
  "train"
)

# Save the modeling dataset without changing the original data.
output_file <- file.path(
  processed_dir, "binary_nv_dataset.csv"
)

write.csv(data, output_file, row.names = FALSE)

# Save the record-group assignment.
split_records <- unique(
  data[, c("record_id", "group_id", "split")]
)

split_records <- split_records[
  order(split_records$split, split_records$group_id),
]

write.csv(
  split_records,
  file.path(results_dir, "ml_record_split.csv"),
  row.names = FALSE
)

# Summarize the split.
split_summary <- as.data.frame(
  table(data$split, data$class)
)

names(split_summary) <- c("split", "class", "beat_count")

write.csv(
  split_summary,
  file.path(results_dir, "ml_split_class_counts.csv"),
  row.names = FALSE
)

cat("\n========== BINARY DATASET SUMMARY ==========\n")
cat("Total N and V beats:", nrow(data), "\n")
cat("Unique record groups:", length(groups), "\n")
cat("Training beats:", sum(data$split == "train"), "\n")
cat("Testing beats:", sum(data$split == "test"), "\n")
cat("Training groups:", length(train_groups), "\n")
cat("Testing groups:", length(test_groups), "\n")
cat("Selected seed:", seed, "\n\n")

cat("Class counts by split:\n")
print(split_summary, row.names = FALSE)

cat("\nRecord-group overlap between training and testing:\n")
cat(
  length(intersect(
    unique(data$group_id[data$split == "train"]),
    unique(data$group_id[data$split == "test"])
  )),
  "overlapping groups\n"
)

cat("\nSaved dataset:", output_file, "\n")
cat("Saved record split: results/ml_record_split.csv\n")
cat("Saved class counts: results/ml_split_class_counts.csv\n")
