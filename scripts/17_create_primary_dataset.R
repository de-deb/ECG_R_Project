
project_dir <- getwd()

input_file <- file.path(
  project_dir, "data", "processed", "all_records_features.csv"
)

output_dir <- file.path(project_dir, "data", "processed")
results_dir <- file.path(project_dir, "results")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Combined dataset not found. Run Script 12 first.")
}

data <- read.csv(input_file, stringsAsFactors = FALSE)
data$record_id <- as.character(data$record_id)

# Separate original records from the overlapping x_ records.
is_x_record <- grepl("^x_", data$record_id)

x_data <- data[is_x_record, , drop = FALSE]
primary_data <- data[!is_x_record, , drop = FALSE]

# Verify that every x_ record has a matching original record.
x_ids <- unique(x_data$record_id)
original_ids <- unique(primary_data$record_id)

paired_ids <- sub("^x_", "", x_ids)
unmatched_x <- x_ids[!paired_ids %in% original_ids]

if (length(unmatched_x) > 0) {
  warning(
    "Some x_ records have no matching original: ",
    paste(unmatched_x, collapse = ", ")
  )
}

# Check whether x_ sample positions are contained in original records.
pair_summary <- list()

for (x_id in x_ids) {
  original_id <- sub("^x_", "", x_id)

  if (!original_id %in% original_ids) next

  x_samples <- x_data$sample[x_data$record_id == x_id]
  original_samples <- primary_data$sample[
    primary_data$record_id == original_id
  ]

  shared <- sum(x_samples %in% original_samples)

  pair_summary[[length(pair_summary) + 1]] <- data.frame(
    x_record = x_id,
    original_record = original_id,
    x_record_beats = length(x_samples),
    shared_sample_positions = shared,
    all_x_samples_found_in_original = shared == length(x_samples)
  )
}

if (length(pair_summary) > 0) {
  pair_table <- do.call(rbind, pair_summary)

  write.csv(
    pair_table,
    file.path(results_dir, "primary_dataset_pair_checks.csv"),
    row.names = FALSE
  )
}

# Save a separate dataset for subsequent statistical analysis.
output_file <- file.path(
  output_dir, "primary_records_features.csv"
)

write.csv(primary_data, output_file, row.names = FALSE)

# Save annotation counts for the retained records.
annotation_counts <- as.data.frame(
  table(primary_data$annotation)
)

names(annotation_counts) <- c("annotation", "beat_count")
annotation_counts <- annotation_counts[
  order(-annotation_counts$beat_count), 
]

annotation_counts$percentage <- 100 *
  annotation_counts$beat_count / nrow(primary_data)

write.csv(
  annotation_counts,
  file.path(results_dir, "primary_annotation_counts.csv"),
  row.names = FALSE
)

summary_lines <- c(
  "PRIMARY ECG ANALYSIS DATASET",
  "============================",
  paste("Original dataset beats:", nrow(data)),
  paste("Original dataset records:", length(unique(data$record_id))),
  paste("Removed x_ record beats:", nrow(x_data)),
  paste("Retained beats:", nrow(primary_data)),
  paste("Retained original records:", length(unique(primary_data$record_id))),
  paste("Remaining x_ records:", sum(grepl("^x_", primary_data$record_id))),
  "",
  "This dataset excludes x_ records because their sample positions",
  "overlap with the corresponding original records.",
  "The original combined dataset remains unchanged."
)

writeLines(
  summary_lines,
  file.path(results_dir, "primary_dataset_summary.txt")
)

cat("\n========== PRIMARY DATASET SUMMARY ==========\n")
cat("Original dataset beats:", nrow(data), "\n")
cat("Original dataset records:", length(unique(data$record_id)), "\n")
cat("Removed x_ record beats:", nrow(x_data), "\n")
cat("Retained beats:", nrow(primary_data), "\n")
cat("Retained original records:", length(unique(primary_data$record_id)), "\n")
cat("Remaining x_ records:",
    sum(grepl("^x_", primary_data$record_id)), "\n")

if (length(pair_summary) > 0) {
  cat("\nPaired record sample checks:\n")
  print(pair_table, row.names = FALSE)
}

cat("\nSaved dataset:", output_file, "\n")
cat(
  "Saved summary:",
  file.path(results_dir, "primary_dataset_summary.txt"), "\n"
)
