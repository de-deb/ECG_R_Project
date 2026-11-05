
# Script 14: Analyze class balance in the primary ECG dataset

project_dir <- getwd()

input_file <- file.path(
  project_dir, "data", "processed", "primary_records_features.csv"
)

results_dir <- file.path(project_dir, "results")
plots_dir <- file.path(project_dir, "plots")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Primary dataset not found. Run Script 17 first.")
}

data <- read.csv(input_file, stringsAsFactors = FALSE)

data$annotation <- as.character(data$annotation)

counts <- as.data.frame(table(data$annotation))
names(counts) <- c("annotation", "beat_count")

counts <- counts[order(-counts$beat_count), ]
counts$percentage <- 100 * counts$beat_count / nrow(data)

write.csv(
  counts,
  file.path(results_dir, "annotation_counts.csv"),
  row.names = FALSE
)

png(
  file.path(plots_dir, "annotation_distribution.png"),
  width = 1200,
  height = 800,
  res = 120
)

par(mar = c(8, 5, 4, 2))

barplot(
  counts$beat_count,
  names.arg = counts$annotation,
  las = 2,
  col = "steelblue",
  main = "ECG Beat Annotation Distribution",
  xlab = "Annotation class",
  ylab = "Number of beats"
)

dev.off()

cat("\n========== PRIMARY DATASET CLASS BALANCE ==========\n")
cat("Total beats:", nrow(data), "\n")
cat("Unique records:", length(unique(data$record_id)), "\n")
cat("Annotation classes:", nrow(counts), "\n\n")

print(counts, row.names = FALSE)

cat("\nSaved: results/annotation_counts.csv\n")
cat("Saved: plots/annotation_distribution.png\n")
