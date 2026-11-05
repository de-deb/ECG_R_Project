
# Script 15: Statistical exploration of the primary ECG dataset

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

# Features used for descriptive statistics.
feature_names <- c(
  "mean", "median", "sd", "variance", "range",
  "IQR", "RMS", "energy", "skewness", "kurtosis"
)

missing_features <- setdiff(feature_names, names(data))

if (length(missing_features) > 0) {
  stop(
    "Missing feature columns: ",
    paste(missing_features, collapse = ", ")
  )
}

# Overall descriptive statistics.
overall_stats <- do.call(
  rbind,
  lapply(feature_names, function(feature) {
    x <- data[[feature]]
    x <- x[is.finite(x)]

    data.frame(
      feature = feature,
      count = length(x),
      mean = mean(x),
      median = median(x),
      sd = sd(x),
      minimum = min(x),
      q1 = unname(quantile(x, 0.25)),
      q3 = unname(quantile(x, 0.75)),
      maximum = max(x)
    )
  })
)

write.csv(
  overall_stats,
  file.path(results_dir, "overall_feature_statistics.csv"),
  row.names = FALSE
)

# Feature summaries for each annotation class.
group_stats <- do.call(
  rbind,
  lapply(sort(unique(data$annotation)), function(label) {
    subset_data <- data[data$annotation == label, ]

    do.call(
      rbind,
      lapply(feature_names, function(feature) {
        x <- subset_data[[feature]]
        x <- x[is.finite(x)]

        data.frame(
          annotation = label,
          feature = feature,
          count = length(x),
          mean = mean(x),
          median = median(x),
          sd = if (length(x) > 1) sd(x) else NA_real_
        )
      })
    )
  })
)

write.csv(
  group_stats,
  file.path(results_dir, "feature_statistics_by_annotation.csv"),
  row.names = FALSE
)

# Correlation matrix between numeric features.
correlation_matrix <- cor(
  data[, feature_names],
  use = "pairwise.complete.obs"
)

write.csv(
  correlation_matrix,
  file.path(results_dir, "feature_correlation_matrix.csv")
)

# Plot RMS distributions for classes with at least 20 beats.
class_counts <- table(data$annotation)
common_classes <- names(class_counts[class_counts >= 20])

plot_data <- data[
  data$annotation %in% common_classes,
  ,
  drop = FALSE
]

png(
  file.path(plots_dir, "RMS_by_annotation.png"),
  width = 1200,
  height = 800,
  res = 120
)

par(mar = c(8, 5, 4, 2))

boxplot(
  RMS ~ annotation,
  data = plot_data,
  las = 2,
  col = "lightblue",
  main = "RMS by ECG Annotation Class",
  xlab = "Annotation class",
  ylab = "RMS amplitude"
)

dev.off()

# Plot feature correlations.
png(
  file.path(plots_dir, "feature_correlation_matrix.png"),
  width = 1000,
  height = 900,
  res = 120
)

correlation_matrix <- as.matrix(correlation_matrix)

image(
  1:ncol(correlation_matrix),
  1:nrow(correlation_matrix),
  t(correlation_matrix[nrow(correlation_matrix):1, ]),
  axes = FALSE,
  col = colorRampPalette(
    c("navy", "white", "firebrick")
  )(100),
  main = "Correlation Between ECG Features"
)

axis(
  1,
  at = 1:ncol(correlation_matrix),
  labels = colnames(correlation_matrix),
  las = 2
)

axis(
  2,
  at = 1:nrow(correlation_matrix),
  labels = rev(rownames(correlation_matrix)),
  las = 2
)

box()

dev.off()

cat("\n========== PRIMARY DATASET STATISTICS ==========\n")
cat("Total beats:", nrow(data), "\n")
cat("Unique records:", length(unique(data$record_id)), "\n")
cat("Features analyzed:", length(feature_names), "\n\n")

print(overall_stats, row.names = FALSE)

cat("\nSaved results:\n")
cat("- results/overall_feature_statistics.csv\n")
cat("- results/feature_statistics_by_annotation.csv\n")
cat("- results/feature_correlation_matrix.csv\n")
cat("- plots/RMS_by_annotation.png\n")
cat("- plots/feature_correlation_matrix.png\n")
