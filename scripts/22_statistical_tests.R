
# Script 22: Record-level statistical testing of ECG features
# Primary comparison: Normal (N) versus Ventricular (V) beats

project_dir <- getwd()

input_file <- file.path(
  project_dir, "data", "processed",
  "primary_records_features.csv"
)

results_dir <- file.path(project_dir, "results")
plots_dir <- file.path(project_dir, "plots")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Primary dataset not found. Run Script 17 first.")
}

data <- read.csv(input_file, stringsAsFactors = FALSE)

features <- c(
  "mean", "median", "sd", "range",
  "IQR", "RMS", "skewness", "kurtosis"
)

required <- c("record_id", "annotation", features)

if (!all(required %in% names(data))) {
  stop("Required columns are missing from the primary dataset.")
}

data$record_id <- as.character(data$record_id)
data$annotation <- trimws(as.character(data$annotation))

# Keep only Normal and Ventricular beat annotations.
data <- data[
  data$annotation %in% c("N", "V"),
  ,
  drop = FALSE
]

if (nrow(data) == 0) {
  stop("No Normal or Ventricular beats were found.")
}

# Check feature values.
for (feature in features) {
  data[[feature]] <- as.numeric(data[[feature]])

  if (any(!is.finite(data[[feature]]))) {
    stop(paste("Invalid values found in feature:", feature))
  }
}

cat("Total Normal and Ventricular beats:", nrow(data), "\n")
cat("Records represented:", length(unique(data$record_id)), "\n")
cat("\nBeat counts by annotation:\n")
print(table(data$annotation))

# Calculate one median per record, class, and feature.
# This prevents thousands of beats from being treated as
# thousands of independent subjects in the primary test.

record_medians <- aggregate(
  data[, features, drop = FALSE],
  by = list(
    record_id = data$record_id,
    annotation = data$annotation
  ),
  FUN = median
)

write.csv(
  record_medians,
  file.path(results_dir, "record_feature_medians.csv"),
  row.names = FALSE
)

# Reshape to one row per record, with separate N and V medians.
normal <- record_medians[
  record_medians$annotation == "N",
  ,
  drop = FALSE
]

ventricular <- record_medians[
  record_medians$annotation == "V",
  ,
  drop = FALSE
]

names(normal)[names(normal) %in% features] <-
  paste0(names(normal)[names(normal) %in% features], "_N")

names(ventricular)[names(ventricular) %in% features] <-
  paste0(names(ventricular)[names(ventricular) %in% features], "_V")

paired <- merge(
  normal,
  ventricular,
  by = "record_id"
)

if (nrow(paired) < 5) {
  stop("Fewer than five records contain both classes.")
}

cat(
  "\nRecords with both Normal and Ventricular beats:",
  nrow(paired), "\n"
)

write.csv(
  paired,
  file.path(results_dir, "paired_record_feature_medians.csv"),
  row.names = FALSE
)

# Paired record-level Wilcoxon tests.
# Difference is defined as Ventricular median minus Normal median.
set.seed(2026)
bootstrap_reps <- 2000

test_results <- list()
difference_data <- data.frame(record_id = paired$record_id)

for (feature in features) {
  normal_values <- paired[[paste0(feature, "_N")]]
  ventricular_values <- paired[[paste0(feature, "_V")]]

  differences <- ventricular_values - normal_values

  # Wilcoxon signed-rank test on paired record-level summaries.
  test_result <- tryCatch(
    wilcox.test(
      ventricular_values,
      normal_values,
      paired = TRUE,
      exact = FALSE,
      correct = TRUE
    ),
    error = function(e) NULL
  )

  p_value <- if (is.null(test_result)) {
    NA_real_
  } else {
    test_result$p.value
  }

  # Bootstrap confidence interval for the median paired difference.
  boot_medians <- replicate(
    bootstrap_reps,
    median(
      sample(
        differences,
        size = length(differences),
        replace = TRUE
      )
    )
  )

  ci <- quantile(
    boot_medians,
    probs = c(0.025, 0.975),
    na.rm = TRUE,
    names = FALSE
  )

  test_results[[feature]] <- data.frame(
    feature = feature,
    records_tested = length(differences),
    median_normal = median(normal_values),
    median_ventricular = median(ventricular_values),
    median_paired_difference = median(differences),
    CI_lower = ci[1],
    CI_upper = ci[2],
    records_with_positive_difference = sum(differences > 0),
    records_with_negative_difference = sum(differences < 0),
    records_with_zero_difference = sum(differences == 0),
    wilcoxon_p_value = p_value
  )

  difference_data[[feature]] <- differences
}

results <- do.call(rbind, test_results)
rownames(results) <- NULL

# Correct for testing eight features using the Benjamini-Hochberg method.
results$BH_adjusted_p_value <- p.adjust(
  results$wilcoxon_p_value,
  method = "BH"
)

results$significant_BH_0_05 <- (
  !is.na(results$BH_adjusted_p_value) &
    results$BH_adjusted_p_value < 0.05
)

write.csv(
  results,
  file.path(results_dir, "paired_feature_statistical_tests.csv"),
  row.names = FALSE
)

write.csv(
  difference_data,
  file.path(results_dir, "paired_feature_differences.csv"),
  row.names = FALSE
)

# Plot record-level paired differences for all eight features.
png(
  file.path(plots_dir, "paired_feature_differences.png"),
  width = 1400,
  height = 900
)

par(
  mfrow = c(2, 4),
  mar = c(4, 4, 3, 1)
)

for (feature in features) {
  differences <- difference_data[[feature]]

  boxplot(
    differences,
    main = feature,
    ylab = "Ventricular minus Normal",
    xlab = "",
    col = "grey85"
  )

  abline(h = 0, lty = 2)
}

dev.off()

cat("\n========== STATISTICAL TEST RESULTS ==========\n")

print(
  results[, c(
    "feature",
    "records_tested",
    "median_paired_difference",
    "CI_lower",
    "CI_upper",
    "wilcoxon_p_value",
    "BH_adjusted_p_value",
    "significant_BH_0_05"
  )],
  row.names = FALSE,
  digits = 5
)

cat("\nSaved results:\n")
cat("- results/record_feature_medians.csv\n")
cat("- results/paired_record_feature_medians.csv\n")
cat("- results/paired_feature_differences.csv\n")
cat("- results/paired_feature_statistical_tests.csv\n")
cat("- plots/paired_feature_differences.png\n")

cat(
  "\nInterpretation note: tests compare record-level medians,",
  "not individual beats. Results apply only to records containing",
  "both annotation classes and do not establish clinical significance.\n"
)
