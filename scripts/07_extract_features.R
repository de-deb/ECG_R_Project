
# 07_extract_features.R
# Extract beat-level statistical features from record 100

library(EGM)

project_dir <- getwd()
data_dir <- file.path(project_dir, "data")
results_dir <- file.path(project_dir, "results")
processed_dir <- file.path(data_dir, "processed")

dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)

# Read ECG signal and annotations
ecg <- read_wfdb(
  record = "100",
  record_dir = data_dir,
  units = "physical"
)

ann <- read_annotation(
  record = "100",
  annotator = "atr",
  record_dir = data_dir
)

signal <- ecg$signal
fs <- 360

# Use MLII lead
x <- signal$MLII

# Keep recognized beat annotations; exclude markers like "+"
beat_types <- c(
  "N", "L", "R", "B", "A", "a", "J", "S",
  "V", "r", "F", "e", "j", "n", "E", "/", "f", "Q"
)

ann <- ann[ann$type %in% beat_types, ]

# Extract 256 samples per beat:
# 90 samples before the annotated position and 165 after
pre_samples <- 90
window_size <- 256

features <- list()
skipped <- 0

for (i in seq_len(nrow(ann))) {
  # Match annotation sample number to the signal's sample column
  center <- match(ann$sample[i], signal$sample)

  if (is.na(center)) {
    skipped <- skipped + 1
    next
  }

  start <- center - pre_samples
  end <- start + window_size - 1

  # Skip beats too close to the recording boundaries
  if (start < 1 || end > length(x)) {
    skipped <- skipped + 1
    next
  }

  beat <- x[start:end]

  # Estimate local baseline from the early part of the window
  baseline <- median(beat[1:60])

  # Center the beat to reduce the effect of baseline offset
  corrected <- beat - baseline

  s <- sd(corrected)

  # Calculate statistical and waveform features
  features[[length(features) + 1]] <- data.frame(
    record = "100",
    sample = ann$sample[i],
    time_seconds = ann$sample[i] / fs,
    annotation = ann$type[i],
    baseline_mV = baseline,
    mean = mean(corrected),
    median = median(corrected),
    sd = s,
    variance = var(corrected),
    min = min(corrected),
    max = max(corrected),
    range = diff(range(corrected)),
    q1 = unname(quantile(corrected, 0.25)),
    q3 = unname(quantile(corrected, 0.75)),
    IQR = IQR(corrected),
    RMS = sqrt(mean(corrected^2)),
    energy = sum(corrected^2),
    skewness = if (s > 0) mean((corrected - mean(corrected))^3) / s^3 else NA_real_,
    kurtosis = if (s > 0) mean((corrected - mean(corrected))^4) / s^4 - 3 else NA_real_
  )
}

if (length(features) == 0) {
  stop("No beat features extracted. Check the annotation and signal sample positions.")
}

feature_data <- do.call(rbind, features)

# Save the extracted dataset
output_file <- file.path(
  processed_dir,
  "record100_features.csv"
)

write.csv(feature_data, output_file, row.names = FALSE)

cat("RECORD 100 FEATURE EXTRACTION\n")
cat("=============================\n")
cat("Beat rows extracted:", nrow(feature_data), "\n")
cat("Boundary/unmatched annotations skipped:", skipped, "\n\n")

cat("Annotation counts:\n")
print(sort(table(feature_data$annotation), decreasing = TRUE))

cat("\nFeature preview:\n")
print(head(feature_data, 6), row.names = FALSE)

cat("\nMissing values per column:\n")
print(colSums(is.na(feature_data)))

cat("\nSaved to:", output_file, "\n")
