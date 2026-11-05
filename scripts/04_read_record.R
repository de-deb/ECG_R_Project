
# 04_read_record.R
# Test reading MIT-BIH record 100
library(EGM)

project_dir <- getwd()
data_dir <- file.path(project_dir, "data")

# Read the ECG signal
cat("Reading ECG signal...\n")

ecg <- read_wfdb(
  record = "100",
  record_dir = data_dir,
  units = "physical"
)

cat("\nSignal object structure:\n")
str(ecg, max.level = 1)

cat("\nFirst few rows (if tabular):\n")
if (is.data.frame(ecg) || is.matrix(ecg)) {
  print(head(ecg))
}

# Read the beat annotations
cat("\nReading beat annotations...\n")

ann <- read_annotation(
  record = "100",
  annotator = "atr",
  record_dir = data_dir
)

cat("\nAnnotation object structure:\n")
str(ann, max.level = 1)

cat("\nFirst few annotations:\n")
print(head(ann))
