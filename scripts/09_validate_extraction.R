
project_dir <- getwd()
data_dir <- file.path(project_dir, "data")

signal <- EGM::read_wfdb(
  record = "100",
  record_dir = data_dir,
  units = "physical"
)$signal

ann <- EGM::read_annotation(
  record = "100",
  annotator = "atr",
  record_dir = data_dir
)

beat_types <- c(
  "N", "L", "R", "B", "A", "a", "J", "S",
  "V", "r", "F", "e", "j", "n", "E", "/", "f", "Q"
)

ann_beats <- ann[ann$type %in% beat_types, ]

pre_samples <- 90
window_size <- 256

center <- match(ann_beats$sample, signal$sample)

valid <- !is.na(center) &
  center - pre_samples >= 1 &
  center - pre_samples + window_size - 1 <= nrow(signal)

cat("Total annotations:", nrow(ann), "\n")
cat("Beat-type annotations:", nrow(ann_beats), "\n")
cat("Valid beat windows:", sum(valid), "\n")
cat("Skipped beat windows:", sum(!valid), "\n\n")

if (any(!valid)) {
  cat("Skipped annotations:\n")
  print(ann_beats[!valid, c("sample", "time", "type")])
}

cat("\nAnnotation counts among valid windows:\n")
print(table(ann_beats$type[valid]))

# Inspect the first valid beat window
i <- which(valid)[1]
idx <- center[i] - pre_samples +
  seq_len(window_size) - 1

beat <- signal$MLII[idx]
baseline <- median(beat[1:60])
corrected <- beat - baseline

cat("\nFirst valid beat:\n")
cat("Annotation:", ann_beats$type[i], "\n")
cat("Sample:", ann_beats$sample[i], "\n")
cat("Time (seconds):",
    ann_beats$sample[i] / 360, "\n")
cat("Window length:", length(corrected), "\n")
cat("Baseline:", baseline, "mV\n")
cat("Corrected range:",
    range(corrected), "mV\n")
