
project_dir <- getwd()
data_dir <- file.path(project_dir, "data")
plot_dir <- file.path(project_dir, "plots")

dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

ecg <- EGM::read_wfdb(
  record = "100",
  record_dir = data_dir,
  units = "physical"
)

signal <- ecg$signal

ann <- EGM::read_annotation(
  record = "100",
  annotator = "atr",
  record_dir = data_dir
)

# Choose the first valid normal beat
beat_ann <- ann[
  ann$type == "N" &
  ann$sample > 90 &
  ann$sample + 165 <= nrow(signal),
][1]

center <- match(beat_ann$sample, signal$sample)
idx <- (center - 90):(center + 165)

beat <- signal$MLII[idx]
baseline <- median(beat[1:60])
corrected <- beat - baseline

time_ms <- ((seq_along(beat) - 91) / 360) * 1000

png(
  file.path(plot_dir, "single_beat_validation.png"),
  width = 1000,
  height = 600
)

plot(
  time_ms,
  corrected,
  type = "l",
  xlab = "Time relative to annotation (ms)",
  ylab = "Baseline-corrected ECG (mV)",
  main = paste("Extracted Normal Beat - Record 100, Sample",
               beat_ann$sample)
)

abline(v = 0, lty = 2)
grid()

dev.off()

cat("Annotation sample:", beat_ann$sample, "\n")
cat("Saved plots/single_beat_validation.png\n")
