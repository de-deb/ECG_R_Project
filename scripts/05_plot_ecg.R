
# 05_plot_ecg.R
# Plot the first 10 seconds of MIT-BIH record 100

library(EGM)

project_dir <- getwd()
data_dir <- file.path(project_dir, "data")

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

cat("Signal columns:\n")
print(names(signal))

cat("\nFirst three signal rows:\n")
print(head(signal, 3))

# Identify an ECG lead
lead_candidates <- intersect(c("MLII", "V5"), names(signal))

if (length(lead_candidates) == 0) {
  stop("Could not identify MLII or V5. Check the signal columns above.")
}

lead <- lead_candidates[1]

# Plot the first 10 seconds (360 samples per second)
fs <- 360
n <- min(nrow(signal), 10 * fs)

time <- (seq_len(n) - 1) / fs
voltage <- signal[[lead]][seq_len(n)]

dir.create(
  file.path(project_dir, "plots"),
  showWarnings = FALSE
)

png(
  file.path(project_dir, "plots", "record100_first10sec.png"),
  width = 1200,
  height = 600
)

plot(
  time,
  voltage,
  type = "l",
  xlab = "Time (seconds)",
  ylab = "Voltage (mV)",
  main = paste("MIT-BIH Record 100 -", lead),
  col = "black"
)

dev.off()

cat("\nLead plotted:", lead, "\n")
cat("Plot saved to plots/record100_first10sec.png\n")

cat("\nAnnotation label counts:\n")
print(sort(table(ann$type), decreasing = TRUE))
