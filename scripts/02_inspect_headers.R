
# 02_inspect_headers.R
# Inspect MIT-BIH ECG record headers using base R

project_dir <- getwd()
data_dir <- file.path(project_dir, "data")

# Locate every header file
hea_files <- list.files(
  data_dir,
  pattern = "\\.hea$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Read the first line of every header
header_info <- data.frame(
  record = character(),
  signals = integer(),
  sampling_hz = numeric(),
  samples = numeric(),
  stringsAsFactors = FALSE
)

for (f in hea_files) {
  line <- readLines(f, n = 1, warn = FALSE)
  parts <- strsplit(trimws(line), "\\s+")[[1]]

  if (length(parts) >= 4) {
    header_info <- rbind(
      header_info,
      data.frame(
        record = tools::file_path_sans_ext(basename(f)),
        signals = as.integer(parts[2]),
        sampling_hz = as.numeric(parts[3]),
        samples = as.numeric(parts[4])
      )
    )
  }
}

# Display the results
cat("MIT-BIH HEADER INSPECTION\n")
cat("=========================\n")
cat("Records inspected:", nrow(header_info), "\n\n")

cat("First 10 records:\n")
print(head(header_info, 10), row.names = FALSE)

cat("\nSampling frequency summary (Hz):\n")
print(summary(header_info$sampling_hz))

cat("\nNumber of signal channels:\n")
print(table(header_info$signals, useNA = "ifany"))

# Save results
results_dir <- file.path(project_dir, "results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(
  header_info,
  file.path(results_dir, "header_summary.csv"),
  row.names = FALSE
)

cat("\nSaved: results/header_summary.csv\n")
