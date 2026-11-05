
# 03_inspect_record.R
# Inspect one MIT-BIH record before reading its waveform

project_dir <- getwd()
data_dir <- file.path(project_dir, "data")

header_file <- file.path(data_dir, "100.hea")

if (!file.exists(header_file)) {
  stop("Cannot find data/100.hea")
}

# Display the complete header
header <- readLines(header_file, warn = FALSE)

cat("RECORD 100: HEADER DETAILS\n")
cat("==========================\n")

cat(paste(header, collapse = "\n"), "\n")

# Display matching files and their sizes
cat("\nFILES FOR RECORD 100\n")
cat("====================\n")

for (ext in c("hea", "dat", "atr")) {
  path <- file.path(data_dir, paste0("100.", ext))

  if (file.exists(path)) {
    info <- file.info(path)
    cat(
      basename(path),
      "-",
      round(info$size / 1024, 2),
      "KB\n"
    )
  } else {
    cat(paste0("100.", ext), "- NOT FOUND\n")
  }
}
