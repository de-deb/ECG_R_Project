
# 01_check_dataset.R
# Check the MIT-BIH dataset structure

# Find the project folder
if (dir.exists("data")) {
  project_dir <- getwd()
} else if (dir.exists("../data")) {
  project_dir <- normalizePath("..")
} else {
  stop("Cannot find the data folder. Open the ECG_R_Project folder.")
}

data_dir <- file.path(project_dir, "data")

cat("ECG PROJECT DATA CHECK\n")
cat("======================\n")
cat("Data folder:", data_dir, "\n\n")

# Find all header files
hea_files <- list.files(
  data_dir,
  pattern = "\\.hea$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Check which records have all three files
records <- data.frame(
  record = character(),
  folder = character(),
  dat = logical(),
  hea = logical(),
  atr = logical()
)

for (f in hea_files) {
  folder <- dirname(f)
  record <- tools::file_path_sans_ext(basename(f))

  has_dat <- file.exists(file.path(folder, paste0(record, ".dat")))
  has_atr <- file.exists(file.path(folder, paste0(record, ".atr")))

  records <- rbind(
    records,
    data.frame(
      record = record,
      folder = folder,
      dat = has_dat,
      hea = TRUE,
      atr = has_atr
    )
  )
}

# Show the results
cat("Header files found:", nrow(records), "\n")

if (nrow(records) > 0) {
  complete <- records$dat & records$hea & records$atr

  cat("Records with DAT, HEA and ATR:", sum(complete), "\n")
  cat("Records missing one or more files:", sum(!complete), "\n\n")

  cat("First 10 records:\n")
  print(head(records, 10), row.names = FALSE)
} else {
  cat("No .hea files found. Check the dataset location.\n")
}
