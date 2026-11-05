
project_dir <- getwd()
data_dir <- file.path(project_dir, "data")
results_dir <- file.path(project_dir, "results")

dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

headers <- list.files(
  data_dir,
  pattern = "\\.hea$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(headers) == 0) {
  stop("No .hea files found in the data folder.")
}

inventory <- lapply(headers, function(file) {
  lines <- readLines(file, warn = FALSE)
  first <- strsplit(trimws(lines[1]), "\\s+")[[1]]

  record_id <- tools::file_path_sans_ext(basename(file))
  n_signals <- if (length(first) >= 2) {
    as.integer(first[2])
  } else {
    NA_integer_
  }

  fs <- if (length(first) >= 3) {
    as.numeric(first[3])
  } else {
    NA_real_
  }

  samples <- if (length(first) >= 4) {
    as.numeric(first[4])
  } else {
    NA_real_
  }

  channel_lines <- character(0)

  if (!is.na(n_signals) &&
      n_signals > 0 &&
      length(lines) >= n_signals + 1) {
    channel_lines <- lines[2:(n_signals + 1)]
  }

  channels <- vapply(channel_lines, function(line) {
    parts <- strsplit(trimws(line), "\\s+")[[1]]
    parts[length(parts)]
  }, character(1))

  dat_file <- file.path(
    dirname(file),
    paste0(record_id, ".dat")
  )

  atr_file <- file.path(
    dirname(file),
    paste0(record_id, ".atr")
  )

  data.frame(
    record_id = record_id,
    folder = dirname(file),
    header = file,
    sampling_rate = fs,
    samples = samples,
    channels = paste(channels, collapse = ", "),
    has_dat = file.exists(dat_file),
    has_atr = file.exists(atr_file),
    stringsAsFactors = FALSE
  )
})

inventory <- do.call(rbind, inventory)

write.csv(
  inventory,
  file.path(results_dir, "record_inventory.csv"),
  row.names = FALSE
)

cat("HEADER FILES:", nrow(inventory), "\n")
cat("UNIQUE RECORD IDs:", length(unique(inventory$record_id)), "\n")
cat("RECORD IDs WITH MULTIPLE COPIES:\n")

counts <- table(inventory$record_id)
duplicates <- names(counts[counts > 1])

if (length(duplicates) == 0) {
  cat("None found.\n")
} else {
  print(duplicates)

  print(
    inventory[inventory$record_id %in% duplicates,
              c("record_id", "folder", "channels")]
  )
}

cat("\nSAMPLING RATES:\n")
print(table(inventory$sampling_rate, useNA = "ifany"))

cat("\nMISSING COMPANION FILES:\n")
print(inventory[!inventory$has_dat | !inventory$has_atr,
                c("record_id", "folder", "has_dat", "has_atr")])

cat("\nSaved: results/record_inventory.csv\n")
