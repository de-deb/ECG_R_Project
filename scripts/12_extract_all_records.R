
project_dir <- getwd()
data_dir <- file.path(project_dir, "data")
results_dir <- file.path(project_dir, "results")
processed_dir <- file.path(data_dir, "processed")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

inventory_file <- file.path(results_dir, "record_inventory.csv")

if (!file.exists(inventory_file)) {
  stop("Run scripts/11_inventory_records.R first.")
}

inventory <- read.csv(
  inventory_file,
  stringsAsFactors = FALSE
)

# Recalculate completeness from the two file-check columns.
to_bool <- function(x) {
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes")
}

if (!all(c("record_id", "folder", "has_dat", "has_atr") %in%
         names(inventory))) {
  stop("Inventory is missing required columns. Rerun Script 11.")
}

inventory$has_dat <- to_bool(inventory$has_dat)
inventory$has_atr <- to_bool(inventory$has_atr)
inventory$complete <- inventory$has_dat & inventory$has_atr

cat("Records in inventory:", nrow(inventory), "\n")
cat("Records with .dat files:", sum(inventory$has_dat), "\n")
cat("Records with .atr files:", sum(inventory$has_atr), "\n")
cat("Complete records:", sum(inventory$complete), "\n\n")

inventory <- inventory[inventory$complete, , drop = FALSE]

if (nrow(inventory) == 0) {
  stop("No complete records. Check results/record_inventory.csv.")
}

beat_types <- c(
  "N", "L", "R", "B", "A", "a", "J", "S",
  "V", "r", "F", "e", "j", "n", "E", "/", "f", "Q"
)

pre_samples <- 90
window_size <- 256

extract_record <- function(id, folder) {
  ecg <- EGM::read_wfdb(
    record = id,
    record_dir = folder,
    units = "physical"
  )

  signal <- ecg$signal

  ann <- EGM::read_annotation(
    record = id,
    annotator = "atr",
    record_dir = folder
  )

  ann <- ann[ann$type %in% beat_types, ]
  center <- match(ann$sample, signal$sample)

  valid <- !is.na(center) &
    center - pre_samples >= 1 &
    center - pre_samples + window_size - 1 <= nrow(signal)

  ann <- ann[valid, ]
  center <- center[valid]

  if (nrow(ann) == 0) return(data.frame())

  channels <- setdiff(names(signal), "sample")
  if (length(channels) == 0) stop("No ECG channels found.")

  channel <- if ("MLII" %in% channels) "MLII" else channels[1]
  x <- signal[[channel]]

  # All records were confirmed as 360 Hz in Script 11.
  fs <- 360

  output <- vector("list", nrow(ann))

  for (i in seq_len(nrow(ann))) {
    idx <- center[i] - pre_samples +
      seq_len(window_size) - 1

    beat <- x[idx]
    if (any(!is.finite(beat))) next

    baseline <- median(beat[1:60])
    corrected <- beat - baseline
    s <- sd(corrected)
    mu <- mean(corrected)

    output[[i]] <- data.frame(
      record_id = id,
      sample = ann$sample[i],
      time_seconds = ann$sample[i] / fs,
      annotation = as.character(ann$type[i]),
      channel = channel,
      baseline = baseline,
      mean = mu,
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
      skewness = if (is.finite(s) && s > 0)
        mean((corrected - mu)^3) / s^3 else NA_real_,
      kurtosis = if (is.finite(s) && s > 0)
        mean((corrected - mu)^4) / s^4 - 3 else NA_real_
    )
  }

  output <- Filter(Negate(is.null), output)
  if (!length(output)) return(data.frame())

  do.call(rbind, output)
}

all_features <- list()
errors <- list()

for (i in seq_len(nrow(inventory))) {
  id <- as.character(inventory$record_id[i])
  folder <- as.character(inventory$folder[i])

  cat(sprintf("[%d/%d] Record %s: ",
              i, nrow(inventory), id))

  result <- tryCatch(
    extract_record(id, folder),
    error = function(e) {
      msg <- conditionMessage(e)
      cat("ERROR -", msg, "\n")

      errors[[length(errors) + 1]] <<- data.frame(
        record_id = id,
        error = msg,
        stringsAsFactors = FALSE
      )

      NULL
    }
  )

  if (is.null(result)) {
    next
  } else if (nrow(result) == 0) {
    cat("no valid beats\n")
  } else {
    all_features[[length(all_features) + 1]] <- result
    cat(nrow(result), "beats\n")
  }
}

error_table <- if (length(errors)) {
  do.call(rbind, errors)
} else {
  data.frame(record_id = character(), error = character())
}

write.csv(
  error_table,
  file.path(results_dir, "extraction_errors.csv"),
  row.names = FALSE
)

if (!length(all_features)) {
  stop("No features extracted. Check the errors printed above.")
}

features <- do.call(rbind, all_features)

write.csv(
  features,
  file.path(processed_dir, "all_records_features.csv"),
  row.names = FALSE
)

cat("\n========== EXTRACTION SUMMARY ==========\n")
cat("Successful records:", length(all_features), "\n")
cat("Records with errors:", length(errors), "\n")
cat("Total beats:", nrow(features), "\n")
cat("Columns:", ncol(features), "\n")
cat("Saved:", file.path(processed_dir, "all_records_features.csv"), "\n")
