
# 06_check_signal.R
# Verify ECG signal units and calibration

library(EGM)

data_dir <- file.path(getwd(), "data")

# Read record 100 in both available unit modes
physical <- read_wfdb(
  record = "100",
  record_dir = data_dir,
  units = "physical"
)

digital <- read_wfdb(
  record = "100",
  record_dir = data_dir,
  units = "digital"
)

cat("HEADER INFORMATION\n")
cat("==================\n")
print(physical$header)

cat("\nPHYSICAL SIGNAL SUMMARY: MLII\n")
cat("=============================\n")
print(summary(physical$signal$MLII))

cat("\nDIGITAL SIGNAL SUMMARY: MLII\n")
cat("============================\n")
print(summary(digital$signal$MLII))

cat("\nFIRST 10 SAMPLES: PHYSICAL\n")
print(head(physical$signal$MLII, 10))

cat("\nFIRST 10 SAMPLES: DIGITAL\n")
print(head(digital$signal$MLII, 10))
