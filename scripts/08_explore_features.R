
# 08_explore_features.R
# Explore features extracted from record 100

project_dir <- getwd()

input_file <- file.path(
  project_dir,
  "data", "processed", "record100_features.csv"
)

output_dir <- file.path(project_dir, "results")
plot_dir <- file.path(project_dir, "plots")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(input_file)

cat("DATASET DIMENSIONS\n")
cat("==================\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n\n")

cat("BEATS BY ANNOTATION\n")
cat("===================\n")
print(table(df$annotation))

# Summarize selected features by annotation
features <- c("sd", "RMS", "range", "energy", "skewness", "kurtosis")

summary_table <- do.call(
  rbind,
  lapply(split(df, df$annotation), function(g) {
    data.frame(
      annotation = g$annotation[1],
      beats = nrow(g),
      mean_sd = mean(g$sd),
      mean_RMS = mean(g$RMS),
      mean_range = mean(g$range),
      mean_energy = mean(g$energy),
      mean_skewness = mean(g$skewness),
      mean_kurtosis = mean(g$kurtosis)
    )
  })
)

cat("\nFEATURE SUMMARY BY ANNOTATION\n")
print(summary_table, row.names = FALSE)

write.csv(
  summary_table,
  file.path(output_dir, "record100_feature_summary.csv"),
  row.names = FALSE
)

# Plot RMS for the two most frequent beat types
plot_data <- df[df$annotation %in% c("N", "A"), ]

png(
  file.path(plot_dir, "record100_rms_by_annotation.png"),
  width = 1000,
  height = 650
)

boxplot(
  RMS ~ annotation,
  data = plot_data,
  xlab = "MIT-BIH annotation",
  ylab = "RMS of baseline-corrected beat",
  main = "Record 100: Beat RMS by Annotation",
  col = c("lightgray", "lightblue")
)

dev.off()

cat("\nSaved summary: results/record100_feature_summary.csv\n")
cat("Saved plot: plots/record100_rms_by_annotation.png\n")
