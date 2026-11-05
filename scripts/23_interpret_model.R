
# Script 23: Interpret logistic regression coefficients

project_dir <- getwd()

data_file <- file.path(
  project_dir, "data", "processed",
  "binary_nv_dataset.csv"
)

results_dir <- file.path(project_dir, "results")
plots_dir <- file.path(project_dir, "plots")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_file)) {
  stop("Binary dataset not found. Run Script 19 first.")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Please install ggplot2 using install.packages('ggplot2').")
}

data <- read.csv(data_file, stringsAsFactors = FALSE)

features <- c(
  "mean", "median", "sd", "range",
  "IQR", "RMS", "skewness", "kurtosis"
)

required <- c("class", "split", "group_id", features)

if (!all(required %in% names(data))) {
  stop("Required columns are missing.")
}

data$class <- as.character(data$class)
data$split <- as.character(data$split)
data$group_id <- as.character(data$group_id)

data <- data[
  data$class %in% c("Normal", "Ventricular") &
    data$split %in% c("train", "test"),
  ,
  drop = FALSE
]

train <- data[data$split == "train", , drop = FALSE]
test <- data[data$split == "test", , drop = FALSE]

# Check for record-group leakage.
overlap <- intersect(
  unique(train$group_id),
  unique(test$group_id)
)

if (length(overlap) > 0) {
  stop("Training and test groups overlap.")
}

# Balance the training data using the same procedure and seed
# as Script 20.
set.seed(2026)

ventricular_train <- train[
  train$class == "Ventricular",
  ,
  drop = FALSE
]

normal_train <- train[
  train$class == "Normal",
  ,
  drop = FALSE
]

normal_sample <- normal_train[
  sample(
    seq_len(nrow(normal_train)),
    size = min(nrow(normal_train), nrow(ventricular_train))
  ),
  ,
  drop = FALSE
]

balanced_train <- rbind(normal_sample, ventricular_train)

# Standardize using the balanced training data only.
train_means <- sapply(
  balanced_train[, features, drop = FALSE],
  mean
)

train_sds <- sapply(
  balanced_train[, features, drop = FALSE],
  sd
)

train_sds[!is.finite(train_sds) | train_sds == 0] <- 1

for (feature in features) {
  balanced_train[[feature]] <-
    (balanced_train[[feature]] - train_means[feature]) /
    train_sds[feature]
}

balanced_train$ventricular <- as.integer(
  balanced_train$class == "Ventricular"
)

model <- glm(
  reformulate(features, response = "ventricular"),
  data = balanced_train,
  family = binomial(),
  control = glm.control(maxit = 100)
)

# Extract coefficient estimates and uncertainty.
coefs <- summary(model)$coefficients

coef_table <- data.frame(
  feature = rownames(coefs),
  coefficient = coefs[, "Estimate"],
  standard_error = coefs[, "Std. Error"],
  z_value = coefs[, "z value"],
  p_value = coefs[, "Pr(>|z|)"],
  row.names = NULL
)

coef_table$odds_ratio <- exp(coef_table$coefficient)
coef_table$CI_lower <- exp(
  coef_table$coefficient - 1.96 * coef_table$standard_error
)
coef_table$CI_upper <- exp(
  coef_table$coefficient + 1.96 * coef_table$standard_error
)

coef_table$BH_adjusted_p_value <- p.adjust(
  coef_table$p_value,
  method = "BH"
)

coef_table$significant_BH_0_05 <- (
  coef_table$BH_adjusted_p_value < 0.05
)

write.csv(
  coef_table,
  file.path(results_dir, "logistic_coefficient_interpretation.csv"),
  row.names = FALSE
)

capture.output(
  summary(model),
  file = file.path(results_dir, "interpreted_logistic_model_summary.txt")
)

# Plot feature coefficients only; exclude intercept.
plot_data <- coef_table[
  coef_table$feature != "(Intercept)",
  ,
  drop = FALSE
]

plot_data$feature <- factor(
  plot_data$feature,
  levels = plot_data$feature[order(plot_data$coefficient)]
)

p <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = coefficient, y = feature)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
  ggplot2::geom_point() +
  ggplot2::geom_errorbarh(
    ggplot2::aes(
      xmin = coefficient - 1.96 * standard_error,
      xmax = coefficient + 1.96 * standard_error
    ),
    height = 0.2
  ) +
  ggplot2::labs(
    title = "Standardized Logistic Regression Coefficients",
    subtitle = "Positive coefficients indicate higher modelled odds of a Ventricular label",
    x = "Coefficient (log-odds scale)",
    y = "Feature"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(plots_dir, "logistic_coefficients.png"),
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)

cat("Training rows used:", nrow(balanced_train), "\n")
cat("Test rows retained but not used for fitting:", nrow(test), "\n")
cat("Training/test group overlap:", length(overlap), "\n\n")

cat("Coefficient interpretation:\n")
print(coef_table, row.names = FALSE, digits = 4)

cat("\nSaved:\n")
cat("- results/logistic_coefficient_interpretation.csv\n")
cat("- results/interpreted_logistic_model_summary.txt\n")
cat("- plots/logistic_coefficients.png\n")

cat(
  "\nImportant: odds ratios describe associations in this fitted model.",
  "They are not causal effects or clinical diagnostic conclusions.\n"
)
