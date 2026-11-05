
# Script 21: Evaluate and visualize ECG classification models

project_dir <- getwd()
results_dir <- file.path(project_dir, "results")
plots_dir <- file.path(project_dir, "plots")

input_file <- file.path(
  results_dir, "test_set_predictions.csv"
)

dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Prediction file not found. Run Script 20 first.")
}

pred <- read.csv(input_file, stringsAsFactors = FALSE)

required <- c(
  "actual_class",
  "logistic_probability",
  "tree_probability"
)

if (!all(required %in% names(pred))) {
  stop("Required prediction columns are missing.")
}

pred$actual_class <- as.character(pred$actual_class)

if (any(!is.finite(pred$logistic_probability)) ||
    any(!is.finite(pred$tree_probability))) {
  stop("Non-finite predictions found.")
}

# Function to calculate confusion matrix and metrics
evaluate_model <- function(actual, probability, threshold, model_name) {
  actual <- factor(
    actual,
    levels = c("Normal", "Ventricular")
  )

  predicted <- factor(
    ifelse(probability >= threshold, "Ventricular", "Normal"),
    levels = c("Normal", "Ventricular")
  )

  cm <- table(Actual = actual, Predicted = predicted)

  tn <- as.numeric(cm["Normal", "Normal"])
  fp <- as.numeric(cm["Normal", "Ventricular"])
  fn <- as.numeric(cm["Ventricular", "Normal"])
  tp <- as.numeric(cm["Ventricular", "Ventricular"])

  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_

  metrics <- data.frame(
    model = model_name,
    threshold = threshold,
    accuracy = (tp + tn) / (tp + tn + fp + fn),
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    F1 = if (is.na(precision) || precision + sensitivity == 0) {
      NA_real_
    } else {
      2 * precision * sensitivity / (precision + sensitivity)
    },
    balanced_accuracy = (sensitivity + specificity) / 2,
    TP = tp, FP = fp, TN = tn, FN = fn
  )

  list(matrix = cm, metrics = metrics)
}

logistic_eval <- evaluate_model(
  pred$actual_class,
  pred$logistic_probability,
  0.50,
  "Logistic regression"
)

tree_eval <- evaluate_model(
  pred$actual_class,
  pred$tree_probability,
  0.50,
  "Decision tree"
)

cat("LOGISTIC REGRESSION CONFUSION MATRIX\n")
print(logistic_eval$matrix)

cat("\nDECISION TREE CONFUSION MATRIX\n")
print(tree_eval$matrix)

metrics <- rbind(
  logistic_eval$metrics,
  tree_eval$metrics
)

write.csv(
  metrics,
  file.path(results_dir, "final_model_metrics.csv"),
  row.names = FALSE
)

# Save confusion matrices as CSV files
write.csv(
  as.data.frame.matrix(logistic_eval$matrix),
  file.path(results_dir, "logistic_confusion_matrix.csv")
)

write.csv(
  as.data.frame.matrix(tree_eval$matrix),
  file.path(results_dir, "tree_confusion_matrix.csv")
)

# Calculate AUC using the rank-based method
calculate_auc <- function(actual, probability) {
  positive <- actual == "Ventricular"
  n_positive <- sum(positive)
  n_negative <- sum(!positive)

  if (n_positive == 0 || n_negative == 0) {
    return(NA_real_)
  }

  ranks <- rank(probability, ties.method = "average")

  (
    sum(ranks[positive]) -
      n_positive * (n_positive + 1) / 2
  ) / (n_positive * n_negative)
}

logistic_auc <- calculate_auc(
  pred$actual_class, pred$logistic_probability
)

tree_auc <- calculate_auc(
  pred$actual_class, pred$tree_probability
)

auc_results <- data.frame(
  model = c("Logistic regression", "Decision tree"),
  AUC = c(logistic_auc, tree_auc)
)

write.csv(
  auc_results,
  file.path(results_dir, "model_auc_results.csv"),
  row.names = FALSE
)

# Generate ROC curve coordinates
roc_points <- function(actual, probability, model_name) {
  thresholds <- c(
    Inf,
    sort(unique(probability), decreasing = TRUE),
    -Inf
  )

  positives <- sum(actual == "Ventricular")
  negatives <- sum(actual == "Normal")

  points <- lapply(thresholds, function(threshold) {
    predicted <- probability >= threshold

    data.frame(
      FPR = sum(predicted & actual == "Normal") / negatives,
      TPR = sum(predicted & actual == "Ventricular") / positives
    )
  })

  result <- unique(do.call(rbind, points))
  result$model <- model_name
  result
}

roc_logistic <- roc_points(
  pred$actual_class, pred$logistic_probability,
  "Logistic regression"
)

roc_tree <- roc_points(
  pred$actual_class, pred$tree_probability,
  "Decision tree"
)

roc_data <- rbind(roc_logistic, roc_tree)

write.csv(
  roc_data,
  file.path(results_dir, "roc_curve_coordinates.csv"),
  row.names = FALSE
)

# Plot ROC curves
png(
  file.path(plots_dir, "model_roc_curves.png"),
  width = 1000, height = 800
)

plot(
  roc_logistic$FPR, roc_logistic$TPR,
  type = "l", lwd = 2,
  xlim = c(0, 1), ylim = c(0, 1),
  xlab = "False Positive Rate (1 - Specificity)",
  ylab = "True Positive Rate (Sensitivity)",
  main = "ROC Curves: ECG Beat Classification"
)

lines(
  roc_tree$FPR, roc_tree$TPR,
  lwd = 2, lty = 2
)

abline(0, 1, lty = 3)

legend(
  "bottomright",
  legend = c(
    sprintf("Logistic regression (AUC = %.3f)", logistic_auc),
    sprintf("Decision tree (AUC = %.3f)", tree_auc),
    "Random classifier"
  ),
  lty = c(1, 2, 3),
  lwd = c(2, 2, 1),
  bty = "n"
)

dev.off()

# Plot confusion matrices side by side
png(
  file.path(plots_dir, "model_confusion_matrices.png"),
  width = 1200, height = 600
)

par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

draw_confusion <- function(cm, title) {
  values <- as.matrix(cm)

  image(
    x = 1:2, y = 1:2,
    z = t(values[2:1, , drop = FALSE]),
    axes = FALSE,
    col = gray.colors(20, start = 0.95, end = 0.35),
    xlab = "Predicted class",
    ylab = "Actual class",
    main = title
  )

  axis(1, at = 1:2, labels = colnames(values))
  axis(2, at = 1:2, labels = rev(rownames(values)))

  for (i in 1:2) {
    for (j in 1:2) {
      text(j, 3 - i, labels = values[i, j], cex = 1.2)
    }
  }
}

draw_confusion(
  logistic_eval$matrix,
  "Logistic Regression"
)

draw_confusion(
  tree_eval$matrix,
  "Decision Tree"
)

dev.off()

cat("\nFINAL METRICS AT THRESHOLD 0.50\n")
print(metrics, row.names = FALSE, digits = 4)

cat("\nAUC RESULTS\n")
print(auc_results, row.names = FALSE, digits = 4)

cat("\nSaved files:\n")
cat("- results/final_model_metrics.csv\n")
cat("- results/model_auc_results.csv\n")
cat("- results/roc_curve_coordinates.csv\n")
cat("- results/logistic_confusion_matrix.csv\n")
cat("- results/tree_confusion_matrix.csv\n")
cat("- plots/model_roc_curves.png\n")
cat("- plots/model_confusion_matrices.png\n")
