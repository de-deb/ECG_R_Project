
# Script 20: Train and evaluate traditional ECG classification models

project_dir <- getwd()

input_file <- file.path(
  project_dir, "data", "processed", "binary_nv_dataset.csv"
)

results_dir <- file.path(project_dir, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Binary dataset not found. Run Script 19 first.")
}

if (!requireNamespace("rpart", quietly = TRUE)) {
  stop("Install rpart using install.packages('rpart') first.")
}

data <- read.csv(input_file, stringsAsFactors = FALSE)

# Exclude variance and energy because they are mathematically
# redundant with SD and RMS for fixed-length beat windows.
features <- c(
  "mean", "median", "sd", "range",
  "IQR", "RMS", "skewness", "kurtosis"
)

required <- c("record_id", "group_id", "class", "split", features)

if (!all(required %in% names(data))) {
  stop("Required columns are missing from the dataset.")
}

data$class <- trimws(as.character(data$class))
data$split <- trimws(as.character(data$split))
data$record_id <- as.character(data$record_id)
data$group_id <- as.character(data$group_id)

data <- data[
  data$class %in% c("Normal", "Ventricular") &
    data$split %in% c("train", "test"),
  ,
  drop = FALSE
]

train <- data[data$split == "train", , drop = FALSE]
test <- data[data$split == "test", , drop = FALSE]

if (nrow(train) == 0 || nrow(test) == 0) {
  stop("Training or testing data is empty.")
}

if (!all(c("Normal", "Ventricular") %in% unique(train$class))) {
  stop("Training data must contain both classes.")
}

if (!all(c("Normal", "Ventricular") %in% unique(test$class))) {
  stop("Testing data must contain both classes.")
}

if (any(!is.finite(as.matrix(data[, features, drop = FALSE])))) {
  stop("Missing or non-finite feature values found.")
}

cat("Original training class counts:\n")
print(table(train$class))

cat("\nTest class counts (left unchanged):\n")
print(table(test$class))

# Balance training by undersampling Normal beats.
# All Ventricular training beats are retained.
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
balanced_train <- balanced_train[
  sample(seq_len(nrow(balanced_train))),
  ,
  drop = FALSE
]

balanced_train$class <- factor(
  balanced_train$class,
  levels = c("Normal", "Ventricular")
)

test$class <- factor(
  test$class,
  levels = c("Normal", "Ventricular")
)

cat("\nBalanced training class counts:\n")
print(table(balanced_train$class))

# Scale using balanced training data only.
train_means <- sapply(
  balanced_train[, features, drop = FALSE],
  mean
)

train_sds <- sapply(
  balanced_train[, features, drop = FALSE],
  sd
)

train_sds[!is.finite(train_sds) | train_sds == 0] <- 1

scale_features <- function(df) {
  result <- df

  for (feature in features) {
    result[[feature]] <-
      (result[[feature]] - train_means[feature]) /
      train_sds[feature]
  }

  result
}

train_scaled <- scale_features(balanced_train)
test_scaled <- scale_features(test)

train_scaled$class <- factor(
  train_scaled$class,
  levels = c("Normal", "Ventricular")
)

test_scaled$class <- factor(
  test_scaled$class,
  levels = c("Normal", "Ventricular")
)

# Model 1: logistic regression.
# Numeric response: 0 = Normal, 1 = Ventricular.
train_scaled$ventricular <- as.integer(
  train_scaled$class == "Ventricular"
)

logistic_formula <- reformulate(
  features,
  response = "ventricular"
)

logistic_model <- glm(
  logistic_formula,
  data = train_scaled,
  family = binomial(),
  control = glm.control(maxit = 100)
)

logistic_prob <- as.numeric(
  predict(
    logistic_model,
    newdata = test_scaled,
    type = "response"
  )
)

# Model 2: decision tree.
tree_formula <- reformulate(
  features,
  response = "class"
)

tree_model <- rpart::rpart(
  tree_formula,
  data = train_scaled,
  method = "class",
  control = rpart::rpart.control(
    cp = 0.005,
    minsplit = 50,
    maxdepth = 8
  )
)

tree_prob_matrix <- predict(
  tree_model,
  newdata = test_scaled,
  type = "prob"
)

tree_prob <- as.numeric(
  tree_prob_matrix[, "Ventricular"]
)

if (
  any(!is.finite(logistic_prob)) ||
  any(!is.finite(tree_prob))
) {
  stop("Non-finite model predictions found.")
}

# Calculate metrics from the held-out test set.
calculate_metrics <- function(actual, probability, threshold) {
  predicted <- ifelse(
    probability >= threshold,
    "Ventricular",
    "Normal"
  )

  actual <- factor(
    actual,
    levels = c("Normal", "Ventricular")
  )

  predicted <- factor(
    predicted,
    levels = c("Normal", "Ventricular")
  )

  cm <- table(Actual = actual, Predicted = predicted)

  tn <- as.numeric(cm["Normal", "Normal"])
  fp <- as.numeric(cm["Normal", "Ventricular"])
  fn <- as.numeric(cm["Ventricular", "Normal"])
  tp <- as.numeric(cm["Ventricular", "Ventricular"])

  safe_divide <- function(a, b) {
    if (b == 0) return(NA_real_)
    a / b
  }

  precision <- safe_divide(tp, tp + fp)
  recall <- safe_divide(tp, tp + fn)
  specificity <- safe_divide(tn, tn + fp)

  f1 <- if (
    is.na(precision) || is.na(recall) ||
    precision + recall == 0
  ) {
    NA_real_
  } else {
    2 * precision * recall / (precision + recall)
  }

  data.frame(
    threshold = threshold,
    accuracy = safe_divide(tp + tn, tp + tn + fp + fn),
    precision = precision,
    recall_sensitivity = recall,
    specificity = specificity,
    F1 = f1,
    balanced_accuracy = (recall + specificity) / 2,
    TP = tp,
    FP = fp,
    TN = tn,
    FN = fn
  )
}

thresholds <- c(0.50, 0.25, 0.10)
results <- list()

for (threshold in thresholds) {
  results[[length(results) + 1]] <- cbind(
    model = "Logistic regression",
    calculate_metrics(test$class, logistic_prob, threshold)
  )

  results[[length(results) + 1]] <- cbind(
    model = "Decision tree",
    calculate_metrics(test$class, tree_prob, threshold)
  )
}

results_table <- do.call(rbind, results)
rownames(results_table) <- NULL

write.csv(
  results_table,
  file.path(results_dir, "model_threshold_comparison.csv"),
  row.names = FALSE
)

prediction_output <- data.frame(
  record_id = test$record_id,
  group_id = test$group_id,
  actual_class = test$class,
  logistic_probability = logistic_prob,
  tree_probability = tree_prob
)

write.csv(
  prediction_output,
  file.path(results_dir, "test_set_predictions.csv"),
  row.names = FALSE
)

capture.output(
  summary(logistic_model),
  file = file.path(results_dir, "logistic_model_summary.txt")
)

capture.output(
  print(tree_model),
  rpart::printcp(tree_model),
  file = file.path(results_dir, "decision_tree_summary.txt")
)

cat("\n========== MODEL TRAINING COMPLETE ==========\n")
cat("Balanced training beats:", nrow(train_scaled), "\n")
cat("Testing beats:", nrow(test_scaled), "\n")
cat("Training groups:", length(unique(train$group_id)), "\n")
cat("Testing groups:", length(unique(test$group_id)), "\n")
cat(
  "Overlapping groups:",
  length(intersect(
    unique(train$group_id),
    unique(test$group_id)
  )),
  "\n"
)

cat("\nThreshold comparison:\n")
print(results_table, row.names = FALSE, digits = 4)

cat("\nSaved model and evaluation results in:", results_dir, "\n")
