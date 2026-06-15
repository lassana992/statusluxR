#' Categorical Frequency Table
#'
#' Creates frequency, relative frequency, and cumulative frequency table
#' for categorical variables.
#'
#' @param x Character or factor vector
#' @return Data frame
#' @export
cat_freq <- function(x){

  if(length(x) == 0) stop("x cannot be empty")

  x <- as.factor(x)

  f <- table(x)

  rf <- round(f / sum(f), 4)
  cf <- cumsum(f)

  data.frame(
    Variable = names(f),
    Frequency = as.numeric(f),
    Relative_Frequency = as.numeric(rf),
    Cumulative_Frequency = as.numeric(cf)
  )
}


#' Categorical Mode
#'
#' Finds most frequent category
#'
#' @param x vector
#' @return mode value
#' @export
cat_mode <- function(x){

  x <- as.factor(x)

  ux <- unique(x)

  ux[which.max(tabulate(match(x, ux)))]
}


#' Ordinal Median
#'
#' Median for ordered categorical data
#'
#' @param x ordered factor
#' @return median category
#' @export
cat_median <- function(x){

  if(!is.ordered(x)){
    x <- ordered(x)
  }

  f <- table(x)
  cf <- cumsum(f)

  n <- sum(f)

  mid_class <- which(cf >= n/2)[1]

  names(f)[mid_class]
}



#' Categorical Summary Report
#'
#' Full summary for categorical variables
#'
#' @param x vector
#' @return list
#' @export
cat_describe <- function(x){

  list(
    frequency_table = cat_freq(x),
    mode = cat_mode(x),
    n_categories = length(unique(x)),
    missing_values = sum(is.na(x))
  )
}



#' Generate a Professional Contingency Table
#'
#' Creates a publication-ready r x c contingency table using the gtsummary package.
#'
#' @param row_var Categorical vector for rows
#' @param col_var Categorical vector for columns
#' @param percent String indicating which percentage to calculate: "column" (default), "row", "cell", or "none"
#' @return A gtsummary object that renders as a formatted HTML/Word table
#' @export
cross_tabulate <- function(row_var, col_var, percent = "column") {
  # 1. Capture the actual variable names passed into the function
  # The regex sub(".*\\$", ...) ensures that if you pass 'my_data$Gender', it just keeps 'Gender'
  row_name <- sub(".*\\$", "", deparse(substitute(row_var)))
  col_name <- sub(".*\\$", "", deparse(substitute(col_var)))

  # 2. Combine into a data frame using placeholder column names
  df <- data.frame(Row_Variable = row_var, Col_Variable = col_var)

  # 3. Generate the table and apply the captured names as custom labels
  table <- gtsummary::tbl_cross(
    data = df,
    row = Row_Variable,
    col = Col_Variable,
    percent = percent,
    label = list(Row_Variable ~ row_name, Col_Variable ~ col_name), # Injects real names here
    margin = c("row", "column")
  ) |>
    gtsummary::bold_labels()

  return(table)
}

#' Calculate Epidemiological Risk Measures (r x c)
#'
#' Computes Odds Ratio (OR) and Relative Risk (RR) with 95% confidence intervals.
#' Automatically handles raw vectors or pre-made tables, and supports tables larger than 2x2.
#'
#' @param var1 A matrix/table, OR the first categorical vector (Exposure).
#' @param var2 The second categorical vector (Outcome). Leave NULL if var1 is already a table.
#' @param ref_row Integer. The row index to use as the baseline/reference exposure (default = 1).
#' @param ref_col Integer. The column index to use as the baseline/reference outcome (default = 1).
#' @return A data frame containing pairwise OR, RR, and 95% CIs.
#' @export
risk_summary <- function(var1, var2 = NULL, ref_row = 1, ref_col = 1) {

  # Capture the variable names before R evaluates them
  var1_name <- sub(".*\\$", "", deparse(substitute(var1)))
  var2_name <- if(!missing(var2)) sub(".*\\$", "", deparse(substitute(var2))) else NULL

  # 1. Auto-Tabulate: If the user passes two raw variables, make the table for them!
  if (!is.null(var2)) {
    data <- table(var1, var2)
    row_var_label <- var1_name
    col_var_label <- var2_name
  } else {
    data <- var1
    # Try to extract names from the table dimensions if it's already a table
    dn <- names(dimnames(data))
    if (!is.null(dn) && length(dn) >= 2 && dn[1] != "" && dn[2] != "") {
      row_var_label <- dn[1]
      col_var_label <- dn[2]
    } else {
      row_var_label <- "Exposure"
      col_var_label <- "Outcome"
    }
  }

  # Input validation
  if (!is.matrix(data) && !is.table(data)) {
    stop("Input could not be converted to a table. Please provide two valid categorical variables.")
  }

  r <- nrow(data)
  c <- ncol(data)

  if (r < 2 || c < 2) {
    stop("Data must have at least two categories for both exposure and outcome.")
  }

  # Extract names for clean output labels
  row_names <- rownames(data)
  if (is.null(row_names)) row_names <- paste0("Row_", 1:r)
  col_names <- colnames(data)
  if (is.null(col_names)) col_names <- paste0("Col_", 1:c)

  results_list <- list()

  # 2. Calculate OR and RR against the baseline
  for (i in 1:r) {
    if (i == ref_row) next
    for (j in 1:c) {
      if (j == ref_col) next

      a <- data[i, j]
      b <- data[i, ref_col]
      c_val <- data[ref_row, j]
      d <- data[ref_row, ref_col]

      # Continuity correction for zero-cells
      if (a == 0 || b == 0 || c_val == 0 || d == 0) {
        a <- a + 0.5; b <- b + 0.5; c_val <- c_val + 0.5; d <- d + 0.5
      }

      # Relative Risk
      prob_exp <- a / (a + b)
      prob_unexp <- c_val / (c_val + d)
      rr <- prob_exp / prob_unexp
      se_log_rr <- sqrt((1/a) + (1/c_val) - (1/(a+b)) - (1/(c_val+d)))
      rr_lower <- exp(log(rr) - 1.96 * se_log_rr)
      rr_upper <- exp(log(rr) + 1.96 * se_log_rr)

      # Odds Ratio
      or <- (a * d) / (b * c_val)
      se_log_or <- sqrt((1/a) + (1/b) + (1/c_val) + (1/d))
      or_lower <- exp(log(or) - 1.96 * se_log_or)
      or_upper <- exp(log(or) + 1.96 * se_log_or)

      # Dynamically build the comparison string with variable names
      comparison_name <- paste0(row_var_label, " (", row_names[i], " vs ", row_names[ref_row], ")",
                                " | ", col_var_label, " (", col_names[j], ")")

      results_list[[length(results_list) + 1]] <- data.frame(
        Comparison = comparison_name, Measure = "Odds Ratio (OR)",
        Estimate = round(or, 3), CI_Lower = round(or_lower, 3), CI_Upper = round(or_upper, 3)
      )

      results_list[[length(results_list) + 1]] <- data.frame(
        Comparison = comparison_name, Measure = "Relative Risk (RR)",
        Estimate = round(rr, 3), CI_Lower = round(rr_lower, 3), CI_Upper = round(rr_upper, 3)
      )
    }
  }

  final_results <- do.call(rbind, results_list)
  rownames(final_results) <- NULL
  return(final_results)
}




#' Calculate Diagnostic Test Accuracy Metrics
#'
#' Computes Sensitivity, Specificity, Positive Predictive Value (PPV), and
#' Negative Predictive Value (NPV) with Exact 95% Confidence Intervals.
#'
#' @param var1 A 2x2 matrix/table, OR a categorical vector representing the Test Result.
#' @param var2 A categorical vector representing the Gold Standard / True Condition. Leave NULL if var1 is a table.
#' @return A data frame with estimates and 95% exact confidence intervals for each metric.
#' @note Assumes that the first row/column represents the "Positive" condition and the second represents "Negative".
#' @export
diagnostic_stats <- function(var1, var2 = NULL) {

  # Capture names for professional output labeling
  var1_name <- sub(".*\\$", "", deparse(substitute(var1)))
  var2_name <- if(!missing(var2)) sub(".*\\$", "", deparse(substitute(var2))) else NULL

  # 1. Auto-Tabulate if raw vectors are provided
  if (!is.null(var2)) {
    data <- table(var1, var2)
    test_label <- var1_name
    gold_label <- var2_name
  } else {
    data <- var1
    dn <- names(dimnames(data))
    test_label <- if (!is.null(dn) && dn[1] != "") dn[1] else "Test Result"
    gold_label <- if (!is.null(dn) && dn[2] != "") dn[2] else "Condition"
  }

  # Validation
  if (!is.matrix(data) && !is.table(data)) {
    stop("Input must be a 2x2 matrix, table, or two raw categorical vectors.")
  }
  if (nrow(data) != 2 || ncol(data) != 2) {
    stop("Diagnostic statistics require exactly a 2x2 table layout.")
  }

  # Extract Confusion Matrix Cells
  # Row 1 = Test Positive, Row 2 = Test Negative
  # Col 1 = Disease Positive, Col 2 = Disease Negative
  tp <- data[1, 1] # True Positives
  fp <- data[1, 2] # False Positives
  fn <- data[2, 1] # False Negatives
  tn <- data[2, 2] # True Negatives

  # 2. Calculate Exact Metrics and CIs using binom.test
  # Sensitivity = TP / (TP + FN)
  sens_test <- binom.test(tp, tp + fn)
  # Specificity = TN / (TN + FP)
  spec_test <- binom.test(tn, tn + fp)
  # PPV = TP / (TP + FP)
  ppv_test  <- binom.test(tp, tp + fp)
  # NPV = TN / (TN + FN)
  npv_test  <- binom.test(tn, tn + fn)

  # 3. Build a beautiful results table
  metrics <- c("Sensitivity (True Positive Rate)",
               "Specificity (True Negative Rate)",
               "Positive Predictive Value (PPV)",
               "Negative Predictive Value (NPV)")

  estimates <- c(sens_test$estimate, spec_test$estimate, ppv_test$estimate, npv_test$estimate)
  lowers    <- c(sens_test$conf.int[1], spec_test$conf.int[1], ppv_test$conf.int[1], npv_test$conf.int[1])
  uppers    <- c(sens_test$conf.int[2], spec_test$conf.int[2], ppv_test$conf.int[2], npv_test$conf.int[2])

  results <- data.frame(
    Context = paste0("Predicting '", gold_label, "' using '", test_label, "'"),
    Metric = metrics,
    Estimate = round(estimates, 4),
    CI_Lower = round(lowers, 4),
    CI_Upper = round(uppers, 4)
  )

  return(results)
}
