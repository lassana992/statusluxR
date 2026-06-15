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
  row_name <- sub(".*\\$", "", deparse(substitute(row_var)))
  col_name <- sub(".*\\$", "", deparse(substitute(col_var)))

  df <- data.frame(Row_Variable = row_var, Col_Variable = col_var)

  table <- gtsummary::tbl_cross(
    data = df,
    row = Row_Variable,
    col = Col_Variable,
    percent = percent,
    label = list(Row_Variable ~ row_name, Col_Variable ~ col_name),
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

  var1_name <- sub(".*\\$", "", deparse(substitute(var1)))
  var2_name <- if(!missing(var2)) sub(".*\\$", "", deparse(substitute(var2))) else NULL

  if (!is.null(var2)) {
    data <- table(var1, var2)
    row_var_label <- var1_name
    col_var_label <- var2_name
  } else {
    data <- var1
    dn <- names(dimnames(data))
    if (!is.null(dn) && length(dn) >= 2 && dn[1] != "" && dn[2] != "") {
      row_var_label <- dn[1]
      col_var_label <- dn[2]
    } else {
      row_var_label <- "Exposure"
      col_var_label <- "Outcome"
    }
  }

  if (!is.matrix(data) && !is.table(data)) {
    stop("Input could not be converted to a table. Please provide two valid categorical variables.")
  }

  r <- nrow(data)
  c <- ncol(data)

  if (r < 2 || c < 2) {
    stop("Data must have at least two categories for both exposure and outcome.")
  }

  row_names <- rownames(data)
  if (is.null(row_names)) row_names <- paste0("Row_", 1:r)
  col_names <- colnames(data)
  if (is.null(col_names)) col_names <- paste0("Col_", 1:c)

  results_list <- list()

  for (i in 1:r) {
    if (i == ref_row) next
    for (j in 1:c) {
      if (j == ref_col) next

      a <- data[i, j]
      b <- data[i, ref_col]
      c_val <- data[ref_row, j]
      d <- data[ref_row, ref_col]

      if (a == 0 || b == 0 || c_val == 0 || d == 0) {
        a <- a + 0.5; b <- b + 0.5; c_val <- c_val + 0.5; d <- d + 0.5
      }

      prob_exp <- a / (a + b)
      prob_unexp <- c_val / (c_val + d)
      rr <- prob_exp / prob_unexp
      se_log_rr <- sqrt((1/a) + (1/c_val) - (1/(a+b)) - (1/(c_val+d)))
      rr_lower <- exp(log(rr) - 1.96 * se_log_rr)
      rr_upper <- exp(log(rr) + 1.96 * se_log_rr)

      or <- (a * d) / (b * c_val)
      se_log_or <- sqrt((1/a) + (1/b) + (1/c_val) + (1/d))
      or_lower <- exp(log(or) - 1.96 * se_log_or)
      or_upper <- exp(log(or) + 1.96 * se_log_or)

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

  var1_name <- sub(".*\\$", "", deparse(substitute(var1)))
  var2_name <- if(!missing(var2)) sub(".*\\$", "", deparse(substitute(var2))) else NULL

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

  if (!is.matrix(data) && !is.table(data)) {
    stop("Input must be a 2x2 matrix, table, or two raw categorical vectors.")
  }
  if (nrow(data) != 2 || ncol(data) != 2) {
    stop("Diagnostic statistics require exactly a 2x2 table layout.")
  }

  tp <- data[1, 1]
  fp <- data[1, 2]
  fn <- data[2, 1]
  tn <- data[2, 2]

  sens_test <- binom.test(tp, tp + fn)
  spec_test <- binom.test(tn, tn + fp)
  ppv_test  <- binom.test(tp, tp + fp)
  npv_test  <- binom.test(tn, tn + fn)

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





#' Generate a Professional Grouped Frequency Distribution Table
#'
#' @param data A data frame containing the dataset.
#' @param num_var The numerical variable.
#' @param cat_var The categorical variable.
#' @param bins Integer. Number of bins.
#' @param width Numeric. Exact class width.
#'
#' @return A list containing the clean data and the gt table.
group_frequency_table <- function(data, num_var, cat_var, bins = 5, width = NULL) {

  # 1. Capture variables and prepare data
  num_var_str <- rlang::as_name(rlang::enquo(num_var))
  cat_var_str <- rlang::as_name(rlang::enquo(cat_var))

  num_vec <- data[[num_var_str]]
  min_val <- floor(min(num_vec, na.rm = TRUE))
  max_val <- ceiling(max(num_vec, na.rm = TRUE))

  if (is.null(width)) {
    width <- max(1, round((max_val - min_val) / bins))
  }

  start_val <- floor(min_val / width) * width
  breaks <- seq(start_val, max_val + width, by = width)
  lbls <- paste0(breaks[-length(breaks)], "-", breaks[-1] - 1)

  # 2. Process data into long format
  df_proc <- data |>
    dplyr::mutate(
      Interval = cut(.data[[num_var_str]], breaks = breaks, labels = lbls, right = FALSE, include.lowest = TRUE),
      Category = as.factor(tidyr::replace_na(as.character(.data[[cat_var_str]]), "Missing"))
    ) |>
    dplyr::filter(!is.na(Interval)) |>
    dplyr::group_by(Interval, Category) |>
    dplyr::summarise(Frequency = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(Interval) |>
    dplyr::mutate(
      Relative_Frequency = Frequency / sum(Frequency),
      Cumulative_Frequency = cumsum(Frequency)
    ) |>
    dplyr::ungroup()

  # 3. Create professional gt table
  # Using groupname_col handles the "overlapping" visual issue automatically
  tbl <- gt::gt(df_proc, groupname_col = "Interval") |>
    gt::tab_header(
      title = gt::md(glue::glue("**Distribution of {num_var_str} by {cat_var_str}**")),
      subtitle = glue::glue("Sample Size: {sum(df_proc$Frequency)}")
    ) |>
    gt::cols_label(
      Frequency = "Freq",
      Relative_Frequency = "Rel. Freq",
      Cumulative_Frequency = "Cum. Freq"
    ) |>
    # Modern formatting (avoids deprecated 'formatter' warning)
    gt::fmt_number(columns = "Relative_Frequency", decimals = 3) |>
    gt::fmt_number(columns = c("Frequency", "Cumulative_Frequency"), decimals = 0) |>
    # Add subtotals for each Interval
    gt::summary_rows(
      groups = TRUE,
      columns = c("Frequency"),
      fns = list(Subtotal = ~sum(., na.rm = TRUE)),
      fmt = ~gt::fmt_number(., decimals = 0)
    ) |>
    # Styling for a professional look
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_options(
      row_group.background.color = "#f4f4f4",
      table.border.top.color = "black",
      table.border.bottom.color = "black",
      column_labels.border.bottom.color = "black"
    )

  return(list(data = df_proc, table = tbl))
}
