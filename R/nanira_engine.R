#' statusluxR Percentage Engine
#'
#' @param data A data frame.
#' @param vars Character vector of numeric columns. If NULL, auto-selects all.
#' @param type Calculation approach: "total", "group", or "row".
#' @param group_vars Optional character vector for group-wise scaling.
#' @return A data frame with appended percentage columns.
#' @export
percent_engine <- function(data, vars = NULL, type = "total", group_vars = NULL) {
  res <- as.data.frame(data)
  if (is.null(vars)) {
    vars <- names(res)[sapply(res, is.numeric)]
  }
  if (length(vars) == 0) return(res)

  for (v in vars) {
    if (type == "group" && !is.null(group_vars)) {
      res <- res %>%
        dplyr::group_by(dplyr::pick(dplyr::all_of(group_vars))) %>%
        dplyr::mutate(
          !!paste0(v, "_cnt") := dplyr::n(),
          !!paste0(v, "_pct") := (.data[[v]] / sum(.data[[v]], na.rm = TRUE)) * 100
        ) %>%
        dplyr::ungroup()
    } else if (type == "row") {
      row_sums <- rowSums(res[, vars, drop = FALSE], na.rm = TRUE)
      row_sums[row_sums == 0] <- NA
      res[[paste0(v, "_pct")]] <- (res[[v]] / row_sums) * 100
    } else {
      total_sum <- sum(res[[v]], na.rm = TRUE)
      res[[paste0(v, "_pct")]] <- if (total_sum != 0) (res[[v]] / total_sum) * 100 else 0
    }
  }
  return(res)
}

#' statusluxR Standardization Engine
#'
#' @param data A data frame.
#' @param vars Target numeric vectors. If NULL, auto-selects all.
#' @param method Normalization formula: "zscore", "minmax", "robust", or "unit".
#' @param keep_original Logical. If FALSE, replaces original columns instead of appending.
#' @return A data frame with normalized transformations.
#' @export
standardize_engine <- function(data, vars = NULL, method = "zscore", keep_original = TRUE) {
  res <- as.data.frame(data)
  if (is.null(vars)) {
    vars <- names(res)[sapply(res, is.numeric)]
  }

  for (v in vars) {
    x <- res[[v]]
    if (all(is.na(x))) next
    new_col <- paste0(v, "_scaled")

    res[[new_col]] <- switch(method,
                             "zscore"  = if(sd(x, na.rm=TRUE) != 0) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE) else 0,
                             "minmax"  = if((max(x, na.rm=TRUE) - min(x, na.rm=TRUE)) != 0) (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)) else 0,
                             "robust"  = if(IQR(x, na.rm=TRUE) != 0) (x - median(x, na.rm=TRUE)) / IQR(x, na.rm=TRUE) else 0,
                             "unit"    = if(sqrt(sum(x^2, na.rm=TRUE)) != 0) x / sqrt(sum(x^2, na.rm=TRUE)) else 0
    )
    if (!keep_original) {
      res[[v]] <- NULL
      names(res)[names(res) == new_col] <- v
    }
  }
  return(res)
}

#' statusluxR Outlier Detection & Treatment Module
#'
#' @param data A data frame.
#' @param vars Target numeric vectors. If NULL, auto-selects all.
#' @param method Outlier detection method: "iqr", "zscore", or "modified_z".
#' @param action Modification response: "none", "winsorize", or "flag".
#' @return A data frame with handled outlier attributes.
#' @export
detect_outliers <- function(data, vars = NULL, method = "iqr", action = "none") {
  res <- as.data.frame(data)
  if (is.null(vars)) {
    vars <- names(res)[sapply(res, is.numeric)]
  }

  for (v in vars) {
    x <- res[[v]]
    if (all(is.na(x))) next

    if (method == "iqr") {
      q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
      iqr_val <- q[2] - q[1]
      lower <- q[1] - 1.5 * iqr_val
      upper <- q[2] + 1.5 * iqr_val
    } else if (method == "zscore") {
      mu <- mean(x, na.rm = TRUE)
      sigma <- sd(x, na.rm = TRUE)
      lower <- mu - 3 * sigma
      upper <- mu + 3 * sigma
    } else if (method == "modified_z") {
      med <- median(x, na.rm = TRUE)
      mad_val <- mad(x, na.rm = TRUE)
      if (mad_val == 0) mad_val <- 1e-6
      mod_z <- (0.6745 * (x - med)) / mad_val
      outliers <- abs(mod_z) > 3.5
    }

    if (method != "modified_z") {
      outliers <- x < lower | x > upper
    }

    if (action == "winsorize") {
      if (method == "modified_z") {
        q_lims <- quantile(x, probs = c(0.05, 0.95), na.rm = TRUE)
        res[[v]][x < q_lims[1]] <- q_lims[1]
        res[[v]][x > q_lims[2]] <- q_lims[2]
      } else {
        res[[v]][x < lower] <- lower
        res[[v]][x > upper] <- upper
      }
    } else if (action == "flag") {
      res[[paste0(v, "_outlier")]] <- as.integer(outliers & !is.na(x))
    }
  }
  return(res)
}

#' Advanced Missing Data Intelligence Engine for statusluxR
#'
#' @param data Input dataframe targeted for compliance evaluation.
#' @param method Process route: "visualize", "impute", "clean", or "full".
#' @param impute_method Math approach used: "mean", "median", "mode", "knn", "hotdeck", "colddeck", "rf", "mice", "auto".
#' @param remove_na Logical. If TRUE, runs structural row pruning on any remaining missing elements.
#' @param standardize Logical. Toggles advanced normalization transformations.
#' @param percent Logical. Appends tracking percentage matrices.
#' @param missing_threshold Fractional limit (0-1) for high-missing variable pruning.
#' @param show_report Toggles execution diagnostics console dashboard output.
#' @param cold_deck_data External reference dataset required only if impute_method = "colddeck".
#' @param group_vars Character vector specifying group variables for diagnostics.
#' @return An optimized structured list wrapper of class 'nanira_intelligence'.
#'
#' @importFrom naniar vis_miss gg_miss_var gg_miss_case gg_miss_upset gg_miss_fct miss_var_summary miss_case_summary
#' @importFrom VIM kNN hotdeck
#' @importFrom dplyr mutate filter group_by summarise count pick all_of everything n row_number case_when ungroup
#' @export
nanira_engine <- function(data,
                          method = "full",
                          impute_method = "auto",
                          remove_na = FALSE,
                          standardize = FALSE,
                          percent = FALSE,
                          missing_threshold = 0.50,
                          show_report = TRUE,
                          cold_deck_data = NULL,
                          group_vars = NULL) {

  orig_data <- as.data.frame(data)
  working_df <- orig_data
  duplicate_count <- sum(duplicated(working_df))

  # Structural Diagnostics Tables (via naniar)
  var_summary <- naniar::miss_var_summary(working_df)
  case_summary <- naniar::miss_case_summary(working_df)

  quality_tracker <- var_summary %>%
    dplyr::mutate(Quality = dplyr::case_when(
      pct_miss <= 5  ~ "Excellent",
      pct_miss <= 15 ~ "Good",
      pct_miss <= 30 ~ "Moderate",
      TRUE           ~ "Poor"
    ))

  # Threshold High-Missing Drop Logic
  kill_vars <- var_summary$variable[var_summary$pct_miss > (missing_threshold * 100)]
  if (length(kill_vars) > 0 && method %in% c("clean", "full")) {
    working_df <- working_df[, !(names(working_df) %in% kill_vars), drop = FALSE]
  }

  # Build Dynamic Missingness Vector Flags Before Imputation
  na_flags <- as.data.frame(lapply(orig_data, function(x) as.integer(is.na(x))))
  names(na_flags) <- paste0(names(orig_data), "_NA")

  # Imputation Matrix Switching Routing
  if (method %in% c("impute", "clean", "full") && sum(is.na(working_df)) > 0) {

    if (impute_method == "auto") {
      calc_mode <- function(v) {
        clean_v <- na.omit(v)
        if(length(clean_v) == 0) return(NA)
        ux <- unique(clean_v)
        ux[which.max(tabulate(match(clean_v, ux)))]
      }
      for (col in names(working_df)) {
        if (!any(is.na(working_df[[col]]))) next
        miss_ratio <- sum(is.na(working_df[[col]])) / nrow(working_df)

        if (miss_ratio > 0.40) {
          if(is.numeric(working_df[[col]])) working_df[[col]][is.na(working_df[[col]])] <- -999
          else working_df[[col]][is.na(working_df[[col]])] <- "Unknown"
        } else if (is.numeric(working_df[[col]])) {
          if (abs(mean(working_df[[col]], na.rm=TRUE) - median(working_df[[col]], na.rm=TRUE)) > (sd(working_df[[col]], na.rm=TRUE)*0.5)) {
            working_df[[col]][is.na(working_df[[col]])] <- median(working_df[[col]], na.rm=TRUE)
          } else {
            working_df[[col]][is.na(working_df[[col]])] <- mean(working_df[[col]], na.rm=TRUE)
          }
        } else {
          working_df[[col]][is.na(working_df[[col]])] <- calc_mode(working_df[[col]])
        }
      }
    } else if (impute_method == "mean") {
      for(c in names(working_df)) if(is.numeric(working_df[[c]])) working_df[[c]][is.na(working_df[[c]])] <- mean(working_df[[c]], na.rm=TRUE)
    } else if (impute_method == "median") {
      for(c in names(working_df)) if(is.numeric(working_df[[c]])) working_df[[c]][is.na(working_df[[c]])] <- median(working_df[[c]], na.rm=TRUE)
    } else if (impute_method == "knn") {
      working_df <- VIM::kNN(working_df, imp_var = FALSE)
    } else if (impute_method == "hotdeck") {
      working_df <- VIM::hotdeck(working_df, imp_var = FALSE)
    } else if (impute_method == "colddeck") {
      if (is.null(cold_deck_data)) stop("Cold Deck method requires an explicit external donor reference dataset.")
      working_df <- VIM::colddeck(working_df, donor = as.data.frame(cold_deck_data), imp_var = FALSE)
    } else if (impute_method == "rf") {
      if (!requireNamespace("missForest", quietly = TRUE)) stop("Package 'missForest' is required for Random Forest Imputation.")
      rf_res <- missForest::missForest(working_df)
      working_df <- rf_res$ximp
    } else if (impute_method == "mice") {
      if (!requireNamespace("mice", quietly = TRUE)) stop("Package 'mice' is required for Multiple Imputation chains.")
      mice_inst <- mice::mice(working_df, m = 1, method = "pmm", printFlag = FALSE)
      working_df <- mice::complete(mice_inst)
    }
  }

  if (remove_na) {
    working_df <- na.omit(working_df)
  }

  # Secondary Modular Engine Transforms
  if (standardize && method == "full") {
    working_df <- standardize_engine(working_df, method = "zscore")
  }
  if (percent && method == "full") {
    working_df <- percent_engine(working_df, type = if(!is.null(group_vars)) "group" else "total", group_vars = group_vars)
  }

  # Append Indicator Flags
  working_df <- cbind(working_df, na_flags)

  # Quality Index Calculations
  total_cells <- nrow(orig_data) * ncol(orig_data)
  missing_penalty <- (sum(is.na(orig_data)) / total_cells) * 100
  outlier_penalty <- min(20, (sum(sapply(orig_data, function(v) if(is.numeric(v)) sum(abs(scale(v)) > 3, na.rm=TRUE) else 0)) / nrow(orig_data)) * 100)
  duplicate_penalty <- if (duplicate_count > 0) min(15, (duplicate_count / nrow(orig_data)) * 100) else 0

  quality_score <- max(0, min(100, 100 - (missing_penalty + outlier_penalty + duplicate_penalty)))
  quality_grade <- if(quality_score >= 90) "Excellent" else if(quality_score >= 70) "Good" else if(quality_score >= 50) "Moderate" else "Poor"

  # Construct Plots List Map using PURE naniar visualization mappings
  plots_list <- list(
    heatmap   = naniar::vis_miss(orig_data),
    variables = naniar::gg_miss_var(orig_data),
    cases     = naniar::gg_miss_case(orig_data)
  )

  # CRITICAL FIX: Only run upset plot if 2 or more variables contain missing values!
  vars_with_nas <- sum(var_summary$n_miss > 0)
  if (vars_with_nas >= 2) {
    plots_list$upset <- naniar::gg_miss_upset(orig_data)
  }

  if (!is.null(group_vars)) {
    plots_list$group_distribution <- naniar::gg_miss_fct(orig_data, fct = !!ggplot2::sym(group_vars[1]))
  }

  # Terminal Dashboard Logger
  if (show_report) {
    cat("\n", rep("█", 70), "\n", sep = "")
    cat("       STATUSLUXR ADVANCED MISSING DATA INTELLIGENCE REPORT \n")
    cat(rep("─", 70), "\n", sep = "")
    cat(sprintf("  DATA SECURITY INDEX PROFILE : %.2f / 100 [%s]\n", quality_score, quality_grade))
    cat(sprintf("  RECORDS CHECKED             : %d Rows  |  %d Columns\n", nrow(orig_data), ncol(orig_data)))
    cat(sprintf("  MISSING PENALTY RATE        : %.2f  |  OUTLIER PENALTY : %.2f\n", missing_penalty, outlier_penalty))
    cat(sprintf("  DUPLICATED ROWS IDENTIFIED  : %d entries\n", duplicate_count))
    if (length(kill_vars) > 0) {
      cat(sprintf("  PRUNED COLUMNS (>%.0f%% NA)    : %s\n", missing_threshold*100, paste(kill_vars, collapse=", ")))
    }
    cat(rep("█", 70), "\n\n", sep = "")
  }

  return(structure(list(
    data = working_df,
    plots = plots_list,
    diagnostics = list(variables = quality_tracker, cases = case_summary),
    score = list(value = quality_score, grade = quality_grade)
  ), class = "nanira_intelligence"))
}
