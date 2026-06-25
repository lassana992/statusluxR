#' statusluxR Percentage Engine
#' @importFrom dplyr group_by mutate ungroup pick all_of
#' @export
percent_engine <- function(data, vars = NULL, type = "total", group_vars = NULL) {
  res <- as.data.frame(data)
  if (is.null(vars)) vars <- names(res)[sapply(res, is.numeric)]
  if (length(vars) == 0) return(res)

  for (v in vars) {
    if (type == "group" && !is.null(group_vars)) {
      res <- res %>%
        dplyr::group_by(dplyr::pick(dplyr::all_of(group_vars))) %>%
        dplyr::mutate(
          !!paste0(v, "_cnt") := dplyr::n(),
          !!paste0(v, "_pct") := (.data[[v]] / sum(.data[[v]], na.rm = TRUE)) * 100
        ) %>% dplyr::ungroup()
    } else if (type == "row") {
      row_sums <- rowSums(res[, vars, drop = FALSE], na.rm = TRUE)
      row_sums[row_sums == 0] <- NA
      res[[paste0(v, "_pct")]] <- (res[[v]] / row_sums) * 100
    } else {
      # Default: total column-wise percentage
      total_sum <- sum(res[[v]], na.rm = TRUE)
      res[[paste0(v, "_pct")]] <- if (total_sum != 0) (res[[v]] / total_sum) * 100 else 0
    }
  }
  return(res)
}

#' statusluxR Standardization Engine
#' @export
standardize_engine <- function(data, vars = NULL, method = "zscore", keep_original = TRUE) {
  res <- as.data.frame(data)
  if (is.null(vars)) vars <- names(res)[sapply(res, is.numeric)]

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
    if (!keep_original) { res[[v]] <- NULL; names(res)[names(res) == new_col] <- v }
  }
  return(res)
}

#' statusluxR Outlier Detection & Treatment Module
#' @export
detect_outliers <- function(data, vars = NULL, method = "iqr", action = "none") {
  res <- as.data.frame(data)
  if (is.null(vars)) vars <- names(res)[sapply(res, is.numeric)]

  for (v in vars) {
    x <- res[[v]]
    if (all(is.na(x))) next

    # Calculate boundaries
    if (method == "iqr") {
      q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
      iqr_val <- q[2] - q[1]
      lower <- q[1] - 1.5 * iqr_val
      upper <- q[2] + 1.5 * iqr_val
    } else if (method == "zscore") {
      mu <- mean(x, na.rm = TRUE); sigma <- sd(x, na.rm = TRUE)
      lower <- mu - 3 * sigma; upper <- mu + 3 * sigma
    } else if (method == "modified_z") {
      med <- median(x, na.rm = TRUE); mad_val <- mad(x, na.rm = TRUE)
      if (mad_val == 0) mad_val <- 1e-6
      mod_z <- (0.6745 * (x - med)) / mad_val
      outliers <- abs(mod_z) > 3.5
    }

    if (method != "modified_z") {
      outliers <- x < lower | x > upper
    }

    if (action == "winsorize") {
      if (method == "modified_z") {
        # Fallback boundaries for clean winsorization scaling limits
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
