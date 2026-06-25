#' Advanced Variable Recoding Engine for statusluxR
#'
#' @description
#' A professional-grade transformation tool for variable recoding, supporting
#' manual mapping, dummy encoding, reverse coding, collapsing, and automatic type detection.
#'
#' @param data A data frame.
#' @param vars Character vector of columns to recode. If NULL, auto-selects based on method.
#' @param method Strategy: "manual", "auto", "reverse", "collapse", "combine", "dummy", "bin".
#' @param labels Named vector for manual mapping or bin labels.
#' @param breaks Numeric vector of cut points for binning.
#' @param ordered Logical. If TRUE, output factors are strictly ordered.
#' @param keep_original Logical. If FALSE, original columns are overwritten.
#' @param suffix Character string appended to new columns.
#' @param combine Named list for combining categories.
#' @param collapse_threshold Numeric (0 to 1). Frequency threshold to lump into "Other".
#' @param missing_label String/Numeric replacement for NA values.
#' @param ignore_case Logical. Standardizes text case before matching.
#' @param var_label Descriptive label for the variable.
#'
#' @return A transformed data frame of class 'data.frame'.
#' @importFrom forcats fct_collapse fct_lump_prop fct_rev
#' @importFrom labelled var_label
#' @export
recode_engine <- function(data,
                          vars = NULL,
                          method = "manual",
                          labels = NULL,
                          breaks = NULL,
                          ordered = FALSE,
                          keep_original = TRUE,
                          suffix = "_recoded",
                          combine = NULL,
                          collapse_threshold = 0.05,
                          missing_label = NULL,
                          ignore_case = FALSE,
                          var_label = NULL) {

  res <- as.data.frame(data)

  if (is.null(vars)) {
    vars <- if (method %in% c("dummy", "collapse")) names(res)[sapply(res, function(x) is.character(x) || is.factor(x))] else names(res)
  }

  for (v in vars) {
    if (!v %in% names(res)) next

    x <- res[[v]]
    new_col <- if (keep_original) paste0(v, suffix) else v

    # 1. Standardize text if required
    if (ignore_case && (is.character(x) || is.factor(x))) {
      x <- tolower(as.character(x))
      if (!is.null(labels)) names(labels) <- tolower(names(labels))
    }

    # 2. Method Routing
    if (!is.null(breaks) || method == "bin") {
      x <- cut(as.numeric(x), breaks = if(is.null(breaks)) 4 else breaks, labels = labels, include.lowest = TRUE, ordered_result = ordered)
    } else if (method == "manual") {
      if (!is.null(labels)) {
        if (is.numeric(x)) {
          map_vec <- setNames(labels, names(labels))
          x <- unname(map_vec[as.character(x)])
        } else {
          x <- unname(labels[as.character(x)])
        }
      }
    } else if (method == "combine") {
      x <- forcats::fct_collapse(as.factor(x), !!!combine)
    } else if (method == "collapse") {
      x <- forcats::fct_lump_prop(as.factor(x), prop = collapse_threshold, other_level = "Other")
    } else if (method == "reverse") {
      x <- if (is.numeric(x)) (max(x, na.rm=T) + min(x, na.rm=T)) - x else forcats::fct_rev(as.factor(x))
    } else if (method == "dummy") {
      x_fact <- as.factor(x)
      for (lvl in levels(x_fact)) {
        res[[paste0(new_col, "_", gsub("[^a-zA-Z0-9]", "_", lvl))]] <- as.integer(x_fact == lvl)
      }
      if (!keep_original) res[[v]] <- NULL
      next
    }

    # 3. Missing Value Handling
    if (!is.null(missing_label)) {
      x[is.na(x)] <- missing_label
    }

    res[[new_col]] <- x
    if (!is.null(var_label)) labelled::var_label(res[[new_col]]) <- var_label
  }
  return(res)
}
