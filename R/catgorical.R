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
