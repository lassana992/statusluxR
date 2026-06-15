#' Advanced Grouped Histogram
#'
#' @param x numeric vector
#' @param rule string: "Sturges", "Scott", "FD", or "Custom"
#' @param width custom interval width (if rule="Custom")
#' @param title custom plot title
#' @param xlab custom x-axis label
#' @param ylab custom y-axis label
#' @param fill_color custom fill color
#' @param border_color custom border color
#' @return A ggplot object
#' @export
group_histogram <- function(x, rule = "Sturges", width = NULL,
                            title = "Grouped Histogram", xlab = "Class Intervals",
                            ylab = "Frequency", fill_color = "#2980B9", border_color = "white") {

  # Explicit manual mathematical calculations for bin widths
  x_clean <- x[!is.na(x)]
  n <- length(x_clean)

  if (rule == "Sturges") {
    k <- ceiling(log2(n) + 1)
    width <- (max(x_clean) - min(x_clean)) / k
  } else if (rule == "Scott") {
    s <- stats::sd(x_clean)
    width <- (3.49 * s) / (n^(1/3))
  } else if (rule == "FD") {
    iqr_val <- stats::IQR(x_clean)
    width <- (2 * iqr_val) / (n^(1/3))
  }

  df <- group_exclusive_freq(x, width)

  ggplot2::ggplot(df, ggplot2::aes(x = Class, y = Frequency)) +
    ggplot2::geom_col(fill = fill_color, color = border_color, width = 1) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank()
    )
}



#' Advanced Boxplot (Single or Grouped)
#'
#' @param num_var numeric variable
#' @param cat_var optional categorical variable for grouping
#' @param title custom title
#' @param xlab custom x label
#' @param ylab custom y label
#' @param fill_color box fill color
#' @param outlier_color color of outlier points
#' @param notches boolean for confidence interval notches
#' @param orientation "vertical" or "horizontal"
#' @return A ggplot object
#' @export
group_boxplot <- function(num_var, cat_var = NULL, title = "Boxplot",
                          xlab = "Group", ylab = "Values",
                          fill_color = "#E67E22", outlier_color = "#C0392B",
                          notches = FALSE, orientation = "horizontal") {

  if(is.null(cat_var)) {
    df <- data.frame(Value = num_var, Group = "All Data")
  } else {
    df <- data.frame(Value = num_var, Group = as.factor(cat_var))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = Group, y = Value)) +
    ggplot2::geom_boxplot(fill = fill_color, color = "#2C3E50",
                          outlier.colour = outlier_color, outlier.size = 3,
                          notch = notches, alpha = 0.8, width = 0.5) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))

  if(orientation == "horizontal") {
    p <- p + ggplot2::coord_flip()
  }

  return(p)
}


#' Frequency Polygon
#'
#' @param x numeric vector
#' @param width optional interval width
#' @return A ggplot object
#' @export
group_polygon <- function(x, width = NULL) {
  # FIX: Updated function name
  df <- group_exclusive_freq(x, width)

  ggplot2::ggplot(df, ggplot2::aes(x = Midpoint, y = Frequency)) +
    ggplot2::geom_line(color = "#2C3E50", linewidth = 1.2) + # Modernized size to linewidth
    ggplot2::geom_point(color = "#E74C3C", size = 3.5) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = "Frequency Polygon",
      x = "Midpoint",
      y = "Frequency"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
}

#' Ogive Curve
#'
#' Creates a modern cumulative frequency curve.
#'
#' @param x numeric vector
#' @param width optional interval width
#' @return A ggplot object
#' @export
group_ogive <- function(x, width = NULL) {
  # FIX: Updated function name
  df <- group_exclusive_freq(x, width)

  ggplot2::ggplot(df, ggplot2::aes(x = Upper, y = Cumulative_Frequency)) +
    ggplot2::geom_line(color = "#27AE60", linewidth = 1.2) +
    ggplot2::geom_point(color = "#2C3E50", size = 3.5) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = "Ogive (Cumulative Frequency Curve)",
      x = "Upper Class Boundary",
      y = "Cumulative Frequency"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
}

#' Customizable Categorical Bar Chart
#'
#' @param x categorical variable
#' @param title custom title
#' @param xlab custom x label
#' @param ylab custom y label
#' @param fill_color custom fill color
#' @param orientation "vertical" or "horizontal"
#' @return A ggplot object
#' @export
cat_barplot <- function(x, title = "Categorical Bar Chart", xlab = "Categories",
                        ylab = "Count", fill_color = "#34495E", orientation = "vertical") {
  df <- as.data.frame(table(x))
  colnames(df) <- c("Category", "Count")

  p <- ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(Category, -Count), y = Count)) +
    ggplot2::geom_col(fill = fill_color, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = Count), vjust = if(orientation == "vertical") -0.5 else 0.5,
                       hjust = if(orientation == "horizontal") -0.2 else 0.5,
                       fontface = "bold", color = "#2C3E50", size = 5) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      panel.grid.major.x = ggplot2::element_blank()
    )

  if(orientation == "horizontal") {
    p <- p + ggplot2::coord_flip()
  } else {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  return(p)
}


#' Professional Pie Chart
#'
#' @param x categorical variable
#' @param title custom title
#' @param show_legend boolean to display legend
#' @param label_pos multiplier for label position (0.5 is center)
#' @param colors Optional vector of custom colors. Defaults to NULL (Viridis).
#' @return A ggplot object
#' @export
cat_pie <- function(x, title = "Categorical Pie Chart", show_legend = TRUE,
                    label_pos = 0.5, colors = NULL) {
  df <- as.data.frame(table(x))
  colnames(df) <- c("Category", "Count")
  df$Percentage <- (df$Count / sum(df$Count)) * 100
  df$Label <- paste0(df$Count, "\n(", round(df$Percentage, 1), "%)")

  p <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = Count, fill = Category)) +
    ggplot2::geom_col(width = 1, color = "white", linewidth = 1) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::geom_text(
      ggplot2::aes(label = Label),
      position = ggplot2::position_stack(vjust = label_pos),
      color = "white", fontface = "bold", size = 5
    ) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::labs(title = title, fill = "Category") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 15))
    )

  # Logic for custom colors
  if (is.null(colors)) {
    p <- p + ggplot2::scale_fill_viridis_d(option = "D")
  } else {
    p <- p + ggplot2::scale_fill_manual(values = colors)
  }

  if(!show_legend) { p <- p + ggplot2::theme(legend.position = "none") }
  return(p)
}


#' Professional Pareto Chart
#'
#' @param x categorical variable
#' @param title custom title
#' @param xlab custom x axis
#' @param ylab custom y axis
#' @param fill_color custom bar color
#' @param line_color custom line color
#' @return A ggplot object
#' @export
pareto_chart <- function(x, title = "Pareto Chart", xlab = "Category",
                         ylab = "Frequency", fill_color = "#34495E", line_color = "#E74C3C") {

  df <- as.data.frame(table(x))
  colnames(df) <- c("Category", "Count")

  # Manual descending order and cumulative logic
  df <- df[order(-df$Count), ]
  df$Category <- factor(df$Category, levels = df$Category)
  df$Cumul_Pct <- cumsum(df$Count) / sum(df$Count) * 100

  scaling_factor <- sum(df$Count) / 100

  ggplot2::ggplot(df, ggplot2::aes(x = Category)) +
    ggplot2::geom_col(ggplot2::aes(y = Count), fill = fill_color, width = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = Cumul_Pct * scaling_factor, group = 1), color = line_color, linewidth = 1.5) +
    ggplot2::geom_point(ggplot2::aes(y = Cumul_Pct * scaling_factor), color = line_color, size = 3) +
    ggplot2::scale_y_continuous(
      name = ylab,
      sec.axis = ggplot2::sec_axis(~ . / scaling_factor, name = "Cumulative Percentage (%)")
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(title = title, x = xlab) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}




