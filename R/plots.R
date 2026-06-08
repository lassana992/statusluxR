#' Grouped Histogram
#'
#' Creates a modern histogram from grouped data. Bars touch directly
#' to accurately represent continuous class intervals.
#'
#' @param x numeric vector (raw data)
#' @param width optional interval width
#' @return A ggplot object
#' @export
group_histogram <- function(x, width = NULL) {
  # FIX: Updated to use the correct frequency function name
  df <- group_exclusive_freq(x, width)

  # width = 1 ensures no spaces between bars, color = "white" adds crisp borders
  ggplot2::ggplot(df, ggplot2::aes(x = Class, y = Frequency)) +
    ggplot2::geom_col(fill = "#2980B9", color = "white", width = 1) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) + # Extra space at top
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = "Grouped Histogram",
      x = "Class Intervals",
      y = "Frequency"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank() # Remove vertical lines for cleaner look
    )
}

#' Grouped Boxplot
#'
#' Creates a modern, horizontal boxplot for a numeric vector.
#'
#' @param x numeric vector
#' @return A ggplot object
#' @export
group_boxplot <- function(x) {
  df <- data.frame(Value = x)

  ggplot2::ggplot(df, ggplot2::aes(x = Value)) +
    ggplot2::geom_boxplot(fill = "#E67E22", color = "#D35400", alpha = 0.8, width = 0.5) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = "Boxplot of Data",
      x = "Values"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank()
    )
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

#' Bar Chart for Categorical Data
#'
#' @param x categorical variable (vector)
#' @return A ggplot object
#' @export
cat_barplot <- function(x) {
  df <- as.data.frame(table(x))
  colnames(df) <- c("Category", "Count")

  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(Category, -Count), y = Count)) +
    ggplot2::geom_col(fill = "#34495E", width = 0.7) +
    # ADDED: Text labels above the bars
    ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.5, fontface = "bold", color = "#2C3E50", size = 5) +
    # Expand Y axis slightly so the top labels don't get cut off
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = "Categorical Bar Chart",
      x = "Categories",
      y = "Count"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank()
    )
}

#' Pie Chart for Categorical Data
#'
#' @param x categorical variable (vector)
#' @return A ggplot object
#' @export
cat_pie <- function(x) {
  df <- as.data.frame(table(x))
  colnames(df) <- c("Category", "Count")

  df$Percentage <- df$Count / sum(df$Count) * 100

  ggplot2::ggplot(df, ggplot2::aes(x = "", y = Count, fill = Category)) +
    ggplot2::geom_col(width = 1, color = "white", linewidth = 1) + # Crisp white borders between slices
    ggplot2::coord_polar("y", start = 0) +
    # ADDED: Percentages exactly inside the pie slices
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(Percentage, 1), "%")),
      position = ggplot2::position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 5
    ) +
    # ADDED: Built-in professional color scale
    ggplot2::scale_fill_viridis_d(option = "D") +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::labs(
      title = "Categorical Pie Chart",
      fill = "Category"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 15)),
      legend.position = "right"
    )
}
