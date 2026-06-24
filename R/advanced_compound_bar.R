#' Create an Advanced Compound Bar Chart
#'
#' Generates a publication-quality compound (grouped or stacked) bar chart
#' with extensive customization options for themes, colors, data labels, and exporting.
#'
#' @param data A data frame containing the dataset.
#' @param x_var The categorical variable for the x-axis (unquoted).
#' @param group_var The categorical grouping variable (unquoted).
#' @param y_var Optional. A numerical variable. If NULL (default), frequencies are calculated automatically.
#' @param type Character. Either `"grouped"` (side-by-side) or `"stacked"`.
#' @param orientation Character. Either `"vertical"` or `"horizontal"`.
#' @param show_labels Logical. If TRUE, displays data labels on the bars.
#' @param label_type Character. Either `"count"` or `"percent"`.
#' @param theme_choice Character. Plot theme: `"minimal"`, `"classic"`, `"bw"`, `"light"`, `"gray"`, or `"void"`.
#' @param palette Character. Built-in palette (e.g., `"Set1"`, `"Dark2"`, `"viridis"`, `"plasma"`, etc.).
#' @param manual_colors A character vector of exact colors to use (overrides `palette` if provided).
#' @param title,subtitle,caption,xlab,ylab,legend_title Character strings for plot text.
#' @param legend_position Character. `"top"`, `"bottom"`, `"left"`, `"right"`, or `"none"`.
#' @param label_angle Numeric. Angle to rotate x-axis labels (e.g., 45 or 90).
#' @param bar_width Numeric. Width of the bars (default 0.9).
#' @param alpha Numeric. Transparency of the bars (0 to 1).
#' @param font_size Numeric. Base font size for the plot.
#' @param font_family Character. Font family (e.g., `"sans"`, `"serif"`, `"mono"`).
#' @param export Logical. If TRUE, saves the plot to the working directory.
#' @param export_format Character. `"png"`, `"jpg"`, `"pdf"`, `"svg"`, or `"tiff"`.
#' @param file_name Character. The base name for the exported file.
#' @param width,height,dpi Numeric. Dimensions and resolution for the exported file.
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @importFrom dplyr count group_by summarise mutate ungroup
#' @importFrom rlang enquo quo_is_null
#' @importFrom scales percent
advanced_compound_bar <- function(data,
                                  x_var,
                                  group_var,
                                  y_var = NULL,
                                  type = c("grouped", "stacked"),
                                  orientation = c("vertical", "horizontal"),
                                  show_labels = TRUE,
                                  label_type = c("count", "percent"),
                                  theme_choice = c("minimal", "classic", "bw", "light", "gray", "void"),
                                  palette = "Set1",
                                  manual_colors = NULL,
                                  title = NULL,
                                  subtitle = NULL,
                                  caption = NULL,
                                  xlab = NULL,
                                  ylab = NULL,
                                  legend_title = NULL,
                                  legend_position = "right",
                                  label_angle = 0,
                                  bar_width = 0.9,
                                  alpha = 1.0,
                                  font_size = 12,
                                  font_family = "sans",
                                  export = FALSE,
                                  export_format = c("png", "jpg", "pdf", "svg", "tiff"),
                                  file_name = "compound_bar_chart",
                                  width = 8,
                                  height = 6,
                                  dpi = 300) {

  # Match arguments
  type <- match.arg(type)
  orientation <- match.arg(orientation)
  label_type <- match.arg(label_type)
  theme_choice <- match.arg(theme_choice)
  export_format <- match.arg(export_format)

  # Data Summarization Logic
  y_q <- rlang::enquo(y_var)

  if (rlang::quo_is_null(y_q)) {
    # If no y_var provided, calculate frequencies
    df_plot <- data |>
      dplyr::count({{ x_var }}, {{ group_var }}, name = "y_val")
  } else {
    # If y_var provided, summarize it (e.g., sum)
    df_plot <- data |>
      dplyr::group_by({{ x_var }}, {{ group_var }}) |>
      dplyr::summarise(y_val = sum({{ y_var }}, na.rm = TRUE), .groups = "drop")
  }

  # Calculate percentages for labels
  df_plot <- df_plot |>
    dplyr::group_by({{ x_var }}) |>
    dplyr::mutate(
      pct = y_val / sum(y_val),
      label_text = if(label_type == "percent") scales::percent(pct, accuracy = 0.1) else as.character(round(y_val, 2))
    ) |>
    dplyr::ungroup()

  # Set positional mapping based on type
  pos <- if (type == "grouped") {
    ggplot2::position_dodge(width = bar_width)
  } else {
    ggplot2::position_stack()
  }

  # Initialize the plot
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = factor({{ x_var }}), y = y_val, fill = factor({{ group_var }}))) +
    ggplot2::geom_col(position = pos, width = bar_width, alpha = alpha, color = "black", linewidth = 0.3)

  # Add Data Labels
  if (show_labels) {
    label_pos <- if (type == "grouped") ggplot2::position_dodge(width = bar_width) else ggplot2::position_stack(vjust = 0.5)

    # Adjust vjust based on orientation and type
    v_adjust <- if(orientation == "vertical" && type == "grouped") -0.5 else 0.5
    h_adjust <- if(orientation == "horizontal" && type == "grouped") -0.2 else 0.5

    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = label_text),
      position = label_pos,
      vjust = v_adjust,
      hjust = h_adjust,
      size = font_size / 3,
      family = font_family
    )
  }

  # Orientation
  if (orientation == "horizontal") {
    p <- p + ggplot2::coord_flip()
  }

  # Apply Theme
  theme_func <- switch(theme_choice,
                       "minimal" = ggplot2::theme_minimal,
                       "classic" = ggplot2::theme_classic,
                       "bw" = ggplot2::theme_bw,
                       "light" = ggplot2::theme_light,
                       "gray" = ggplot2::theme_gray,
                       "void" = ggplot2::theme_void)

  p <- p + theme_func(base_size = font_size, base_family = font_family) +
    ggplot2::theme(
      legend.position = legend_position,
      axis.text.x = ggplot2::element_text(angle = label_angle, hjust = if(label_angle > 0) 1 else 0.5)
    )

  # Titles and Labels
  p <- p + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = if(!is.null(xlab)) xlab else rlang::as_name(rlang::enquo(x_var)),
    y = if(!is.null(ylab)) ylab else ifelse(rlang::quo_is_null(y_q), "Frequency", rlang::as_name(rlang::enquo(y_var))),
    fill = if(!is.null(legend_title)) legend_title else rlang::as_name(rlang::enquo(group_var))
  )

  # Color Configuration
  viridis_pals <- c("viridis", "plasma", "inferno", "magma", "cividis")
  brewer_pals <- c("Set1", "Set2", "Dark2", "Paired", "Pastel1", "Pastel2", "Accent", "Spectral", "RdYlBu")

  if (!is.null(manual_colors)) {
    p <- p + ggplot2::scale_fill_manual(values = manual_colors)
  } else if (palette %in% viridis_pals) {
    p <- p + ggplot2::scale_fill_viridis_d(option = palette)
  } else if (palette %in% brewer_pals) {
    p <- p + ggplot2::scale_fill_brewer(palette = palette)
  }

  # Exporting
  if (export) {
    full_file_name <- paste0(file_name, ".", export_format)
    ggplot2::ggsave(
      filename = full_file_name,
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      device = export_format
    )
    message(paste("Plot successfully exported as:", full_file_name))
  }

  return(p)
}
