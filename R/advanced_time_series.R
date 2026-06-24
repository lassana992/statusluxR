#' Create an Advanced and Interactive Time Series Plot
#'
#' Generates publication-quality time series visualizations from raw or aggregated data,
#' featuring dynamic date truncation, smoothing trends, and optional interactive output.
#'
#' @param data A data frame containing the dataset.
#' @param date_var The date or date-time variable (unquoted).
#' @param y_var The numerical variable to plot on the y-axis (unquoted).
#' @param group_var Optional categorical grouping variable for multiple series (unquoted).
#' @param aggregation Character. Temporal aggregation level: `"none"`, `"daily"`, `"weekly"`, `"monthly"`, `"quarterly"`, or `"yearly"`.
#' @param agg_func Character. Summary function for aggregation: `"mean"` or `"sum"`.
#' @param smooth_method Character. Trend line smoothing method: `"none"`, `"loess"`, `"lm"`, or `"gam"`.
#' @param theme_choice Character. Plot theme: `"minimal"`, `"classic"`, `"bw"`, `"light"`, `"gray"`, or `"void"`.
#' @param palette Character. Built-in discrete color palette (e.g., `"Set1"`, `"Dark2"`, `"viridis"`, `"plasma"`).
#' @param manual_colors A character vector of exact colors to map to groups (overrides `palette`).
#' @param line_size Numeric. Thickness of the time series lines (default 0.8).
#' @param line_type Character. Style of the line: `"solid"`, `"dashed"`, or `"dotted"`.
#' @param show_points Logical. If TRUE, adds point markers along the lines.
#' @param point_size Numeric. Size of the point markers.
#' @param alpha Numeric. Line and point transparency (0 to 1).
#' @param title,subtitle,caption,xlab,ylab,legend_title Character strings for plot text.
#' @param legend_position Character. `"top"`, `"bottom"`, `"left"`, `"right"`, or `"none"`.
#' @param font_size Numeric. Base font size for the plot layouts.
#' @param font_family Character. Font family (e.g., `"sans"`, `"serif"`, `"mono"`).
#' @param interactive Logical. If TRUE, converts the static plot to an interactive plotly object.
#' @param export Logical. If TRUE, saves the static plot to the working directory.
#' @param export_format Character. `"png"`, `"jpg"`, `"pdf"`, `"svg"`, or `"tiff"`.
#' @param file_name Character. The base name for the exported file.
#' @param width,height,dpi Numeric. Dimensions and resolution for the exported file.
#'
#' @return A \code{ggplot} object, or a \code{plotly} object if \code{interactive = TRUE}.
#' @export
#' @import ggplot2
#' @importFrom dplyr mutate group_by summarise ungroup filter
#' @importFrom lubridate as_date floor_date
#' @importFrom rlang enquo quo_is_null as_name
#' @importFrom plotly ggplotly
#' @importFrom scales date_format
advanced_time_series <- function(data,
                                 date_var,
                                 y_var,
                                 group_var = NULL,
                                 aggregation = c("none", "daily", "weekly", "monthly", "quarterly", "yearly"),
                                 agg_func = c("mean", "sum"),
                                 smooth_method = c("none", "loess", "lm", "gam"),
                                 theme_choice = c("minimal", "classic", "bw", "light", "gray", "void"),
                                 palette = "Set1",
                                 manual_colors = NULL,
                                 line_size = 0.8,
                                 line_type = c("solid", "dashed", "dotted"),
                                 show_points = TRUE,
                                 point_size = 1.5,
                                 alpha = 1.0,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 legend_title = NULL,
                                 legend_position = "right",
                                 font_size = 12,
                                 font_family = "sans",
                                 interactive = FALSE,
                                 export = FALSE,
                                 export_format = c("png", "jpg", "pdf", "svg", "tiff"),
                                 file_name = "time_series_plot",
                                 width = 10,
                                 height = 5,
                                 dpi = 300) {

  # Match internal configuration parameters
  aggregation <- match.arg(aggregation)
  agg_func <- match.arg(agg_func)
  smooth_method <- match.arg(smooth_method)
  theme_choice <- match.arg(theme_choice)
  line_type <- match.arg(line_type)
  export_format <- match.arg(export_format)

  # Capture variables safely via rlang
  date_q <- rlang::enquo(date_var)
  y_q <- rlang::enquo(y_var)
  group_q <- rlang::enquo(group_var)

  # Basic Validation Checks
  if (!rlang::as_name(date_q) %in% names(data) || !rlang::as_name(y_q) %in% names(data)) {
    stop("Error: Key operational columns missing from provided data framework.")
  }

  # Initial Data Preparation: Parse date and strip NAs
  df_proc <- data |>
    dplyr::filter(!is.na(!!date_q), !is.na(!!y_q)) |>
    dplyr::mutate(..clean_date = lubridate::as_date(!!date_q))

  # Apply Lubridate floor_date mapping intervals
  if (aggregation != "none") {
    time_unit <- switch(aggregation,
                        "daily"     = "day",
                        "weekly"    = "week",
                        "monthly"   = "month",
                        "quarterly" = "quarter",
                        "yearly"    = "year")
    df_proc <- df_proc |>
      dplyr::mutate(..clean_date = lubridate::floor_date(..clean_date, unit = time_unit))
  }

  # Setup Grouped summaries
  summary_func <- if (agg_func == "sum") sum else mean

  if (rlang::quo_is_null(group_q)) {
    df_proc <- df_proc |>
      dplyr::group_by(..clean_date) |>
      dplyr::summarise(..y_val = summary_func(!!y_q, na.rm = TRUE), .groups = "drop")
  } else {
    df_proc <- df_proc |>
      dplyr::group_by(..clean_date, !!group_q) |>
      dplyr::summarise(..y_val = summary_func(!!y_q, na.rm = TRUE), .groups = "drop")
  }

  # Map Aesthetics Base
  if (rlang::quo_is_null(group_q)) {
    p <- ggplot2::ggplot(df_proc, ggplot2::aes(x = ..clean_date, y = ..y_val)) +
      ggplot2::geom_line(linewidth = line_size, linetype = line_type, alpha = alpha, color = "#2c3e50")

    if (show_points) {
      p <- p + ggplot2::geom_point(size = point_size, alpha = alpha, color = "#2c3e50")
    }
  } else {
    p <- ggplot2::ggplot(df_proc, ggplot2::aes(x = ..clean_date, y = ..y_val,
                                               color = factor(!!group_q),
                                               group = factor(!!group_q))) +
      ggplot2::geom_line(linewidth = line_size, linetype = line_type, alpha = alpha)

    if (show_points) {
      p <- p + ggplot2::geom_point(size = point_size, alpha = alpha)
    }
  }

  # Add Trend Smoothing Lines if requested
  if (smooth_method != "none") {
    p <- p + ggplot2::geom_smooth(method = smooth_method, se = FALSE,
                                  linewidth = line_size * 0.8, linetype = "dashed", alpha = 0.7)
  }

  # Configure Colors Configurations
  viridis_pals <- c("viridis", "plasma", "inferno", "magma", "cividis")
  brewer_pals <- c("Set1", "Set2", "Dark2", "Paired", "Pastel1", "Pastel2", "Accent", "Spectral", "RdYlBu")

  if (!rlang::quo_is_null(group_q)) {
    if (!is.null(manual_colors)) {
      p <- p + ggplot2::scale_color_manual(values = manual_colors)
    } else if (palette %in% viridis_pals) {
      p <- p + ggplot2::scale_color_viridis_d(option = palette)
    } else if (palette %in% brewer_pals) {
      p <- p + ggplot2::scale_color_brewer(palette = palette)
    }
  }

  # Apply Selected Layout Theme structures
  theme_func <- switch(theme_choice,
                       "minimal" = ggplot2::theme_minimal,
                       "classic" = ggplot2::theme_classic,
                       "bw"      = ggplot2::theme_bw,
                       "light"   = ggplot2::theme_light,
                       "gray"    = ggplot2::theme_gray,
                       "void"    = ggplot2::theme_void)

  p <- p + theme_func(base_size = font_size, base_family = font_family) +
    ggplot2::theme(legend.position = legend_position)

  # Dynamic Date Format Adjustments
  date_lbl_fmt <- switch(aggregation,
                         "yearly"    = "%Y",
                         "quarterly" = "%Y-Q%q",
                         "monthly"   = "%b %Y",
                         "%Y-%m-%d")

  p <- p + ggplot2::scale_x_date(labels = scales::date_format(date_lbl_fmt))

  # Add Labels
  p <- p + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = if (!is.null(xlab)) xlab else "Timeline",
    y = if (!is.null(ylab)) ylab else rlang::as_name(y_q),
    color = if (!is.null(legend_title)) legend_title else if (!rlang::quo_is_null(group_q)) rlang::as_name(group_q) else NULL
  )

  # Execute Static File Export operations BEFORE Plotly transforms structures
  if (export) {
    full_path <- paste0(file_name, ".", export_format)
    ggplot2::ggsave(filename = full_path, plot = p, width = width, height = height, dpi = dpi, device = export_format)
    message(paste("Time Series Plot successfully exported as local file:", full_path))
  }

  # Convert to Interactive Output Engine if active
  if (interactive) {
    p <- plotly::ggplotly(p)
  }

  return(p)
}
