#' Launch Interactive Drag-and-Drop Pivot UI & Advanced Chart Builder
#'
#' Opens an advanced corporate analytics interface containing a classic Excel-style drag-and-drop
#' pivot table, a raw data viewer, and a dedicated Chart Builder.
#'
#' @param data A data frame to explore.
#' @param rows Optional character vector of starting row variables.
#' @param cols Optional character vector of starting column variables.
#' @param vals Optional character vector of starting value variables.
#' @param aggregator Character. Default math operation (e.g., "Average", "Sum").
#' @return A Shiny application object.
#' @export
#' @import shiny rpivotTable ggplot2
pivot_gui <- function(data, rows = NULL, cols = NULL, vals = NULL, aggregator = "Average") {

  if (!requireNamespace("rpivotTable", quietly = TRUE)) {
    stop("The 'rpivotTable' package is required.")
  }

  # FORCE strict data.frame conversion
  data <- as.data.frame(data)

  # Identify available columns safely
  all_cols <- names(data)
  num_cols <- names(data)[sapply(data, is.numeric)]
  cat_cols <- names(data)[!sapply(data, is.numeric)]

  # --- THE FIX: PREVENT JS CRASH ON MATH AGGREGATORS ---
  # If the aggregator does math (Average, Sum) but 'vals' is empty,
  # auto-assign the first numeric column so the JS grid doesn't crash.
  if (is.null(vals) && length(num_cols) > 0) {
    vals <- num_cols[1]
  }

  # Fail-safe defaults for the charting tab
  default_x <- all_cols[1]
  default_y <- if(length(num_cols) > 0) num_cols[1] else all_cols[1]
  default_fill <- if(length(cat_cols) > 0) cat_cols[1] else "None"

  ui <- shiny::fluidPage(
    shiny::titlePanel("statusluxR Corporate Explorer Hub"),
    shiny::tabsetPanel(

      # --- TAB 1: THE DRAG & DROP TABLE ---
      shiny::tabPanel("Interactive Pivot Table",
                      shiny::fluidRow(
                        shiny::column(12, style = "padding-top: 15px; margin-bottom: 10px;",
                                      shiny::downloadButton("dl_data", "Download Raw Data (.csv)", class = "btn-primary")
                        )
                      ),
                      shiny::fluidRow(
                        shiny::column(12,
                                      shiny::div(style = "min-height: 650px; width: 100%; overflow: auto;",
                                                 rpivotTable::rpivotTableOutput("pivot")
                                      )
                        )
                      )
      ),

      # --- TAB 2: RAW DATA VALUES VIEW ---
      shiny::tabPanel("Data Spreadsheet Viewer",
                      shiny::fluidRow(
                        shiny::column(12, style = "padding-top: 15px;",
                                      shiny::h4("Row-by-Row Spreadsheet Values"),
                                      shiny::hr(),
                                      shiny::dataTableOutput("raw_spreadsheet")
                        )
                      )
      ),

      # --- TAB 3: ADVANCED CHART BUILDER ---
      shiny::tabPanel("Advanced Chart Builder",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(style = "margin-top: 15px;",
                                            shiny::selectInput("chart_type", "1. Select Chart Type:",
                                                               choices = c("Bar Chart (Grouped)", "Compound Bar Chart (Stacked)",
                                                                           "Histogram", "Pie Chart", "Time Series Line Plot")),

                                            shiny::selectInput("x_var", "2. Select Main Variable (X-Axis):",
                                                               choices = all_cols, selected = default_x),

                                            shiny::conditionalPanel(
                                              condition = "input.chart_type == 'Bar Chart (Grouped)' || input.chart_type == 'Compound Bar Chart (Stacked)' || input.chart_type == 'Time Series Line Plot'",
                                              shiny::selectInput("y_var", "3. Select Numeric Target (Y-Axis Metric):",
                                                                 choices = num_cols, selected = default_y)
                                            ),

                                            shiny::conditionalPanel(
                                              condition = "input.chart_type == 'Bar Chart (Grouped)' || input.chart_type == 'Compound Bar Chart (Stacked)'",
                                              shiny::selectInput("fill_var", "4. Group/Color Bars By:",
                                                                 choices = c("None", cat_cols), selected = default_fill)
                                            ),

                                            shiny::hr(),
                                            shiny::downloadButton("dl_plot", "Download Custom Plot (.png)", class = "btn-success")
                        ),
                        shiny::mainPanel(style = "margin-top: 15px;",
                                         shiny::plotOutput("custom_plot", height = "550px")
                        )
                      )
      )
    )
  )

  server <- function(input, output, session) {

    # Render the drag and drop engine with strict heights and protected variables
    output$pivot <- rpivotTable::renderRpivotTable({
      rpivotTable::rpivotTable(
        data = data, rows = rows, cols = cols, vals = vals,
        aggregatorName = aggregator, theme = "default", width = "100%", height = "600px"
      )
    })

    # Render the native spreadsheet table values instantly
    output$raw_spreadsheet <- shiny::renderDataTable({
      data
    }, options = list(pageLength = 10, scrollX = TRUE))

    # Download raw data spreadsheet
    output$dl_data <- shiny::downloadHandler(
      filename = function() { paste0("data-export-", Sys.Date(), ".csv") },
      content = function(file) { write.csv(data, file, row.names = FALSE) }
    )

    # Reactive Core Plotting Engine
    plot_object <- shiny::reactive({
      shiny::req(input$x_var, input$chart_type)

      if (input$chart_type == "Bar Chart (Grouped)") {
        shiny::req(input$y_var)
        group_vars <- input$x_var
        if (input$fill_var != "None") group_vars = c(group_vars, input$fill_var)

        df_agg <- aggregate(as.formula(paste(input$y_var, "~", paste(group_vars, collapse = "+"))),
                            data = data, FUN = mean)

        p <- ggplot2::ggplot(df_agg, ggplot2::aes(x = factor(.data[[input$x_var]]), y = .data[[input$y_var]]))

        if (input$fill_var != "None") {
          p <- p + ggplot2::geom_col(ggplot2::aes(fill = factor(.data[[input$fill_var]])), position = ggplot2::position_dodge(width = 0.9)) +
            ggplot2::geom_text(ggplot2::aes(group = factor(.data[[input$fill_var]]), label = round(.data[[input$y_var]], 1)),
                               position = ggplot2::position_dodge(width = 0.9), vjust = -0.5, fontface = "bold", size = 4.5)
        } else {
          p <- p + ggplot2::geom_col(fill = "#F8766D", width = 0.7) +
            ggplot2::geom_text(ggplot2::aes(label = round(.data[[input$y_var]], 1)), vjust = -0.5, fontface = "bold", size = 4.5)
        }
        p <- p + ggplot2::labs(title = paste("Average of", input$y_var, "by", input$x_var), x = input$x_var, y = paste("Average", input$y_var)) +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))

      } else if (input$chart_type == "Compound Bar Chart (Stacked)") {
        shiny::req(input$y_var)
        group_vars <- input$x_var
        fill_active <- if (input$fill_var != "None") input$fill_var else input$x_var
        group_vars <- c(group_vars, fill_active)

        df_agg <- aggregate(as.formula(paste(input$y_var, "~", paste(unique(group_vars), collapse = "+"))),
                            data = data, FUN = sum)

        p <- ggplot2::ggplot(df_agg, ggplot2::aes(x = factor(.data[[input$x_var]]), y = .data[[input$y_var]], fill = factor(.data[[fill_active]]))) +
          ggplot2::geom_col(position = "stack", color = "white", width = 0.7) +
          ggplot2::geom_text(ggplot2::aes(label = round(.data[[input$y_var]], 1)), position = ggplot2::position_stack(vjust = 0.5), fontface = "bold", color = "white", size = 4.5) +
          ggplot2::labs(title = paste("Compound Stacked Aggregation of", input$y_var), x = input$x_var, y = paste("Total Sum of", input$y_var), fill = fill_active)

      } else if (input$chart_type == "Histogram") {
        p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[input$x_var]])) +
          ggplot2::geom_histogram(fill = "#27AE60", color = "white", bins = 10) +
          ggplot2::labs(title = paste("Distribution of", input$x_var), x = input$x_var, y = "Frequency")

      } else if (input$chart_type == "Pie Chart") {
        df_pie <- as.data.frame(table(data[[input$x_var]]))
        colnames(df_pie) <- c("Category", "Count")
        df_pie$Percentage <- (df_pie$Count / sum(df_pie$Count)) * 100

        p <- ggplot2::ggplot(df_pie, ggplot2::aes(x = "", y = Count, fill = factor(Category))) +
          ggplot2::geom_col(width = 1, color = "white") +
          ggplot2::coord_polar("y", start = 0) +
          ggplot2::geom_text(ggplot2::aes(label = paste0(round(Percentage, 1), "%")),
                             position = ggplot2::position_stack(vjust = 0.5), fontface = "bold", color = "white", size = 5) +
          ggplot2::labs(title = paste("Percentage Structural Share of", input$x_var), fill = "Category") +
          ggplot2::theme_void()

      } else if (input$chart_type == "Time Series Line Plot") {
        shiny::req(input$y_var)
        df_sorted <- data[order(data[[input$x_var]]), ]

        p <- ggplot2::ggplot(df_sorted, ggplot2::aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = 1)) +
          ggplot2::geom_line(color = "#D35400", linewidth = 1.2) +
          ggplot2::geom_point(color = "#2C3E50", size = 3) +
          ggplot2::labs(title = paste("Chronological Trend of", input$y_var, "Over", input$x_var), x = input$x_var, y = input$y_var)
      }

      if (input$chart_type != "Pie Chart") {
        p <- p + ggplot2::theme_minimal(base_size = 14)
      }
      p + ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16),
        axis.title = ggplot2::element_text(face = "bold")
      )
    })

    output$custom_plot <- shiny::renderPlot({ plot_object() })

    output$dl_plot <- shiny::downloadHandler(
      filename = function() { paste0("statusluxR-plot-", Sys.Date(), ".png") },
      content = function(file) {
        ggplot2::ggsave(file, plot = plot_object(), device = "png", width = 9, height = 6, dpi = 300)
      }
    )
  }

  shiny::shinyApp(ui, server)
}
