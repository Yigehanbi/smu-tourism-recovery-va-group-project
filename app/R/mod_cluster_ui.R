library(shiny)
library(bslib)

mod_cluster_ui <- function(id) {
  ns <- NS(id)
  series_choices <- available_clustering_series()
  default_choices <- default_clustering_series(n = min(8, length(series_choices)))

  tagList(
    div(
      class = "va-module-hero",
      div(
        class = "va-module-copy",
        div(class = "va-kicker", "Module 2"),
        h2("Time Series Clustering"),
        p(
          "Group country-level arrival trajectories by similarity, then interpret which markets rebounded early, which remained delayed, and where China sits in the wider recovery landscape."
        )
      ),
      div(
        class = "va-chip-row",
        span(class = "va-chip", "Country series"),
        span(class = "va-chip", "2017-2025 window"),
        span(class = "va-chip", "Similarity-driven"),
        span(class = "va-chip", "Download-ready")
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        class = "va-sidebar",
        h3("Configure the module"),
        p(
          class = "va-side-intro",
          "Start with a focused set of markets, choose how to normalize their paths, then run clustering to compare recovery behaviour."
        ),
        selectizeInput(
          ns("series_subset"),
          "Country series",
          choices = series_choices,
          selected = default_choices,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        ),
        sliderInput(
          ns("year_window"),
          "Year window",
          min = 2017,
          max = 2025,
          value = c(2017, 2025),
          step = 1,
          sep = ""
        ),
        radioButtons(
          ns("normalization_mode"),
          "Normalization mode",
          choices = c(
            "Indexed (base 100)" = "indexed",
            "Z-score" = "zscore",
            "Raw arrivals" = "raw"
          ),
          selected = "indexed",
          inline = FALSE
        ),
        sliderInput(ns("k_value"), "Number of clusters", min = 2, max = 8, value = 3, step = 1),
        actionButton(ns("run_cluster"), "Run clustering", class = "btn-primary"),
        div(
          class = "va-sidebar-note",
          h4("What to look for"),
          tags$ul(
            tags$li("Does one cluster rebound much earlier than the others?"),
            tags$li("Which markets travel with China, and which diverge?"),
            tags$li("Do representative patterns match the narrative in the proposal and storyboard?")
          )
        )
      ),
      tagList(
        layout_columns(
          card(
            class = "va-card va-quality-card",
            card_header("Cluster Quality"),
            card_body(uiOutput(ns("quality_panel")))
          ),
          card(
            class = "va-card va-insight-card",
            card_header("Cluster Insights"),
            card_body(uiOutput(ns("insight_panel")))
          ),
          col_widths = c(4, 8)
        ),
        layout_columns(
          card(
            class = "va-card va-plot-card",
            full_screen = TRUE,
            card_header("Representative Patterns"),
            card_body(plotOutput(ns("cluster_pattern_plot"), height = "440px"))
          ),
          card(
            class = "va-card va-table-card",
            card_header("Cluster Diagnostics"),
            card_body(DT::DTOutput(ns("diagnostics_table")))
          ),
          col_widths = c(8, 4)
        ),
        layout_columns(
          card(
            class = "va-card va-map-card",
            card_header("Recovery Position Map"),
            card_body(plotOutput(ns("recovery_position_plot"), height = "380px"))
          ),
          card(
            class = "va-card va-table-card",
            card_header("Cluster Summary"),
            card_body(DT::DTOutput(ns("cluster_summary_table")))
          ),
          col_widths = c(7, 5)
        ),
        layout_columns(
          card(
            class = "va-card va-table-card",
            card_header("Membership Table"),
            card_body(
              div(
                class = "va-download-row",
                downloadButton(ns("download_clusters"), "Download assignments")
              ),
              DT::DTOutput(ns("membership_table"))
            )
          ),
          card(
            class = "va-card va-table-card",
            card_header("Recovery Metrics By Series"),
            card_body(DT::DTOutput(ns("recovery_metrics_table")))
          ),
          col_widths = c(7, 5)
        )
      )
    )
  )
}
