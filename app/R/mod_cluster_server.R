library(shiny)
library(dplyr)
library(ggplot2)
library(cluster)

cluster_palette <- function(cluster_values) {
  levels <- unique(as.character(cluster_values))
  stats::setNames(
    grDevices::hcl.colors(length(levels), palette = "Green-Brown"),
    levels
  )
}

cluster_reference_value <- function(normalization) {
  switch(
    normalization,
    indexed = 100,
    zscore = 0,
    raw = NA_real_,
    NA_real_
  )
}

axis_limits_with_padding <- function(x, center = NULL, prop = 0.16) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  span <- diff(rng)

  if (!is.finite(span) || span == 0) {
    anchor <- if (length(rng) == 0 || !is.finite(rng[1])) 0 else rng[1]
    span <- max(abs(anchor), 1) * 0.35
    rng <- c(anchor - span / 2, anchor + span / 2)
  }

  limits <- c(rng[1] - span * prop, rng[2] + span * prop)

  if (!is.null(center) && is.finite(center)) {
    limits[1] <- min(limits[1], center - span * 0.18)
    limits[2] <- max(limits[2], center + span * 0.18)
  }

  limits
}

cluster_pattern_plot_object <- function(res) {
  palette <- cluster_palette(res$plot_data$cluster_view)
  normalization <- res$panel$normalization
  ref_value <- cluster_reference_value(normalization)
  n_clusters <- length(unique(as.character(res$plot_data$cluster_view)))
  facet_cols <- if (n_clusters <= 2) {
    1
  } else if (n_clusters <= 4) {
    2
  } else {
    3
  }

  y_axis_label <- switch(
    normalization,
    indexed = "Indexed level (base = 100)",
    zscore = "Standardized level (z-score)",
    raw = "Monthly arrivals",
    "Value"
  )

  plot <- ggplot(
    res$plot_data,
    aes(x = date, y = value, group = interaction(series, type), color = cluster_view)
  ) +
    geom_line(
      data = subset(res$plot_data, type == "Series"),
      alpha = 0.22,
      linewidth = 0.5
    ) +
    geom_line(
      data = subset(res$plot_data, type == "Cluster mean"),
      linewidth = 1.55
    ) +
    facet_wrap(~cluster_view, ncol = facet_cols, scales = "free_y") +
    labs(
      title = "Representative Time-Series Patterns by Cluster",
      subtitle = paste(
        "Selected country series clustered by trajectory similarity using",
        normalization,
        "normalization"
      ),
      x = "Month",
      y = y_axis_label,
      color = "Cluster"
    ) +
    scale_color_manual(values = palette) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(1, "lines")
    )

  if (is.finite(ref_value)) {
    plot <- plot +
      geom_hline(yintercept = ref_value, linetype = "dashed", color = "#c9b79d", linewidth = 0.45)
  }

  if (identical(normalization, "raw")) {
    plot <- plot +
      scale_y_continuous(labels = scales::label_number(big.mark = ",", accuracy = 1))
  }

  plot
}

recovery_position_plot_object <- function(res) {
  df <- res$series_features
  normalization <- res$panel$normalization
  ref_value <- cluster_reference_value(normalization)
  palette <- cluster_palette(df$cluster_label)

  x_axis_label <- switch(
    normalization,
    indexed = "Lowest indexed level in the selected window",
    zscore = "Lowest z-score in the selected window",
    raw = "Lowest monthly arrival level",
    "Lowest level"
  )
  y_axis_label <- switch(
    normalization,
    indexed = "Final indexed level in the selected window",
    zscore = "Final z-score in the selected window",
    raw = "Final monthly arrival level",
    "Final level"
  )

  label_series <- unique(c("china", res$summary$representative_series))
  label_df <- df |>
    filter(series %in% label_series) |>
    distinct(series, .keep_all = TRUE)

  plot <- ggplot(df, aes(x = trough_index, y = end_index)) +
    geom_point(
      aes(fill = cluster_label),
      shape = 21,
      size = 4.2,
      alpha = 0.95,
      colour = "#fffaf3",
      stroke = 0.9
    ) +
    geom_text(
      data = label_df,
      aes(label = series_name, color = cluster_label),
      vjust = -1,
      size = 3.4,
      show.legend = FALSE,
      check_overlap = TRUE
    ) +
    labs(
      title = "Recovery Position Map",
      subtitle = "Only China and one representative series per cluster are labelled to avoid overlap",
      x = x_axis_label,
      y = y_axis_label,
      fill = "Cluster pattern"
    ) +
    scale_fill_manual(values = palette) +
    scale_color_manual(values = palette, guide = "none") +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )

  if (identical(normalization, "raw")) {
    plot <- plot +
      scale_x_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        labels = scales::label_number(big.mark = ",", accuracy = 1)
      ) +
      scale_y_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        labels = scales::label_number(big.mark = ",", accuracy = 1)
      )
  } else if (identical(normalization, "indexed")) {
    x_limits <- axis_limits_with_padding(df$trough_index, center = NULL)
    y_limits <- axis_limits_with_padding(df$end_index, center = ref_value)

    plot <- plot +
      coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off")

    if (is.finite(ref_value)) {
      plot <- plot +
        geom_hline(yintercept = ref_value, linetype = "dashed", color = "#c9b79d", linewidth = 0.45)
    }
  } else {
    x_limits <- axis_limits_with_padding(df$trough_index, center = ref_value)
    y_limits <- axis_limits_with_padding(df$end_index, center = ref_value)

    plot <- plot +
      coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off")

    if (is.finite(ref_value)) {
      plot <- plot +
        geom_hline(yintercept = ref_value, linetype = "dashed", color = "#c9b79d", linewidth = 0.45) +
        geom_vline(xintercept = ref_value, linetype = "dashed", color = "#c9b79d", linewidth = 0.45)
    }
  }

  plot
}

mod_cluster_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    cluster_results <- eventReactive(input$run_cluster, {
      validate(
        need(length(input$series_subset) >= 2, "Select at least two series."),
        need(length(input$year_window) == 2, "Select a valid year window.")
      )

      panel <- prepare_country_clustering_data(
        data(),
        selected_series = input$series_subset,
        year_window = input$year_window,
        normalization = input$normalization_mode
      )

      mat <- panel$matrix
      d <- stats::dist(mat)

      validate(
        need(nrow(mat) > input$k_value, "Not enough series for the selected k."),
        need(nrow(mat) >= 3, "Need at least three series to form meaningful clusters.")
      )

      hc <- stats::hclust(d, method = "ward.D2")
      cluster_id <- stats::cutree(hc, k = input$k_value)
      solution <- summarize_cluster_solution(
        panel,
        cluster_id,
        d,
        china_series = "china",
        series_labels = clustering_display_lookup()
      )

      list(
        panel = panel,
        silhouette = solution$silhouette,
        membership = solution$membership,
        diagnostics = solution$diagnostics,
        summary = solution$summary,
        plot_data = solution$plot_data,
        series_features = solution$series_features,
        china_context = solution$china_context,
        china_note = solution$china_note,
        hc = hc
      )
    }, ignoreNULL = FALSE)

    output$quality_panel <- renderUI({
      res <- cluster_results()
      selected_row <- res$diagnostics |>
        filter(k == input$k_value) |>
        slice(1)

      tags$div(
        class = "va-stat-grid",
        tags$div(
          class = "va-stat",
          tags$div(class = "va-stat-label", "Mean silhouette"),
          tags$div(class = "va-stat-value", sprintf("%.3f", res$silhouette))
        ),
        tags$div(
          class = "va-stat",
          tags$div(class = "va-stat-label", "Selected k"),
          tags$div(class = "va-stat-value", input$k_value)
        ),
        tags$div(
          class = "va-stat",
          tags$div(class = "va-stat-label", "Series in view"),
          tags$div(class = "va-stat-value", length(res$panel$series))
        ),
        tags$div(
          class = "va-stat va-stat-note",
          tags$div(
            class = "va-stat-note-text",
            sprintf(
              "Window %s to %s | %s normalization | selected-k silhouette %.3f",
              format(min(res$panel$dates), "%Y-%m"),
              format(max(res$panel$dates), "%Y-%m"),
              tools::toTitleCase(res$panel$normalization),
              selected_row$mean_silhouette
            )
          )
        )
      )
    })

    output$diagnostics_table <- DT::renderDT({
      res <- cluster_results()
      diagnostics <- res$diagnostics |>
        mutate(
          choice = ifelse(k == input$k_value, "Selected", ""),
          mean_silhouette = sprintf("%.3f", mean_silhouette)
        ) |>
        rename(
          `Number of clusters` = k,
          `Mean silhouette` = mean_silhouette,
          Status = choice
        )

      DT::datatable(
        diagnostics,
        rownames = FALSE,
        options = list(pageLength = 8, dom = "tip", ordering = FALSE, autoWidth = TRUE)
      )
    })

    output$membership_table <- DT::renderDT({
      res <- cluster_results()
      membership <- res$membership |>
        transmute(
          Series = series_name,
          Cluster = cluster,
          Pattern = cluster_label,
          Silhouette = sprintf("%.3f", silhouette),
          `End index` = end_index,
          `Trough index` = trough_index
        )

      DT::datatable(
        membership,
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = FALSE, autoWidth = TRUE)
      )
    })

    output$cluster_summary_table <- DT::renderDT({
      res <- cluster_results()
      summary_tbl <- res$summary |>
        transmute(
          Cluster = cluster,
          Pattern = cluster_label,
          `Series count` = n_series,
          `Representative series` = representative_series_name,
          `Mean silhouette` = sprintf("%.3f", mean_silhouette),
          `Average end index` = avg_end_index,
          `Average trough index` = avg_trough_index,
          `Average rebound multiple` = avg_rebound_multiple,
          Members = members
        )

      DT::datatable(
        summary_tbl,
        rownames = FALSE,
        options = list(pageLength = 6, dom = "tip", autoWidth = TRUE)
      )
    })

    output$recovery_metrics_table <- DT::renderDT({
      res <- cluster_results()
      metrics_tbl <- res$series_features |>
        transmute(
          Series = series_name,
          Cluster = cluster,
          Pattern = cluster_label,
          `End index` = end_index,
          `Trough index` = trough_index,
          `Rebound multiple` = rebound_multiple,
          Volatility = volatility
        )

      DT::datatable(
        metrics_tbl,
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = FALSE, autoWidth = TRUE)
      )
    })

    output$cluster_pattern_plot <- renderPlot({
      res <- cluster_results()
      cluster_pattern_plot_object(res)
    })

    output$recovery_position_plot <- renderPlot({
      res <- cluster_results()
      recovery_position_plot_object(res)
    })

    output$insight_panel <- renderUI({
      res <- cluster_results()
      strongest <- res$summary |>
        arrange(desc(avg_end_index)) |>
        slice(1)

      tags$div(
        class = "va-insight-stack",
        tags$div(
          class = "va-insight-pill",
          paste("Lead pattern:", strongest$cluster_label)
        ),
        tags$p(
          class = "va-insight-main",
          sprintf(
            "%s is the strongest rebound group in the current selection. It ends around %.1f on the normalized scale and is represented by %s.",
            strongest$cluster_label,
            strongest$avg_end_index,
            strongest$representative_series_name
          )
        ),
        tags$p(
          class = "va-insight-secondary",
          res$china_note
        )
      )
    })

    output$download_clusters <- downloadHandler(
      filename = function() {
        sprintf("country-series-cluster-assignments-%s.csv", Sys.Date())
      },
      content = function(file) {
        res <- cluster_results()
        utils::write.csv(res$membership, file, row.names = FALSE)
      }
    )
  })
}
