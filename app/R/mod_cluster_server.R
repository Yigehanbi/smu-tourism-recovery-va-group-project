library(shiny)
library(dplyr)
library(ggplot2)
library(cluster)

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
      sil <- as.data.frame(cluster::silhouette(cluster_id, d))
      sil_width <- as.numeric(sil$sil_width)

      membership <- data.frame(
        series = rownames(mat),
        cluster = paste0("Cluster ", cluster_id),
        silhouette = round(sil_width, 3),
        stringsAsFactors = FALSE
      ) |>
        arrange(cluster, desc(silhouette), series)

      diagnostics <- lapply(
        seq_len(min(8, nrow(mat) - 1)),
        function(k) {
          cl <- stats::cutree(hc, k = k)
          sil_k <- as.data.frame(cluster::silhouette(cl, d))
          sil_k_width <- as.numeric(sil_k$sil_width)
          data.frame(
            k = k,
            mean_silhouette = mean(sil_k_width, na.rm = TRUE)
          )
        }
      ) |>
        bind_rows() |>
        mutate(mean_silhouette = round(mean_silhouette, 3))

      dmat <- as.matrix(d)
      cluster_levels <- sort(unique(cluster_id))

      cluster_summary <- lapply(cluster_levels, function(cl) {
        members <- membership$series[membership$cluster == paste0("Cluster ", cl)]
        cluster_dmat <- dmat[members, members, drop = FALSE]
        medoid <- if (length(members) == 1) {
          members[[1]]
        } else {
          members[which.min(rowSums(cluster_dmat))]
        }
        data.frame(
          cluster = paste0("Cluster ", cl),
          n_series = length(members),
          representative_series = medoid,
          mean_silhouette = round(mean(membership$silhouette[membership$cluster == paste0("Cluster ", cl)], na.rm = TRUE), 3),
          stringsAsFactors = FALSE
        )
      }) |>
        bind_rows() |>
        arrange(cluster)

      member_plot_long <- lapply(seq_along(panel$series), function(i) {
        series_name <- panel$series[i]
        tibble::tibble(
          date = panel$normalized_wide$date,
          value = panel$normalized_wide[[series_name]],
          series = series_name,
          cluster = paste0("Cluster ", cluster_id[i]),
          type = "Series"
        )
      }) |>
        bind_rows()

      cluster_mean_long <- lapply(cluster_levels, function(cl) {
        members <- membership$series[membership$cluster == paste0("Cluster ", cl)]
        cluster_mean <- rowMeans(panel$normalized_wide[, members, drop = FALSE], na.rm = TRUE)
        tibble::tibble(
          date = panel$normalized_wide$date,
          value = cluster_mean,
          series = paste0("Cluster mean ", cl),
          cluster = paste0("Cluster ", cl),
          type = "Cluster mean"
        )
      }) |>
        bind_rows()

      plot_data <- bind_rows(member_plot_long, cluster_mean_long) |>
        mutate(cluster = factor(cluster, levels = paste0("Cluster ", cluster_levels)))

      list(
        panel = panel,
        silhouette = mean(sil_width, na.rm = TRUE),
        membership = membership,
        diagnostics = diagnostics,
        summary = cluster_summary,
        plot_data = plot_data,
        hc = hc
      )
    }, ignoreNULL = FALSE)

    output$silhouette_value <- renderText({
      res <- cluster_results()
      sprintf("Mean silhouette score: %.3f", res$silhouette)
    })

    output$cluster_window_note <- renderText({
      res <- cluster_results()
      sprintf(
        "Window: %s to %s | Normalization: %s | Series: %d",
        format(min(res$panel$dates), "%Y-%m"),
        format(max(res$panel$dates), "%Y-%m"),
        res$panel$normalization,
        length(res$panel$series)
      )
    })

    output$diagnostics_table <- DT::renderDT({
      res <- cluster_results()
      DT::datatable(
        res$diagnostics,
        rownames = FALSE,
        options = list(pageLength = 5, dom = "t")
      )
    })

    output$membership_table <- DT::renderDT({
      res <- cluster_results()
      DT::datatable(
        res$membership,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$cluster_summary_table <- DT::renderDT({
      res <- cluster_results()
      DT::datatable(
        res$summary,
        rownames = FALSE,
        options = list(pageLength = 5, dom = "t")
      )
    })

    output$cluster_pattern_plot <- renderPlot({
      res <- cluster_results()

      ggplot(res$plot_data, aes(x = date, y = value, group = interaction(series, type), color = cluster)) +
        geom_line(
          data = subset(res$plot_data, type == "Series"),
          alpha = 0.15,
          linewidth = 0.4
        ) +
        geom_line(
          data = subset(res$plot_data, type == "Cluster mean"),
          linewidth = 1.3
        ) +
        facet_wrap(~cluster, ncol = 1, scales = "free_y") +
        labs(
          title = "Representative Time-Series Patterns by Cluster",
          subtitle = paste(
            "Selected country series clustered by trajectory similarity using",
            res$panel$normalization,
            "normalization"
          ),
          x = "Month",
          y = "Normalized value",
          color = "Cluster"
        ) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "none")
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
