library(dplyr)
library(ggplot2)
library(cluster)

source(file.path("app", "R", "data_utils.R"))

dir.create(file.path("artifacts", "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("artifacts", "tables"), recursive = TRUE, showWarnings = FALSE)

# Shared clustering contract smoke check
country_wide <- load_clustering_country_wide()
selected_series <- default_clustering_series(n = 6)

cluster_panel <- prepare_country_clustering_data(
  wide = country_wide,
  selected_series = selected_series,
  year_window = c(2017, 2025),
  normalization = "indexed"
)

dist_mat <- dist(cluster_panel$matrix)
hc <- hclust(dist_mat, method = "ward.D2")
cluster_id <- cutree(hc, k = 3)

cluster_solution <- summarize_cluster_solution(
  panel = cluster_panel,
  cluster_id = cluster_id,
  distance_matrix = dist_mat,
  china_series = if ("china" %in% selected_series) "china" else selected_series[[1]],
  series_labels = clustering_display_lookup()
)

write.csv(cluster_solution$summary, file.path("artifacts", "tables", "cluster_summary.csv"), row.names = FALSE)

cluster_plot <- ggplot(
  cluster_solution$series_features,
  aes(x = trough_index, y = end_index, color = cluster_label, label = series_name)
) +
  geom_point(size = 3.2, alpha = 0.9) +
  geom_text(vjust = -0.8, size = 3.1, show.legend = FALSE) +
  labs(
    title = "Cluster Smoke Test",
    subtitle = "Country-level recovery position map",
    x = "Lowest indexed level in the selected window",
    y = "Final indexed level in the selected window",
    color = "Pattern"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path("artifacts", "plots", "cluster_smoke_plot.png"),
  plot = cluster_plot,
  width = 8,
  height = 4.5,
  dpi = 160
)

# Forecast smoke check
tourism_data <- load_tourism_data()

forecast_series <- prepare_forecast_series(
  tourism_data$long_monthly,
  "Visitor Arrivals: China"
)

forecast_results <- run_modeltime_forecast_workflow(
  series_df = forecast_series,
  horizon = 12
)

forecast_plot <- forecast_results$calibration_forecast_tbl |>
  modeltime::plot_modeltime_forecast(
    .interactive = FALSE,
    .legend_max_width = 25
  ) +
  labs(
    title = "Forecast Smoke Test",
    subtitle = "Seasonal Naive benchmark with ETS and ARIMA holdout comparison"
  )

ggsave(
  filename = file.path("artifacts", "plots", "forecast_smoke_plot.png"),
  plot = forecast_plot,
  width = 8,
  height = 4.5,
  dpi = 160
)

write.csv(forecast_results$accuracy_tbl, file.path("artifacts", "tables", "forecast_accuracy.csv"), row.names = FALSE)

summary_lines <- c(
  paste("timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste("cluster_series_count:", nrow(cluster_panel$matrix)),
  paste("cluster_window_months:", ncol(cluster_panel$matrix)),
  paste("cluster_selected_k:", 3),
  paste("cluster_silhouette_mean:", round(cluster_solution$silhouette, 4)),
  paste("forecast_series:", "Visitor Arrivals: China"),
  paste("forecast_models:", paste(forecast_results$accuracy_tbl$.model_desc, collapse = ", "))
)

writeLines(summary_lines, file.path("artifacts", "tables", "smoke_test_summary.txt"))

message("Smoke test complete.")
