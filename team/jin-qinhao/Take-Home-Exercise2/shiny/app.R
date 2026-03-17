library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

candidate_project_dirs <- function() {
  env_dir <- Sys.getenv("TOURISM_PROJECT_DIR", unset = "")

  unique(
    Filter(
      nzchar,
      c(
        env_dir,
        getwd(),
      file.path(getwd(), "team", "jin-qinhao", "Take-Home-Exercise2"),
      file.path(
        getwd(),
        "smu-tourism-recovery-va-group-project",
        "team",
        "jin-qinhao",
        "Take-Home-Exercise2"
      ),
      file.path(
        Sys.getenv("USERPROFILE"),
        "Documents",
        "Playground",
        "smu-tourism-recovery-va-group-project",
        "team",
        "jin-qinhao",
        "Take-Home-Exercise2"
      ),
      Sys.glob(
        file.path(
          Sys.getenv("USERPROFILE"),
          "Desktop",
          "*",
          "team",
          "jin-qinhao",
          "Take-Home-Exercise2"
        )
      )
      )
    )
  )
}

locate_project_dir <- function() {
  required_files <- c(
    file.path("outputs", "decision_tree_metrics.csv"),
    file.path("outputs", "random_forest_metrics.csv"),
    file.path("data", "tourism_decision_tree_ready.csv")
  )

  for (dir in candidate_project_dirs()) {
    if (!dir.exists(dir)) {
      next
    }

    if (all(file.exists(file.path(dir, required_files)))) {
      return(normalizePath(dir, winslash = "/", mustWork = TRUE))
    }
  }

  stop(
    paste(
      "Could not locate the Take-Home-Exercise2 project folder.",
      "Set `TOURISM_PROJECT_DIR` or update `candidate_project_dirs()` in app.R if your files live elsewhere."
    ),
    call. = FALSE
  )
}

read_csv_safe <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  readr::read_csv(path, show_col_types = FALSE)
}

metric_value <- function(metrics_tbl, metric_name) {
  if (is.null(metrics_tbl)) {
    return(NA_real_)
  }

  value <- metrics_tbl %>%
    filter(metric == metric_name) %>%
    pull(value)

  value[1] %||% NA_real_
}

format_metric <- function(x) {
  out <- scales::percent(x, accuracy = 0.1)
  out[is.na(x)] <- "N/A"
  out
}

has_rows <- function(x) {
  !is.null(x) && nrow(x) > 0
}

max_or_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}

format_decimal <- function(x, digits = 3) {
  out <- sprintf(paste0("%.", digits, "f"), x)
  out[is.na(x)] <- "N/A"
  out
}

format_gap_pp <- function(x) {
  if (is.na(x)) {
    return("N/A")
  }

  paste0(ifelse(x > 0, "+", ""), scales::number(100 * x, accuracy = 0.1), " pp")
}

table_note <- function(message) {
  datatable(
    tibble(Note = message),
    options = list(dom = "t", paging = FALSE, ordering = FALSE),
    rownames = FALSE
  )
}

display_feature_label <- function(x) {
  x <- as.character(x)

  case_when(
    x %in% c("visitor_arrivals", "arrivals_million") ~ "Visitor arrivals",
    x %in% c("avg_stay_monthly", "avg_stay_monthly_capped", "stay_days") ~ "Average stay",
    x %in% c("china_share", "china_share_pct") ~ "China share",
    grepl("^month[A-Z]", x) ~ paste("Month", sub("^month", "", x)),
    x == "month" ~ "Month",
    TRUE ~ tools::toTitleCase(gsub("_", " ", x))
  )
}

concept_feature_label <- function(x) {
  x <- as.character(x)

  case_when(
    x %in% c("visitor_arrivals", "arrivals_million") ~ "Visitor arrivals",
    x %in% c("avg_stay_monthly", "avg_stay_monthly_capped", "stay_days") ~ "Average stay",
    x %in% c("china_share", "china_share_pct") ~ "China share",
    grepl("^month", x) ~ "Month",
    TRUE ~ tools::toTitleCase(gsub("_", " ", x))
  )
}

prediction_confidence <- function(pred_tbl) {
  prob_cols <- intersect(c("low", "medium", "high"), names(pred_tbl))

  if (length(prob_cols) == 0 || !has_rows(pred_tbl)) {
    return(rep(NA_real_, if (is.null(pred_tbl)) 0 else nrow(pred_tbl)))
  }

  probs <- as.data.frame(pred_tbl[prob_cols])
  conf <- do.call(pmax, c(probs, list(na.rm = TRUE)))
  conf[!is.finite(conf)] <- NA_real_
  conf
}

prepare_prediction_rows <- function(pred_tbl) {
  if (!has_rows(pred_tbl)) {
    return(tibble())
  }

  pred_tbl %>%
    mutate(
      date = as.Date(date),
      actual = hotel_occ_level_tertile,
      correct = actual == predicted_class,
      confidence = prediction_confidence(pred_tbl)
    ) %>%
    arrange(desc(date))
}

filter_prediction_rows <- function(pred_tbl, result_filter = "all", class_filter = "All") {
  pred_tbl <- prepare_prediction_rows(pred_tbl)

  if (!has_rows(pred_tbl)) {
    return(pred_tbl)
  }

  if (!identical(class_filter, "All")) {
    pred_tbl <- pred_tbl %>% filter(actual == class_filter)
  }

  if (identical(result_filter, "misclassified")) {
    pred_tbl <- pred_tbl %>% filter(!correct)
  } else if (identical(result_filter, "correct")) {
    pred_tbl <- pred_tbl %>% filter(correct)
  }

  pred_tbl
}

class_recall_table <- function(conf_tbl, model_name) {
  if (!has_rows(conf_tbl)) {
    return(tibble(model = character(), class = character(), total = numeric(), correct = numeric(), recall = numeric()))
  }

  conf_tbl %>%
    mutate(
      Prediction = tolower(Prediction),
      Reference = tolower(Reference)
    ) %>%
    group_by(Reference) %>%
    summarise(
      total = sum(Freq),
      correct = sum(if_else(Prediction == Reference, Freq, 0)),
      .groups = "drop"
    ) %>%
    rename(class = Reference) %>%
    right_join(tibble(class = c("low", "medium", "high")), by = "class") %>%
    mutate(
      model = model_name,
      total = coalesce(total, 0),
      correct = coalesce(correct, 0),
      recall = if_else(total > 0, correct / total, NA_real_)
    ) %>%
    select(model, class, total, correct, recall)
}

metric_rows_for_model <- function(model_name, accuracy, kappa, recall_tbl) {
  bind_rows(
    tibble(
      Model = model_name,
      Metric = c("Holdout accuracy", "Kappa"),
      Display = c(format_metric(accuracy), format_decimal(kappa))
    ),
    recall_tbl %>%
      transmute(
        Model = model_name,
        Metric = paste(display_feature_label(class), "recall"),
        Display = format_metric(recall)
      )
  )
}

build_confusion_plot <- function(conf_tbl, title_text, high_color) {
  conf_tbl %>%
    mutate(
      Reference = factor(Reference, levels = c("low", "medium", "high")),
      Prediction = factor(Prediction, levels = c("low", "medium", "high"))
    ) %>%
    ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = Freq), size = 5, color = "#1f2a2e") +
    scale_fill_gradient(low = "#f8f3ea", high = high_color) +
    labs(
      title = title_text,
      x = "Actual class",
      y = "Predicted class",
      fill = "Count"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank()
    )
}

build_importance_plot <- function(importance_tbl, value_col, title_text, top_n, fill_color) {
  importance_tbl %>%
    mutate(
      score = .data[[value_col]],
      display = display_feature_label(variable)
    ) %>%
    slice_max(order_by = score, n = top_n, with_ties = FALSE) %>%
    ggplot(aes(x = reorder(display, score), y = score)) +
    geom_col(fill = fill_color, width = 0.72) +
    coord_flip() +
    labs(
      title = title_text,
      x = NULL,
      y = "Importance"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

build_prediction_plot <- function(pred_tbl, title_text = "Actual vs Predicted Occupancy Level") {
  class_map <- c(low = 1, medium = 2, high = 3)

  pred_tbl %>%
    transmute(
      date = as.Date(date),
      actual = hotel_occ_level_tertile,
      predicted = predicted_class,
      actual_value = unname(class_map[actual]),
      predicted_value = unname(class_map[predicted]),
      agreement = if_else(actual == predicted, "Correct", "Mismatch")
    ) %>%
    ggplot(aes(x = date)) +
    geom_segment(
      aes(xend = date, y = actual_value, yend = predicted_value, color = agreement),
      linewidth = 1,
      alpha = 0.55
    ) +
    geom_point(aes(y = actual_value, shape = "Actual"), color = "#d86f45", size = 3) +
    geom_point(aes(y = predicted_value, shape = "Predicted"), color = "#0f6b6f", size = 2.8) +
    scale_color_manual(values = c(Correct = "#8a9a94", Mismatch = "#d86f45")) +
    scale_shape_manual(values = c(Actual = 16, Predicted = 17)) +
    scale_y_continuous(
      breaks = c(1, 2, 3),
      labels = c("Low", "Medium", "High"),
      limits = c(0.7, 3.3)
    ) +
    labs(
      title = title_text,
      x = "Date",
      y = "Class level",
      color = NULL,
      shape = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

build_metric_comparison_plot <- function(tree_accuracy, tree_kappa, rf_accuracy, rf_kappa) {
  tibble(
    model = c("Decision Tree", "Decision Tree", "Random Forest", "Random Forest"),
    metric = c("Accuracy", "Kappa", "Accuracy", "Kappa"),
    value = c(tree_accuracy, tree_kappa, rf_accuracy, rf_kappa)
  ) %>%
    filter(!is.na(value)) %>%
    mutate(label = if_else(metric == "Accuracy", format_metric(value), format_decimal(value))) %>%
    ggplot(aes(x = model, y = value, fill = model)) +
    geom_col(width = 0.62, show.legend = FALSE) +
    geom_text(aes(label = label), vjust = -0.35, size = 4) +
    facet_wrap(~metric, scales = "free_y") +
    scale_fill_manual(values = c("Decision Tree" = "#d86f45", "Random Forest" = "#0f6b6f")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
    labs(
      title = "Accuracy and Kappa by Model",
      x = NULL,
      y = "Score"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

build_class_recall_plot <- function(recall_tbl) {
  recall_tbl %>%
    mutate(class = factor(tools::toTitleCase(class), levels = c("Low", "Medium", "High"))) %>%
    ggplot(aes(x = class, y = recall, fill = model)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.64) +
    scale_fill_manual(values = c("Decision Tree" = "#d86f45", "Random Forest" = "#0f6b6f")) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = "Per-Class Recall on the Test Set",
      x = NULL,
      y = "Recall",
      fill = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

build_trend_plot <- function(data_tbl, metric_name) {
  label_text <- switch(
    metric_name,
    hotel_occ = "Hotel occupancy (%)",
    visitor_arrivals = "Visitor arrivals",
    china_share_pct = "China share (%)",
    avg_stay_monthly = "Average stay (days)",
    tools::toTitleCase(gsub("_", " ", metric_name))
  )

  formatter <- switch(
    metric_name,
    hotel_occ = label_number(accuracy = 0.1),
    visitor_arrivals = label_comma(),
    china_share_pct = label_number(accuracy = 0.1, suffix = "%"),
    avg_stay_monthly = label_number(accuracy = 0.1),
    label_number()
  )

  ggplot(data_tbl, aes(x = date, y = .data[[metric_name]])) +
    geom_line(color = "#95a3a7", linewidth = 1) +
    geom_point(aes(color = hotel_occ_level_tertile), size = 2.8, alpha = 0.9) +
    scale_color_manual(values = c(low = "#c77d4e", medium = "#8a9a94", high = "#0f6b6f")) +
    scale_y_continuous(labels = formatter) +
    labs(
      title = paste(label_text, "Over Time"),
      x = NULL,
      y = label_text,
      color = "Occupancy class"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

build_oob_plot <- function(oob_tbl) {
  ggplot(oob_tbl, aes(x = num_trees, y = oob_accuracy)) +
    geom_line(color = "#2f7d4f", linewidth = 1.2) +
    geom_point(color = "#2f7d4f", size = 2.5) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "OOB Accuracy vs Number of Trees",
      x = "Number of trees",
      y = "OOB accuracy"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

load_model_bundle <- function(project_dir) {
  output_dir <- file.path(project_dir, "outputs")
  data_dir <- file.path(project_dir, "data")

  data_tbl <- read_csv_safe(file.path(data_dir, "tourism_decision_tree_ready.csv"))
  tree_metrics <- read_csv_safe(file.path(output_dir, "decision_tree_metrics.csv"))
  rf_metrics <- read_csv_safe(file.path(output_dir, "random_forest_metrics.csv"))
  tree_conf <- read_csv_safe(file.path(output_dir, "decision_tree_confusion_matrix.csv"))
  rf_conf <- read_csv_safe(file.path(output_dir, "random_forest_confusion_matrix.csv"))
  tree_importance <- read_csv_safe(file.path(output_dir, "decision_tree_variable_importance.csv"))
  rf_importance <- read_csv_safe(file.path(output_dir, "random_forest_variable_importance.csv"))
  tree_predictions <- read_csv_safe(file.path(output_dir, "decision_tree_test_predictions.csv"))
  rf_predictions <- read_csv_safe(file.path(output_dir, "random_forest_test_predictions.csv"))
  rf_oob <- read_csv_safe(file.path(output_dir, "random_forest_oob_accuracy.csv"))

  list(
    project_dir = project_dir,
    output_dir = output_dir,
    data_tbl = data_tbl,
    tree_metrics = tree_metrics,
    rf_metrics = rf_metrics,
    tree_conf = tree_conf,
    rf_conf = rf_conf,
    tree_importance = tree_importance,
    rf_importance = rf_importance,
    tree_predictions = tree_predictions,
    rf_predictions = rf_predictions,
    rf_oob = rf_oob
  )
}

metric_card <- function(title, value, subtitle = NULL, accent = "#0f6b6f") {
  card(
    class = "metric-card",
    style = paste0("border-top: 4px solid ", accent, ";"),
    card_body(
      p(class = "metric-title", title),
      h2(class = "metric-value", value),
      if (!is.null(subtitle)) p(class = "metric-subtitle", subtitle)
    )
  )
}

PROJECT_DIR <- locate_project_dir()
MODEL_BUNDLE <- load_model_bundle(PROJECT_DIR)

if (dir.exists(MODEL_BUNDLE$output_dir)) {
  addResourcePath("model_outputs", MODEL_BUNDLE$output_dir)
}

tree_accuracy <- metric_value(MODEL_BUNDLE$tree_metrics, "accuracy")
rf_accuracy <- metric_value(MODEL_BUNDLE$rf_metrics, "accuracy")
tree_kappa <- metric_value(MODEL_BUNDLE$tree_metrics, "kappa")
rf_kappa <- metric_value(MODEL_BUNDLE$rf_metrics, "kappa")
tree_recall_tbl <- class_recall_table(MODEL_BUNDLE$tree_conf, "Decision Tree")
rf_recall_tbl <- class_recall_table(MODEL_BUNDLE$rf_conf, "Random Forest")
all_recall_tbl <- bind_rows(tree_recall_tbl, rf_recall_tbl)
tree_low_recall <- tree_recall_tbl %>% filter(class == "low") %>% pull(recall) %>% first(default = NA_real_)
rf_low_recall <- rf_recall_tbl %>% filter(class == "low") %>% pull(recall) %>% first(default = NA_real_)
tree_high_recall <- tree_recall_tbl %>% filter(class == "high") %>% pull(recall) %>% first(default = NA_real_)
rf_high_recall <- rf_recall_tbl %>% filter(class == "high") %>% pull(recall) %>% first(default = NA_real_)
accuracy_gap <- if (is.na(tree_accuracy) || is.na(rf_accuracy)) NA_real_ else tree_accuracy - rf_accuracy

best_model <- if (is.na(tree_accuracy) || is.na(rf_accuracy)) {
  "Model outputs incomplete"
} else if (tree_accuracy >= rf_accuracy) {
  "Decision Tree"
} else {
  "Random Forest"
}

summary_tbl <- MODEL_BUNDLE$data_tbl %||% tibble(
  date = as.Date(character()),
  period = character(),
  visitor_arrivals = numeric(),
  china_share_pct = numeric(),
  hotel_occ = numeric(),
  avg_stay_monthly = numeric(),
  hotel_occ_level_tertile = character(),
  dataset_split = character()
)
train_rows <- sum(summary_tbl$dataset_split == "train", na.rm = TRUE)
test_rows <- sum(summary_tbl$dataset_split == "test", na.rm = TRUE)
coverage_text <- if (has_rows(summary_tbl)) {
  paste(format(min(summary_tbl$date, na.rm = TRUE), "%b %Y"), "to", format(max(summary_tbl$date, na.rm = TRUE), "%b %Y"))
} else {
  "N/A"
}
feature_count <- length(unique(c(
  if (has_rows(MODEL_BUNDLE$tree_importance)) concept_feature_label(MODEL_BUNDLE$tree_importance$variable) else character(),
  if (has_rows(MODEL_BUNDLE$rf_importance)) concept_feature_label(MODEL_BUNDLE$rf_importance$variable) else character()
)))
if (feature_count == 0) {
  feature_count <- 4L
}
data_metric_choices <- c(
  "Hotel occupancy (%)" = "hotel_occ",
  "Visitor arrivals" = "visitor_arrivals",
  "China share (%)" = "china_share_pct",
  "Average stay (days)" = "avg_stay_monthly"
)
date_limits <- if (has_rows(summary_tbl)) range(summary_tbl$date, na.rm = TRUE) else rep(Sys.Date(), 2)
split_choices <- sort(unique(summary_tbl$dataset_split))
tree_top_n_max <- max(1L, nrow(MODEL_BUNDLE$tree_importance %||% tibble()))
rf_top_n_max <- max(1L, min(12L, nrow(MODEL_BUNDLE$rf_importance %||% tibble())))
tree_preview_max <- max(1L, min(20L, nrow(MODEL_BUNDLE$tree_predictions %||% tibble())))
rf_preview_max <- max(1L, min(20L, nrow(MODEL_BUNDLE$rf_predictions %||% tibble())))

ui <- page_navbar(
  title = "Tourism Classification Explorer",
  theme = bs_theme(
    bg = "#f4f2eb",
    fg = "#1f2a2e",
    primary = "#0f6b6f",
    secondary = "#d86f45",
    base_font = font_collection("Aptos", "Segoe UI", "sans-serif"),
    heading_font = font_collection("Palatino Linotype", "Georgia", "serif")
  ),
  header = tags$head(
    tags$style(HTML(
      "
      .metric-card { min-height: 180px; background: rgba(255,255,255,0.82); }
      .metric-title { text-transform: uppercase; letter-spacing: 0.08em; font-size: 0.78rem; color: #5b666a; margin-bottom: 0.35rem; }
      .metric-value { margin-bottom: 0.45rem; font-weight: 700; }
      .metric-subtitle { color: #5b666a; margin-bottom: 0; }
      .section-note { color: #5b666a; max-width: 72ch; }
      .filepath { font-family: monospace; font-size: 0.92rem; word-break: break-all; color: #445156; }
      .app-hero { padding: 0.35rem 0 0.85rem 0; }
      .app-hero h1 { margin-bottom: 0.4rem; }
      .app-hero p { margin-bottom: 0; color: #4d5b60; max-width: 76ch; }
      iframe.tree-frame { width: 100%; height: 760px; border: 0; border-radius: 14px; background: white; }
      img.full-card { width: 100%; height: auto; display: block; border-radius: 12px; }
      "
    ))
  ),
  nav_panel(
    "Overview",
    div(
      class = "app-hero",
      h1("Tourism Classification Explorer"),
      p(
        "A single-file Shiny interface for reviewing classification tree and random forest outputs,",
        "including holdout performance, confusion patterns, feature importance, and prediction previews."
      )
    ),
    layout_column_wrap(
      width = 1 / 4,
      metric_card("Best Holdout Accuracy", format_metric(max_or_na(c(tree_accuracy, rf_accuracy))), paste(best_model, "leads on test accuracy"), "#0f6b6f"),
      metric_card("Decision Tree", format_metric(tree_accuracy), paste("Kappa:", format_decimal(tree_kappa)), "#d86f45"),
      metric_card("Random Forest", format_metric(rf_accuracy), paste("Kappa:", format_decimal(rf_kappa)), "#2f7d4f"),
      metric_card("Dataset Window", coverage_text, "Detected data coverage", "#7f5a83")
    ),
    layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Project Summary"),
        card_body(
          p(class = "section-note", "This UI wraps your previously exported model artifacts instead of retraining the models inside the app."),
          tags$ul(
            tags$li("Target: hotel occupancy level tertile (low / medium / high)"),
            tags$li("Conceptual features: visitor arrivals, China share, average stay, month"),
            tags$li("Data rows loaded: ", nrow(summary_tbl)),
            tags$li("Train / test rows: ", train_rows, " / ", test_rows),
            tags$li("Detected project folder:"),
            tags$li(tags$span(class = "filepath", PROJECT_DIR))
          )
        )
      ),
      card(
        card_header("Model Metrics"),
        card_body(
          DTOutput("overview_metrics")
        )
      ),
      card(
        card_header("Data Preview"),
        card_body(
          DTOutput("overview_data")
        )
      ),
      card(
        card_header("Recommendation"),
        card_body(
          uiOutput("recommendation_text")
        )
      )
    )
  ),
  nav_panel(
    "Data Explorer",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput("data_metric", "Trend metric", choices = data_metric_choices, selected = "hotel_occ"),
        checkboxGroupInput(
          "data_split_filter",
          "Dataset split",
          choices = split_choices,
          selected = split_choices
        ),
        selectInput(
          "data_class_filter",
          "Occupancy class",
          choices = c("All", "low", "medium", "high"),
          selected = "All"
        ),
        dateRangeInput(
          "data_date_range",
          "Date range",
          start = date_limits[1],
          end = date_limits[2],
          min = date_limits[1],
          max = date_limits[2]
        ),
        helpText("Use this page to show the tourism pattern behind the labels before discussing model quality.")
      ),
      layout_column_wrap(
        width = 1 / 2,
        card(
          card_header("Trend View"),
          card_body(plotOutput("data_trend_plot", height = "340px"))
        ),
        card(
          card_header("Filtered Preview"),
          card_body(DTOutput("data_explorer_tbl"))
        )
      )
    )
  ),
  nav_panel(
    "Classification Tree",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        radioButtons(
          "tree_view_mode",
          "Tree View",
          choices = c("Interactive HTML", "Static PNG"),
          selected = if (file.exists(file.path(MODEL_BUNDLE$output_dir, "decision_tree_visual.html"))) {
            "Interactive HTML"
          } else {
            "Static PNG"
          }
        ),
        sliderInput("tree_top_n", "Top variables to show", min = 1, max = tree_top_n_max, value = min(4L, tree_top_n_max)),
        sliderInput("tree_preview_n", "Prediction preview rows", min = 1, max = tree_preview_max, value = min(10L, tree_preview_max)),
        selectInput(
          "tree_result_filter",
          "Prediction rows",
          choices = c("All rows" = "all", "Misclassified only" = "misclassified", "Correct only" = "correct"),
          selected = "all"
        ),
        selectInput(
          "tree_class_filter",
          "Actual class filter",
          choices = c("All", "low", "medium", "high"),
          selected = "All"
        ),
        helpText("Use the tree page to inspect interpretability first, then compare it against the forest.")
      ),
      layout_column_wrap(
        width = 1 / 2,
        card(
          card_header("Tree Structure"),
          card_body(
            uiOutput("tree_visual")
          )
        ),
        card(
          card_header("Tree Metrics"),
          card_body(DTOutput("tree_metrics_tbl"))
        ),
        card(
          card_header("Decision Tree Confusion Matrix"),
          card_body(plotOutput("tree_conf_plot", height = "320px"))
        ),
        card(
          card_header("Decision Tree Variable Importance"),
          card_body(plotOutput("tree_importance_plot", height = "320px"))
        ),
        card(
          card_header("Tree Notes"),
          card_body(uiOutput("tree_notes"))
        ),
        card(
          card_header("Prediction Timeline"),
          card_body(plotOutput("tree_prediction_plot", height = "320px"))
        ),
        card(
          card_header("Prediction Preview"),
          card_body(DTOutput("tree_predictions_tbl"))
        )
      )
    )
  ),
  nav_panel(
    "Random Forest",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        sliderInput("rf_top_n", "Top variables to show", min = 1, max = rf_top_n_max, value = min(8L, rf_top_n_max)),
        sliderInput("rf_preview_n", "Prediction preview rows", min = 1, max = rf_preview_max, value = min(10L, rf_preview_max)),
        selectInput(
          "rf_result_filter",
          "Prediction rows",
          choices = c("All rows" = "all", "Misclassified only" = "misclassified", "Correct only" = "correct"),
          selected = "all"
        ),
        selectInput(
          "rf_class_filter",
          "Actual class filter",
          choices = c("All", "low", "medium", "high"),
          selected = "All"
        ),
        helpText("The forest page focuses on accuracy diagnostics, OOB behavior, and richer importance output.")
      ),
      layout_column_wrap(
        width = 1 / 2,
        card(
          card_header("Random Forest Metrics"),
          card_body(DTOutput("rf_metrics_tbl"))
        ),
        card(
          card_header("Random Forest Confusion Matrix"),
          card_body(plotOutput("rf_conf_plot", height = "320px"))
        ),
        card(
          card_header("Random Forest Variable Importance"),
          card_body(plotOutput("rf_importance_plot", height = "320px"))
        ),
        card(
          card_header("OOB Accuracy"),
          card_body(plotOutput("rf_oob_plot", height = "320px"))
        ),
        card(
          card_header("Forest Notes"),
          card_body(uiOutput("rf_notes"))
        ),
        card(
          card_header("Actual vs Predicted Over Time"),
          card_body(plotOutput("rf_prediction_plot", height = "320px"))
        ),
        card(
          card_header("Prediction Preview"),
          card_body(DTOutput("rf_predictions_tbl"))
        )
      )
    )
  ),
  nav_panel(
    "Compare Models",
    layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Accuracy and Kappa Comparison"),
        card_body(plotOutput("compare_metrics_plot", height = "340px"))
      ),
      card(
        card_header("Per-Class Recall"),
        card_body(plotOutput("compare_class_recall_plot", height = "340px"))
      ),
      card(
        card_header("Top Features by Model"),
        card_body(plotOutput("compare_importance_plot", height = "340px"))
      ),
      card(
        card_header("Model Selection Notes"),
        card_body(uiOutput("compare_notes"))
      ),
      card(
        card_header("Merged Metric Table"),
        card_body(DTOutput("compare_metrics_tbl"))
      )
    )
  ),
  nav_panel(
    "About",
    layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("What This App Uses"),
        card_body(
          tags$ul(
            tags$li("decision_tree_metrics.csv"),
            tags$li("decision_tree_confusion_matrix.csv"),
            tags$li("decision_tree_variable_importance.csv"),
            tags$li("decision_tree_test_predictions.csv"),
            tags$li("random_forest_metrics.csv"),
            tags$li("random_forest_confusion_matrix.csv"),
            tags$li("random_forest_variable_importance.csv"),
            tags$li("random_forest_oob_accuracy.csv"),
            tags$li("random_forest_test_predictions.csv")
          )
        )
      ),
      card(
        card_header("How To Run"),
        card_body(
          tags$pre("shiny::runApp()\n# or\nshiny::runApp('path/to/this/shiny/folder')"),
          p("This folder is self-contained as long as `app.R`, `data/`, and `outputs/` stay together.")
        )
      ),
      card(
        card_header("Detected Paths"),
        card_body(
          p("Project directory"),
          div(class = "filepath", PROJECT_DIR),
          p("Outputs directory"),
          div(class = "filepath", MODEL_BUNDLE$output_dir)
        )
      ),
      card(
        card_header("Design Intent"),
        card_body(
          p("The overview page gives fast performance context."),
          p("The tree page emphasizes interpretability."),
          p("The forest page emphasizes predictive diagnostics."),
          p("The comparison page supports a final recommendation.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data_tbl <- reactive({
    if (!has_rows(summary_tbl)) {
      return(tibble())
    }

    req(input$data_date_range)

    out <- summary_tbl %>%
      filter(
        date >= as.Date(input$data_date_range[1]),
        date <= as.Date(input$data_date_range[2])
      )

    if (length(input$data_split_filter) > 0) {
      out <- out %>% filter(dataset_split %in% input$data_split_filter)
    }

    if (!identical(input$data_class_filter, "All")) {
      out <- out %>% filter(hotel_occ_level_tertile == input$data_class_filter)
    }

    out
  })

  tree_prediction_rows <- reactive({
    filter_prediction_rows(
      MODEL_BUNDLE$tree_predictions,
      result_filter = input$tree_result_filter,
      class_filter = input$tree_class_filter
    )
  })

  rf_prediction_rows <- reactive({
    filter_prediction_rows(
      MODEL_BUNDLE$rf_predictions,
      result_filter = input$rf_result_filter,
      class_filter = input$rf_class_filter
    )
  })

  output$overview_metrics <- renderDT({
    metrics_tbl <- bind_rows(
      metric_rows_for_model("Decision Tree", tree_accuracy, tree_kappa, tree_recall_tbl),
      metric_rows_for_model("Random Forest", rf_accuracy, rf_kappa, rf_recall_tbl)
    )

    datatable(metrics_tbl, options = list(dom = "t", pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$overview_data <- renderDT({
    preview_tbl <- MODEL_BUNDLE$data_tbl %>%
      mutate(date = as.character(date)) %>%
      select(date, hotel_occ, hotel_occ_level_tertile, dataset_split, visitor_arrivals, visitor_arrivals_china) %>%
      head(12)

    datatable(preview_tbl, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })

  output$data_trend_plot <- renderPlot({
    req(input$data_metric)
    plot_tbl <- filtered_data_tbl()
    validate(need(has_rows(plot_tbl), "No rows match the current Data Explorer filters."))
    build_trend_plot(plot_tbl, input$data_metric)
  })

  output$data_explorer_tbl <- renderDT({
    preview_tbl <- filtered_data_tbl()

    if (!has_rows(preview_tbl)) {
      return(table_note("No rows match the current Data Explorer filters."))
    }

    preview_tbl <- preview_tbl %>%
      arrange(desc(date)) %>%
      transmute(
        Date = as.character(date),
        Split = dataset_split,
        Class = hotel_occ_level_tertile,
        `Hotel Occ (%)` = round(hotel_occ, 1),
        Arrivals = round(visitor_arrivals),
        `China Share (%)` = round(china_share_pct, 1),
        `Average Stay` = round(avg_stay_monthly, 2)
      )

    datatable(preview_tbl, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })

  output$recommendation_text <- renderUI({
    decision_text <- if (!is.na(tree_accuracy) && !is.na(rf_accuracy) && tree_accuracy >= rf_accuracy) {
      paste(
        "Decision Tree is the safer presentation choice here because it leads the forest by",
        format_gap_pp(accuracy_gap),
        "on holdout accuracy and is easier to explain."
      )
    } else if (!is.na(rf_accuracy) && !is.na(tree_accuracy)) {
      paste(
        "Random Forest leads on holdout accuracy by",
        format_gap_pp(-accuracy_gap),
        "so it is the better predictive choice if your focus is performance rather than interpretability."
      )
    } else {
      "Recommendation is unavailable until both model metric files are present."
    }

    tagList(
      p(decision_text),
      tags$ul(
        tags$li(paste("Low-class recall is still weak:", "tree", format_metric(tree_low_recall), "| forest", format_metric(rf_low_recall))),
        tags$li(paste("High-class recall remains stronger:", "tree", format_metric(tree_high_recall), "| forest", format_metric(rf_high_recall))),
        tags$li("Use the tree when you need transparent splits and classroom-friendly storytelling."),
        tags$li("Use the forest when you need stronger robustness checks such as OOB tracking and richer variable importance.")
      )
    )
  })

  output$tree_visual <- renderUI({
    html_path <- file.path(MODEL_BUNDLE$output_dir, "decision_tree_visual.html")
    png_path <- file.path(MODEL_BUNDLE$output_dir, "decision_tree_static.png")

    if (identical(input$tree_view_mode, "Interactive HTML") && file.exists(html_path)) {
      tags$iframe(class = "tree-frame", src = "model_outputs/decision_tree_visual.html")
    } else if (file.exists(png_path)) {
      tags$img(class = "full-card", src = "model_outputs/decision_tree_static.png", alt = "Decision tree static view")
    } else {
      p("No decision tree visual file was found.")
    }
  })

  output$tree_metrics_tbl <- renderDT({
    metrics_tbl <- metric_rows_for_model("Decision Tree", tree_accuracy, tree_kappa, tree_recall_tbl) %>%
      select(Metric, Value = Display)

    datatable(metrics_tbl, options = list(dom = "t"), rownames = FALSE)
  })

  output$tree_conf_plot <- renderPlot({
    req(MODEL_BUNDLE$tree_conf)
    build_confusion_plot(MODEL_BUNDLE$tree_conf, "Decision Tree Confusion Matrix", "#d86f45")
  })

  output$tree_importance_plot <- renderPlot({
    req(MODEL_BUNDLE$tree_importance)
    build_importance_plot(
      MODEL_BUNDLE$tree_importance,
      value_col = "importance",
      title_text = "Decision Tree Variable Importance",
      top_n = input$tree_top_n,
      fill_color = "#d86f45"
    )
  })

  output$tree_notes <- renderUI({
    top_feature <- if (has_rows(MODEL_BUNDLE$tree_importance)) {
      MODEL_BUNDLE$tree_importance %>%
        mutate(label = display_feature_label(variable)) %>%
        arrange(desc(importance)) %>%
        pull(label) %>%
        first(default = "N/A")
    } else {
      "N/A"
    }

    tagList(
      p("The tree is still the easier model to defend in a presentation because its rules stay explicit."),
      tags$ul(
        tags$li(paste("Top tree feature:", top_feature)),
        tags$li(paste("High-class recall:", format_metric(tree_high_recall))),
        tags$li(paste("Low-class recall:", format_metric(tree_low_recall))),
        tags$li("Use the filters to isolate which months the tree misses.")
      )
    )
  })

  output$tree_prediction_plot <- renderPlot({
    pred_tbl <- tree_prediction_rows()
    validate(need(has_rows(pred_tbl), "No tree prediction rows match the selected filters."))
    build_prediction_plot(pred_tbl, "Decision Tree: Actual vs Predicted Class")
  })

  output$tree_predictions_tbl <- renderDT({
    preview_tbl <- tree_prediction_rows()

    if (!has_rows(preview_tbl)) {
      return(table_note("No tree prediction rows match the selected filters."))
    }

    preview_tbl <- preview_tbl %>%
      transmute(
        Date = as.character(date),
        `Hotel Occ (%)` = if ("hotel_occ" %in% names(preview_tbl)) round(hotel_occ, 1) else NA_real_,
        Actual = actual,
        Predicted = predicted_class,
        Correct = if_else(correct, "Yes", "No"),
        Confidence = if_else(is.na(confidence), "N/A", percent(confidence, accuracy = 0.1))
      ) %>%
      head(input$tree_preview_n)

    datatable(preview_tbl, options = list(pageLength = input$tree_preview_n, scrollX = TRUE), rownames = FALSE)
  })

  output$rf_metrics_tbl <- renderDT({
    metrics_tbl <- metric_rows_for_model("Random Forest", rf_accuracy, rf_kappa, rf_recall_tbl) %>%
      select(Metric, Value = Display)

    datatable(metrics_tbl, options = list(dom = "t"), rownames = FALSE)
  })

  output$rf_conf_plot <- renderPlot({
    req(MODEL_BUNDLE$rf_conf)
    build_confusion_plot(MODEL_BUNDLE$rf_conf, "Random Forest Confusion Matrix", "#0f6b6f")
  })

  output$rf_importance_plot <- renderPlot({
    req(MODEL_BUNDLE$rf_importance)
    build_importance_plot(
      MODEL_BUNDLE$rf_importance,
      value_col = "Overall",
      title_text = "Random Forest Variable Importance",
      top_n = input$rf_top_n,
      fill_color = "#0f6b6f"
    )
  })

  output$rf_oob_plot <- renderPlot({
    req(MODEL_BUNDLE$rf_oob)
    build_oob_plot(MODEL_BUNDLE$rf_oob)
  })

  output$rf_notes <- renderUI({
    top_feature <- if (has_rows(MODEL_BUNDLE$rf_importance)) {
      MODEL_BUNDLE$rf_importance %>%
        mutate(label = display_feature_label(variable)) %>%
        arrange(desc(Overall)) %>%
        pull(label) %>%
        first(default = "N/A")
    } else {
      "N/A"
    }

    peak_oob <- if (has_rows(MODEL_BUNDLE$rf_oob)) max_or_na(MODEL_BUNDLE$rf_oob$oob_accuracy) else NA_real_

    tagList(
      p("The forest gives richer diagnostics, but the test-set story still matters more than OOB alone."),
      tags$ul(
        tags$li(paste("Top forest feature:", top_feature)),
        tags$li(paste("Best OOB accuracy:", format_metric(peak_oob))),
        tags$li(paste("High-class recall:", format_metric(rf_high_recall))),
        tags$li(paste("Low-class recall:", format_metric(rf_low_recall)))
      )
    )
  })

  output$rf_prediction_plot <- renderPlot({
    pred_tbl <- rf_prediction_rows()
    validate(need(has_rows(pred_tbl), "No random forest prediction rows match the selected filters."))
    build_prediction_plot(pred_tbl, "Random Forest: Actual vs Predicted Class")
  })

  output$rf_predictions_tbl <- renderDT({
    preview_tbl <- rf_prediction_rows()

    if (!has_rows(preview_tbl)) {
      return(table_note("No random forest prediction rows match the selected filters."))
    }

    preview_tbl <- preview_tbl %>%
      transmute(
        Date = as.character(date),
        Actual = actual,
        Predicted = predicted_class,
        Correct = if_else(correct, "Yes", "No"),
        Confidence = if_else(is.na(confidence), "N/A", percent(confidence, accuracy = 0.1))
      ) %>%
      head(input$rf_preview_n)

    datatable(preview_tbl, options = list(pageLength = input$rf_preview_n, scrollX = TRUE), rownames = FALSE)
  })

  output$compare_metrics_plot <- renderPlot({
    build_metric_comparison_plot(tree_accuracy, tree_kappa, rf_accuracy, rf_kappa)
  })

  output$compare_class_recall_plot <- renderPlot({
    validate(need(has_rows(all_recall_tbl), "Per-class recall is unavailable."))
    build_class_recall_plot(all_recall_tbl)
  })

  output$compare_importance_plot <- renderPlot({
    tree_tbl <- MODEL_BUNDLE$tree_importance %>%
      transmute(model = "Decision Tree", variable = concept_feature_label(variable), score = importance)

    rf_tbl <- MODEL_BUNDLE$rf_importance %>%
      transmute(model = "Random Forest", variable = concept_feature_label(variable), score = Overall)

    bind_rows(tree_tbl, rf_tbl) %>%
      group_by(model, variable) %>%
      summarise(score = sum(score), .groups = "drop") %>%
      group_by(model) %>%
      slice_max(order_by = score, n = 4, with_ties = FALSE) %>%
      ungroup() %>%
      ggplot(aes(x = reorder(variable, score), y = score, fill = model)) +
      geom_col(position = position_dodge(width = 0.72), width = 0.64) +
      coord_flip() +
      scale_fill_manual(values = c("Decision Tree" = "#d86f45", "Random Forest" = "#0f6b6f")) +
      labs(
        title = "Conceptual Feature Importance by Model",
        x = NULL,
        y = "Importance",
        fill = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
  })

  output$compare_notes <- renderUI({
    tagList(
      p(paste("Best holdout accuracy:", best_model)),
      tags$ul(
        tags$li(paste("Holdout accuracy gap:", if (is.na(accuracy_gap)) "N/A" else format_gap_pp(accuracy_gap), "(tree minus forest)")),
        tags$li(paste("Decision Tree accuracy:", format_metric(tree_accuracy), "| kappa:", format_decimal(tree_kappa))),
        tags$li(paste("Random Forest accuracy:", format_metric(rf_accuracy), "| kappa:", format_decimal(rf_kappa))),
        tags$li(paste("High-class recall:", "tree", format_metric(tree_high_recall), "| forest", format_metric(rf_high_recall))),
        tags$li(paste("Low-class recall:", "tree", format_metric(tree_low_recall), "| forest", format_metric(rf_low_recall))),
        tags$li("Decision Tree gives clearer rule-based storytelling for class presentations."),
        tags$li("Random Forest adds stronger diagnostic depth through OOB accuracy and expanded feature importance.")
      )
    )
  })

  output$compare_metrics_tbl <- renderDT({
    compare_tbl <- bind_rows(
      metric_rows_for_model("Decision Tree", tree_accuracy, tree_kappa, tree_recall_tbl),
      metric_rows_for_model("Random Forest", rf_accuracy, rf_kappa, rf_recall_tbl)
    )

    datatable(compare_tbl, options = list(dom = "t"), rownames = FALSE)
  })
}

shinyApp(ui, server)
