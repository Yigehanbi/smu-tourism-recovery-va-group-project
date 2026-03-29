library(dplyr)
library(readxl)

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(current, "_quarto.yml"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Project root not found. Expected to locate _quarto.yml.")
    }

    current <- parent
  }
}

resolve_arrival_workbook <- function() {
  project_root <- find_project_root()

  candidates <- c(
    file.path(project_root, "data", "raw", "visitor_arrivals_full_dataset.xlsx"),
    file.path(project_root, "data", "raw", "tourism_four_part_analysis_ready.xlsx")
  )

  path <- candidates[file.exists(candidates)][1]

  if (is.na(path) || !nzchar(path)) {
    stop("Dataset not found. Expected a tourism workbook under data/raw/.")
  }

  path
}

resolve_processed_path <- function(filename) {
  file.path(find_project_root(), "data", "processed", filename)
}

read_arrival_metadata <- function(path = resolve_arrival_workbook()) {
  meta <- read_excel(
    path,
    sheet = "My Series",
    col_names = FALSE,
    n_max = 4,
    .name_repair = "minimal"
  )

  headers <- as.character(meta[1, ])
  frequencies <- as.character(meta[4, ])

  data.frame(
    column_index = seq_along(headers),
    raw_name = headers,
    frequency = frequencies,
    stringsAsFactors = FALSE
  )
}

clean_country_series_label <- function(x) {
  x <- sub("^Visitor Arrivals:\\s*", "", x)
  x <- sub("\\.\\d+$", "", x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}

country_series_catalog <- function(path = resolve_arrival_workbook()) {
  meta <- read_arrival_metadata(path)

  catalog <- meta |>
    filter(
      frequency == "Monthly",
      grepl("^Visitor Arrivals:", raw_name),
      !grepl("^Visitor Arrivals: (Air|Sea|Land)(:|$)", raw_name),
      !grepl(
        "^Visitor Arrivals: (Total|ASEAN|Americas|Asia|Africa|Europe|North Asia|South Asia|West Asia|Oceania|Scandinavia|Age:|sa:)",
        raw_name
      ),
      !grepl("^Tourist Arrivals:", raw_name)
    ) |>
    mutate(
      series_label = make.unique(clean_country_series_label(raw_name))
    )

  catalog
}

load_clustering_country_wide <- function(path = resolve_arrival_workbook()) {
  processed_path <- resolve_processed_path("clustering_country_wide.csv")

  if (file.exists(processed_path)) {
    wide <- read.csv(processed_path, stringsAsFactors = FALSE, check.names = FALSE)
    wide$date <- as.Date(wide$date)
    return(wide)
  }

  catalog <- country_series_catalog(path)
  raw_data <- read_excel(
    path,
    sheet = "My Series",
    col_names = FALSE,
    skip = 29,
    .name_repair = "minimal"
  )
  raw_data <- as.data.frame(raw_data, stringsAsFactors = FALSE)

  wide <- data.frame(
    date = as.Date(raw_data[[1]]),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(catalog))) {
    col_idx <- catalog$column_index[i]
    series_name <- catalog$series_label[i]
    wide[[series_name]] <- suppressWarnings(as.numeric(raw_data[[col_idx]]))
  }

  wide <- wide |>
    filter(!is.na(date)) |>
    filter(date >= as.Date("2016-12-01")) |>
    arrange(date)

  wide
}

load_clustering_country_long <- function(path = resolve_arrival_workbook()) {
  processed_path <- resolve_processed_path("clustering_country_long.csv")

  if (file.exists(processed_path)) {
    long <- read.csv(processed_path, stringsAsFactors = FALSE, check.names = FALSE)
    long$date <- as.Date(long$date)
    return(long)
  }

  wide <- load_clustering_country_wide(path)
  series_cols <- setdiff(names(wide), "date")

  long <- data.frame(
    date = rep(wide$date, times = length(series_cols)),
    series = rep(series_cols, each = nrow(wide)),
    arrivals = as.vector(as.matrix(wide[, series_cols, drop = FALSE])),
    stringsAsFactors = FALSE
  )

  long <- long |>
    mutate(
      year = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m")),
      quarter = ((month - 1) %/% 3) + 1
    )

  long
}

available_clustering_series <- function(path = resolve_arrival_workbook()) {
  wide <- load_clustering_country_wide(path)
  setdiff(names(wide), "date")
}

default_clustering_series <- function(path = resolve_arrival_workbook(), n = 8) {
  wide <- load_clustering_country_wide(path)
  series_cols <- setdiff(names(wide), "date")

  means <- vapply(series_cols, function(col) mean(wide[[col]], na.rm = TRUE), numeric(1))
  ordered <- names(sort(means, decreasing = TRUE))

  head(ordered, n = min(n, length(ordered)))
}

normalize_series_vector <- function(x, mode = c("indexed", "zscore", "raw")) {
  mode <- match.arg(mode)
  x <- as.numeric(x)

  if (mode == "raw") {
    return(x)
  }

  if (mode == "indexed") {
    base_value <- x[which(!is.na(x) & x != 0)[1]]
    if (is.na(base_value) || base_value == 0) {
      base_value <- x[which(!is.na(x))[1]]
    }
    if (is.na(base_value) || base_value == 0) {
      return(rep(NA_real_, length(x)))
    }
    return((x / base_value) * 100)
  }

  if (all(is.na(x))) {
    return(x)
  }

  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    return(rep(0, length(x)))
  }

  as.numeric(scale(x))
}

prepare_country_clustering_data <- function(
  wide,
  selected_series,
  year_window,
  normalization = c("indexed", "zscore", "raw")
) {
  normalization <- match.arg(normalization)

  if (!"date" %in% names(wide)) {
    stop("The wide table must contain a date column.")
  }

  selected_series <- intersect(selected_series, setdiff(names(wide), "date"))
  if (length(selected_series) < 2) {
    stop("Select at least two country series.")
  }

  start_year <- min(year_window)
  end_year <- max(year_window)

  windowed <- wide |>
    filter(
      as.integer(format(date, "%Y")) >= start_year,
      as.integer(format(date, "%Y")) <= end_year
    ) |>
    select(date, all_of(selected_series)) |>
    arrange(date)

  if (nrow(windowed) < 12) {
    stop("The selected window is too short for clustering.")
  }

  normalized <- windowed
  for (series_name in selected_series) {
    normalized[[series_name]] <- normalize_series_vector(windowed[[series_name]], normalization)
  }

  matrix_data <- t(as.matrix(normalized[, selected_series, drop = FALSE]))
  rownames(matrix_data) <- selected_series

  list(
    raw_wide = windowed,
    normalized_wide = normalized,
    matrix = matrix_data,
    dates = windowed$date,
    series = selected_series,
    normalization = normalization
  )
}

load_tourism_data <- function(path = resolve_arrival_workbook()) {
  load_clustering_country_wide(path)
}
