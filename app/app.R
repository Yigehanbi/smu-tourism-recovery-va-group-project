library(shiny)
library(bslib)

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

project_root <- find_project_root()

source(file.path(project_root, "app", "R", "data_utils.R"))
source(file.path(project_root, "app", "R", "mod_forecast_ui.R"))
source(file.path(project_root, "app", "R", "mod_forecast_server.R"))

ui <- page_fillable(
  title = "Singapore Tourism Forecasting Studio",
  theme = bs_theme(
    bg = "#f4f2eb",
    fg = "#1f2a2e",
    primary = "#0f6b6f",
    secondary = "#d86f45",
    base_font = font_google("Space Grotesk"),
    heading_font = font_google("Space Grotesk")
  ),
  fillable = TRUE,
  gap = 0,
  padding = 0,
  div(
    class = "forecast-app-page",
    mod_forecast_ui("forecast_module")
  )
)

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app-theme.css")
  ),
  ui
)

server <- function(input, output, session) {
  shared_data <- reactive({
    load_tourism_data()
  })
  mod_forecast_server("forecast_module", data = shared_data)
}

shinyApp(ui, server)
