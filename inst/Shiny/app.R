# Support both shiny::runApp("inst/Shiny") and sourcing from the repository root.
app_dir <- if (file.exists("ui.R")) "." else "inst/Shiny"
source(file.path(app_dir, "ui.R"))
source(file.path(app_dir, "server.R"))

options(shiny.maxRequestSize = 1000 * 1024^2)
shiny::shinyApp(ui, server)
