# library(shiny)
# library(ggplot2)
# library(zoo)
# library(knitr)

# Run the application 
shinyApp(ui = ui, server = server)

Appui <- system.file("Shiny","ui.R", package = "GelAnalyser")
Appserver <- system.file("Shiny","server.R", package = "GelAnalyser")

shinyApp(Appui, Appserver )
