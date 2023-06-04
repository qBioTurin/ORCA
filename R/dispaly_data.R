#' @title Run InteGreat
#' @description function to lunch the InteGreat shiny application.
#'
#' @param 
#'
#' @author Pernice Simone Tortarolo Dora
#' @import ggplot2 shinydashboard shinyWidgets shiny dashboardthemes dplyr OpenImageR knitr zoo shinythemes readxl DT shinyjs
#' 
#' @examples
#'\dontrun{
#' InteGreat.run()
#' }
#' @export

InteGreat.run <-function()
{
  x = T
  runApp(
    appDir = system.file("Shiny", package = "InteGreat")
  )
}
