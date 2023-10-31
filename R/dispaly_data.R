#' @title Run InteGreat
#' @description function to lunch the InteGreat shiny application.
#'
#' @param 
#'
#' @author Pernice Simone, Tortarolo Dora
#' @import rjson openxlsx ggplot2 patchwork shinydashboard shinyWidgets shiny dashboardthemes dplyr OpenImageR knitr zoo shinythemes readxl DT shinyjs
#' 
#' @examples
#'\dontrun{
#' InteGreat.run()
#' }
#' @export

InteGreat.run <-function()
{
  x = T
  
  Appui <- system.file("Shiny","ui.R", package = "InteGreat")
  Appserver <- system.file("Shiny","server.R", package = "InteGreat")
  
  source(Appui)
  source(Appserver)
  
  shinyApp(ui, server,
           options =  options(shiny.maxRequestSize=1000*1024^2,
                              shiny.launch.browser = .rs.invokeShinyWindowExternal)
  )
  
  # runApp(
  #   appDir = system.file("Shiny", package = "InteGreat"),
  #   launch.browser = T
  # )
}
