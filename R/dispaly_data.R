#' @title Run OCA
#' @description function to lunch the OCA shiny application.
#'
#' @param 
#'
#' @author Pernice Simone, Tortarolo Dora
#' @import rjson openxlsx ggplot2 patchwork shinydashboard shinyWidgets shiny dashboardthemes dplyr OpenImageR knitr zoo shinythemes readxl DT shinyjs
#' 
#' @examples
#'\dontrun{
#' OCA.run()
#' }
#' @export

OCA.run <-function()
{
  x = T
  
  Appui <- system.file("Shiny","ui.R", package = "OCA")
  Appserver <- system.file("Shiny","server.R", package = "OCA")
  
  source(Appui)
  source(Appserver)
  
  shinyApp(ui, server,
           options =  options(shiny.maxRequestSize=1000*1024^2,
                              shiny.launch.browser = .rs.invokeShinyWindowExternal)
  )
  
  # runApp(
  #   appDir = system.file("Shiny", package = "OCA"),
  #   launch.browser = T
  # )
}
