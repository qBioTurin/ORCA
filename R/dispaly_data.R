#' @title Run ORCA
#' @description function to lunch the ORCA shiny application.
#'
#' @param 
#'
#' @author Beccuti Marco, Pernice Simone, Tortarolo Dora
#' @import shinydashboard shinyjs jsonlite shinyalert shinybusy zoo knitr ggplot2 shinythemes OpenImageR dplyr openxlsx patchwork stringr
#' @rawNamespace import(DT, except=c(dataTableOutput,renderDataTable))
#' @rawNamespace import(shiny,except=runExample)
#' @rawNamespace import(shinyWidgets,except=alert)
#' 
#' @examples
#'\dontrun{
#' InteGreat.run()
#' }
#' @export

ORCA.run <-function()
{
  x = T
  
  Appui <- system.file("Shiny","ui.R", package = "ORCA")
  Appserver <- system.file("Shiny","server.R", package = "ORCA")
  
  source(Appui)
  source(Appserver)
  
  shinyApp(ui, server,
           options =  options(shiny.maxRequestSize=1000*1024^2,
                              shiny.launch.browser = .rs.invokeShinyWindowExternal)
  )
  
  # runApp(
  #   appDir = system.file("Shiny", package = "ORCA"),
  #   launch.browser = T
  # )
}
