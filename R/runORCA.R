#' @title Run ORCA
#' @description function to lunch the ORCA shiny application.
#'
#' @param 
#'
#' @author Beccuti Marco, Pernice Simone, Tortarolo Dora
#' @import shinydashboard shinyjs jsonlite shinyalert shinybusy zoo knitr ggplot2 shinythemes OpenImageR dplyr openxlsx patchwork stringr randomcoloR plotly flowCore xml2 visNetwork shinyFiles
#' @rawNamespace import(DT, except=c(dataTableOutput,renderDataTable))
#' @rawNamespace import(shiny,except=runExample)
#' @rawNamespace import(shinyWidgets,except=alert)
#' 
#' @examples
#'\dontrun{
#' ORCA.run()
#' }
#' @export

ORCA.run <-function(port = 3838, inDocker = FALSE)
{
  x = T
  
  Appui <- system.file("Shiny","ui.R", package = "ORCA")
  Appserver <- system.file("Shiny","server.R", package = "ORCA")
  
  source(Appui)
  source(Appserver)
  
  if(inDocker){
    app <-shinyApp(ui, server,
                   options =  options(shiny.maxRequestSize=1000*1024^2)
    )
    host = '0.0.0.0'
  }else{
    app <-shinyApp(ui, server,
                   options =  options(shiny.maxRequestSize=1000*1024^2,
                                      shiny.launch.browser = .rs.invokeShinyWindowExternal)
    )
    host = getOption("shiny.host", "127.0.0.1")
  }

  app$staticPaths <- list(
      `/` = httpuv::staticPath(system.file("Shiny","www", package = "ORCA"), indexhtml = FALSE, fallthrough = TRUE)
    )
  
  runApp(app, host = host, port = port)
  # runApp(
  #   appDir = system.file("Shiny", package = "ORCA"),
  #   launch.browser = T
  # )
}
