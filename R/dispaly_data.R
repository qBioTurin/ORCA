#' @title Run display data
#' @description ....
#'
#' @param 
#'
#' @author Pernice Simone
#' @import shiny zoo knitr ggplot2 shinythemes OpenImageR dplyr
#' 
#' @examples
#'\dontrun{
#' displayGel()
#' }
#' @export
displayGel <-function()
{
  x = T
  runApp(
    appDir = system.file("Shiny", package = "GelAnalyser")
  )
}
