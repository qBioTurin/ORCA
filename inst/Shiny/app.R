
#Appui <- system.file("Shiny","ui.R", package = "GelAnalyser")
#Appserver <- system.file("Shiny","server.R", package = "GelAnalyser")

shinyApp(ui, server,
         options =  options(shiny.maxRequestSize=1000*1024^2) )
