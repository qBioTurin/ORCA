
#Appui <- system.file("Shiny","ui.R", package = "InteGreat")
#Appserver <- system.file("Shiny","server.R", package = "InteGreat")

shinyApp(ui, server,
         options =  options(shiny.maxRequestSize=1000*1024^2) )
