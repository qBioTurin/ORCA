library(shinythemes)
library(shinydashboard)

ui <- fluidPage(
  #theme = shinytheme("paper"),
  headerPanel("Gel Analysis"),
  plotOutput("DataPlot"),
  hr(),
  fluidRow(
      column(3,
             h4("Loading Data from ImJ "),
             fileInput("txtImport","",
                       placeholder = "Select a txt file",
                       width = "100%"),
             fluidRow(column(width = 10,offset = 1,verbatimTextOutput("LoadingError"))),
             column(1,offset = 1,
                    actionButton( label = "Load", inputId = "LoadingTxt", icon = icon("file-upload") )
             ),
             column(1,offset = 2,
                  downloadButton("downloadData", "Download") 
             ),
             selectInput(inputId = "LaneChoice",
                         label = "Choose a Lane:",
                         choices = c(""))
             
      ),
      column(5,
             tabsetPanel(id = "tabs",
                         tabPanel("Vertical cut", value= "V",textOutput("V"),
                                  sliderInput(inputId = "truncX", label = h4("Truncation:"),
                                              min = 0, max = 0, value = c(0,0),step = 1),
                                  actionButton( label = "Truncate", inputId = "TruncateDataV",
                                                icon = icon("cut") )),
                         tabPanel("Horizontal cut", value= "H",textOutput("H"),
                                  sliderInput(inputId = "truncH1", label = h4("Horizontal truncation:"),
                                              min = 0, max = 0, value = 0,step = 1),
                                  actionButton( label = "Truncate", inputId = "TruncateDataH",
                                                icon = icon("cut") )),
                         tabPanel("Derivatives cut", value= "D",textOutput("D"))
             )),
      column(3,
             tableOutput('AUC')
      )
  )
)
  
  
#   fluidPage(
#   
#   # Application title
#   titlePanel("Gel Analysis Cutting"),
#   # Show a plot of the generated distribution
#   mainPanel( 
#     plotOutput("DataPlot"),
#     tableOutput('AUC'),
#     sliderInput(inputId = "truncX", label = h4("Truncation:"),
#                 min = 0, max = 0, value = c(0,0),step = 1),
#     actionButton( label = "Truncate", inputId = "TruncateData",
#                   icon = icon("cut") )
#   )
# )
# 
