library(shinydashboard)
library(shiny)
library(zoo)
library(knitr)
library(ggplot2) 
library(shinythemes) 
library(OpenImageR)
library(dplyr)
library("shinyWidgets")


ui <- dashboardPage(
  #theme = shinytheme("paper"),
  dashboardHeader(title = "Nome Pacchetto",
                  tags$li(a(onclick = "onclick =window.open('https://github.com/xxx/xxx')",
                            href = NULL,
                            icon("github"),
                            title = "GitHub",
                            style = "cursor: pointer;"),
                          class = "dropdown"),
                  ## DOWNLOAD
                  tags$li(class = "dropdown",
                          tags$style("#mydropdown{cursor: pointer; background-color: #852fb4 ;
                              color: white;
                              border: 1px solid #852fb4; height: 50px;left:0;}"),
                          dropdownButton(inputId = "mydropdown",label = "",
                                         right = T,
                                         circle = FALSE,
                                         icon = icon("download"),
                                         tags$li(shinyjs::useShinyjs(),
                                                 downloadButton(outputId = 'downloadReport',
                                                                href = "report.html",
                                                                download = "report.html",
                                                                label = "Report",title = "Report generation",
                                                                style = "cursor: pointer; width: 98%;
                                      text-align: center; vertical-align: middle;
                                      border: 1px solid #9809AF;",
                                                                class="dlButton")
                                         )
                          ),
                          block = TRUE)
  ),
  dashboardSidebar(   
    sidebarMenu(id = "SideTabs",
                menuItem('Home',
                         tabName = 'Home',
                         icon = icon('home')
                ),
                menuItem('WB data',
                         tabName = 'wb',
                         menuSubItem("Image uploading", tabName = "uploadIm"),
                         menuSubItem("Planes", tabName = "plane"),
                         menuSubItem("Lanes", tabName = "grey")
                ),
                menuItem('RT-PCR data',
                         tabName = 'pcr',
                         menuSubItem("Data uploading", tabName = "uploadPCR"),
                         menuSubItem("Tables", tabName = "tablesPCR"),
                         menuSubItem("Plot", tabName = "plotsPCR")
                )#,
                # menuItem('??? data',
                #          tabName = 'newdat'
                # )
    )),
  dashboardBody(
    tabItems(
      ## HOME
      tabItem(tabName = "Home",
              h2("Super Package for different data analysis!!!!")
      ),
      ## RT-PCR 
      # First tab content
      tabItem(tabName = "uploadPCR",
              h2("Data Loading"),
              fluidRow(
                column(10,
                       fileInput(inputId = "PCRImport",
                                 label = "",
                                 placeholder = "Select an Excel file",
                                 width = "80%"),
                       fluidRow(column(width = 10,offset = 1,
                                       verbatimTextOutput("LoadingError_PCR")
                       )
                       ),
                       fluidRow(
                         box(width = 12, title = "Data visualization:",
                             column(width = 6,
                                    checkboxGroupInput(inputId = "PCRnorm",
                                                       "Select the normalizers:")
                             ),
                             column(width = 6,
                                    selectInput(inputId = "PCRbaseline", label = "Select the baseline:",
                                                choices = "ID" )
                             ) 
                         )
                       )
                       
                ),
                column(1,
                       actionButton( label = "Load",
                                     inputId = "LoadPCR_Button" )
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "tablesPCR",
              h2("Tables"),
              fluidRow(
                box(width= 12,title = "Summary",collapsible = TRUE,collapsed = TRUE,
                    uiOutput("PCRtables")
                )
              ),
              fluidRow(
                box(width= 12,title = "Comparison",collapsible = TRUE,collapsed = TRUE,
                    uiOutput("PCRtablesComp")
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "plotsPCR",
              h2("Plots"),
              fluidRow(
                box(width= 12,title = "",
                    plotOutput("PCRplot",width = "100%")
                )
              )
      ),
      ## WB 
      # First tab content
      tabItem(tabName = "uploadIm",
              h2("Loading Image"),
              fluidRow(
                column(10,
                       fileInput(inputId = "imImport",
                                 label = "",
                                 placeholder = "Select a tif file",
                                 width = "80%"),
                       fluidRow(column(width = 10,offset = 1,
                                       verbatimTextOutput("LoadingError")))
                       
                ),
                column(1,
                       actionButton( label = "Load",
                                     inputId = "LoadingTif" )
                )
                # column(1,offset = 1,
                #        )
                # column(1,offset = 2,
                #        downloadButton("downloadData", "Download") 
                # )
              ),
              plotOutput("TifPlot")
      ),
      # Second tab content
      tabItem(tabName = "plane",
              h2("Planes definition"),
              # tags$head(
              #   tags$style(css),
              #   tags$script(HTML(js))
              # ),
              fluidRow(
                box( width = 12,
                     plotOutput("TifPlot2",width="100%",
                                hover = "plot_hover",
                                brush = "plot_brush")
                )
              ),
              fluidRow(
                box( width = 6,
                     title = tagList(shiny::icon("gear"), "Select planes"),
                     tableOutput("PlanesStructureTable")
                     #uiOutput('panelset')
                ),
                box( width = 6,
                     actionButton(inputId = "panelSelect_button", label = "Select panel"),
                     actionButton(inputId = "ResetPan", label = 'Reset panel'),
                     actionButton(inputId = "GenLines", label = 'Generate lines'),
                     verbatimTextOutput("rectCoordOutput")
                )
              )
      ),
      #   
      #   # Third tab content
      tabItem(tabName = "grey",
              h2("Lanes generation"),
              box(width = 12,
                  plotOutput("DataPlot")
              ),
              fluidRow(
                selectInput(inputId = "LaneChoice",
                            label = "Choose a Lane:",
                            choices = c(""))
              ),
              fluidRow(
                box(width = 6,
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
                                                       icon = icon("cut") ))
                                #tabPanel("Derivatives cut", value= "D",textOutput("D"))
                    )
                ),
                box(width=6,
                    tableOutput('AUC')
                )
              )
      )
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
