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
  dashboardHeader(title = "InteGreat",
                  tags$li(a(onclick = "onclick =window.open('https://github.com/qBioTurin/InteGreat')",
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
                menuItem('Western Blot analysis',
                         tabName = 'wb',
                         menuSubItem("Upload Image", tabName = "uploadIm"),
                         menuSubItem("Protein Bands", tabName = "plane"),
                         menuSubItem("Lanes", tabName = "grey"),
                         menuSubItem("Quantification", tabName = "quantification")
                ),
                menuItem('RT-qPCR analysis',
                         tabName = 'pcr',
                         menuSubItem("Upload data", tabName = "uploadPCR"),
                         menuSubItem("Quantification", tabName = "tablesPCR"),
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
              h2("Load RT-qPCR raw data"),
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
                         box(width = 12, title = "Experimental Setup:",
                             column(width = 6,
                                    selectizeInput(
                                      inputId = "selectPCRcolumns",
                                      label = "Select the columns to assign \n
                                      the Gene, Sample, Value names (in order):",
                                      choices = c(""),
                                      selected = "",
                                      multiple = TRUE,
                                      options = list(maxItems = 3),
                                      width = "99%"
                                    ),
                                    tableOutput("PCRpreview")
                             ),
                             column(width = 3,
                                    checkboxGroupInput(inputId = "PCRnorm",
                                                       "Select housekeeping genes:")
                             ),
                             column(width = 3,
                                    selectInput(inputId = "PCRbaseline",
                                                label = "Select control sample:",
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
                box(width= 12,title = "Single gene Quantification",collapsible = TRUE,collapsed = TRUE,
                    uiOutput("PCRtables")
                )
              ),
              fluidRow(
                box(width= 12,title = "“Normalization on Housekeeping Genes",collapsible = TRUE,collapsed = TRUE,
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
                column(9,
                       fileInput(inputId = "imImport",
                                 label = "",
                                 placeholder = "Select a tif file",
                                 width = "90%")
                ),
                column(2,
                       actionButton( label = "Load",
                                     width = "100%",
                                     inputId = "LoadingTif" )
                       
                ),
                tags$style(type='text/css', "#LoadingTif { width:100%; margin-top: 20px;}")
              ),
              fluidRow(
                column(
                  width = 10,
                  offset = 1,
                  verbatimTextOutput("LoadingError")
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "plane",
              h2("Select protein bands"),
              fluidRow(
                box( width = 12,
                     plotOutput("TifPlot2",width="100%",
                                hover = "plot_hover",
                                brush = "plot_brush")
                )
              ),
              fluidRow(
                box( width = 6,
                     title = tagList(shiny::icon("gear"), "Protein bands"),
                     tableOutput("PlanesStructureTable")
                ),
                box( width = 6,
                     actionButton(inputId = "panelSelect_button", label = "Select protein band"),
                     actionButton(inputId = "ResetPan", label = 'Reset protein bands'),
                     actionButton(inputId = "GenLanes", label = 'Generate lanes'),
                     verbatimTextOutput("rectCoordOutput")
                )
              )
      ),
      #   # Third tab content
      tabItem(tabName = "grey",
              h2("Signal Profiles"),
              fluidRow(
                column(width = 6, offset = 0.5,
                       selectInput(inputId = "LaneChoice",
                                   label = "Choose a lane:",
                                   choices = c("")
                       ) 
                )
                # column(width = 5,
                #        textInput(inputId = "text_LaneName",
                #                  label = "Assign a name to the lane",
                #                  value = ""
                #                  )
                # )
              ),
              box(width = 12,
                  plotOutput("DataPlot")
              ),
              fluidRow(
                box(width = 6,
                    tabsetPanel(id = "tabs",
                                tabPanel("Vertical cut", value= "V",textOutput("V"),
                                         sliderInput(inputId = "truncV", label = h4("Truncation:"),
                                                     min = 0, max = 0, value = c(0,0),step = 1
                                         ),
                                         actionButton( 
                                           label = "Cut", inputId = "actionButton_TruncV",
                                           icon = icon("cut")
                                         )
                                ),
                                tabPanel("Horizontal cut", value= "H",textOutput("H"),
                                         sliderInput(inputId = "truncH", label = h4("Horizontal truncation:"),
                                                     min = 0, max = 0, value = 0,step = 1),
                                         actionButton( label = "Cut", inputId = "actionButton_TruncH",
                                                       icon = icon("cut") 
                                         )
                                )
                    )
                ),
                box(width=6,
                    tableOutput('AUC'),
                    actionButton( label = "Reset all", 
                                  inputId = "actionButton_ResetPlanes"),
                    downloadButton( label = "Save the analysis", 
                                    outputId = "downloadButton_saveRes",
                                    #href = "Results.RData",
                                    #download = "Results.RData",
                                    icon = icon("save") )
                )
              )
      ),
      # fourth tab content
      tabItem(tabName = "quantification",
              h2("WB quantification"),
              fluidRow(
                box( width = 6,
                     title = tagList(shiny::icon("gear"), "Set the WB analysis as normalizer"),
                     column(9,
                            fileInput(inputId = "NormWBImport",
                                      label = "",
                                      placeholder = "Select an WB RData file",
                                      width = "90%"
                            )
                     ),
                     column(2,
                            actionButton( label = "Load",
                                          width = "100%",
                                          inputId = "actionB_loadingNormWB"
                            )
                            
                     ),
                     tags$style(type='text/css',
                                "#actionB_loadingNormWB { width:100%; margin-top: 20px;}"
                     ),
                     fluidRow(
                       column(
                         width = 10,
                         offset = 1,
                         verbatimTextOutput("LoadingErrorNormWB")
                       )
                     ),
                     fluidRow(
                       column(
                         width = 10,
                         offset = 1,
                         tableOutput('AUC_WBnorm')
                       )
                     )
                ),
                box( width = 6,
                     title = tagList(shiny::icon("gear"), "Set the WB analysis to normalize"),
                     column(9,
                            fileInput(inputId = "WBImport",
                                      label = "",
                                      placeholder = "Select an WB RData file",
                                      width = "90%"
                            )
                     ),
                     column(2,
                            actionButton( label = "Load",
                                          width = "100%",
                                          inputId = "actionB_loadingWB"
                            )
                            
                     ),
                     tags$style(type='text/css',
                                "#actionB_loadingWB { width:100%; margin-top: 20px;}"
                     ),
                     fluidRow(
                       column(
                         width = 10,
                         offset = 1,
                         verbatimTextOutput("LoadingErrorWB")
                       )
                     ),
                     fluidRow(
                       column(
                         width = 10,
                         offset = 1,
                         tableOutput('AUC_WB')
                       )
                     )
                )
              ),
              box( width = 12,
                   title = tagList(shiny::icon("gear"), "WB quantification")
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
