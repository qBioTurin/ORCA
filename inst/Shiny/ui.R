library(shinydashboard)
library(shiny)
library(zoo)
library(knitr)
library(ggplot2) 
library(shinythemes) 
library(OpenImageR)
library(dplyr)
library("shinyWidgets")
library(DT)


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
                                         ),
                                         tags$li(shinyjs::useShinyjs(),
                                                 downloadButton(outputId = 'downloadRDSwholeAnalysis',
                                                                label = "RDs",title = "RDS storing the whole analysis",
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
                menuItem("Data Analysis",
                         tabName = 'DataAnaslysis',
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
                         ),
                         menuItem('ELISA analysis',
                                  tabName = 'elisa',
                                  menuSubItem("Upload data", tabName = "uploadELISA"),
                                  menuSubItem("Quantification", tabName = "tablesELISA")
                         )
                ),
                menuItem('Model integration',
                         tabName = 'integ',
                         menuSubItem("Proteomic Data", tabName = "Proteomic_tab"),
                         menuSubItem("WB,RT-qPCR,ELISA ", tabName = "WbPcrElisa_tab"),
                         menuSubItem("IF,.. ", tabName = "otherData_tab")
                )
                
    )),
  dashboardBody(
    tabItems(
      ## HOME
      tabItem(
        tabName = "Home",
        h2("Super Package for different data analysis!!!!")
      ),
      ###### BEGIN MODEL INTEGRATION ####
      ## BEGIN model integration: proteomic ####
      tabItem(tabName = "Proteomic_tab",
              h2("Proteomic data"),
              fluidRow(
                box(width = 12,
                    title = "Proteomic from literature",
                    collapsible = T,
                    numericInput(inputId = "InputDefault_rescaling",
                                 label = "Rescaling factor",
                                 value = "1",
                                 min = 0,
                                 step = 100,
                                 width = "10%"),
                    DTOutput("ProteomicDefault_table",width = "90%")
                ),
                box(width = 12,
                    title = "User Proteomic Data",
                    collapsible = T,
                    collapsed = T,
                    fluidRow(
                      column(10,
                             fileInput(
                               inputId = "ProtImport",
                               label = "",
                               placeholder = "Select an excel file",
                               width = "80%", 
                               multiple = TRUE
                             )
                      ),
                      column(1,
                             actionButton(
                               label = "Load",
                               inputId = "LoadProt_Button" 
                             )
                      ),
                      column(1,
                             actionButton(
                               label = "Reset",
                               inputId = "ResetProt_Button" 
                             )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 10,
                        offset = 1,
                        verbatimTextOutput("LoadingError_Prot")
                      ),
                      fluidRow(
                        column(
                          width = 3,
                          offset = 1,
                          numericInput(
                            inputId = "InputUser_rescaling",
                            label = "Rescaling factor",
                            value = "1",
                            min = 0,
                            step = 100,
                            width = "100%"
                          )
                        ),
                        column(
                          width = 3,
                          selectInput(
                            inputId = "RescalingColms_User",
                            label = "Select columns to rescale:",
                            choices = "" ,
                            multiple = T
                          )
                        ) 
                      ),
                      DTOutput("ProteomicUser_table",width = "80%")
                    )
                ),
              )
      ),
      ## END model integration: proteomic
      ## BEGIN model integration: WB PCR ELISA ####
      tabItem(
        tabName = "WbPcrElisa_tab",
        h2("Data harmization as initial marking into the Petri Net model "),
        fluidRow(
          box(
            width = 12,
            fluidRow(
              column(
                10,
                fileInput(
                  inputId = "IntGImport",
                  label = "",
                  placeholder = "Select the RDs files storing InteGreat analysises",
                  width = "80%", 
                  multiple = TRUE)
              ),
              column(
                1,
                actionButton( label = "Load",
                              inputId = "LoadIntG_Button" )
              )
            ),
            fluidRow(
              column(
                width = 10,
                offset = 1,
                verbatimTextOutput("LoadingError_IntG")
              )
            )
          ),
          box(
            width = 12,
            collapsible = T,
            collapsed = T,
            title = "WB analysis",
            selectizeInput(
              width = "50%",
              inputId = "Selectprot_wb",
              multiple = F,
              options = list(maxItems = 1),
              label = "Select the value from proteomic:",
              choices = ""
            ),
            DTOutput("Tab_IntG_wb")
          ),
          box(
            width = 12,
            collapsible = T,
            collapsed = T,
            title = "RT-qPCR analysis",
            selectizeInput(
              width = "50%",
              inputId = "SelectGene",
              multiple = F,
              options = list(maxItems = 1),
              label = "Select the gene of interest:",
              choices = ""
            ),
            uiOutput("tables_IntG_pcr")
          ),
          box(
            width = 12,
            collapsible = T,
            collapsed = T,
            title = "ELISA analysis",
            tableOutput("Tab_IntG_elisa")
          )
        )
      ),
      ## END model integration: WB PCR ELISA
      ## BEGIN model integration: IF..etc  #######
      tabItem( tabName = "otherData_tab",
               h2("Other expertiments"),
               h4("It is possible to upload an excel file with at least (i) one character column \n
                  identifying the experiments ID, and (ii) one numeric's from which it is possible \n
                  calculate the average that will be rescaled with the proteomic value. "),
               fluidRow(
                 column(
                   10,
                   fileInput(
                     inputId = "OtherImport",
                     label = "",
                     placeholder = "Select an excel file",
                     width = "80%", 
                     multiple = TRUE
                   )
                 ),
                 column(1,
                        actionButton(
                          label = "Load",
                          inputId = "LoadOther_Button" 
                        )
                 )
               ),
               fluidRow(
                 column(
                   width = 10,
                   offset = 1,
                   verbatimTextOutput("LoadingError_Other")
                 ),  
                 column(
                   width = 10,
                   offset = 1,
                   selectizeInput(
                     width = "50%",
                     inputId = "Selectprot_other",
                     multiple = F,
                     options = list(maxItems = 1),
                     label = "Select the value from proteomic:",
                     choices = ""
                   )
                 ),
                 box(
                   width = 12,
                   title = "Data",
                   collapsible = T,
                   DTOutput("Other_table",width = "80%")
                 ),
                 fluidRow(
                   DTOutput("Other_tableMean",width = "80%")
                 )
               )
      ),
      ## END model integration:  
      ######### END MODEL INTEGRATION
      
      ###### BEGIN DATA ANALYSIS ####
      ## BEGIN data integration: RT-PCR  #######
      # First tab content
      tabItem(tabName = "uploadPCR",
              h2("Load RT-qPCR raw data"),
              fluidRow(
                column(10,
                       fileInput(inputId = "PCRImport",
                                 label = "",
                                 placeholder = "Select an Excel file",
                                 width = "80%"),
                       fluidRow(
                         column(
                           width = 10,offset = 1,
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
              actionButton(
                label = "Pre-Save the analysis",
                inputId = "savePCRButton",
                align = "right" 
              ),
              fluidRow(
                box(width= 12,title = "Single gene Quantification",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    uiOutput("PCRtables")
                )
              ),
              fluidRow(
                box(width= 12,title = "“Normalization on Housekeeping Genes",
                    collapsible = TRUE,
                    collapsed = TRUE,
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
      ## END data analysis:  RT-PCR 
      
      ## BEGIN data analysis: ELISA  #######
      tabItem(
        tabName = "uploadELISA",
        h2("Load ELISA data"),
        fluidRow(
          box(
            width = 12,
            fluidRow(
              column(
                10,
                fileInput(
                  inputId = "ELISAImport",
                  label = "",
                  placeholder = "Select an Excel file.",
                  width = "80%", 
                  multiple = TRUE
                )
              ),
              actionButton(
                label = "Load",
                inputId = "LoadELISA_Button"
              )
            ),
            fluidRow(
              column(
                width = 10,offset = 1,
                verbatimTextOutput("LoadingError_ELISA")
              )
            )
          )
        ),
        fluidRow(
          box(width = 12, title = "ELISA matrix:",
              column(width = 6,
                     DTOutput("ELISAmatrix")
              ),
              column(width = 6,
                     selectizeInput("ELISAcell_EXP",
                                    label = "Experiments:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     textInput("ELISAcell_TIME",label = "Time:",value = "")
              )
          ),
          plotOutput("ELISAinitplots")
        )
      ),
      # Second tab content
      tabItem(tabName = "tablesELISA",
              h2("Tables"),
              fluidRow(
                actionButton(
                  label = "Pre-Save the analysis",
                  inputId = "saveElisaButton",
                  align = "right"
                ),
                tags$head(tags$script(src = "message-handler.js")),
                box(width= 12,
                    title = "Select a baseline for the following experiment replicants",
                    collapsible = TRUE,
                    collapsed = FALSE,
                    uiOutput("ElisaBaselineSelection"),
                    actionButton(
                      label = "Average calculation",
                      inputId = "MeanCalcELISA_Button",
                      align = "center" 
                    ),
                ),
                box(width= 12,
                    title = "Quantification",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    DTOutput("ELISAtables"),
                    plotOutput("ELISAplots")
                )
              )
      ),
      ## END data analysis: ELISA
      ## BEGIN data analysis: WB  #######
      # First tab content
      tabItem(tabName = "uploadIm",
              h2("Loading Image"),
              fluidRow(
                column(9,
                       fileInput(
                         inputId = "imImport",
                         label = "",
                         placeholder = "Select a tif file",
                         width = "90%"
                       )
                ),
                column(2,
                       actionButton( 
                         label = "Load",
                         width = "100%",
                         inputId = "LoadingTif"
                       )
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
      ## Third tab content
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
                actionButton(
                  label = "Pre-Save the analysis",
                  inputId = "saveWBButton",
                  align = "right" 
                ),
                box( width = 6,
                     title = tagList(shiny::icon("gear"), "Set the WB analysis as normalizer"),
                     column(9,
                            fileInput(
                              inputId = "NormWBImport",
                              label = "",
                              placeholder = "Select an WB RData file",
                              width = "90%"
                            )
                     ),
                     column(2,
                            actionButton(
                              label = "Load",
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
                         DTOutput('AUC_WBnorm')
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
                         DTOutput('AUC_WB')
                       )
                     )
                )
              ),
              box( width = 12,
                   title = tagList(shiny::icon("gear"), "WB quantification"),
                   fluidRow(
                     column(
                       width = 10,
                       offset = 1,
                       DTOutput('AUC_rapp')
                     )
                   )
              )
      )
    )
    ## END data analysis: WB
    ###### END DATA ANALYSIS ####
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
