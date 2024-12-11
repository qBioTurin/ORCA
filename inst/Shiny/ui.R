library(colourpicker)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(jsonlite)
library(shinyalert)
library(shinybusy)
library(zoo)
library(knitr)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(tibble)
library(igraph)
library(ggraph)
library(tidyverse)
library(tidyr)
library(shinythemes)
library(OpenImageR)
library(dplyr)
library(shinyWidgets)
library(DT)
library(openxlsx)
library(patchwork)
library(stringr)
library(dplyr)
library(flowCore)
library(xml2)
library(visNetwork)
library(shinyFiles)  # Load shinyFiles package
library(colourpicker)

ui <- dashboardPage(
  dashboardHeader(title = "ORCA",
                  tags$li(
                    class = "dropdown d-flex align-items-center",
                    tags$head(tags$link(rel = "shortcut icon", href = "ORCAlogo.png")),
                    tags$style(".main-header {max-height: 60px;}
                 .icon-container {
        position: relative;
        display: inline-block;
      }
      .icon-container .icon-text {
        visibility: hidden;
        width:400px;
        background-color: #333;
        color: #fff;
        text-align: center;
        border-radius: 6px;
        padding: 5px;
        position: absolute;
        z-index: 2;
        top: 50%;
        left: 110%;
        transform: translateY(-50%);
        opacity: 0;
        transition: opacity 0.3s;
      }
      .icon-container:hover .icon-text {
        visibility: visible;
        opacity: 1;
      }
      h3 {
        padding-top: 0px; /* Adjust the top padding */
        padding-bottom: 0px; /* Adjust the bottom padding */
        margin-top: 0; /* Adjust the top margin */
        margin-bottom: 0; /* Adjust the bottom margin */
      }"
                    )
                  ),
                  tags$li(a(onclick = "window.open('https://github.com/qBioTurin/ORCA')",
                            href = NULL,
                            icon("github"),
                            title = "GitHub",
                            style = "cursor: pointer;"),
                          class = "dropdown"),
                  tags$li(class = "dropdown",
                          tags$style("#mydropdown {cursor: pointer; background-color: #852fb4; color: white; border: 1px solid #852fb4; height: 50px; left: 0;}"),
                          dropdownButton(inputId = "mydropdown", label = "", right = TRUE, circle = FALSE, icon = icon("download"),
                                         tags$li(shinyjs::useShinyjs(),
                                                 downloadButton(outputId = 'downloadReport', label = "Report", title = "Report generation", style = "cursor: pointer; width: 98%; text-align: center; vertical-align: middle; border: 1px solid #9809AF;", class = "dlButton")),
                                         tags$li(shinyjs::useShinyjs(),
                                                 downloadButton(outputId = 'downloadRDSwholeAnalysis', label = "RDs", title = "RDS storing the whole analysis", style = "cursor: pointer; width: 98%; text-align: center; vertical-align: middle; border: 1px solid #9809AF;", class = "dlButton"))
                          ),
                          block = TRUE)
  ),
  dashboardSidebar(
    sidebarMenu(id = "SideTabs",
                menuItem('Home', tabName = 'Home', icon = icon('home')),
                menuItem("Data Analysis", tabName = 'DataAnaslysis', icon = icon('chart-line'),
                         menuItem('Western Blot analysis', tabName = 'wb',
                                  menuItem('Protein quantification', tabName = 'bca',
                                           menuSubItem("Upload data", tabName = "uploadBCA"),
                                           menuSubItem("Quantification", tabName = "tablesBCA")
                                  ),
                                  menuItem('Image analysis', tabName = 'wbImage',
                                           menuSubItem("Upload Image", tabName = "uploadIm"),
                                           menuSubItem("Protein Bands", tabName = "plane"),
                                           menuSubItem("Profile Plots", tabName = "grey")
                                  ),
                                  menuSubItem("Quantification", tabName = "quantification")
                         ),
                         menuItem('RT-qPCR analysis', tabName = 'pcr',
                                  menuSubItem("Upload data", tabName = "uploadPCR"),
                                  menuSubItem("Quantification", tabName = "tablesPCR")),
                         menuItem('ELISA analysis', tabName = 'elisa',
                                  menuSubItem("Upload data", tabName = "uploadELISA"),
                                  menuSubItem("Quantification", tabName = "tablesELISA")),
                         menuItem('Endocytosis assay', tabName = 'endoc',
                                  menuSubItem("Upload data", tabName = "uploadENDOC"),
                                  menuSubItem("Quantification", tabName = "tablesENDOC")),
                         menuItem('Cytotoxicity assay', tabName = 'cytotox',
                                  menuSubItem("Upload data", tabName = "uploadCYTOTOX"),
                                  menuSubItem("Quantification", tabName = "tablesCYTOTOX")),
                         menuItem('Immunofluorescence analysis', tabName = 'if',
                                  menuSubItem("Upload data", tabName = "uploadIF"),
                                  menuSubItem("Quantification", tabName = "tablesIF")),
                         menuItem('Flow Cytometry analysis', tabName = 'facs',
                                  menuItem("Raw data", tabName = "RawFACS",
                                           menuSubItem("Uppload data", tabName = "uploadRawFACS"),
                                           menuSubItem("Raw data", tabName = "plotRawFACS")),
                                  menuSubItem("Upload data", tabName = "uploadFACS"),
                                  menuSubItem("Hierarchical gating", tabName = "tablesFACS"),
                                  menuSubItem("Statistics", tabName = "statFACS")) 
                ),
                menuItem('Statistical analysis', tabName = 'StatAnalysis_tab', icon = icon('magnifying-glass-chart')),
                menuItem('GreatORCA',
                         tabName = 'integ',
                         icon = icon('file'),
                         menuItem("Data Integration",
                                  tabName = 'data_integr',
                                  icon = icon('file-circle-plus'),
                                  menuSubItem("Omics Data Upload", tabName = "Omics_tab"),
                                  menuSubItem("Data Harmonization", tabName = "DataIntegration_tab")
                         ),
                         menuItem('Computational Modeling',
                                  tabName = 'greatmod',
                                  icon = icon('diagram-project'),
                                  menuSubItem("Model Parametrization", tabName = "greatspn_tab"),
                                  menuSubItem("Model Simulation", tabName = "epimod_tab")
                         )
                ),
                menuItem('Dataverse', tabName = 'Dataverse_tab', icon = icon('eye')),
                menuItem('Load analysis', tabName = 'LoadAnalysis', icon = icon('upload'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
              h1("ORCA: Omni Reproducible Cell Analysis"),
              h1(" "),
              fluidRow(
                column(width = 6,
                       p(img(src = "ORCAlogo.png", height = "30%", width = "30%"), align = "center")
                ),
                column(width = 6,
                       h2(em("A cellular biologist’s toolbox for data analysis.")),
                       h4("ORCA provides an exhaustive platform where scientists can analyze raw:"),
                       tags$ol(
                         tags$li(h4(strong("Western Blot"), " (WB),")),
                         tags$li(h4(strong("Reverse Transcription-quantitative PCR"), " (RT-qPCR),")),
                         tags$li(h4(strong("Enzyme-Linked ImmunoSorbent Assay"), " (ELISA),")),
                         tags$li(h4(strong("Endocytosis"), ",")),
                         tags$li(h4(strong("Cytotoxicity experiments"), ",")),
                         tags$li(h4(strong("Immunofluorescence"), ", and")),
                         tags$li(h4(strong("Flow Cytometry analysis"), "."))
                       )
                )
              ),
              p(img(src = "Logo_QBio.png", height = "15%", width = "15%", style = "margin:100px 0px"), align = "center")
      ),
      ###### BEGIN Computational Modeling ####
      ## BEGIN model integration: Omics ####
      tabItem(tabName = "Omics_tab",
              h2("Omics data"),
              fluidRow(
                box(width = 12,
                    title = "Omics data from literature",
                    collapsible = T,
                    numericInput(inputId = "InputDefault_rescaling",
                                 label = "Rescaling factor",
                                 value = "1",
                                 min = 0,
                                 step = 100,
                                 width = "10%"),
                    DTOutput("OmicsDefault_table",width = "90%")
                ),
                box(width = 12,
                    title = "User Omics Data",
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
                               icon = shiny::icon("upload"),
                               inputId = "LoadOmics_Button" 
                             )
                      ),
                      column(1,
                             actionButton(
                               label = "Reset",
                               inputId = "ResetOmics_Button" 
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
                      DTOutput("OmicsUser_table",width = "80%")
                    )
                ),
              )
      ),
      ## END model integration: Omics
      ## BEGIN model integration: WB PCR ENDOC ####
      tabItem(
        tabName = "DataIntegration_tab",
        h2("Data integration as initial marking into the Petri Net model"),
        fluidRow(
          box(
            width = 12,
            collapsible = T,
            title = "Selected rows in the Omics",
            column(
              10,
              fluidRow(
                DTOutput("SelectedOmicsDefault_table")
              ),
              fluidRow(
                DTOutput("SelectedOmicsUser_table")
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            fluidRow(
              column(
                10,
                fileInput(
                  inputId = "IntGImport",
                  label = "",
                  placeholder = "Select the RDs files storing ORCA analyses",
                  width = "80%", 
                  multiple = TRUE)
              ),
              column(
                1,
                actionButton( label = "Load",style = "margin-top: 20px;",
                              icon = shiny::icon("upload"),
                              inputId = "LoadIntG_Button" )
              )
            )
          ),
          box(
            width = 12,
            collapsible = T,
            collapsed = T,
            title = "Load Data Analysed",
            selectizeInput("IntegrAnalysis",
                           label = "Select the analysis:",
                           choices = ""),
            uiOutput("tables_IntG")
          )
          #   pickerInput(
          #     width = "50%",
          #     inputId = "Selectprot_wb",
          #     multiple = F,
          #     label = "Select the molecule from the omics dataset:",
          #     choices = "",
          #     options = list( maxItems = 1, `live-search` = TRUE),
          #     choicesOpt = list(`style` = "btn-info",
          #                       `liveSearchPlaceholder`="Search" )
          #   ),
          #   DTOutput("Tab_IntG_wb")
          # )
          # box(
          #   width = 12,
          #   collapsible = T,
          #   collapsed = T,
          #   title = "RT-qPCR analysis",
          #   selectizeInput(
          #     width = "50%",
          #     inputId = "SelectGene",
          #     multiple = F,
          #     options = list(maxItems = 1),
          #     label = "Select the gene of interest:",
          #     choices = ""
          #   ),
          #   uiOutput("tables_IntG_pcr")
          # ),
          # box(
          #   width = 12,
          #   collapsible = T,
          #   collapsed = T,
          #   title = "Endocytosis assay",
          #   tableOutput("Tab_IntG_endoc")
          # )
        )
      ),
      ## END model integration: WB PCR ENDOC
      ## BEGIN model integration: IF..etc  #######
      # tabItem( tabName = "otherData_tab",
      #          h2("Other expertiments"),
      #          h4("It is possible to upload an excel file with at least (i) one character column \n
      #             identifying the experiments ID, and (ii) one numeric's from which it is possible \n
      #             calculate the average that will be rescaled with the omics value. "),
      #          
      #          box(
      #            width = 12,
      #            fluidRow(
      #              column(
      #                8,
      #                fileInput(
      #                  inputId = "OtherImport",
      #                  label = "",
      #                  placeholder = "Select an excel file",
      #                  width = "100%", 
      #                  multiple = TRUE
      #                )
      #              ),
      #              column(
      #                1,
      #                actionButton(
      #                  label = "Load",style = "margin-top: 20px;",
      #                  icon = shiny::icon("upload"),
      #                  inputId = "LoadOther_Button" 
      #                )
      #              )
      #            ),
      #            fluidRow(
      #              column(
      #                width = 10,
      #                offset = 1,
      #                verbatimTextOutput("LoadingError_Other")
      #              )
      #            )
      #          ),
      #          fluidRow(  
      #            column(
      #              width = 10,
      #              offset = 1,
      #              selectizeInput(
      #                width = "50%",
      #                inputId = "Selectprot_other",
      #                multiple = F,
      #                options = list(maxItems = 1),
      #                label = "Select the molecule from the omics dataset:",
      #                choices = ""
      #              )
      #            ),
      #            box(
      #              width = 12,
      #              title = "Data",
      #              collapsible = T,
      #              DTOutput("Other_table",width = "80%")
      #            ),
      #            fluidRow(
      #              DTOutput("Other_tableMean",width = "80%")
      #            )
      #          )
      # ),
      ## END model integration:  
      ######### END MODEL INTEGRATION
      
      
      ## BEGIN model integration: GreatMOD ####
      ## BEGIN greatspn ####
      tabItem(tabName = "greatspn_tab",
              h2("Load analysis"),
              fluidRow(
                box(width = 12,
                    column(7,
                           fileInput("loadAnalysis_PNPRO", "Upload .PNPRO File",width = "100%",  accept = ".PNPRO")
                    ),
                    column(1,
                           actionButton(label = "Load",
                                        style = "margin-top: 20px;  width: 100%;", 
                                        icon = shiny::icon("upload"), 
                                        inputId = "loadPNPRO_Button")
                    ),
                    column(2,
                           downloadButton( label = "Download PNPRO", 
                                           style = "margin-top: 20px;  width: 100%;",
                                           outputId = "downloadPNPRO",
                                           icon = icon("download") )
                    ),
                    column(2,
                           actionButton("button_greatspn",style = "margin-top: 20px;  width: 100%;",label =  "Open GreatPSN")
                    )
                ),
                tags$style(type='text/css', "#loadAnalysis_PNPRO { width:100%; margin-top: 20px;}")
              ),
              fluidRow(
                column(6,
                       box( width = 12, title = "Places",collapsible = T,
                            DTOutput("placesTable")
                       )
                ),
                column(6,
                       box( width = 12, title = "Transitions",collapsible = T,
                            DTOutput("transitionsTable")
                       )
                )
              ),
              fluidRow(
                column(9,
                       visNetworkOutput("petrinetPlot",height = "800px")
                )
              )
      ),
      ## END greatspn ####
      ## BEGIN epimod ####
      tabItem(tabName = "epimod_tab",
              h4("Model Generation and Analysis"),
              fluidRow(
                box(width = 12,
                    shinyDirButton("directory", "Set Working Directory", "Select the directory"),
                    verbatimTextOutput("selectedDir")
                )
              ),
              fluidRow(
                box(width = 12,collapsible = T,title = "Model Generation",
                    # Inputs for model generation function
                    fluidRow(
                      column(4,
                           fileInput("transFileInput", "Upload Transitions File (.cpp)", accept = ".cpp")
                    ),
                    column(2,offset = 6,
                           actionButton("generateModel", "Generate the model")
                    )
                    ),
                    fluidRow(
                      column(10,offset = 1,verbatimTextOutput("EPIMODgenerationOutput"))
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Model Simulation", collapsible = T,
                    fluidRow(
                      column(2,numericInput("initialTime", "Initial Time (i_time)", value = 0)),
                      column(2,numericInput("finalTime", "Final Time (f_time)", value = 1)),
                      column(2,numericInput("stepTime", "Step Time (s_time)", value = 1)),
                      column(2,offset = 4,
                             # Button to run model analysis
                             actionButton("runModelAnalysis", "Run Model Analysis")
                      )
                    ),
                    fluidRow(
                      column(2, selectInput("solverType", "Solver Type", choices = c("LSODA", "SSA")) ),
                      # column(2, numericInput("nConfig", "Number of Configurations (n_config)", value = 1, min = 1)),
                      # conditionalPanel(condition = "input.nConfig  > 1||input.solverType != 'LSODA'",
                      #                  column(2, numericInput("parallelProcessors", "Parallel Processors", value = 1, min = 1 ) )
                      # ),
                      conditionalPanel(condition = "input.solverType != 'LSODA'",
                                       column(2, numericInput("nRun", "Number of Runs (n_run)", value = 1, min = 1)),
                                       column(2, numericInput("parallelProcessors", "Parallel Processors", value = 1, min = 1 ))
                      )
                    ),
                    fluidRow(
                      column(4,fileInput("parametersFileInput", "Parameters File (.csv)", accept = c(".csv"))),
                      column(4,fileInput("functionsFileInput", "Functions File (.R)", accept = c(".R")))
                    ),
                    fluidRow(
                      conditionalPanel(condition = "input.functionsFileInput != 'NULL'",
                                       column(4,selectizeInput("epimod_event_function", "Function to use for simulating discrete event",choices ="" )),
                                       column(4,textAreaInput("epimod_event_times", "Time points for simulating discrete events", 
                                                              placeholder = "Enter values separated by commas (e.g.: 1, 2, 3)"))
                      )
                    ),
                    fluidRow(
                      column(4,
                             textAreaInput("iniV", "Parametric Values (ini_v)", placeholder = "Enter values separated by commas (e.g.: 1, 2, 3)")
                      ),
                      column(4, fileInput("epimod_multiFileInput", "Files exploited in 'Parameters File' ", multiple = TRUE))
                    ),
                    fluidRow( 
                      column(10,offset = 1,verbatimTextOutput("analysisOutput") )
                    )
                )
              ),
      fluidRow(
        box(width = 12, title = "Visualisations", collapsible = T,
            fluidRow(
              column(10,offset = 1,selectInput("epimod_placesPlot","Places dynamics to show:", choices = "" , multiple = TRUE))
            ),
            fluidRow(
              plotOutput("epimod_tracePlot")
            )
        )
      )
      ),
      ## END epimod ####
      ## END model integration: GreatMOD ####
      
      ###### BEGIN DATA ANALYSIS ####
      
      ## BEGIN load analysis  #######
      tabItem(tabName = "LoadAnalysis",
              h2("Load analysis"),
              fluidRow(
                column(
                  9,
                  fileInput(inputId = "loadAnalysis_file", 
                            label = "", 
                            placeholder = "Select the RDs files storing ORCA analyses", 
                            width = "80%", 
                            multiple = TRUE)
                ),
                column(
                  2,
                  actionButton(label = "Load", 
                               style = "margin-top: 20px;  width: 100%;", 
                               icon = shiny::icon("upload"), 
                               inputId = "loadAnalysis_Button")
                )
              ),
              tags$style(type='text/css', "#loadAnalysis_Button { width:100%; margin-top: 20px;}")
      ),
      ## END load analysis  #######
      
      ## BEGIN data analysis: WB  #######
      tabItem(tabName = "uploadIm",
              h2("Upload Image"),
              fluidRow(
                column(9,
                       fileInput(
                         inputId = "imImport",
                         label = "Select a tif file",
                         placeholder = "Select a tif file",
                         width = "90%"
                       )
                ),
                column(2,
                       actionButton(label = "Load",style = "margin-top: 20px;",
                                    icon = shiny::icon("upload"),
                                    inputId = "LoadingTif"
                       )
                ),
                tags$style(type='text/css', "#LoadingTif { width:100%; margin-top: 20px;}")
              ),
              fluidRow(
                column(9, offset = 1,
                       tags$h5("In section «Upload Image», you have to upload the original file.tif and then you are directly redirected to «Protein Band» section.",
                               style = "margin-top: 20px; text: center") # Stile aggiunto per un po' di margine
                )
              )
      ),
      tabItem(tabName = "plane",
              h2("Select Protein Bands"),
              fluidRow(
                uiOutput("TiffBox")
              ),
              fluidRow(
                box( width = 6,
                     title = tagList(shiny::icon("gear", verify_fa = FALSE), 
                                     "Protein Band Selection Coordinates"),
                     h5(em("Click on the SampleName column to associate the lane with a sample name. Let us note that equal sample names are not allowed.")),
                     DTOutput("PlanesStructureTable")
                ),
                box( width = 6,
                     actionButton(inputId = "panelSelect_button", label = "Select Protein Bands"),
                     actionButton(inputId = "ResetPan", label = 'Reset Protein Bands'),
                     actionButton(inputId = "GenLanes", label = 'Generate Plots'),
                     verbatimTextOutput("rectCoordOutput")
                )
              )
      ),
      tabItem(tabName = "grey",
              h2("Signal Profiles"),
              fluidRow(
                column(width = 6, offset = 0.5,
                       selectInput(inputId = "LaneChoice",
                                   label = "Choose a lane:",
                                   choices = c("")
                       ) 
                )
              ),
              box(
                width = 12,
                plotOutput("DataPlot"),
                actionButton(
                  inputId = "CustomizePlotWB1",
                  label = "Customize or Download Plot",
                  icon = icon("paint-brush"),
                  class = "btn-primary"
                )
              ),
              fluidRow(
                box(width = 6,
                    tabsetPanel(id = "tabs",
                                tabPanel("Vertical cut", value= "V",textOutput("V"),
                                         sliderInput(inputId = "truncV", label = h4("Vertical cutting:"),
                                                     min = 0, max = 0, value = c(0,0),step = 1
                                         ),
                                         actionButton( 
                                           label = "Cut", inputId = "actionButton_TruncV",
                                           icon = icon("cut")
                                         )
                                ),
                                tabPanel("Horizontal cut", value= "H",textOutput("H"),
                                         sliderInput(inputId = "truncH", label = h4("Horizontal cutting:"),
                                                     min = 0, max = 0, value = 0),
                                         actionButton( label = "Cut", inputId = "actionButton_TruncH",
                                                       icon = icon("cut") 
                                         )
                                )
                    )
                ),
                box(width=6,
                    DTOutput('AUC'),
                    fluidRow(
                      column(width = 3,
                             actionButton(label = "Reset all", 
                                          inputId = "actionButton_ResetPlanes")
                      ),
                      column(width = 4,
                             downloadButton(label = "Download Analysis & Excel", 
                                            outputId = "downloadWBAnalysis",
                                            icon = icon("download"))
                      ),
                      column(width = 3,
                             actionButton(inputId = "NextWBQuantif",
                                          label = 'Proceed to Quantification',
                                          align = "right",
                                          icon = shiny::icon("forward"))
                      )
                    )
                )
              )
      ),
      tabItem(tabName = "quantification",
              h2("WB quantification"),
              fluidRow(
                box( width = 6,
                     title = tagList(shiny::icon("gear", verify_fa = FALSE), "Set the WB analysis as normalizer"),
                     h5("To keep the necessary samples (rows) just clicking on it."),
                     column(9,
                            fileInput(
                              inputId = "NormWBImport",
                              label = "",
                              placeholder = "Select an WB RDs file generated through the Profile Plots step",
                              width = "90%"
                            )
                     ),
                     column(3,
                            actionButton(
                              label = "Load",
                              icon = shiny::icon("upload"),
                              width = "100%",
                              inputId = "actionB_loadingNormWB"
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
                     title = tagList(shiny::icon("gear", verify_fa = FALSE),
                                     "Set the WB analysis to normalize"),
                     h5("To keep the necessary samples (rows) just clicking on it."),
                     column(9,
                            fileInput(inputId = "WBImport",
                                      label = "",
                                      placeholder = "Select an WB RDs file generated through the Profile Plots step",
                                      width = "90%"
                            )
                     ),
                     column(3,
                            actionButton( label = "Load",
                                          icon = shiny::icon("upload"),
                                          width = "100%",
                                          inputId = "actionB_loadingWB"
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
                   title = tagList(shiny::icon("gear", verify_fa = FALSE), "WB quantification"),
                   h3("Relative Density"),
                   fluidRow(
                     column(
                       width = 8,
                       selectInput("IdLaneNorm_RelDens",
                                   label = "Select the Lane to use for the relative normalization",
                                   choices = "Nothing selected",selected = "Nothing selected")
                     ),
                     column(
                       width = 12,
                       DTOutput('AUC_RelDens')
                     )
                   ),
                   h3("Adjusted Relative Density"),
                   fluidRow(
                     column(
                       width = 12,
                       DTOutput('AUC_AdjRelDens')
                     )
                   ),
                   plotOutput("plot_AdjRelDens"),
                   fluidRow(
                     column(
                       width = 2, offset = 7,  # Adjusting offset to align properly
                       actionButton(
                         inputId = "CustomizePlotWBQuantification", 
                         label = "Customize or Download Plot", 
                         icon = icon("paint-brush")
                       )
                     ),
                     column(
                       width = 2, 
                       downloadButton(
                         label = "Download Analysis & Excel", 
                         outputId = "downloadWBquantAnalysis",
                         #href = "Results.RData",
                         #download = "Results.RData",
                         icon = icon("download")
                       )
                     )
                   )
              )
      ),
      ## END data analysis: WB  #######
      
      #### BEGIN data analysis: RT-PCR ####
      tabItem(tabName = "uploadPCR",
              h2("Load RT-qPCR raw data"),
              fluidRow( 
                column(
                  9,
                  fileInput(
                    inputId = "PCRImport",
                    label = "",
                    placeholder = "Select an Excel file",
                    width = "80%", 
                    multiple = TRUE
                  )
                ),
                column(
                  2,
                  actionButton(
                    label = "Load",
                    style = "margin-top: 20px; width: 100%;",
                    icon = shiny::icon("upload"),
                    inputId = "LoadPCR_Button"
                  )
                ),
                tags$style(type='text/css', "#loadAnalysis_Button { width:100%; margin-top: 20px;}")
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Experimental Setup:",
                  column(
                    width = 6,
                    h2("Select the columns to assign"),
                    fluidRow(
                      column(
                        width = 3,
                        selectInput(
                          inputId = "PCR_gene",
                          label = "Gene names:",
                          choices = ""
                        )
                      ),
                      column(
                        width = 3,
                        selectInput(
                          inputId = "PCR_sample",
                          label = "Sample names:",
                          choices = ""
                        )
                      ),
                      column(
                        width = 3,
                        selectInput(
                          inputId = "PCR_value",
                          label = "Values:",
                          choices = ""
                        )
                      ),
                      column(
                        width = 3,
                        selectInput(
                          inputId = "PCR_time",
                          label = "Times:",
                          choices = ""
                        )
                      )
                    ),
                    tableOutput("PCRpreview")
                  ),
                  column(
                    width = 3,
                    checkboxGroupInput(
                      inputId = "PCRnorm",
                      "Select housekeeping genes:"
                    )
                  ),
                  column(
                    width = 3,
                    selectInput(
                      inputId = "PCRbaseline",
                      label = "Select baseline sample:",
                      choices = "ID"
                    )
                  ),
                  column(
                    width = 3,
                    offset = 9,
                    actionButton(
                      inputId = "NextQuantif",
                      label = 'Proceed to Quantification',
                      align = "right",
                      icon = shiny::icon("forward")
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "tablesPCR",
              h2("Normalization on Housekeeping Gene"),
              fluidRow(
                box(width= 12,
                    title = "All genes",collapsible = T,
                    fluidRow(
                      column(
                        width = 3,
                        radioButtons("PCR_cut_type",label = "Define the cut-off:",
                                     choices = c("Greater","Smaller","Both"), selected = "Greater")
                      ),
                      column(
                        width = 6,
                        conditionalPanel(condition = "input.PCR_cut_type != 'Both'",
                                         sliderInput(
                                           inputId = "CutFoldChange_slider",
                                           "Filter genes by :", min = 0, max=0, value = 0
                                         )
                        ),
                        conditionalPanel(condition = "input.PCR_cut_type == 'Both'",
                                         sliderInput(
                                           inputId = "BothCutFoldChange_slider",
                                           "Filter genes by :", min = 0, max=0, value = c(0,0)
                                         )
                        )
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             plotly::plotlyOutput("FoldchangeAllGenesPlot")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             uiOutput("AllGenesTable")
                      )
                    )
                )
              ),
              fluidRow(
                box(width= 12,
                    title = "Focus on specific genes",
                    column(
                      width = 3,
                      offset = 1,
                      selectizeInput(
                        inputId = "Gene_plot",
                        "Select gene:", 
                        choices = c(""),
                        selected = ""
                      )
                    ),
                    column(
                      width = 3,
                      selectizeInput(
                        inputId = "HousKgene_plot",
                        "Select housekeeping gene:", 
                        choices = c(""), selected = ""
                      )
                    ),
                    column(
                      width = 3,
                      radioButtons("PCR_plot_type", "Plot Type", choices = c("Point" = "point", "Bar" = "bar"), selected = "point")
                    ),
                    fluidRow(
                      column(width = 12,
                             plotOutput("SingleGenePlot")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             tableOutput("SingleGeneTable")
                      )
                    ),
                    fluidRow(
                      column(width = 3, offset = 7, 
                             actionButton(
                               inputId = "SavePCRplot",
                               label = 'Save the specific analysis',
                               align = "right",
                               icon = shiny::icon("save")
                             )
                      )
                    )
                )
              ),
              fluidRow(
                box(width= 12,
                    title = "Saved Normalization",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    fluidRow(
                      column(width = 12,
                             plotOutput("PointGenePlot")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             uiOutput("PCRplot")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             uiOutput("PCRtables")
                      )
                    ),
                    fluidRow(
                      column(width = 8, offset = 2, 
                             downloadButton(outputId = "downloadRTPCRAnalysis", 
                                            label = "Download Analysis & Excel", 
                                            icon = icon("download"),
                                            style = "float: right;"), 
                      )
                    )
                )
              ),
      ),
      #### END data analysis:  RT-PCR ####
      
      #### BEGIN data analysis: BCA ####
      tabItem(
        tabName = "uploadBCA",
        h2("Load data"),
        fluidRow(
          column(9,
                 fileInput(
                   inputId = "BCAImport",
                   label = "",
                   placeholder = "Select an Excel file.",
                   width = "80%", 
                   multiple = TRUE
                 )
          ),
          column(2,
                 actionButton(
                   label = "Load",
                   style = "margin-top: 20px; width: 100%;",
                   icon = shiny::icon("upload"),
                   inputId = "LoadBCA_Button"
                 )
          ),
          tags$style(type='text/css', "#loadAnalysis_Button { width:100%; margin-top: 20px;}")
        ),
        fluidRow(
          box(width = 12,
              title = "Assign experimental information to values:",
              column(width = 6,
                     dataTableOutput("BCAmatrix")
              ),
              column(width = 6,
                     selectizeInput("BCAcell_SN",
                                    label = "Sample name:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     selectizeInput("BCAcell_EXP",
                                    div(class = "icon-container",
                                        h4("Experimental condition or Analyte Concentrations:", icon("info-circle")),
                                        div(class = "icon-text", "Analyte Concentrations refers to the standard curve sample.")
                                    ),
                                    choices = "",
                                    options = list(create = TRUE)),
                     fluidRow(
                       column(4,
                              selectizeInput("BCA_standcurve",
                                             label = "Select standard curve:",
                                             choices = NULL)
                       ),
                       column(4,
                              radioButtons(
                                inputId = "BCA_blanks",
                                label = div(class = "icon-container",
                                            h4("Removing blank: ", icon("info-circle")),
                                            div(class = "icon-text", "Blank refers to the standard curve value with the smaller concetrantion.")
                                ),
                                choices = c("No" = "no", "Yes" = "yes"),
                                selected = "no"
                              )
                       )
                     ),
                     fluidRow(
                       column(12,
                              tags$div(
                                textOutput("BCASelectedValues"),
                                style = "font-size: 24px; text-align: center; color: green;
                                             width: 100%; margin-top: 20px;"
                              )
                       )
                     )
              )
          ),
          fluidRow(
            uiOutput("tablesBCA")
            # column(6, dataTableOutput("leftTableBCA")),
            # column(6, dataTableOutput("rightTableBCA"))
          ),
          fluidRow(
            column(width = 1,offset = 9,
                   actionButton(inputId = "NextBCAQuantif",
                                label = 'Proceed to Quantification',
                                align = "right",
                                icon = shiny::icon("forward"))
            )
          )
        )
      ),
      # Second tab content
      tabItem(tabName = "tablesBCA",
              h2("Quantification"),
              fluidRow(
                box(width = 12,
                    title = "Regression of the standard curve:",
                    collapsible = TRUE,
                    fluidRow(
                      column(3,
                             actionButton(inputId = "BCA_buttonRegression",
                                          label = 'Calculate the regression',
                                          align = "right")
                      )
                    ),
                    fluidRow(
                      column(6,
                             DTOutput("BCA_Table_stdcurve")
                      ),
                      column(6,
                             plotOutput("BCAregression")
                      )
                    )
                ),
                box(width= 12,
                    title = "Select desired protein quantity and sample volume",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    fluidRow(
                      column(width = 3,
                             textInput(inputId = "BCA_UGvalue_init", label = "Initial protein quantity (ug)", value = "5")
                      ),
                      column(width = 4,
                             textInput(inputId = "BCA_UGvalue" , label = "Write desidered protein quantity (ug)", value = "")
                      ),
                      column(width = 4,
                             actionButton(inputId = "confirmBCA_UGvalue", label= "Confirm")
                      )
                    ),
                    DTOutput("BCAtablesUG")
                ),
                box(width= 12,
                    title = "Quantification",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    #plotOutput("BCAplots"),
                    DTOutput("BCAtables")
                ),
                fluidRow(
                  column(width = 4, offset = 8,
                         downloadButton(label = "Download Analysis & Excel", 
                                        outputId = "downloadBCAAnalysis",
                                        icon = icon("download"))
                  )
                )
              )
      ),
      #### END data analysis: BCA ####
      
      #### BEGIN data analysis:  ELISA ####
      tabItem(
        tabName = "uploadELISA",
        h2("Load ELISA data"),
        fluidRow(
          column(9,
                 fileInput(
                   inputId = "ELISAImport",
                   label = "",
                   placeholder = "Select an Excel file.",
                   width = "80%", 
                   multiple = TRUE
                 )
          ),
          column(2,
                 actionButton(
                   label = "Load",
                   style = "margin-top: 20px; width: 100%;",
                   icon = shiny::icon("upload"),
                   inputId = "LoadELISA_Button"
                 )
          ),
          tags$style(type='text/css', "#loadAnalysis_Button { width:100%; margin-top: 20px;}")
        ),
        fluidRow(
          box(width = 12,
              title = "Assign experimental information to values:",
              column(width = 6,
                     dataTableOutput("ELISAmatrix")
              ),
              column(width = 6,
                     selectizeInput("ELISAcell_SN",
                                    label = "Sample name:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     selectizeInput("ELISAcell_EXP",label = "Experimental condition or standard curve Concentrations:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     fluidRow(
                       column(4,
                              selectizeInput(inputId = "ELISA_standcurve",
                                             label = "Select standard curve:",
                                             choices = NULL)
                       ),
                       # column(4,
                       #        checkboxGroupInput(inputId = "ELISA_baselines",
                       #                           "Select control:")
                       # ),
                       # column(4,
                       #        checkboxGroupInput(inputId = "ELISA_blanks",
                       #                           "Select blank:")
                       # )
                       column(4,
                              radioButtons(
                                inputId = "ELISA_blanks",
                                label = div(class = "icon-container",
                                            h4("Removing blank: ", icon("info-circle")),
                                            div(class = "icon-text", "Blank refers to the standard curve value with the smaller concetrantion.")
                                ),
                                choices = c("No" = "no", "Yes" = "yes"),
                                selected = "no"
                              )
                       )
                     ),
                     fluidRow(
                       column(12,
                              tags$div(
                                textOutput("ELISASelectedValues"),
                                style = "font-size: 24px; text-align: center; color: green;
                                             width: 100%; margin-top: 20px;"
                              )
                       )
                     )
              )
          ),
          fluidRow(
            column(6, dataTableOutput("leftTableELISA")),
            column(6, dataTableOutput("rightTableELISA"))
          ),
          fluidRow(
            column(width = 1,offset = 9,
                   actionButton(inputId = "NextElisaQuantif",
                                label = 'Proceed to Quantification',
                                align = "right",
                                icon = shiny::icon("forward"))
            )
          )
        )
      ),
      # Second tab content
      tabItem(tabName = "tablesELISA",
              h2("Quantification"),
              fluidRow(
                tags$head(tags$script(src = "message-handler.js")),
                box(width = 12,
                    title = "Regression of the standard curve:",
                    collapsible = TRUE,
                    fluidRow(column(4,
                                    selectizeInput("ELISAregressionType",
                                                   label="Select the regression model:",
                                                   choices = c("Linear","Hyperbola"))
                    ),
                    column(3,
                           actionButton(inputId = "ELISA_buttonRegression",
                                        label = 'Calculate the regression',
                                        align = "right")
                    )
                    ),
                    fluidRow(
                      column(6,
                             DTOutput("ELISA_Table_stdcurve")
                      ),
                      column(6,
                             plotOutput("ELISAregression")
                      )
                    )
                ),
                # box(width= 12,
                #     title = "Select a baseline for the following experimental conditions",
                #     collapsible = TRUE,
                #     collapsed = T,
                #     uiOutput("ElisaBaselineSelection")
                # ),
                box(width= 12,
                    title = "Quantification",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotOutput("ELISAplots"),
                    DTOutput("ELISAtables"),
                    fluidRow(
                      column(width = 1,offset = 9,
                             downloadButton( label = "Download the RDs", 
                                             outputId = "downloadButton_ELISA",
                                             #href = "Results.RData",
                                             #download = "Results.RData",
                                             icon = icon("download") )
                      ),
                      column(width = 1,offset = 7,
                             downloadButton( label = "Download xlsx", 
                                             outputId = "downloadButtonExcel_ELISA",
                                             #href = "Results.RData",
                                             #download = "Results.RData",
                                             icon = icon("download") )
                      )
                    )
                )
              )
      ),
      #### END data analysis: ELISA ####
      
      ## BEGIN data analysis:  ENDOC ####
      tabItem(
        tabName = "uploadENDOC",
        h2("Load Endocytosis data"),
        fluidRow(
          column(
            9,
            fileInput(
              inputId = "ENDOCImport",
              label = "",
              placeholder = "Select an Excel file.",
              width = "80%", 
              multiple = TRUE
            )
          ),
          column(2,
                 actionButton(
                   label = "Load",
                   style = "margin-top: 20px; width: 100%;",
                   icon = shiny::icon("upload"),
                   inputId = "LoadENDOC_Button"
                 )
          ),
          tags$style(type='text/css', "#loadAnalysis_Button { width:100%; margin-top: 20px;}")
        ),
        fluidRow(
          column(
            width = 10,offset = 1,
            verbatimTextOutput("LoadingError_ENDOC")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "Assign experimental information to values:",
              column(width = 6,
                     dataTableOutput("ENDOCmatrix")
              ),
              column(width = 6,
                     fluidRow(
                       column(width = 8, offset = 2,
                              selectizeInput("ENDOCcell_EXP",
                                             label = "Experimental condition:",
                                             choices = c(),  
                                             options = list(create = TRUE))
                       )
                     ),
                     fluidRow(
                       column(width = 8, offset = 2,
                              selectizeInput("ENDOCcell_TIME",
                                             label = "Time:",
                                             choices = c(),  
                                             options = list(create = TRUE))
                       )
                     ),
                     fluidRow(
                       column(5, offset = 2,
                              checkboxGroupInput(inputId = "ENDOC_baselines",
                                                 "Select baselines:")
                       ),
                       column(4, offset = 1,
                              checkboxGroupInput(inputId = "ENDOC_blanks",
                                                 "Select blank:")
                       )
                     ),
                     fluidRow(
                       column(12,
                              tags$div(
                                textOutput("ENDOCSelectedValues"),
                                style = "font-size: 24px; text-align: center; color: green;
                                             width: 100%; margin-top: 20px;"
                              )
                       )
                     )
              )
          ),
          fluidRow(
            column(6, dataTableOutput("leftTableEndoc")),
            column(6, dataTableOutput("rightTableEndoc"))
          ),
          fluidRow(
            column(width = 1,offset = 9,
                   actionButton(inputId = "NextEndocQuantif",
                                label = 'Proceed to Quantification',
                                align = "right",
                                icon = shiny::icon("forward"))
            )
          )
        ),
        plotOutput("ENDOCinitplots")
      ),
      # Second tab content
      tabItem(tabName = "tablesENDOC",
              h2("Quantification"),
              fluidRow(
                box(width= 12,
                    title = "Select a blank for the following experimental conditions",
                    collapsible = TRUE,
                    collapsed = FALSE,
                    h4("If time information is associated with the experimental conditions
                       defined as blank, then it will be lost during the averaging of its values."),
                    uiOutput("EndocBlankSelection")
                ),
                box(width= 12,
                    title = "Select a baseline for the following experimental conditions",
                    collapsible = TRUE,
                    collapsed = FALSE,
                    uiOutput("EndocBaselineSelection")
                ),
                box(width= 12,
                    title = "Quantification",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    DTOutput("ENDOCtables"),
                    plotOutput("ENDOCplots"),
                    fluidRow(
                      column(width = 4, offset = 8,
                             downloadButton(label = "Download Analysis & Excel", 
                                            outputId = "downloadENDOCAnalysis",
                                            icon = icon("download"))
                      )
                    )
                )
              )
      ),
      #### END data analysis: ENDOC ####
      
      ## BEGIN data analysis: CYTOTOX  #######
      tabItem(
        tabName = "uploadCYTOTOX",
        h2("Load Cytotoxicity data"),
        fluidRow(
          column(9,
                 fileInput(
                   inputId = "CYTOTOXImport",
                   label = "",
                   placeholder = "Select an Excel file.",
                   width = "80%", 
                   multiple = TRUE
                 )
          ),
          column(2,
                 actionButton(
                   label = "Load",
                   style = "margin-top: 20px; width: 100%;",
                   icon = shiny::icon("upload"),
                   inputId = "LoadCYTOTOX_Button"
                 )
          )
        ),
        fluidRow(
          column(
            width = 10,offset = 1,
            verbatimTextOutput("LoadingError_CYTOTOX")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "Assign experimental information to values:",
              column(width = 6,
                     dataTableOutput("CYTOTOXmatrix")
              ),
              column(width = 6,
                     selectizeInput("CYTOTOXcell_SN",
                                    label = "Sample name:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     selectizeInput("CYTOTOXcell_EXP",
                                    label = "Experimental condition:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     selectizeInput("CYTOTOXcell_REP",
                                    label = "Replicate number:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     fluidRow(
                       column(4,
                              selectizeInput(inputId = "CYTOTOX_baselines",
                                             "Select baseline cell:",
                                             choices = "")
                       )
                     ),
                     fluidRow(
                       column(12,
                              tags$div(
                                textOutput("CYTOTOXSelectedValues"),
                                style = "font-size: 24px; text-align: center; color: green;
                                             width: 100%; margin-top: 20px;"
                              )
                       )
                     )
              )
          ),
          fluidRow(
            column(12, dataTableOutput("leftTableCytotox")),
            #column(6, dataTableOutput("rightTableCytotox"))            
          ),
          fluidRow(
            column(width = 1,offset = 9,
                   actionButton(inputId = "NextCytotoxQuantif",
                                label = 'Proceed to Quantification',
                                align = "right",
                                icon = shiny::icon("forward"))
            )
          )
        )
      ),
      # Second tab content
      tabItem(tabName = "tablesCYTOTOX",
              h2("Quantification"),
              # fluidRow(
              #   box(width= 12,
              #       title = "Select a baseline for the following experimental conditions",
              #       collapsible = TRUE,
              #       collapsed = T,
              #       uiOutput("CytotoxBaselineSelection")
              #   )
              # ),
              fluidRow(
                box(width= 12,
                    title = "Quantification",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotOutput("CYTOTOXplots"),
                    DTOutput("CYTOTOXtables"),
                    fluidRow(
                      column(width = 2, offset = 9,
                             downloadButton(label = "Download Analysis & Excel", 
                                            outputId = "downloadCYTOTOXAnalysis",
                                            icon = icon("download"))
                      )
                    )
                )
              )
      ),
      ## END data analysis: CYTOTOX ####
      
      #### BEGIN data analysis: FACS ####
      
      tabItem(
        tabName = "uploadRawFACS",
        h2("Load Flow Cytometry data"),
        fluidRow(
          column(
            9,
            fileInput(
              inputId = "rawFACSImport",
              label = "",
              placeholder = "Select an fcs file.",
              width = "80%", 
              multiple = T,
              accept = ".fcs"
            )
          ),
          column(2,
                 actionButton(
                   label = "Load",
                   style = "margin-top: 20px; width: 100%;",
                   icon = shiny::icon("upload"),
                   inputId = "LoadRawFACS_Button"
                 )
          ),
          tags$style(type='text/css', "#loadAnalysis_Button { width:100%; margin-top: 20px;}")
        )
      ),
      tabItem(tabName = "plotRawFACS",
              h2("Raw FACS data analysis"),
              fluidRow(
                tags$head(
                  tags$style(HTML("
                      #rawFACSmatrix { 
                        float: center;
                      }
                    "))
                ),
                column(12, 
                       tableOutput("rawFACSmatrix")          
                )
              ),
              box(width = 12,
                  fluidRow(
                    column(
                      4,
                      selectInput("facs_xChannel", "X-axis Channel", choices = NULL)
                    ),
                    column(
                      4,
                      selectInput("facs_yChannel", "Y-axis Channel", choices = NULL)
                    ),
                    column(
                      3,
                      actionButton("facs_plotChannelButton", "Plot")
                    )
                  ),
                  fluidRow(
                    plotOutput("facs_ChannelscatterPlot"),
                    plotOutput("facs_autoPlot",height = "800px")
                  )
              )
      ),
      tabItem(
        tabName = "uploadFACS",
        h2("Load Flow Cytometry data"),
        fluidRow(
          column(
            9,
            fileInput(
              inputId = "FACSImport",
              label = "",
              placeholder = "Select an Excel file.",
              width = "80%", 
              multiple = TRUE
            )
          ),
          column(2,
                 actionButton(
                   label = "Load",
                   style = "margin-top: 20px; width: 100%;",
                   icon = shiny::icon("upload"),
                   inputId = "LoadFACS_Button"
                 )
          ),
          tags$style(type='text/css', "#loadAnalysis_Button { width:100%; margin-top: 20px;}")
        ),
      ),
      tabItem(tabName = "tablesFACS",
              h2("Hierarchical gating"),
              box(width = 12,
                  fluidRow(
                    tags$head(
                      tags$style(HTML("
                      #dynamicSelectize { 
                        margin-right: 40px;
                      }
                    "))
                    ),
                    uiOutput("dynamicSelectize")
                  ),
                  fluidRow(
                    tags$head(
                      tags$style(HTML("
                      #FACSmatrix { 
                        float: left;
                      }
                    "))
                    ),
                    column(12, 
                           dataTableOutput("FACSmatrix")          
                    )
                  ),
                  fluidRow(
                    column(2, offset = 7,
                           selectizeInput(inputId = "selectBaseGate",
                                          label = div(class = "icon-container",
                                                      h4("Parental gate:", icon("info-circle")),
                                                      div(class = "icon-text", "Parental gate refers to the gate from which the percetages are calculated")
                                          ),
                                          choices = ""
                           )
                    ),
                    column(2,
                           actionButton(inputId = "SaveFACSanalysis",
                                        label = 'Save',
                                        style = "width: 100%; margin-top: 25px;",
                                        align = "right",
                                        icon = shiny::icon("forward"))
                    )
                  )
              ),
              box(width = 12,collapsible = T,
                  fluidRow(
                    style="width: 95%; margin-left: 30px;", 
                    dataTableOutput("FACSresult")           
                  ),
              ),
              fluidRow(
                box(
                  title = "FACS Name Update", 
                  solidHeader = TRUE, 
                  collapsible = TRUE, 
                  collapsed = TRUE, 
                  width = 12,
                  dataTableOutput("FACSnameUpdate")
                )
              ),
              fluidRow(
                box(
                  title = "FACS column name Update", 
                  solidHeader = TRUE, 
                  collapsible = TRUE, 
                  collapsed = TRUE, 
                  width = 12,
                  dataTableOutput("FACScolumnNameUpdate")
                )
              )
      ),
      tabItem(tabName = "statFACS",
              h2("Statistics"),
              box(width = 12,
                  fluidRow(
                    style="width: 95%; margin-left: 30px;", 
                    column(6,
                           dataTableOutput("FACSexpcond_tab") 
                    ),
                    column(6,
                           uiOutput("FACSexpcond_plot")
                    )
                  ),
                  fluidRow(
                    style="width: 95%; margin-left: 30px;", 
                    column(6,
                           actionButton(inputId = "FACSstatButton",
                                        label = 'Calculate Statistic')
                    )
                  ),
                  fluidRow(
                    style="width: 95%; margin-left: 30px;", 
                    dataTableOutput("FACSstat_tab")
                  )
              ),
              fluidRow(
                column(width = 2,offset = 9,
                       downloadButton( label = "Download Analysis & Excel", 
                                       outputId = "downloadFACSanalysis",
                                       icon = icon("download") 
                       )
                )
              )
      ),
      #### END data analysis: FACS ####
      
      #### BEGIN data analysis: IF ####
      tabItem(
        tabName = "uploadIF",
        h2("Load IF data"),
        fluidRow(
          column(9,
                 fileInput(
                   inputId = "IFImport",
                   label = "",
                   placeholder = "Select an Excel file.",
                   width = "80%", 
                   multiple = TRUE
                 )
          ),
          column(2,
                 actionButton(
                   label = "Load",
                   style = "margin-top: 20px; width: 100%;",
                   icon = shiny::icon("upload"),
                   inputId = "LoadIF_Button"
                 )
          ),
          tags$style(type='text/css', "#loadAnalysis_Button { width:100%; margin-top: 20px;}")
        )
      ),
      tabItem(tabName = "tablesIF",
              h2("Quantification"),
              fluidRow(
                column(
                  width = 3,
                  selectInput(
                    inputId = "IF_expcond",
                    label = "Experimental condition:",
                    choices = ""
                  )
                )
              ),
              fluidRow(
                box(width = 12,title = "Data informations",
                    fluidRow(
                      column(
                        width = 12,
                        DTOutput("IFtable"),
                        DTOutput("IFtable_stat")
                      )
                    )
                ),
                box(width = 12,title = "T-test",
                    fluidRow(
                      column(
                        width = 6,
                        selectInput(
                          inputId = "IF_TTestvariable",
                          label = "Ttest variable:",
                          choices = ""
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 6,
                        DTOutput("IFsummariseMean")
                      ),
                      column(
                        width = 6,
                        plotOutput("IFsummarise_plot"),
                        actionButton("Customize_IF_TTestButton", "Customize or Download Plot")
                      ),
                      
                    ),
                    fluidRow(
                      column(
                        width = 12,
                        DTOutput("IFtable_ttest")
                      )
                    )
                ),
                fluidRow(
                  column(width = 2,offset = 9,
                         downloadButton( label = "Download Analysis & Excel", 
                                         outputId = "downloadIFAnalysis",
                                         icon = icon("download") 
                         )
                  )
                )
              )
      ),
      #### END data analysis: IF ####
      
      #### BEGIN statistical analysis ####
      tabItem(tabName = "StatAnalysis_tab",
              h2("Statistical analysis"),
              fluidRow(
                box(width = 12,
                    title = "Upload the analysis",
                    fluidRow(
                      column(
                        9,
                        fileInput(
                          inputId = "loadStatAnalysis_file",
                          label = "",
                          placeholder = "Select the RDs files storing ORCA analyses",
                          width = "80%", 
                          multiple = TRUE)
                      ),
                      column(
                        2,
                        actionButton( label = "Load",
                                      style = "margin-top: 20px; width: 100%;",
                                      icon = shiny::icon("upload"),
                                      inputId = "loadStatAnalysis_file_Button" )
                      )
                    ),
                    fluidRow(
                      column(3,offset = 1,
                        selectizeInput("StatAnalysis",
                                     label = "Select the analysis:",
                                     choices = "")
                      ),
                      column(3,
                             conditionalPanel(condition = "input.StatAnalysis == 'PCR'",
                                              selectizeInput("stat_genHpcr",
                                                             label = "Select the Gene:",
                                                             choices = "")))
                    )
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  title = "Statistical decision",
                  fluidRow(
                    column(9,
                           style = "border-right: 1px solid #000000;",
                           plotOutput("decision_tree_plot")
                    ),
                    column(3,
                           tags$style(HTML("#analysis_output {font-size: 12px; font-style: italic; }")),
                           htmlOutput("analysis_output")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  collapsible = T,
                  collapsed = T,
                  title = "Comparison analysis",
                  fluidRow(
                    column(10, offset = 1,
                           fluidRow(plotOutput("PlotStat")),
                           fluidRow(DTOutput("TabStat")),
                           fluidRow(DTOutput("TabBivTest")),
                           fluidRow(DTOutput("TabMulvTest")),
                           fluidRow(DTOutput("PairwiseTest"))
                    ), 
                  ),
                  fluidRow(
                    column(8,
                           # New download button for statistical analysis
                           downloadButton("downloadStatisticalAnalysis", "Download Statistical Analysis")
                    )
                  )
                )
              )
      ),
      ###### BEGIN DATAVERSE #####
      tabItem(tabName = "Dataverse_tab",
              h2("Dataverse"),
              fluidRow(
                box(width = 12,
                    title = "Upload and Maintain",
                    collapsible = T,
                    h4(
                      em(
                        "Check ", a("here", href="https://guides.dataverse.org/en/latest/user/account.html"),
                        " for obtaining an account and setting up an API key."
                      ) 
                    ),
                    fluidRow(
                      column(
                        width = 10,offset = 1,
                        verbatimTextOutput("LoadingError_DATAVERSE")
                      )
                    ),
                    fluidRow(
                      column(width=6, 
                             textInput("APIkey",
                                       value = ifelse(system.file("Data",".APIkey", package = "ORCA") != "",
                                                      read.table(paste0(system.file("Data", package = "ORCA"),
                                                                        "/.APIkey"),
                                                                 quote="\"",
                                                                 comment.char=""),
                                                      ""), 
                                       label = "API key linked to a Dataverse installation account:")),
                      column(2,
                             selectizeInput("selectAnalysis_DV",
                                            label = "Select the analysis:",
                                            choices = "")
                      )
                    ),
                    fluidRow(
                      column(3,
                             textInput("Title_DV",
                                       label = "Title:",
                                       value=""
                             )
                      ),
                      column(4,
                             textInput("Description_DV",
                                       label = "Description:",
                                       value=""
                             )
                      )
                    ),
                    fluidRow(
                      column(3,
                             textInput("Author_DV",
                                       label = "Author name:",
                                       value=""
                             )
                      ),
                      column(3,
                             textInput("AuthorAff_DV",
                                       label = "Author affiliation:",
                                       value=""
                             )
                      )
                    ),
                    fluidRow(
                      column(3,
                             textInput("ContactN_DV",
                                       label = "Contact name:",
                                       value=""
                             )
                      ),
                      column(3,
                             textInput("ContactEmail_DV",
                                       label = "Contact email:",
                                       value=""
                             )
                      )
                    ),
                    fluidRow(
                      column(3,
                             actionButton(
                               label = "Upload",
                               inputId = "DataverseUpload_Button" 
                             )
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Download Analysis",
                    collapsible = T,
                    fluidRow(
                      column(3,
                             textInput("DOIdownload",
                                       label = "DOI:",
                                       value=""
                             )
                      ),
                      column(3,
                             actionButton(
                               label = "Download",
                               inputId = "DataverseDownload_Button" 
                             )
                      )
                    )
                )
              )
      )
      ####### END DATAVERSE ####
    )
  )
)

