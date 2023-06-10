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
  dashboardHeader(title = "InteGREAT",
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
                         icon = icon('fa-regular fa-magnifying-glass-chart',verify_fa = FALSE),
                         menuItem('Western Blot analysis',
                                  tabName = 'wb',
                                  menuSubItem("Upload Image", tabName = "uploadIm"),
                                  menuSubItem("Protein Bands", tabName = "plane"),
                                  menuSubItem("Profile Plots", tabName = "grey"),
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
                menuItem('Model Integration',
                         tabName = 'integ',
                         icon = icon('fa-regular fa-inboxes',verify_fa = FALSE),
                         menuSubItem("Omics Data", tabName = "Omics_tab"),
                         menuSubItem("WB, RT-qPCR, ELISA ", tabName = "WbPcrElisa_tab"),
                         menuSubItem("IF, and other data ", tabName = "otherData_tab")
                ),
                menuItem('Load analysis',
                         tabName = 'LoadAnalysis',
                         icon = icon('fa-light fa-upload',verify_fa = FALSE)
                )
    )
  ),
  dashboardBody(
    tabItems(
      ## HOME ####
      tabItem(
        tabName = "Home",
        h1("InteGreat"),
        h2(em("A framework for cellular biology data analysis and integration for computational modelling.")),
        h4("The InteGreat workflow provides an exhaustive platform where scientists
	          can experience the discovery process from the analysis of a single datum to
	          the creation of a complex computational model."),
        h4("InteGreat consists of two modules:"),
        tags$ol(
          tags$li(
            h4("The ", strong("Data Analysis module"), "includes tools specifically developed or adapted for the elaboration
           of raw Western Blot (WB), Reverse Transcription-quantitative PCR (RT-qPCR) and Enzyme-Linked ImmunoSorbent Assay (ELISA) experiments.")
          ),
          tags$li(
            h4("The ",strong("Model Integration module"),"supports scientists in the process of integration of lab data resulting from any type of experiment into a computational model.")
          )
        ),
        h4(em("Check", a("here", href="https://www.google.com/")," for a brief video presentation of the InteGreat framework, or ",
              a("here", href="https://www.google.com/"),"to download the user guide.")),
        p(img(src = "images/Logo_QBio.png", height="15%", width="15%",style = "margin:100px 0px"), align = "center")
      ),
      
      ###### BEGIN LOAD ANALYSIS ####
      tabItem(tabName = "LoadAnalysis",
              h2("Load analysis"),
              box(
                width = 12,
                fluidRow(
                  column(
                    10,
                    fileInput(
                      inputId = "loadAnalysis_file",
                      label = "",
                      placeholder = "Select the RDs files storing InteGreat analyses",
                      width = "80%", 
                      multiple = TRUE)
                  ),
                  column(
                    1,
                    actionButton( label = "Load",style = "margin-top: 20px;",
                                  icon = shiny::icon("fa-regular fa-upload",verify_fa = FALSE),
                                  inputId = "loadAnalysis_Button" )
                  )
                ),
                fluidRow(
                  column(
                    width = 10,
                    offset = 1,
                    verbatimTextOutput("loadAnalysis_Error")
                  )
                )
              )
      ),
      ###### BEGIN MODEL INTEGRATION ####
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
                               icon = shiny::icon("fa-regular fa-upload",verify_fa = FALSE),
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
                      DTOutput("OmicsUser_table",width = "80%")
                    )
                ),
              )
      ),
      ## END model integration: Omics
      ## BEGIN model integration: WB PCR ELISA ####
      tabItem(
        tabName = "WbPcrElisa_tab",
        h2("Data harmonization as initial marking into the Petri Net model "),
        fluidRow(
          box(
            width = 12,
            fluidRow(
              column(
                10,
                fileInput(
                  inputId = "IntGImport",
                  label = "",
                  placeholder = "Select the RDs files storing InteGreat analyses",
                  width = "80%", 
                  multiple = TRUE)
              ),
              column(
                1,
                actionButton( label = "Load",style = "margin-top: 20px;",
                              icon = shiny::icon("fa-regular fa-upload",verify_fa = FALSE),
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
              label = "Select the molecule from the omics dataset:",
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
                  calculate the average that will be rescaled with the omics value. "),
               
               box(
                 width = 12,
                 fluidRow(
                   column(
                     8,
                     fileInput(
                       inputId = "OtherImport",
                       label = "",
                       placeholder = "Select an excel file",
                       width = "100%", 
                       multiple = TRUE
                     )
                   ),
                   column(
                     1,
                     actionButton(
                       label = "Load",style = "margin-top: 20px;",
                       icon = shiny::icon("fa-regular fa-upload",verify_fa = FALSE),
                       inputId = "LoadOther_Button" 
                     )
                   )
                 ),
                 fluidRow(
                   column(
                     width = 10,
                     offset = 1,
                     verbatimTextOutput("LoadingError_Other")
                   )
                 )
               ),
               fluidRow(  
                 column(
                   width = 10,
                   offset = 1,
                   selectizeInput(
                     width = "50%",
                     inputId = "Selectprot_other",
                     multiple = F,
                     options = list(maxItems = 1),
                     label = "Select the molecule from the omics dataset:",
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
                ),
                column(1,
                       style = "margin-top: 20px;",
                       actionButton( label = "Load",
                                     icon = shiny::icon("fa-regular fa-upload",verify_fa = FALSE),
                                     inputId = "LoadPCR_Button")
                )
              ),
              fluidRow(
                column(
                  width = 10,offset = 1,
                  verbatimTextOutput("LoadingError_PCR")
                )
              ),
              fluidRow(
                box(width = 12, title = "Experimental Setup:",
                    column(width = 6,
                           h2("Select the columns to assign"),
                           fluidRow(
                             column(width = 4,
                                    selectInput(inputId = "PCR_gene",
                                                label = "Gene names:",
                                                choices = "" )
                             ),
                             column(width = 4,
                                    selectInput(inputId = "PCR_sample",
                                                label = "Sample names:",
                                                choices = "" )
                             ),
                             column(width = 4,
                                    selectInput(inputId = "PCR_value",
                                                label = "Value names:",
                                                choices = "" )
                             )
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
                    ),
                    column(width = 3,offset = 9,
                           actionButton(inputId = "NextQuantif",
                                        label = 'Proceed to Quantification',
                                        align = "right",
                                        icon = shiny::icon("fa-solid fa-forward",verify_fa = FALSE))
                    )
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "tablesPCR",
              h2("Quantification"),
              fluidRow(
                box(width= 12,title = "Single Gene Quantification",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    uiOutput("PCRtables")
                )
              ),
              fluidRow(
                box(width= 12,title = "Normalization on Housekeeping Genes",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    uiOutput("PCRtablesComp")
                )
              ),
              fluidRow(
                column(width = 1,offset = 7,
                       actionButton(inputId = "NextpcrPlots",
                                    label = 'Proceed to Plots',
                                    align = "right",
                                    icon = shiny::icon("fa-solid fa-forward",verify_fa = FALSE))
                ),
                column(width = 1,offset = 1,
                       downloadButton( label = "Download the analysis", 
                                       outputId = "downloadButton_PCR",
                                       #href = "Results.RData",
                                       #download = "Results.RData",
                                       icon = icon("download") )
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
                8,
                fileInput(
                  inputId = "ELISAImport",
                  label = "",
                  placeholder = "Select an Excel file.",
                  width = "100%", 
                  multiple = TRUE
                )
              ),
              column(1,style = "margin-top: 20px;",
                     actionButton(
                       label = "Load",
                       icon = shiny::icon("fa-regular fa-upload",verify_fa = FALSE),
                       inputId = "LoadELISA_Button"
                     )
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
          box(width = 12,
              title = "Assign experimental information to values:",
              column(width = 6,
                     DTOutput("ELISAmatrix")
              ),
              column(width = 6,
                     selectizeInput("ELISAcell_EXP",
                                    label = "Experimental condition:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     selectizeInput("ELISAcell_TIME",label = "Time:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     checkboxGroupInput(inputId = "ELISA_baselines",
                                        "Select baselines:")
                     
              ),
              fluidRow(
                column(width = 1,offset = 9,
                       actionButton(inputId = "NextElisaQuantif",
                                    label = 'Proceed to Quantification',
                                    align = "right",
                                    icon = shiny::icon("fa-solid fa-forward",verify_fa = FALSE))
                )
              )
          ),
          plotOutput("ELISAinitplots")
        )
      ),
      # Second tab content
      tabItem(tabName = "tablesELISA",
              h2("Quantification"),
              fluidRow(
                tags$head(tags$script(src = "message-handler.js")),
                box(width= 12,
                    title = "Select a baseline for the following experimental conditions",
                    collapsible = TRUE,
                    collapsed = FALSE,
                    uiOutput("ElisaBaselineSelection")
                ),
                box(width= 12,
                    title = "Quantification",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    DTOutput("ELISAtables"),
                    plotOutput("ELISAplots")
                )
              ),
              fluidRow(
                column(width = 1,offset = 9,
                       downloadButton( label = "Download the analysis", 
                                       outputId = "downloadButton_ELISA",
                                       #href = "Results.RData",
                                       #download = "Results.RData",
                                       icon = icon("download") )
                )
              )
      ),
      ## END data analysis: ELISA
      ## BEGIN data analysis: WB  #######
      # First tab content
      tabItem(tabName = "uploadIm",
              h2("Upload Image"),
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
              h2("Select Protein Bands"),
              fluidRow(
                box( width = 12,
                     plotOutput("TifPlot2",width="100%",
                                hover = "plot_hover",
                                brush = "plot_brush")
                )
              ),
              fluidRow(
                box( width = 6,
                     title = tagList(shiny::icon("gear", verify_fa = FALSE), "Protein Band Selection Coordinates"),
                     tableOutput("PlanesStructureTable")
                ),
                box( width = 6,
                     actionButton(inputId = "panelSelect_button", label = "Select Protein Bands"),
                     actionButton(inputId = "ResetPan", label = 'Reset Protein Bands'),
                     actionButton(inputId = "GenLanes", label = 'Generate Plots'),
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
                    downloadButton( label = "Download the analysis", 
                                    outputId = "downloadButton_WB",
                                    #href = "Results.RData",
                                    #download = "Results.RData",
                                    icon = icon("download") ),
                    actionButton(inputId = "NextWBQuantif",
                                 label = 'Proceed to Quantification',
                                 align = "right",
                                 icon = shiny::icon("fa-solid fa-forward",verify_fa = FALSE))
                )
              )
      ),
      # fourth tab content
      tabItem(tabName = "quantification",
              h2("WB quantification"),
              fluidRow(
                box( width = 6,
                     title = tagList(shiny::icon("gear", verify_fa = FALSE), "Set the WB analysis as normalizer"),
                     h5("To remove the not necessary rows just clicking on it."),
                     column(9,
                            fileInput(
                              inputId = "NormWBImport",
                              label = "",
                              placeholder = "Select an WB RDs file generated through the Profile Plots step",
                              width = "90%"
                            )
                     ),
                     column(2,
                            actionButton(
                              label = "Load",
                              icon = shiny::icon("fa-regular fa-upload",verify_fa = FALSE),
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
                     title = tagList(shiny::icon("gear", verify_fa = FALSE),
                                     "Set the WB analysis to normalize"),
                     h5("To remove the not necessary rows just clicking on it."),
                     column(9,
                            fileInput(inputId = "WBImport",
                                      label = "",
                                      placeholder = "Select an WB RDs file generated through the Profile Plots step",
                                      width = "90%"
                            )
                     ),
                     column(2,
                            actionButton( label = "Load",
                                          icon = shiny::icon("fa-regular fa-upload",verify_fa = FALSE),
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
                   title = tagList(shiny::icon("gear", verify_fa = FALSE), "WB quantification"),
                   h3("Relative Density"),
                   fluidRow(
                     column(
                       width = 3,
                       selectInput("IdLaneNorm_RelDens",
                                   label = "Select the Lane to use for the relative normalization",
                                   choices = "")
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
                   )
              ),
              fluidRow(
                column(width = 1, offset = 9,
                       downloadButton( label = "Download the analysis", 
                                       outputId = "downloadButton_WBquant",
                                       #href = "Results.RData",
                                       #download = "Results.RData",
                                       icon = icon("download") 
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
