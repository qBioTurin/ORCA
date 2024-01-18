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
library(openxlsx)
library(patchwork)


ui <- dashboardPage(
  #theme = shinytheme("paper"),
  dashboardHeader(title = "ORCA",
                  tags$li(a(onclick = "onclick =window.open('https://github.com/qBioTurin/ORCA')",
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
                         icon = icon('chart-line'),
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
                                  menuSubItem("Quantification", tabName = "tablesPCR")
                                 # menuSubItem("Plot", tabName = "plotsPCR")
                         ),
                         menuItem('ELISA analysis',
                                  tabName = 'elisa',
                                  menuSubItem("Upload data", tabName = "uploadELISA"),
                                  menuSubItem("Quantification", tabName = "tablesELISA")
                         ),
                         menuItem('Endocytosis assay',
                                  tabName = 'endoc',
                                  menuSubItem("Upload data", tabName = "uploadENDOC"),
                                  menuSubItem("Quantification", tabName = "tablesENDOC")
                         ),
                         menuItem('Cytotoxicity assay',
                                  tabName = 'cytotox',
                                  menuSubItem("Upload data", tabName = "uploadCYTOTOX"),
                                  menuSubItem("Quantification", tabName = "tablesCYTOTOX")
                         )
                ),
                # menuItem('Model Integration',
                #          tabName = 'integ',
                #          icon = icon('file'),
                #          menuSubItem("Omics Data", tabName = "Omics_tab"),
                #          menuSubItem("WB, RT-qPCR, ENDOC ", tabName = "WbPcrElisa_tab"),
                #          menuSubItem("IF, and other data ", tabName = "otherData_tab")
                # ),
                menuItem('Statistical analysis',
                         tabName = 'StatAnalysis_tab',
                         icon = icon('magnifying-glass-chart')
                ),
                menuItem('Dataverse',
                         tabName = 'Dataverse_tab',
                         icon = icon('eye')
                ),
                menuItem('Load analysis',
                         tabName = 'LoadAnalysis',
                         icon = icon('upload')
                )
    )
  ),
  dashboardBody(
    tabItems(
      ## HOME ####
      tabItem(
        tabName = "Home",
        h1("ORCA: Omni Reproducible Cell Analysis"),
        h1("  "),
        fluidRow(
          column(width = 6,
                 p(img(src = system.file("Shiny/www/images","ORCAlogo.png", package = "ORCA"),
                       height="30%", width="30%"), align = "center"),
          ),
        column(width = 6,
               h2(em("A cellular biologist’s toolbox for data analysis.")),
               h4("ORCA  provides an exhaustive platform where scientists can analyze raw:"),
               tags$ol(
                 tags$li(
                   h4(strong("Western Blot")," (WB), ")
                 ),
                 tags$li(
                   h4(strong("Reverse Transcription-quantitative PCR ")," (RT-qPCR),")
                 ),
                 tags$li(
                   h4(strong("Enzyme-Linked ImmunoSorbent Assay ")," (ELISA),")
                 ),
                 tags$li(
                   h4(strong("Endocytosis")," and,")
                 ),
                 tags$li(
                   h4(strong("Cytotoxicity experiments"),".")
                 )
               )
               )
        ),
        p(img(src = system.file("Shiny/www/images","Logo_QBio.png", package = "ORCA"),
              height="15%", width="15%",style = "margin:100px 0px"), align = "center")
        # h4("ORCA consists of two modules:"),
        # tags$ol(
        #   tags$li(
        #     h4("The ", strong("Data Analysis module"), "includes tools specifically developed or adapted for the elaboration
        #    of raw Western Blot (WB), Reverse Transcription-quantitative PCR (RT-qPCR) and Enzyme-Linked ImmunoSorbent Assay (ELISA) experiments.")
        #   ),
        #   tags$li(
        #     h4("The ",strong("Model Integration module"),"supports scientists in the process of integration of lab data resulting from any type of experiment into a computational model.")
        #   )
        # ),
        # h4(em("Check", a("here", href="https://www.google.com/")," for a brief video presentation of the ORCA framework, or ",
        #       a("here", href="https://www.google.com/"),"to download the user guide.")),
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
                      placeholder = "Select the RDs files storing ORCA analyses",
                      width = "80%", 
                      multiple = TRUE)
                  ),
                  column(
                    1,
                    actionButton( label = "Load",style = "margin-top: 20px;",
                                  icon = shiny::icon("upload"),
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
                               icon = shiny::icon("upload"),
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
      ## BEGIN model integration: WB PCR ENDOC ####
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
            pickerInput(
              width = "50%",
              inputId = "Selectprot_wb",
              multiple = F,
              label = "Select the molecule from the omics dataset:",
              choices = "",
              options = list( maxItems = 1, `live-search` = TRUE),
              choicesOpt = list(`style` = "btn-info",
                                `liveSearchPlaceholder`="Search" )
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
            title = "Endocytosis assay",
            tableOutput("Tab_IntG_endoc")
          )
        )
      ),
      ## END model integration: WB PCR ENDOC
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
                       icon = shiny::icon("upload"),
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
                                     icon = shiny::icon("upload"),
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
                             column(width = 3,
                                    selectInput(inputId = "PCR_gene",
                                                label = "Gene names:",
                                                choices = "" )
                             ),
                             column(width = 3,
                                    selectInput(inputId = "PCR_sample",
                                                label = "Sample names:",
                                                choices = "" )
                             ),
                             column(width = 3,
                                    selectInput(inputId = "PCR_value",
                                                label = "Values:",
                                                choices = "" )
                             ),
                             column(width = 3,
                                    selectInput(inputId = "PCR_time",
                                                label = "Times:",
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
                                       label = "Select baseline sample:",
                                       choices = "ID" )
                    ),
                    column(width = 3,offset = 9,
                           actionButton(inputId = "NextQuantif",
                                        label = 'Proceed to Quantification',
                                        align = "right",
                                        icon = shiny::icon("forward"))
                    )
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "tablesPCR",
              h2("Quantification"),
              fluidRow(
                box(width= 12,
                    #title = "Single Gene Quantification",
                    title = "Normalization on Housekeeping Genes",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    uiOutput("PCRtables")
                )
              ),
              # fluidRow(
              #   box(width= 12,title = "Normalization on Housekeeping Genes",
              #       collapsible = TRUE,
              #       collapsed = TRUE,
              #       uiOutput("PCRtablesComp")
              #   )
              # ),
              fluidRow(
                box(width= 12,title = "Plot",
                    plotOutput("PCRplot",width = "100%"),
                    fluidRow(
                      # column(width = 1,offset = 7,
                      #        actionButton(inputId = "NextpcrPlots",
                      #                     label = 'Proceed to Plots',
                      #                     align = "right",
                      #                     icon = shiny::icon("forward"))
                      # ),
                      column(width = 1,offset = 1,
                             downloadButton( label = "Download the analysis", 
                                             outputId = "downloadButton_PCR",
                                             #href = "Results.RData",
                                             #download = "Results.RData",
                                             icon = icon("download") )
                      ),
                      column(width = 1,offset = 2,
                             downloadButton( label = "Download xlsx", 
                                             outputId = "downloadButtonExcel_PCR",
                                             #href = "Results.RData",
                                             #download = "Results.RData",
                                             icon = icon("download") )
                      )
                    )
                )
              )
      ),
      # Second tab content
      # tabItem(tabName = "plotsPCR",
      #         h2("Plots"),
      #         fluidRow(
      #           box(width= 12,title = "",
      #               plotOutput("PCRplot",width = "100%")
      #           )
      #         )
      # ),
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
                       icon = shiny::icon("upload"),
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
                     dataTableOutput("ELISAmatrix")
              ),
              column(width = 6,
                     selectizeInput("ELISAcell_SN",
                                    label = "Sample name:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     selectizeInput("ELISAcell_EXP",label = "Experimental condition:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     fluidRow(
                       column(4,
                              selectizeInput(inputId = "ELISA_standcurve",
                                             label = "Select standard curve:",
                                             choices = NULL)
                       ),
                       column(4,
                              checkboxGroupInput(inputId = "ELISA_baselines",
                                                 "Select control:")
                       ),
                       column(4,
                              checkboxGroupInput(inputId = "ELISA_blanks",
                                                 "Select blank:")
                       )
                     )
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
                                    selectizeInput("regressionType",
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
                box(width= 12,
                    title = "Select a blank for the following experimental conditions",
                    collapsible = TRUE,
                    collapsed = T,
                    h4("If time information is associated with the experimental conditions
                       defined as blank, then it will be lost during the averaging of its values."),
                    uiOutput("ElisaBlankSelection")
                ),
                box(width= 12,
                    title = "Select a baseline for the following experimental conditions",
                    collapsible = TRUE,
                    collapsed = T,
                    uiOutput("ElisaBaselineSelection")
                ),
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
      ## END data analysis: ELISA
      
      ## BEGIN data analysis: ENDOC  #######
      tabItem(
        tabName = "uploadENDOC",
        h2("Load Endocytosis data"),
        fluidRow(
          box(
            width = 12,
            fluidRow(
              column(
                8,
                fileInput(
                  inputId = "ENDOCImport",
                  label = "",
                  placeholder = "Select an Excel file.",
                  width = "100%", 
                  multiple = TRUE
                )
              ),
              column(1,style = "margin-top: 20px;",
                     actionButton(
                       label = "Load",
                       icon = shiny::icon("upload"),
                       inputId = "LoadENDOC_Button"
                     )
              )
            ),
            fluidRow(
              column(
                width = 10,offset = 1,
                verbatimTextOutput("LoadingError_ENDOC")
              )
            )
          )
        ),
        fluidRow(
          box(width = 12,
              title = "Assign experimental information to values:",
              column(width = 6,
                     dataTableOutput("ENDOCmatrix")
              ),
              column(width = 6,
                     selectizeInput("ENDOCcell_SN",
                                    label = "Experimental condition:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     selectizeInput("ENDOCcell_TIME",label = "Time:",
                                    choices = "",
                                    options = list(create = TRUE)),
                     fluidRow(
                       column(6,
                              checkboxGroupInput(inputId = "ENDOC_baselines",
                                                 "Select baselines:")
                       ),
                       column(6,
                              checkboxGroupInput(inputId = "ENDOC_blanks",
                                                 "Select blank:")
                       )
                     )
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
        )
      ),
      # Second tab content
      tabItem(tabName = "tablesENDOC",
              h2("Quantification"),
              fluidRow(
                tags$head(tags$script(src = "message-handler.js")),
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
                      column(width = 1,offset = 9,
                             downloadButton( label = "Download the RDs", 
                                             outputId = "downloadButton_ENDOC",
                                             #href = "Results.RData",
                                             #download = "Results.RData",
                                             icon = icon("download") )
                      ),
                      column(width = 1,offset = 7,
                             downloadButton( label = "Download xlsx", 
                                             outputId = "downloadButtonExcel_ENDOC",
                                             #href = "Results.RData",
                                             #download = "Results.RData",
                                             icon = icon("download") )
                      )
                    )
                )
              )
      ),
      ## END data analysis: ENDOC
      ## BEGIN data analysis: CYTOTOX  #######
      tabItem(
        tabName = "uploadCYTOTOX",
        h2("Load Cytotoxicity data"),
        fluidRow(
          box(
            width = 12,
            fluidRow(
              column(
                8,
                fileInput(
                  inputId = "CYTOTOXImport",
                  label = "",
                  placeholder = "Select an Excel file.",
                  width = "100%", 
                  multiple = TRUE
                )
              ),
              column(1,style = "margin-top: 20px;",
                     actionButton(
                       label = "Load",
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
                         )
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
                      column(width = 1,
                             downloadButton( label = "Download the RDs", 
                                             outputId = "downloadButton_CYTOTOX",
                                             #href = "Results.RData",
                                             #download = "Results.RData",
                                             icon = icon("download") )
                      ),
                      column(width = 1,offset = 2,
                             downloadButton( label = "Download xlsx", 
                                             outputId = "downloadButtonExcel_CYTOTOX",
                                             #href = "Results.RData",
                                             #download = "Results.RData",
                                             icon = icon("download") )
                      )
                    )
                )
              )
      ),
      ## END data analysis: CYTOTOX
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
                #shinydashboard::box( width = 12,
                     #column(12,align="center",
                            uiOutput("TiffBox")
                     # plotOutput("TifPlot2",
                     #            hover = "plot_hover",
                     #            brush = "plot_brush")
                     #)
                #)
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
                                         sliderInput(inputId = "truncV", label = h4("Vertical truncation:"),
                                                     min = 0, max = 0, value = c(0,0),step = 1
                                         ),
                                         actionButton( 
                                           label = "Cut", inputId = "actionButton_TruncV",
                                           icon = icon("cut")
                                         )
                                ),
                                tabPanel("Horizontal cut", value= "H",textOutput("H"),
                                         sliderInput(inputId = "truncH", label = h4("Horizontal truncation:"),
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
                             actionButton( label = "Reset all", 
                                           inputId = "actionButton_ResetPlanes")
                             ),
                      column(width = 3,
                             downloadButton( label = "Download the analysis", 
                                             outputId = "downloadButton_WB",
                                             #href = "Results.RData",
                                             #download = "Results.RData",
                                             icon = icon("download") ) 
                      ),
                      column(width = 3,
                             downloadButton( label = "Download xlsx", 
                                             outputId = "downloadButtonExcel_WB",
                                             #href = "Results.RData",
                                             #download = "Results.RData",
                                             icon = icon("download") )  
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
      # fourth tab content
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
                     column(width = 1, offset = 7,
                            downloadButton( label = "Download the analysis", 
                                            outputId = "downloadButton_WBquant",
                                            #href = "Results.RData",
                                            #download = "Results.RData",
                                            icon = icon("download") 
                            )
                     ),
                     column(width = 1,offset = 9,
                            downloadButton( label = "Download xlsx", 
                                            outputId = "downloadButtonExcel_WBquant",
                                            #href = "Results.RData",
                                            #download = "Results.RData",
                                            icon = icon("download") )
                     )
                   )
              )
      ),
      ## END data analysis: WB
      ###### END DATA ANALYSIS ####
      ###### BEGIN Statistic ####
      tabItem(tabName = "StatAnalysis_tab",
              h2("Statistical analysis"),
              fluidRow(
                box(width = 12,
                    title = "Upload the analysis",
                    fluidRow(
                      column(
                        10,
                        fileInput(
                          inputId = "loadStatAnalysis_file",
                          label = "",
                          placeholder = "Select the RDs files storing ORCA analyses",
                          width = "80%", 
                          multiple = TRUE)
                      ),
                      column(
                        1,
                        actionButton( label = "Load",style = "margin-top: 20px;",
                                      icon = shiny::icon("upload"),
                                      inputId = "loadStatAnalysis_file_Button" )
                      )
                    )
                )
              ),
              fluidRow(
                column(
                  width = 10,
                  offset = 1,
                  verbatimTextOutput("loadStatAnalysis_Error")
                )
              ),
              box(
                width = 12,
                collapsible = T,
                collapsed = T,
                title = "Comparison analysis",
                selectizeInput("StatAnalysis",
                               label = "Select the analysis:",
                               choices = ""),
                fluidRow(
                  column(12,
                         plotOutput("PlotStat"),
                         DTOutput("TabStat"),
                         DTOutput("TabTTest")
                  )
                )
              )
      ),
      ###### END Statitcs ###
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
                      actionButton(
                        label = "Upload",
                        inputId = "DataverseUpload_Button" 
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Search and Get",
                    collapsible = T
                )
              )
      )
      ####### END DATAVERSE ####
      
      # Here ends the ui body
    )
    
  )
  
)
