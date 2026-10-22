library(flowCore)
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
library(xml2)
library(visNetwork)
library(shinyFiles)  # Load shinyFiles package
library(colourpicker)
library(sortable)
library(stringi)

ui <- dashboardPage(
  dashboardHeader(title = "ORCA",
                  tags$li(
                    class = "dropdown d-flex align-items-center",
                    tags$head(tags$link(rel = "shortcut icon", href = "ORCAlogo.png")),
                    tags$style("
      /* ORCA Ocean Blue Theme */
      .main-header {
        max-height: 60px;
        background: linear-gradient(135deg, #0066cc 0%, #004d99 100%) !important;
      }
      .main-header .logo {
        background: linear-gradient(135deg, #0066cc 0%, #003d7a 100%) !important;
        color: white !important;
        font-weight: bold;
        font-size: 24px;
      }
      .main-header .logo:hover {
        background: linear-gradient(135deg, #0052a3 0%, #003366 100%) !important;
      }
      .main-header .navbar {
        background: linear-gradient(135deg, #0066cc 0%, #004d99 100%) !important;
      }
      
      /* Sidebar Styling */
      .sidebar {
        background: linear-gradient(180deg, #001a33 0%, #003d7a 100%) !important;
      }
      .sidebar-menu > li > a {
        color: #e6f2ff !important;
        border-left: 3px solid transparent;
        transition: all 0.3s ease;
      }
      .sidebar-menu > li > a:hover {
        background-color: #0052a3 !important;
        border-left: 3px solid #00bfff;
        color: white !important;
      }
      .sidebar-menu > li.active > a {
        background-color: #0066cc !important;
        border-left: 3px solid #00bfff;
        color: white !important;
        font-weight: bold;
      }
      .sidebar-menu .treeview-menu {
        background: #001a33 !important;
      }
      .sidebar-menu .treeview-menu > li > a {
        color: #b3d9ff !important;
      }
      .sidebar-menu .treeview-menu > li > a:hover {
        background-color: #003366 !important;
        color: white !important;
      }
      
      /* Box Styling */
      .box {
        border-top-color: #0066cc !important;
        box-shadow: 0 2px 4px rgba(0, 102, 204, 0.1);
      }
      .box-header {
        background: linear-gradient(135deg, #e6f2ff 0%, #cce6ff 100%);
        color: #003d7a;
        font-weight: bold;
      }
      .box-primary {
        border-top-color: #0066cc !important;
      }
      .box-info {
        border-top-color: #00bfff !important;
      }
      
      /* Button Styling */
      .btn-primary {
        background: linear-gradient(135deg, #0066cc 0%, #004d99 100%);
        border-color: #0052a3;
        color: white;
        transition: all 0.3s ease;
      }
      .btn-primary:hover {
        background: linear-gradient(135deg, #0052a3 0%, #003d7a 100%);
        border-color: #003366;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0, 102, 204, 0.3);
      }
      
      /* Action Button Styling */
      .btn-default {
        background-color: #e6f2ff;
        border-color: #0066cc;
        color: #003d7a;
      }
      .btn-default:hover {
        background-color: #cce6ff;
        border-color: #0052a3;
      }
      
      /* Data Table Styling */
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: linear-gradient(135deg, #0066cc 0%, #004d99 100%) !important;
        border-color: #0052a3 !important;
        color: white !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: #e6f2ff !important;
        border-color: #0066cc !important;
        color: #003d7a !important;
      }
      
      /* Content Wrapper */
      .content-wrapper {
        background-color: #f5f9fc !important;
      }
      
      /* Tab Styling */
      .nav-tabs > li.active > a {
        background-color: #0066cc !important;
        color: white !important;
        border-color: #0052a3 !important;
      }
      .nav-tabs > li > a:hover {
        background-color: #e6f2ff !important;
        color: #003d7a !important;
      }
      
      /* Icon Container */
      .icon-container {
        position: relative;
        display: inline-block;
      }
      .icon-container .icon-text {
        visibility: hidden;
        width: 400px;
        background-color: #001a33;
        color: #e6f2ff;
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
        border: 1px solid #0066cc;
      }
      .icon-container:hover .icon-text {
        visibility: visible;
        opacity: 1;
      }
      
      h3 {
        padding-top: 0px;
        padding-bottom: 0px;
        margin-top: 0;
        margin-bottom: 0;
        color: #003d7a;
      }
      
      h2 {
        color: #003d7a;
        border-bottom: 2px solid #0066cc;
        padding-bottom: 10px;
      }
      
      /* Progress bars */
      .progress-bar {
        background: linear-gradient(135deg, #0066cc 0%, #004d99 100%) !important;
      }
      
      /* Info boxes */
      .info-box {
        box-shadow: 0 2px 4px rgba(0, 102, 204, 0.1);
      }
      .info-box-icon {
        background: linear-gradient(135deg, #0066cc 0%, #004d99 100%) !important;
      }
    ")
                  ),
                  tags$li(a(onclick = "window.open('https://github.com/qBioTurin/ORCA')",
                            href = NULL,
                            icon("github"),
                            title = "GitHub",
                            style = "cursor: pointer; color: white;"),
                          class = "dropdown"),
                  tags$li(class = "dropdown",
                          tags$style("#mydropdown {cursor: pointer; background-color: #0066cc; color: white; border: 1px solid #0052a3; height: 50px; left: 0;}"),
                          dropdownButton(inputId = "mydropdown", label = "", right = TRUE, circle = FALSE, icon = icon("download"),
                                         tags$li(shinyjs::useShinyjs(),
                                                 downloadButton(outputId = 'downloadReport', label = "Report", title = "Report generation", style = "cursor: pointer; width: 98%; text-align: center; vertical-align: middle; border: 1px solid #0066cc; background-color: #e6f2ff; color: #003d7a;", class = "dlButton")),
                                         tags$li(shinyjs::useShinyjs(),
                                                 downloadButton(outputId = 'downloadRDSwholeAnalysis', label = "RDs", title = "RDS storing the whole analysis", style = "cursor: pointer; width: 98%; text-align: center; vertical-align: middle; border: 1px solid #0066cc; background-color: #e6f2ff; color: #003d7a;", class = "dlButton"))
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
                         menuItem('Fluorescence Microscopy analysis', tabName = 'if',
                                  menuSubItem("Upload data", tabName = "uploadIF"),
                                  menuSubItem("Quantification", tabName = "tablesIF")),
                         menuItem('Flow Cytometry analysis', tabName = 'facs',
                                  menuItem("Raw data", tabName = "RawFACS",
                                            menuSubItem("Upload raw data", tabName = "uploadRawFACS"),
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
                         tags$li(h4(strong("Fluorescence Microscopy"), ", and")),
                         tags$li(h4(strong("Flow Cytometry analysis"), "."))
                       )
                )
              ),
              div(class = "orca-logo-container",
                  p(img(src = "Logo_QBio.png", height = "15%", width = "15%", 
                        style = "margin:50px 0px; opacity: 0.8;"))
              ),
              tags$style(HTML("
                .orca-welcome {
                  background: linear-gradient(135deg, #0066cc 0%, #003d7a 100%);
                  color: white;
                  padding: 40px;
                  border-radius: 15px;
                  margin: 20px;
                  box-shadow: 0 8px 16px rgba(0, 102, 204, 0.3);
                }
                .orca-subtitle {
                  color: #e6f2ff;
                  font-style: italic;
                  margin-top: 10px;
                }
                .orca-feature-list {
                  background: white;
                  padding: 30px;
                  border-radius: 10px;
                  box-shadow: 0 4px 8px rgba(0, 102, 204, 0.15);
                  margin: 20px;
                }
                .orca-logo-container {
                  text-align: center;
                  padding: 20px;
                }
                .analysis-cards-container {
                  display: grid;
                  grid-template-columns: repeat(auto-fit, minmax(350px, 1fr));
                  gap: 25px;
                  padding: 20px;
                  margin-top: 20px;
                }
                .analysis-card {
                  background: white;
                  border-radius: 12px;
                  box-shadow: 0 4px 15px rgba(0, 102, 204, 0.15);
                  overflow: hidden;
                  transition: transform 0.3s ease, box-shadow 0.3s ease;
                  border: 1px solid #e6f2ff;
                }
                .analysis-card:hover {
                  transform: translateY(-5px);
                  box-shadow: 0 8px 25px rgba(0, 102, 204, 0.25);
                }
                .analysis-card-header {
                  background: linear-gradient(135deg, #0066cc 0%, #004d99 100%);
                  color: white;
                  padding: 20px;
                  display: flex;
                  align-items: center;
                  gap: 15px;
                }
                .analysis-card-header i {
                  font-size: 28px;
                  opacity: 0.9;
                }
                .analysis-card-header h4 {
                  margin: 0;
                  font-size: 18px;
                  font-weight: 600;
                }
                .analysis-card-body {
                  padding: 20px;
                }
                .analysis-card-description {
                  color: #555;
                  font-size: 14px;
                  line-height: 1.5;
                  margin-bottom: 15px;
                  padding-bottom: 15px;
                  border-bottom: 1px solid #e6f2ff;
                }
                .analysis-card-features {
                  list-style: none;
                  padding: 0;
                  margin: 0;
                }
                .analysis-card-features li {
                  padding: 8px 0;
                  color: #333;
                  font-size: 13px;
                  display: flex;
                  align-items: flex-start;
                  gap: 10px;
                }
                .analysis-card-features li i {
                  color: #0066cc;
                  font-size: 14px;
                  margin-top: 2px;
                  min-width: 16px;
                }
                .analysis-guide-header {
                  text-align: center;
                  padding: 30px 20px;
                  background: linear-gradient(180deg, #f9fcff 0%, #f0f8ff 100%);
                  border-radius: 10px;
                  margin: 20px 0;
                }
                .analysis-guide-header h2 {
                  color: #0066cc;
                  margin-bottom: 10px;
                }
                .analysis-guide-header p {
                  color: #666;
                  font-size: 16px;
                  max-width: 800px;
                  margin: 0 auto;
                }
              ")),
              
              # ===== Analysis Features Summary Guide =====
              div(class = "analysis-guide-header",
                  h2(icon("chart-line"), " Analysis Features Summary Guide"),
                  p("Explore the comprehensive analysis capabilities of ORCA. Click on any analysis section in the sidebar to get started.")
              ),
              
              div(class = "analysis-cards-container",
                  
                  # ===== RT-qPCR Card =====
                  div(class = "analysis-card",
                      div(class = "analysis-card-header",
                          icon("dna"),
                          h4("RT-qPCR Gene Expression")
                      ),
                      div(class = "analysis-card-body",
                          p(class = "analysis-card-description",
                            "Reverse Transcription quantitative PCR analysis for gene expression quantification with ΔΔCt calculations and fold-change analysis."
                          ),
                          tags$ul(class = "analysis-card-features",
                              tags$li(icon("calculator"), "ΔΔCt Method for relative expression"),
                              tags$li(icon("balance-scale"), "Housekeeping gene normalization"),
                              tags$li(icon("chart-line"), "Time course expression tracking"),
                              tags$li(icon("chart-bar"), "Fold change visualization"),
                              tags$li(icon("download"), "Export data & publication plots")
                          )
                      )
                  ),
                  
                  # ===== BCA Card =====
                  div(class = "analysis-card",
                      div(class = "analysis-card-header",
                          icon("flask"),
                          h4("BCA Protein Assay")
                      ),
                      div(class = "analysis-card-body",
                          p(class = "analysis-card-description",
                            "Bicinchoninic Acid assay for protein quantification with automatic calibration curve fitting and concentration calculations."
                          ),
                          tags$ul(class = "analysis-card-features",
                              tags$li(icon("chart-area"), "Polynomial regression calibration"),
                              tags$li(icon("calculator"), "Concentration from absorbance"),
                              tags$li(icon("chart-bar"), "Statistical analysis & CV"),
                              tags$li(icon("palette"), "Interactive scatter plots"),
                              tags$li(icon("download"), "Excel reports & high-res plots")
                          )
                      )
                  ),
                  
                  # ===== ELISA Card =====
                  div(class = "analysis-card",
                      div(class = "analysis-card-header",
                          icon("flask-vial"),
                          h4("ELISA Analysis")
                      ),
                      div(class = "analysis-card-body",
                          p(class = "analysis-card-description",
                            "Enzyme-Linked ImmunoSorbent Assay analysis with 4-parameter logistic curve fitting and dilution factor corrections."
                          ),
                          tags$ul(class = "analysis-card-features",
                              tags$li(icon("chart-area"), "4PL or polynomial curve fitting"),
                              tags$li(icon("calculator"), "OD to concentration interpolation"),
                              tags$li(icon("chart-bar"), "t-tests and ANOVA comparisons"),
                              tags$li(icon("palette"), "Bar plots with error bars"),
                              tags$li(icon("download"), "High resolution exports")
                          )
                      )
                  ),
                  
                  # ===== Endocytosis Card =====
                  div(class = "analysis-card",
                      div(class = "analysis-card-header",
                          icon("arrow-circle-down"),
                          h4("Endocytosis Assay")
                      ),
                      div(class = "analysis-card-body",
                          p(class = "analysis-card-description",
                            "Endocytosis analysis for tracking cellular internalization dynamics with time-course measurements and normalization."
                          ),
                          tags$ul(class = "analysis-card-features",
                              tags$li(icon("calculator"), "Internalization rate calculations"),
                              tags$li(icon("balance-scale"), "Baseline normalization"),
                              tags$li(icon("chart-bar"), "Statistical testing & ANOVA"),
                              tags$li(icon("clock"), "Time-series dynamics plots"),
                              tags$li(icon("download"), "Publication-ready figures")
                          )
                      )
                  ),
                  
                  # ===== Cytotoxicity Card =====
                  div(class = "analysis-card",
                      div(class = "analysis-card-header",
                          icon("virus"),
                          h4("Cytotoxicity Assay")
                      ),
                      div(class = "analysis-card-body",
                          p(class = "analysis-card-description",
                            "Cell viability and cytotoxicity analysis with IC50 determination from dose-response curves."
                          ),
                          tags$ul(class = "analysis-card-features",
                              tags$li(icon("percentage"), "Cell viability percentages"),
                              tags$li(icon("chart-line"), "IC50 determination"),
                              tags$li(icon("chart-bar"), "Multiple comparison corrections"),
                              tags$li(icon("palette"), "Sigmoidal dose-response curves"),
                              tags$li(icon("download"), "Viability data & IC50 export")
                          )
                      )
                  ),
                  
                  # ===== Immunofluorescence Card =====
                  div(class = "analysis-card",
                      div(class = "analysis-card-header",
                          icon("microscope"),
                          h4("Fluorescence Microscopy")
                      ),
                      div(class = "analysis-card-body",
                          p(class = "analysis-card-description",
                            "Quantitative analysis of Fluorescence Microscopy data with compartment distributions and statistical testing."
                          ),
                          tags$ul(class = "analysis-card-features",
                              tags$li(icon("calculator"), "Automatic mean & SD calculations"),
                              tags$li(icon("chart-bar"), "t-tests, ANOVA & post-hoc"),
                              tags$li(icon("palette"), "Customizable bar plots"),
                              tags$li(icon("filter"), "Significance level filtering"),
                              tags$li(icon("download"), "Excel reports with statistics")
                          )
                      )
                  ),
                  
                  # ===== Flow Cytometry Card =====
                  div(class = "analysis-card",
                      div(class = "analysis-card-header",
                          icon("chart-area"),
                          h4("Flow Cytometry (FACS)")
                      ),
                      div(class = "analysis-card-body",
                          p(class = "analysis-card-description",
                            "Flow cytometry analysis with hierarchical gating strategies and population frequency calculations."
                          ),
                          tags$ul(class = "analysis-card-features",
                              tags$li(icon("project-diagram"), "Hierarchical gating strategy"),
                              tags$li(icon("calculator"), "Population frequencies & counts"),
                              tags$li(icon("chart-bar"), "Cross-group statistical tests"),
                              tags$li(icon("palette"), "Heatmaps & multi-parameter plots"),
                              tags$li(icon("download"), "Gating results & figures")
                          )
                      )
                  )
              )
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
                fluidRow(
                  column(width = 1,offset = 9,
                         actionButton(inputId = "NextDataOmicsQuantif",
                                      label = 'Proceed to Data Harmonization',
                                      align = "right",
                                      icon = shiny::icon("forward"))
                  )
                )
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
              div(class = "if-header-box",
                  h2(icon("image"), " Western Blot Analysis - Image Analysis", style = "margin-top: 0;"),
                  p(class = "if-description",
                    "Western blotting enables the separation and identification of proteins based on their molecular weight by 
                    gel electrophoresis. ORCA provides accurate quantification of proteins of interest, normalized to the loading 
                    control, allowing reliable comparison of protein expression levels across different experimental conditions."
                  ),
                  hr(),
                  p(class = "if-description",
                    strong("Image Analysis: "),
                    "This section enables precise and reproducible quantification of proteins of interest from Western 
                    blot images. Users can upload the original TIFF file of the blot and select the bands corresponding to 
                    the protein of interest. ORCA generates densitometric profiles (intensity plots) for each band, 
                    allowing background noise removal. The area under each curve is then automatically calculated and 
                    exported in Excel and RDS formats."
                  )
              ),
              fluidRow(
                column(12,
                       div(class = "if-upload-box",
                           h3(icon("cloud-upload-alt"), " Upload Your Image", style = "color: #0066cc; margin-top: 0;"),
                           p(style = "color: #666; margin-bottom: 20px;",
                             "Select a TIFF file containing your Western blot image. After loading, you will be redirected to the Protein Band selection section."
                           ),
                           fluidRow(
                             column(9,
                                    fileInput(
                                      inputId = "imImport",
                                      label = "",
                                      placeholder = "Click to browse or drag TIFF file here...",
                                      width = "100%"
                                    )
                             ),
                             column(3,
                                    actionButton(
                                      label = "Load Image",
                                      style = "margin-top: 0px; width: 100%; background-color: #0066cc; color: white; 
                                               border: none; padding: 12px; font-size: 16px; font-weight: bold;
                                               border-radius: 5px; transition: all 0.3s ease;",
                                      icon = shiny::icon("upload"),
                                      inputId = "LoadingTif"
                                    )
                             )
                           )
                       )
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
              div(class = "if-header-box",
                  h2(icon("calculator"), " Western Blot Quantification", style = "margin-top: 0;"),
                  p(class = "if-description",
                    "Users can upload the RDS files generated from the analysis of both the normalizer and the protein 
                    of interest. ORCA automatically calculates the Relative Density (RD) and the Adjusted Relative 
                    Density (AdjRD). Relative Density (RD) is obtained by dividing each area under the curve (AUC) of 
                    the protein of interest by a single reference AUC from the normalizer. Adjusted Relative Density 
                    (AdjRD) is calculated by dividing the AUCs of the protein of interest by their corresponding AUCs of 
                    the normalizer, enabling lane-specific normalization."
                  )
              ),
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
              fluidRow(
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
              )
      ),
      ## END data analysis: WB  #######
      
      #### BEGIN data analysis: RT-PCR ####
      tabItem(tabName = "uploadPCR",
              div(class = "if-header-box",
                  h2(icon("dna"), " RT-qPCR Gene Expression Analysis", style = "margin-top: 0;"),
                  p(class = "if-description",
                    "Reverse Transcription quantitative Polymerase Chain Reaction (RT-qPCR) analysis for gene expression 
                    quantification. Upload an Excel file containing Ct values, gene names, sample names, and time points (if 
                    available). The module automatically performs ΔΔCt calculations, normalizes gene expression to 
                    housekeeping genes, computes fold changes, and generates comprehensive gene expression profiles."
                  )
              ),
              fluidRow(
                column(12,
                       div(class = "if-upload-box",
                           h3(icon("cloud-upload-alt"), " Upload Your Data", style = "color: #0066cc; margin-top: 0;"),
                           p(style = "color: #666; margin-bottom: 20px;",
                             "Select one or more Excel files containing your RT-qPCR data. Files should include Ct values, gene identifiers, 
                             sample names, and experimental time points."
                           ),
                           fluidRow(
                             column(9,
                                    fileInput(
                                      inputId = "PCRImport",
                                      label = "",
                                      placeholder = "Click to browse or drag files here...",
                                      width = "100%", 
                                      multiple = TRUE,
                                      accept = c(".xlsx", ".xls")
                                    )
                             ),
                             column(3,
                                    actionButton(
                                      label = "Load Data",
                                      style = "margin-top: 0px; width: 100%; background-color: #0066cc; color: white; 
                                               border: none; padding: 12px; font-size: 16px; font-weight: bold;
                                               border-radius: 5px; transition: all 0.3s ease;",
                                      icon = shiny::icon("upload"),
                                      inputId = "LoadPCR_Button"
                                    )
                             )
                           )
                       )
                )
              ),

              fluidRow(
                box(
                  width = 12,
                  title = "Experimental Setup:",
                  fluidRow(
                    box(width = 8,
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
                        DTOutput("PCRpreview")
                    ),
                    box(width = 4,
                        fluidRow(
                          column(
                            width = 6,
                            selectizeInput(
                              inputId = "PCRnorm", multiple = T, choices = "",
                              label = "Select housekeeping genes:")
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            checkboxInput( inputId = "CheckBaseline", 
                                           label = "Baseline equal for each sample", 
                                           value = TRUE)
                          ),
                          fluidRow(
                            conditionalPanel(condition = "input.CheckBaseline == true",
                                             column(
                                               width = 6,
                                               selectInput(
                                                 inputId = "PCRbaseline",label = "Select baseline:",
                                                 choices = ""
                                               )
                                             )
                            ),
                            conditionalPanel(condition = "input.CheckBaseline == false",
                                             column(
                                               width = 12,
                                               uiOutput("PCRbaseline_different")
                                             )
                            )
                          )
                        )
                    )
                  ),
                  br(),
                  fluidRow(
                    column(
                      width = 12,
                      actionButton("toggleTimePatterns", "Define Time Patterns", icon = icon("caret-down")),
                      hidden(
                        div(
                          id = "timePatternsSection",
                          fluidRow(
                            column(
                              width = 6,
                              textInput(
                                inputId = "PCR_time_patterns",
                                label = "Enter time patterns (comma-separated):",
                                placeholder = "e.g. 1T, 3T, 48H, 5DAYS"
                              )
                            ),
                            column(
                              width = 6,
                              textInput(
                                inputId = "PCR_time_colname",
                                label = "Column name for saving the timings:",
                                placeholder = "e.g. Time"
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              selectInput(
                                inputId = "PCR_sample_colname",
                                label = "Column name where searching the timings:",
                                choices = ""
                              )
                            ),
                            column(
                              width = 6,
                              textInput(
                                inputId = "PCR_sample_colname_new",
                                label = "Column name for saving the splitted sample:",
                                placeholder = "e.g. Sample"
                              )
                            )
                          ),
                          actionButton(
                            inputId = "ApplyTimePatterns",
                            label = "Apply Time Extraction",
                            icon = shiny::icon("check")
                          )
                        )
                      )
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
              ),
              fluidRow(
                box(
                  title = "Help: RT-qPCR Q Value Calculation",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 12,
                  HTML("
        <h4>Standard ΔΔCt Method:</h4>
        <ol>
          <li><b>ΔCt_gene</b> = mean(Ct<sub>gene</sub>) − mean(Ct<sub>housekeeping</sub>) </li>
          <li><b>ΔCt_baseline</b> = mean(Ct<sub>baseline</sub>) − mean(Ct<sub>housekeeping</sub>) </li>
          <li><b>ΔΔCt</b> = ΔCt<sub>gene</sub> − ΔCt<sub>baseline</sub></li>
          <li><b>Q</b> = 2<sup>−ΔΔCt</sup></li>
        </ol>
        This normalizes gene expression to a housekeeping gene, then compares it to a control condition.<br><br>

        <h4>Time-Dependent Analysis:</h4>
        <ol>
          <li>Perform the same ΔCt calculation at each time point</li>
          <li>Choose a baseline time (e.g., <b>0h</b>): ΔCt<sub>ref</sub> = mean(Ct<sub>gene,0h</sub>) − mean(Ct<sub>housekeeping,0h</sub>)</li>
          <li><b>ΔΔCt<sub>t</sub></b> = ΔCt<sub>t</sub> − ΔCt<sub>ref</sub></li>
          <li><b>Q<sub>t</sub></b> = 2<sup>−ΔΔCt<sub>t</sub></sup></li>
        </ol>
        This shows how expression changes over time <b>per sample</b> relative to the starting point (e.g., 0h).<br><br>
        ")
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
                    fluidRow(
                      column(
                        width = 2,
                        offset = 1,
                        selectizeInput(
                          inputId = "PCR_sample_plot",
                          "Select Sample:", 
                          choices = c(""),
                          selected = "", multiple = TRUE
                        )
                      ),
                      column(
                        width = 2,
                        selectizeInput(
                          inputId = "PCR_Baseline_plot",
                          "Select Baseline:", 
                          choices = c(""),
                          selected = "", multiple = TRUE
                        )
                      ),
                      column(
                        width = 2,
                        selectizeInput(
                          inputId = "Gene_plot",
                          "Select gene:", 
                          choices = c(""),
                          selected = ""
                        )
                      ),
                      column(
                        width = 2,
                        selectizeInput(
                          inputId = "HousKgene_plot",
                          "Select housekeeping gene:", 
                          choices = c(""), selected = ""
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        offset = 1,
                        width = 2,
                        checkboxGroupInput("PCR_plot_type", "Plot Type", choices = c("Point" = "point", "Bar" = "bar"), selected = "bar")
                      ),
                      column(
                        width = 2,
                        radioButtons("PCR_plot_y", "Visualization: ", choices = c("DDCT" = "ddCt", "2^(-DDCT)" = "Q"), selected = "ddCt")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             #plotly::plotlyOutput("SingleGenePlot")
                             plotOutput("SingleGenePlot")
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             tableOutput("SingleGeneTable")
                      )
                    ),
                    fluidRow(
                      column(width = 3, offset = 9, 
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
                    h3("Plot with all the saved genes:"),
                    fluidRow(
                      column(width = 12,
                             plotOutput("PointGenePlot")
                      )
                    ),
                    fluidRow(
                      column(width = 3, offset = 9,
                             actionButton("Customize_PCR_Button_1", "Customize or Download Plot") )
                    ),
                    br(),br(),
                    h3("Plots of the saved genes:"),
                    fluidRow(
                      column(width = 12,
                             fluidRow(
                               uiOutput("PCRplot")
                             ),
                             h3("Select the plot to customize:"),
                             fluidRow(
                               column(
                                 width = 3,
                                 selectizeInput(
                                   inputId = "Select_Customize_Gene_plot",
                                   "Select gene for customization:", 
                                   choices = c(""),
                                   selected = ""
                                 )
                               ),
                               column(
                                 width = 4,
                                 selectizeInput(
                                   inputId = "Select_Customize_HousKgene_plot",
                                   "Select housekeeping gene for customization:", 
                                   choices = c(""), 
                                   selected = ""
                                 )
                               ),
                               column(
                                 width = 3,
                                 actionButton("Customize_PCR_Button_2", "Customize or Download Plot", style="margin-top:25px;")
                               )
                             )
                      )
                    ), 
                    br(),
                    h3("Tables of the saved genes:"),
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
        div(class = "if-header-box",
            h2(icon("flask"), " Protein Quantification Assay Analysis", style = "margin-top: 0;"),
            p(class = "if-description",
              "Bicinchoninic Acid (BCA) or Bradford assay data in Excel format can be uploaded into this module for 
              protein quantification. The Excel file should contain absorbance measurements along with standard 
              curve data. The module automatically generates calibration curves using polynomial regression, 
              calculates protein concentrations, and delivers comprehensive statistical analysis with clear data 
              visualization."
            )
        ),
        fluidRow(
          column(12,
                 div(class = "if-upload-box",
                     h3(icon("cloud-upload-alt"), " Upload Your Data", style = "color: #0066cc; margin-top: 0;"),
                     p(style = "color: #666; margin-bottom: 20px;",
                       "Select one or more Excel files containing your BCA assay measurements. Files should include sample absorbance 
                       values, experimental conditions, and standard curve concentrations."
                     ),
                     fluidRow(
                       column(9,
                              fileInput(
                                inputId = "BCAImport",
                                label = "",
                                placeholder = "Click to browse or drag files here...",
                                width = "100%", 
                                multiple = TRUE,
                                accept = c(".xlsx", ".xls")
                              )
                       ),
                       column(3,
                              actionButton(
                                label = "Load Data",
                                style = "margin-top: 0px; width: 100%; background-color: #0066cc; color: white; 
                                         border: none; padding: 12px; font-size: 16px; font-weight: bold;
                                         border-radius: 5px; transition: all 0.3s ease;",
                                icon = shiny::icon("upload"),
                                inputId = "LoadBCA_Button"
                              )
                       )
                     )
                 )
          )
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
                                        div(class = "icon-text", style = "font-size: 12px; max-width: 100px; white-space: normal;",
                                            "Analyte Concentrations refers to the standard curve sample.")
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
                                            div(class = "icon-text", style = "font-size: 12px; max-width: 100px; white-space: normal;",
                                                "Blank refers to the standard curve value with the smaller concentration.")
                                ),
                                choices = c("No" = "no", "Yes" = "yes"),
                                selected = "no"
                              )
                       )
                     )
              )
          ),
          fluidRow(
            uiOutput("tablesBCA")
          ),
          fluidRow(
            column(width = 1,offset = 9,
                   actionButton(inputId = "NextBCAQuantif",
                                label = 'Proceed to Quantification',
                                align = "right",
                                icon = shiny::icon("forward"))
            )
          )
        ),
        fluidRow(
          box(
            title = "Help: BCA Protein Quantification",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            HTML("
    <h4>BCA Assay Overview</h4>
    The BCA assay quantifies total protein concentration based on the color change produced by the reaction of protein with BCA reagent, which is measured via absorbance.

    <h4>Steps in Quantification</h4>
    <ol>
      <li><b>Blank Subtraction:</b> For each sample, the average blank absorbance is subtracted:
        <br><code>Corrected Absorbance = Sample Mean − Blank Mean</code>
      </li>
      <li><b>Standard Curve Regression:</b> A regression function <i>f(x)</i> is estimated from known analyte concentrations and their corrected absorbance values.</li>
      <li><b>Protein Quantification:</b> Apply the regression to the corrected absorbance:
        <br><code>Protein (µg) = f(Corrected Absorbance)</code>
      </li>
    </ol>

    <h4>Outputs</h4>
    <ul>
      <li><b>Quantification (µg):</b> Estimated protein content per sample.</li>
      <li><b>Standard Curve:</b> Used to interpolate protein concentration from absorbance.</li>
    </ul>
  ")
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
                             textInput(inputId = "BCA_UGvalue_init", label = "How much lysate did you add to the protein quantitation solution? (ug)", value = "5")
                      ),
                      column(width = 4,
                             textInput(inputId = "BCA_UGvalue" , label = "Write desidered protein quantity (ug)", value = "")
                      ),
                      column(width = 4,
                             actionButton(inputId = "confirmBCA_UGvalue", label= "Confirm")
                      )
                    ),
                    selectInput("BCA_ratio_direction", "Ratio to calculate",
                      choices = c("Target / baseline" = "target_over_baseline",
                                  "Baseline / target" = "baseline_over_target")),
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
        div(class = "if-header-box",
            h2(icon("vial"), " ELISA Analysis", style = "margin-top: 0;"),
            p(class = "if-description",
              "The Enzyme-Linked Immunosorbent Assay (ELISA) analysis module enables accurate and reproducible 
              quantification of analytes. Users can upload an Excel file containing absorbance readings and standard 
              curve concentrations. The module automatically fits calibration curves, calculates sample concentrations 
              while accounting for dilution factors, and performs statistical comparisons across experimental conditions, 
              providing a detailed and reliable quantification of the analyte of interest in the sample."
            )
        ),
        fluidRow(
          column(12,
                 div(class = "if-upload-box",
                     h3(icon("cloud-upload-alt"), " Upload Your Data", style = "color: #0066cc; margin-top: 0;"),
                     p(style = "color: #666; margin-bottom: 20px;",
                       "Select one or more Excel files containing your ELISA measurements. Files should include sample absorbance values, 
                       experimental conditions, replicates, and standard curve data."
                     ),
                     fluidRow(
                       column(9,
                              fileInput(
                                inputId = "ELISAImport",
                                label = "",
                                placeholder = "Click to browse or drag files here...",
                                width = "100%", 
                                multiple = TRUE,
                                accept = c(".xlsx", ".xls")
                              )
                       ),
                       column(3,
                              actionButton(
                                label = "Load Data",
                                style = "margin-top: 0px; width: 100%; background-color: #0066cc; color: white; 
                                         border: none; padding: 12px; font-size: 16px; font-weight: bold;
                                         border-radius: 5px; transition: all 0.3s ease;",
                                icon = shiny::icon("upload"),
                                inputId = "LoadELISA_Button"
                              )
                       )
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
            uiOutput("tablesELISA")
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
                    plotOutput("ELISAbarPlot"),
                    actionButton("Customize_plot_ELISA", "Customize or Download Bar Plot"),
                    tags$div(style = "margin-top: 20px;"),
                    DTOutput("ELISAtables"),
                    fluidRow(
                      column(width = 1,offset = 9,
                             downloadButton(label = "Download Analysis & Excel", 
                                            outputId = "downloadELISAAnalysis",
                                            icon = icon("download"))
                      )
                    )
                )
              )
      ),
      #### END data analysis: ELISA ####
      
      ## BEGIN data analysis:  ENDOC ####
      tabItem(
        tabName = "uploadENDOC",
        div(class = "if-header-box",
            h2(icon("exchange-alt"), " Endocytosis Analysis", style = "margin-top: 0;"),
            p(class = "if-description",
              "The Endocytosis Analysis module quantifies the percentage of surface proteins internalized over a given 
              time frame. Users can upload an Excel file containing fluorescence intensity measurements from 
              endocytosis experiments. The module calculates internalization rates, normalizes data to control conditions, 
              performs statistical comparisons, and generates time-course or dose–response visualizations for 
              comprehensive analysis."
            )
        ),
        fluidRow(
          column(12,
                 div(class = "if-upload-box",
                     h3(icon("cloud-upload-alt"), " Upload Your Data", style = "color: #0066cc; margin-top: 0;"),
                     p(style = "color: #666; margin-bottom: 20px;",
                       "Select one or more Excel files containing your endocytosis measurements. Files should include fluorescence values, 
                       experimental conditions, time points, and biological replicates."
                     ),
                     fluidRow(
                       column(9,
                              fileInput(
                                inputId = "ENDOCImport",
                                label = "",
                                placeholder = "Click to browse or drag files here...",
                                width = "100%", 
                                multiple = TRUE,
                                accept = c(".xlsx", ".xls")
                              )
                       ),
                       column(3,
                              actionButton(
                                label = "Load Data",
                                style = "margin-top: 0px; width: 100%; background-color: #0066cc; color: white; 
                                         border: none; padding: 12px; font-size: 16px; font-weight: bold;
                                         border-radius: 5px; transition: all 0.3s ease;",
                                icon = shiny::icon("upload"),
                                inputId = "LoadENDOC_Button"
                              )
                       )
                     )
                 )
          )
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
                              selectizeInput("ENDOCcell_SN",
                                             label = "Experimental condition:",
                                             choices = c(),  
                                             options = list(create = TRUE))
                       )
                     ),
                     fluidRow(
                       column(width = 8, offset = 2,
                              selectizeInput("ENDOCcell_EXP",
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
                              selectizeInput(inputId = "ENDOC_blanks",
                                             label = "Select black:",
                                             choices = NULL)
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
            uiOutput("tablesENDOC")
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
                    title = "Selected blank for the experimental conditions",
                    collapsible = TRUE,
                    collapsed = TRUE,
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
        div(class = "if-header-box",
            h2(icon("skull-crossbones"), " Cytotoxicity Assay Analysis", style = "margin-top: 0;"),
            p(class = "if-description",
              "Cytotoxicity assays evaluate the proportion of cell death induced by a tested experimental compound. Users 
              can upload an Excel file containing absorbance or fluorescence measurements from MTT, LDH, or similar 
              cytotoxicity assays. ORCA calculates the percentage of dead cells, enabling IC₅₀ determination, dose–
              response curve analysis, and comparison of cytotoxic effects across different treatments."
            )
        ),
        fluidRow(
          column(12,
                 div(class = "if-upload-box",
                     h3(icon("cloud-upload-alt"), " Upload Your Data", style = "color: #0066cc; margin-top: 0;"),
                     p(style = "color: #666; margin-bottom: 20px;",
                       "Select one or more Excel files containing your cytotoxicity assay data. Files should include absorbance/fluorescence 
                       values, experimental conditions, concentrations, and replicate information."
                     ),
                     fluidRow(
                       column(9,
                              fileInput(
                                inputId = "CYTOTOXImport",
                                label = "",
                                placeholder = "Click to browse or drag files here...",
                                width = "100%", 
                                multiple = TRUE,
                                accept = c(".xlsx", ".xls")
                              )
                       ),
                       column(3,
                              actionButton(
                                label = "Load Data",
                                style = "margin-top: 0px; width: 100%; background-color: #0066cc; color: white; 
                                         border: none; padding: 12px; font-size: 16px; font-weight: bold;
                                         border-radius: 5px; transition: all 0.3s ease;",
                                icon = shiny::icon("upload"),
                                inputId = "LoadCYTOTOX_Button"
                              )
                       )
                     )
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
            uiOutput("tablesCYTOTOX")   
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
                    selectInput("CYTOTOX_plot_customize_selection", "Plot selection", c("Plot 1", "Plot 2")),
                    actionButton("Customize_plot_CYTOTOX", "Customize or Download Plot"),
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
              fluidRow(
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
                    #plotOutput("facs_autoPlot",height = "800px")
                  )
              )),
              fluidRow(
                # Nuova sezione per la selezione gerarchica
                box(width = 12, title = "Interactive Hierarchical Selection", status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             selectInput("facs_sampleSelector", 
                                         "Select Sample (Name):",
                                         choices = character(0),
                                         selected = NULL)
                      ),
                      column(4,
                             selectInput("facs_hierarchySelector", 
                                         "Hierarchical level:",
                                         choices = character(0),
                                         selected = NULL)
                      )
                    ),
                    
                    fluidRow(
                      column(8,
                             plotOutput("facs_previewPlot", height = "300px"),
                             br(),
                             actionButton("CustomizePlotFACS", 
                                          "Customize Plot", 
                                          icon = icon("paint-brush"), 
                                          class = "btn-secondary",
                                          style = "width: 30%; margin-top: 10px;")
                      ),
                      column(4,
                             br(),
                             actionButton("facs_openSelectionModal", 
                                          "Open Interactive Selection", 
                                          class = "btn-info",
                                          icon = icon("mouse-pointer"),
                                          style = "width: 100%;"),
                             actionButton("facs_clearSelection",
                                          "Remove Level and Sons",
                                          class = "btn-danger",
                                          icon = icon("eraser"),
                                          style = "width: 100%; margin-top: 10px;"),
                             br(),
                             div(style = "display: flex; gap: 10px; align-items: center; margin-top: 10px;",
                                 textInput("facs_hierarchyName", "Hierarchical Name", placeholder = "Enter hierarchy name", width = "70%"),
                                 actionButton("facs_saveSelections", "Save", class = "btn-success", style = "width: 30%;")
                             ),
                             div(style = "display: flex; gap: 10px; align-items: center; margin-top: 10px;",
                                 uiOutput("facs_hierarchySelectorUI", style = "width: 70%;"),
                                 actionButton("facs_loadSelections", "Apply", class = "btn-info", style = "width: 30%;")
                             )
                      )
                    ),
                    
                    fluidRow(
                      column(4,
                             div(style = "margin-top: 10px;",
                                 selectInput("facs_sampleSelector_to_norm", 
                                             "Select Sample (Name) to normalize:",
                                             choices = character(0),
                                             selected = NULL)
                             )
                      ),
                      column(4,
                             div(style = "margin-top: 10px;",
                                 selectInput("facs_sampleSelector_normalizer", 
                                             "Select Sample (Name) of normalizer:",
                                             choices = character(0),
                                             selected = NULL)
                             )
                      ),
                      column(3,
                             actionButton("facs_normalizeButton", 
                                          "Normalize", 
                                          class = "btn-warning", 
                                          icon = icon("balance-scale"), 
                                          style = "width: 100%; margin-top: 35px;")
                      )
                    ),
                    # Pannello informativo delle selezioni
                    conditionalPanel(
                      condition = "output.facs_hasSelections",
                      hr(),
                      h4("Selections Created:", style = "color: #337ab7;"),
                      div(style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; margin-top: 10px; overflow-x: auto;",
                          tableOutput("facs_selectionsInfo")
                      )
                    )
                ),
                column(width = 12,
                       div(style = "display: flex; justify-content: flex-end; gap: 10px; margin-top: 10px;",
                           downloadButton(label = "Download Analysis & Excel", 
                                          outputId = "downloadRawFacsAnalysis",
                                          icon = icon("download")),
                           actionButton(inputId = "NextFacsHierarchicalGating",
                                        label = 'Proceed to Hierarchical gating',
                                        icon = shiny::icon("forward"))
                       )
                )
              )
      ),
      tabItem(
        tabName = "uploadFACS",
        div(class = "if-header-box",
            h2(icon("dot-circle"), " Flow Cytometry Analysis", style = "margin-top: 0;"),
            p(class = "if-description",
              "The Flow Cytometry Analysis module enables the separation and quantification of distinct cell populations 
              starting from a heterogeneous sample. Users can upload an Excel file containing pre-gated cell population 
              percentages or counts. The module supports hierarchical gating strategies, calculates population 
              frequencies, performs statistical comparisons across experimental conditions, and generates detailed 
              visualizations for comprehensive data analysis."
            )
        ),
        fluidRow(
          column(12,
                 div(class = "if-upload-box",
                     h3(icon("cloud-upload-alt"), " Upload Your Data", style = "color: #0066cc; margin-top: 0;"),
                     p(style = "color: #666; margin-bottom: 20px;",
                       "Select one or more Excel files containing your flow cytometry data. Files should include gated population percentages, 
                       experimental conditions, and sample identifiers."
                     ),
                     fluidRow(
                       column(9,
                              fileInput(
                                inputId = "FACSImport",
                                label = "",
                                placeholder = "Click to browse or drag files here...",
                                width = "100%", 
                                multiple = TRUE,
                                accept = c(".xlsx", ".xls")
                              )
                       ),
                       column(3,
                              actionButton(
                                label = "Load Data",
                                style = "margin-top: 0px; width: 100%; background-color: #0066cc; color: white; 
                                         border: none; padding: 12px; font-size: 16px; font-weight: bold;
                                         border-radius: 5px; transition: all 0.3s ease;",
                                icon = shiny::icon("upload"),
                                inputId = "LoadFACS_Button"
                              )
                       )
                     )
                 )
          )
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
        tags$head(
          tags$style(HTML("
            .if-header-box {
              background: linear-gradient(135deg, #0066cc 0%, #003d7a 100%);
              color: white;
              padding: 30px;
              border-radius: 10px;
              margin-bottom: 30px;
              box-shadow: 0 4px 12px rgba(0, 102, 204, 0.3);
            }
            .if-description {
              font-size: 16px;
              line-height: 1.6;
              margin-top: 15px;
              color: #e6f2ff;
            }
            .if-upload-box {
              background: white;
              padding: 30px;
              border-radius: 10px;
              box-shadow: 0 2px 8px rgba(0, 102, 204, 0.15);
              border: 2px dashed #0066cc;
              transition: all 0.3s ease;
            }
            .if-upload-box:hover {
              border-color: #003d7a;
              box-shadow: 0 4px 12px rgba(0, 102, 204, 0.25);
              transform: translateY(-2px);
            }
            .if-features {
              margin-top: 20px;
            }
            .if-feature-item {
              padding: 10px 0;
              border-left: 3px solid #0066cc;
              padding-left: 15px;
              margin: 10px 0;
              background-color: #f5f9fc;
              border-radius: 0 5px 5px 0;
            }
          "))
        ),
        div(class = "if-header-box",
            h2(icon("microscope"), " Fluorescence Microscopy Analysis", style = "margin-top: 0;"),
            p(class = "if-description",
              "Welcome to the Fluorescence Microscopy Analysis module. This tool enables robust and quantitative 
              analysis of fluorescence microscopy data. Users can upload an Excel file containing fluorescence intensity 
              measurements from different cellular compartments, experimental conditions, and biological replicates. 
              The module automatically computes descriptive statistics, performs comparative analyses across conditions, 
              and generates publication-ready visualizations."
            )
        ),
        fluidRow(
          column(12,
                 div(class = "if-upload-box",
                     h3(icon("cloud-upload-alt"), " Upload Your Data", style = "color: #0066cc; margin-top: 0;"),
                     p(style = "color: #666; margin-bottom: 20px;",
                       "Select one or more Excel files containing your IF quantification data. Each file should include experimental conditions, 
                       cellular compartments, and measured fluorescence values."
                     ),
                     fluidRow(
                       column(9,
                              fileInput(
                                inputId = "IFImport",
                                label = "",
                                placeholder = "Click to browse or drag files here...",
                                width = "100%", 
                                multiple = TRUE,
                                accept = c(".xlsx", ".xls")
                              )
                       ),
                       column(3,
                              actionButton(
                                label = "Load Data",
                                style = "margin-top: 0px; width: 100%; background-color: #0066cc; color: white; 
                                         border: none; padding: 12px; font-size: 16px; font-weight: bold;
                                         border-radius: 5px; transition: all 0.3s ease;",
                                icon = shiny::icon("upload"),
                                inputId = "LoadIF_Button",
                                class = "btn-primary"
                              )
                       )
                     )
                 )
          )
        ),

        tags$style(type='text/css', "#loadAnalysis_Button { width:100%; margin-top: 20px;}")
      ),
      tabItem(tabName = "tablesIF",
              h2("Quantification"),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "IF_expcond",
                    label = "Experimental condition:",
                    choices = ""
                  )
                ),
                column(
                  width = 8,
                  style = "padding-top: 25px;", 
                  checkboxInput("IF_calc_perc", "Calculate percentages (e.g., Nucleus vs Cytoplasm)", value = TRUE)
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
                          choices = "",
                          selected = ""
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
                        tags$div(
                          style = "background-color: #f0f8ff; padding: 10px; border-left: 4px solid #0066cc; margin-bottom: 15px; border-radius: 4px;",
                          tags$p(
                            style = "margin: 0; color: #333;",
                            icon("info-circle"), 
                            tags$strong(" Tip: "),
                            "Click on rows in the table below to select/deselect which comparisons to display in the plot. By default, all comparisons are shown."
                          )
                        ),
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
                           fluidRow(
                             actionButton("Customize_StatPlotButton", "Customize or Download Plot", 
                                          icon = icon("paint-brush"), class = "btn-primary")
                           ),
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


