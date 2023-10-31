#shiny.maxRequestSize=1000*1024^2
#shiny.launch.browser = .rs.invokeShinyWindowExternal

Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
APIkey_path = system.file("Data",".APIkey", package = "OCA")

#source(system.file("Shiny","AuxFunctions.R", package = "OCA"))
# source("./inst/Shiny/AuxFunctions.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  DataAnalysisModule <- reactiveValues(wbResult = NULL,
                                       wbquantResult = NULL,
                                       endocResult = NULL,
                                       elisaResult = NULL,
                                       pcrResult = NULL,
                                       cytotoxResult = NULL)
  
  DataIntegrationModule <- reactiveValues(dataLoaded = NULL,
                                          data = NULL,
                                          wbTabs = NULL, 
                                          pcrTabs = NULL,
                                          cytotoxTabs= NULL,
                                          endocTabs=NULL,
                                          otherTabs = NULL,
                                          otherTabsMean = NULL)
  
  MapAnalysisNames =c("WB", "WB comparison", "Endocytosis", "ELISA", "RT-qPCR", "Cytotoxicity") 
  names(MapAnalysisNames) =c("wbResult", "wbquantResult", "endocResult", "elisaResult", "pcrResult", "cytotoxResult") 
  
  ### WB analysis ####
  # DECLARE REACTIVEVALUES FUNCTION HERE
  PanelData = data.frame(SampleName = character(),
                         xmin = numeric(), ymin = numeric(), 
                         xmax = numeric(), ymax = numeric())
  
  wbResult <- reactiveValues(
                             Normalizer = NULL,
                             Im = NULL,
                             Planes = NULL,
                             TruncatedPanelsValue = NULL,
                             PanelsValue = NULL,
                             Plots = NULL,
                             TruncatedPlots = NULL,
                             pl = NULL,
                             AUCdf=data.frame(SampleName = "-", Truncation = "-", AUC = "-"  ))
  
  wbResult0 <- list(
                         Normalizer = NULL,
                         Im = NULL,
                         Planes = NULL,
                         TruncatedPanelsValue = NULL,
                         PanelsValue = NULL,
                         Plots = NULL,
                         TruncatedPlots = NULL,
                         pl = NULL,
                         AUCdf=data.frame(SampleName = "-", Truncation = "-", AUC = "-" )
  )
  
  
  # save everytime there is a change in the results
  WBresultList <- reactive({
    reactiveValuesToList(wbResult)
  })
  observeEvent(WBresultList(), {
    DataAnalysisModule$wbResult = reactiveValuesToList(wbResult)
  })
  
  Flags <- reactiveValues( ShowTif = F, 
                           LanesCut = F,
                           CutTab="V",
                           IDlane = 0)
  prev_vals <- NULL
  PanelStructures <- reactiveValues(data = PanelData )
  NumberOfPlanes <- reactiveValues(N = 0)
  PlaneSelected <- reactiveValues(First = NULL)
  
  observeEvent(input$LoadingTif,{
    output$LoadingError <- renderText({
      validate(
        need(!is.null(input$imImport) && file.exists(input$imImport$datapath) ,
             "Please select a tif file!!" )
      )
      
      mess = readfile(
        filename = input$imImport$datapath,
        type = "tif"
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]] )
      )
      
      Flags$ShowTif <- TRUE
      wbResult$Im = mess
      
      updateTabsetPanel(session, "SideTabs",
                        selected = "plane")
      
      "The image has been uploaded  with success"
    })
    
    if( !is.null(wbResult$Im) )
    { ### alert!!! if it is already present! 
      showModal(modalDialog(
        title = "Important message",
        "Do you want to update the WB data already present, by resetting the previous analysis?",
        easyClose = TRUE,
        footer= tagList(actionButton("confirmUpload", "Update"),
                        modalButton("Cancel")
        )
      ))
    }
  })
  
  observeEvent(input$confirmUpload,{
    DataAnalysisModule$wbResult = wbResult0
    
    for(j in names(wbResult))
      wbResult[[j]] = wbResult0[[j]]
    
    output$AUC <- renderDT({
      wbResult$AUCdf %>% 
        dplyr::select(SampleName,Truncation, AUC)},
      selection = 'none',  
      rownames= FALSE,
      editable = list(target = "cell", 
                      disable = list(columns = 1:4)))
    
    
    output$LoadingError <- renderText({
      validate(
        need(!is.null(input$imImport) && file.exists(input$imImport$datapath) ,
             "Please select a tif file!!" )
      )
      
      mess = readfile(
        filename = input$imImport$datapath,
        type = "tif"
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]] )
      )
      
      Flags$ShowTif <- TRUE
      wbResult$Im = mess
      
      updateTabsetPanel(session, "SideTabs",
                        selected = "plane")
      
      "The image has been uploaded  with success"
    })
    
    removeModal()
  })
  
  observe({
    if(Flags$ShowTif)
    {
      #output$LoadingError <- renderText({      })
      #ListIm = LoadImage(input$imImport$datapath)
      wbResult$Im -> ListIm 
      im = ListIm$RGB
      
      #output$TifPlot  <- renderPlot({ imageShow(im) })
      output$TifPlot2 <- renderPlot({ 
        plot(c(1,dim(im)[2]),c(1,dim(im)[1]), type='n',ann=FALSE)
        rasterImage(im,1,1,dim(im)[2],dim(im)[1])
        if (nrow(PanelStructures$data) > 0) {
          r <- PanelStructures$data
          rect(r$xmin, r$ymin, r$xmax, r$ymax, border = "red")
        }
      })
      Flags$ShowTif = FALSE
    }
  })
  
  observeEvent(input$panelSelect_button,{
    e <- input$plot_brush
    if (!is.null(e)) {
      vals <- data.frame(xmin = round(e$xmin, 1),
                         ymin = round(e$ymin, 1),
                         xmax = round(e$xmax, 1),
                         ymax = round(e$ymax, 1))
      
      if (!identical(vals,prev_vals))  #We dont want to change anything if the values havent changed.
      {
        NumberOfPlanes$N = NumberOfPlanes$N + 1
        if(NumberOfPlanes$N > 1){
          # we set the same height in the new panel
          vals$ymax = prev_vals$ymax
          vals$ymin = prev_vals$ymin
          
          newH = vals$ymax - vals$ymin
          newW = vals$xmax - vals$xmin
          prevH = prev_vals$ymax - prev_vals$ymin
          prevW = prev_vals$xmax - prev_vals$xmin
          # print(paste0("newH: ",newH,"; prevH: ",prevH,
          #              "; newW: ",newW,"; prevW: ", prevW))
          # print(paste0("Res: ", (newH - prevH) + (newW - prevW) ))
          if( abs((newH - prevH) + (newW - prevW)) > 1 )
          {
            NumberOfPlanes$N = 1
            PanelStructures$data <- data.frame(SampleName = "1",vals)
          }else{
            PanelStructures$data <- rbind(PanelStructures$data,
                                          cbind(data.frame(
                                            SampleName = paste(nrow(PanelStructures$data)+1) ),
                                            vals))
          }
        }else{
          PanelStructures$data <- rbind(PanelStructures$data,
                                        cbind(data.frame(
                                          SampleName = paste(nrow(PanelStructures$data)+1) ),
                                          vals))
        }
        prev_vals <<- vals
      }
    }
  })
  
  observeEvent(input$ResetPan,{
    NumberOfPlanes$N = 1
    PanelStructures$data <- PanelData
  })
  
  output$rectCoordOutput <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "Mouse hover: ", xy_str(input$plot_hover),
      "New band coordinates: ", xy_range_str(input$plot_brush)
    )
    
  })
  
  ## Profile plots
  
  output$PlanesStructureTable <- renderDT(
    {
      datatable(PanelStructures$data,
                editable = list(target = "cell", 
                                disable = list(columns = 1:4)),
                options = list(lengthChange = FALSE, autoWidth = TRUE),
                rownames= FALSE
      )
    }
  )
  
  # check names Planes
  observeEvent(input$PlanesStructureTable_cell_edit, {
    cells = input$PlanesStructureTable_cell_edit
    
    data = PanelStructures$data %>% filter(SampleName == PanelStructures$data[cells$row,"SampleName"])
    
    # we check that the same LANE should not have same names.
    if(cells$value %in% data$SampleName){
      k = table(data$SampleName)[cells$value]
      cells$value = paste0(cells$value, " (",k,")")
    }
    cells$col = 1
    
    PanelStructures$data <- editData( PanelStructures$data , cells, 'PlanesStructureTable')
    wbResult$Planes = PanelStructures$data
  })
  
  observeEvent(wbResult$AUCdf,{
    output$AUC <-  renderDT({
      wbResult$AUCdf %>%
        dplyr::select(SampleName,Truncation, AUC)
    },
    selection = 'none',
    rownames= FALSE)
  })
  
  
  #observeEvent(list(input$GenLanes,wbResult$Planes),{
  observeEvent(input$GenLanes,{
    if(NumberOfPlanes$N >1){
      Planes = PanelStructures$data
      Planes[,-1] = round(Planes[,-1])
      wbResult$Planes = Planes
      print(Planes)
      Flags$LanesCut= T
    }else{
      Flags$LanesCut= F
    }
    
    if(Flags$LanesCut)
    {
      updateTabsetPanel(session, "SideTabs",
                        selected = "grey")
      
      im = wbResult$Im$WB
      PanelData = wbResult$Planes
      
      PanelsValue = do.call("rbind",
                            lapply(1:dim(PanelData)[1],
                                   function(i,im,PanelData){
                                     p = PanelData[i,]
                                     Nrow = dim(im)[1]
                                     Ncol= dim(im)[2]
                                     plane = abs(im[(Nrow-p$ymax):(Nrow-p$ymin),p$xmin:p$xmax]-1)
                                     #imageShow(plane)
                                     GreyPlane = apply(plane,1,"mean")
                                     data.frame(Values = GreyPlane,# - min(GreyPlane),
                                                ID = paste0(i,". ",p$SampleName),
                                                Y = 1:length(GreyPlane) )
                                   },
                                   im,PanelData)
      )
      
      
      pl<-ggplot(PanelsValue, aes(x =Y,y=Values)) +
        geom_line() + theme_bw() +
        facet_wrap(~ID)
      
      wbResult$PanelsValue <- PanelsValue
      wbResult$Plots <- pl
      
      updateSelectInput(session, "LaneChoice",
                        choices = unique(PanelsValue$ID),
                        selected = 0
      )
      
      output$DataPlot <- renderPlot({pl})
      
    }
  })
  
  # reset all the truncation analysis
  observeEvent(input$actionButton_ResetPlanes,{
    
    wbResult$AUCdf = wbResult0$AUCdf
    wbResult$TruncatedPanelsValue = wbResult0$TruncatedPanelsValue
    wbResult$TruncatedPlots = wbResult0$TruncatedPlots 
    
    output$AUC <-  renderDT({wbResult$AUCdf %>%
        dplyr::select(SampleName,Truncation, AUC)},
        selection = 'none', 
        rownames= FALSE,
        # editable = list(target = "cell", 
        #                 disable = list(columns = 1:4))
    )
    
    output$DataPlot <- renderPlot({wbResult$Plots})
    
  })
  
  observeEvent(list(input$LaneChoice),{
    if(Flags$LanesCut & !is.null(wbResult$PanelsValue))
    {
      if(!is.null(wbResult$TruncatedPanelsValue ))
      {
        pl <- wbResult$TruncatedPlots    
        wbResult$TruncatedPanelsValue -> PanelsValue
      }
      else{
        wbResult$PanelsValue -> PanelsValue
        pl<-wbResult$Plots
      }
      
      Plots.Lane <- PanelsValue[which(PanelsValue$ID == input$LaneChoice),]
      colnames(Plots.Lane) = c("Y","ID","X")
      
      cat(input$LaneChoice,"\n")
      updateSliderInput(session,"truncV",
                        min = min(Plots.Lane$X),
                        max = max(Plots.Lane$X),
                        value = c(min(Plots.Lane$X), max(Plots.Lane$X) ) ) 
      updateSliderInput(session,"truncH",
                        min = round(min(Plots.Lane$Y),digits = 3),
                        max = round(max(Plots.Lane$Y),digits = 3),
                        value = round(min(Plots.Lane$Y),digits = 3) )
      
    }
    
  } )  
  
  observe({  Flags$CutTab <- input$tabs })
  
  observeEvent(c(input$truncV,input$truncH,input$LaneChoice), {
    if(!is.null(wbResult$PanelsValue))
    {
      if(!is.null(wbResult$TruncatedPanelsValue ))
      {
        pl <- wbResult$TruncatedPlots    
        wbResult$TruncatedPanelsValue -> PanelsValue
      }
      else{
        wbResult$PanelsValue -> PanelsValue
        pl<-wbResult$Plots
      }
      
      IDlane = input$LaneChoice
      Flags$IDlane <- IDlane
      
      if(Flags$CutTab=="V")
      {
        MinTrunc<-input$truncV[1]
        MaxTrunc<-input$truncV[2]
        
        vline.dat <- data.frame( ID = as.factor(rep(PanelsValue$ID,2)),
                                 vl = 0 )
        
        vline.dat  <-  vline.dat[ vline.dat$ID == IDlane, ]
        vline.dat$vl <- c(MinTrunc,MaxTrunc)
        
        pl <- pl + geom_vline(data=vline.dat,aes(xintercept=vl),linetype="dashed")
      }else if(Flags$CutTab=="H")
      {
        TruncY<-input$truncH[1]
        hline.dat <- data.frame(ID=as.factor(PanelsValue$ID), hl =min(PanelsValue$Y))
        hline.dat  <-  hline.dat[ hline.dat$ID == IDlane, ]
        hline.dat$hl <- TruncY
        
        pl <- pl + geom_hline(data=hline.dat,aes(yintercept = hl),linetype="dashed")
      }
      
      output$DataPlot <- renderPlot({pl})
      
      ### AUC calculation of the whole lane without cuts:
      wbResult$AUCdf <- AUCfunction(wbResult$AUCdf,PanelsValue,SName = IDlane)
    }  
  })
  
  observeEvent(c(input$actionButton_TruncV,input$actionButton_TruncH),{
    
    if( !is.null(wbResult$PanelsValue))
    {
      Flags$IDlane -> IDlane
      if(!is.null(wbResult$TruncatedPanelsValue ))
      {
        pl <- wbResult$TruncatedPlots    
        wbResult$TruncatedPanelsValue -> PanelsValue
      }
      else{
        wbResult$PanelsValue -> PanelsValue
        pl<-wbResult$Plots
      }
      
      wbResult$AUCdf -> AUCdf
      AUCdf.new <- AUCdf[length(AUCdf$Truncation),]
      #AUCdf.new$ExpName = "-"
      lastTrunc = AUCdf %>% 
        group_by(SampleName) %>%
        filter(SampleName == IDlane, row_number()==n() ) %>%
        ungroup() %>%
        dplyr::select(Truncation) 
      
      if(length(lastTrunc$Truncation) > 0 & lastTrunc$Truncation != "-")
        AUCdf.new$Truncation <- lastTrunc$Truncation
      
      AUCdf.new$SampleName <- IDlane
      
      if(Flags$CutTab=="V")
      {
        MinTrunc<-input$truncV[1]
        MaxTrunc<-input$truncV[2]
        AUCdf.new$Truncation <- paste(AUCdf.new$Truncation ,";\n X = [", MinTrunc," ; ", MaxTrunc ,"]",collapse = "")
        PanelsValue<- PanelsValue[!((PanelsValue$Y < MinTrunc | PanelsValue$Y > MaxTrunc) & PanelsValue$ID == IDlane),]
        PanelsValue$Values[PanelsValue$ID == IDlane] <- PanelsValue$Values[PanelsValue$ID == IDlane] -min(PanelsValue$Values[PanelsValue$ID == IDlane]) 
        pl <- ggplot(PanelsValue, aes(x =Y,y=Values)) +
          geom_line() + theme_bw() +
          facet_wrap(~ID)
        updateSliderInput(session,"truncV",
                          min = min(PanelsValue$Y[PanelsValue$ID == IDlane]),
                          max = max(PanelsValue$Y[PanelsValue$ID == IDlane]),
                          value = c(min(PanelsValue$Y[PanelsValue$ID == IDlane]),
                                    max(PanelsValue$Y[PanelsValue$ID == IDlane]) ) )
      }
      else if(Flags$CutTab=="H")
      {
        TruncY<-input$truncH[1]
        PanelsValue <- PanelsValue[!(PanelsValue$Values<TruncY & PanelsValue$ID == IDlane),]
        PanelsValue$Values[PanelsValue$ID == IDlane] <- PanelsValue$Values[PanelsValue$ID == IDlane] - TruncY
        AUCdf.new$Truncation <- paste(AUCdf.new$Truncation ,";\n Y = ", TruncY)
        pl <- ggplot(PanelsValue, aes(x =Y,y=Values)) +
          geom_line() + theme_bw() +
          facet_wrap(~ID)
        updateSliderInput(session,"truncH",
                          min = min(PanelsValue$Values[PanelsValue$ID == IDlane]),
                          max = max(PanelsValue$Values[PanelsValue$ID == IDlane]),
                          value = c(min(PanelsValue$Values[PanelsValue$ID == IDlane]),
                                    max(PanelsValue$Values[PanelsValue$ID == IDlane]) ) )
      }
      
      wbResult$TruncatedPanelsValue <- PanelsValue
      wbResult$TruncatedPlots <- pl
      output$DataPlot <- renderPlot({pl})
      AUCdf<-AUCfunction(AUCdf.new=AUCdf.new,wbResult$AUCdf,PanelsValue,SName = IDlane)
      
      output$AUC <- renderDT({
        AUCdf  %>% 
          dplyr::select(SampleName,Truncation, AUC) 
      },
      selection = 'none', 
      rownames= FALSE
      # editable = list(target = "cell", 
      #                 disable = list(columns = 1:4))
      )
      wbResult$AUCdf <- AUCdf
    }
  })
  
  # # edit AUC sample name
  # observeEvent(input$AUC_cell_edit, {
  #   cells = input$AUC_cell_edit
  #   wbResult$AUCdf -> AUCdf
  #   AUCdf = AUCdf %>% filter(SampleName == AUCdf[cells$row,"SampleName"])
  #   
  #   # we check that the same LANE should not have same names.
  #   if(cells$value %in% AUCdf$ExpName){
  #     k = table(AUCdf$ExpName )[cells$value]
  #     cells$value = paste0(cells$value, " (",k,")")
  #   }
  #   cells$col = 1
  #   wbResult$AUCdf <- editData( wbResult$AUCdf , cells, 'AUC')
  # })
  # 
  ## next buttons
  observeEvent(input$NextWBQuantif,{
    if(!is.null(wbResult$AUCdf))
      wbquantResult$WBanalysis = reactiveValuesToList(wbResult)
    
    updateTabsetPanel(session, "SideTabs",
                      selected = "quantification")
  })
  
  ## quantification WB
  wbquantResult = reactiveValues(NormWBanalysis = NULL,
                                 NormWBanalysis_filtered = NULL,
                                 WBanalysis = NULL,
                                 WBanalysis_filtered = NULL,
                                 RelDensitiy = NULL,
                                 AdjRelDensitiy = NULL
  )
  wbquantResult0 = list(NormWBanalysis = NULL,
                                 NormWBanalysis_filtered = NULL,
                                 WBanalysis = NULL,
                                 WBanalysis_filtered = NULL,
                                 RelDensitiy = NULL,
                                 AdjRelDensitiy = NULL
  )
  FlagsWBquant = reactiveValues(BothUploaded = F)
  
  # load the two wb analyses
  observeEvent(input$actionB_loadingWB,{
    
    output$LoadingErrorWB <- renderText({
      validate(
        need(!is.null(input$WBImport) && file.exists(input$WBImport$datapath) ,
             "Please select a RDs file!!" )
      )
      
      mess = readfile(
        filename = input$WBImport$datapath,
        type = "RDs",
        namesAll = namesAll
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]])
      )
      
      wbquantResult$WBanalysis = mess
      wbquantResult$WBanalysis_filtered = NULL
      
      "The RDs has been uploaded  with success"
    })
    
  })
  observeEvent(input$actionB_loadingNormWB,{
    output$LoadingErrorNormWB <- renderText({
      validate(
        need(!is.null(input$NormWBImport) && file.exists(input$NormWBImport$datapath) ,
             "Please select a RDs file!!" )
      )
      
      mess = readfile(
        filename = input$NormWBImport$datapath,
        type = "RDs",
        namesAll = namesAll
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]])
      )
      
      validate(
        need(!is.null(mess$AUCdf) ,
             "The WB analysis must contain the AUC table")
      )
      
      choices = paste0(mess$AUCdf$SampleName, " with ", mess$AUCdf$Truncation)
      
      wbquantResult$NormWBanalysis  = mess
      
      "The RDs has been uploaded  with success"
    })
    
  })
  
  observe({
    if(!is.null(wbquantResult$WBanalysis) & !is.null(wbquantResult$NormWBanalysis))
      FlagsWBquant$BothUploaded = T
  })
  
  # edit AUC_WB and AUC_WBnorm sample name
  # observeEvent(input$AUC_WB_cell_edit, {
  #   cells = input$AUC_WB_cell_edit
  #   cells$col = cells$col + 1
  #   wbquantResult$WBanalysis$AUCdf -> AUCdf
  #   AUCdf = AUCdf %>% filter(SampleName == AUCdf[cells$row,"SampleName"])
  #   
  #   # we check that the same LANE should not have same names.
  #   if(cells$value %in% AUCdf$SampleName){
  #     k = table(AUCdf$SampleName )[cells$value]
  #     cells$value = paste0(cells$value, " (",k,")")
  #   }
  #   
  #   wbquantResult$WBanalysis$AUCdf <- editData( wbquantResult$WBanalysis$AUCdf ,
  #                                               cells, 'AUC_WB')
  # })
  # observeEvent(input$AUC_WBnorm_cell_edit, {
  #   cells = input$AUC_WBnorm_cell_edit
  #   cells$col = cells$col + 1
  #   wbquantResult$NormWBanalysis$AUCdf -> AUCdf
  #   AUCdf = AUCdf %>% filter(Lane == AUCdf[cells$row,"Lane"])
  #   
  #   # we check that the same LANE should not have same names.
  #   if(cells$value %in% AUCdf$ExpName){
  #     k = table(AUCdf$ExpName )[cells$value]
  #     cells$value = paste0(cells$value, " (",k,")")
  #   }
  #   
  #   wbquantResult$NormWBanalysis$AUCdf <- editData( wbquantResult$NormWBanalysis$AUCdf ,
  #                                                   cells, 'AUC_WBnorm')
  #   updateSelectInput("IdLaneNorm_RelDens",
  #                     session = session,
  #                     choices = "Nothing selected",
  #                     selected = "Nothing selected")
  # })
  # 
  # update the wb tables
  observe({
    if(is.null(wbquantResult$NormWBanalysis)){
      table = wbResult0$AUCdf
    }else{
      table = wbquantResult$NormWBanalysis$AUCdf
    }
    output$AUC_WBnorm <- renderDT(
      table , 
      #filter = 'top', server = FALSE, 
      selection = "multiple", 
      # editable = list(target = "cell", 
      #                 disable = list(columns = 1:3)),
      options = list(lengthChange = FALSE, autoWidth = TRUE),
      rownames= FALSE
    )
  })
  observe({
    if(is.null(wbquantResult$WBanalysis)){
      table = wbResult0$AUCdf
    }else{
      table = wbquantResult$WBanalysis$AUCdf
    }
    output$AUC_WB <- renderDT(
      table,
      #filter = 'top', server = FALSE, 
      selection = "multiple", 
      # editable = list(target = "cell", 
      #                 disable = list(columns = 1:3)),
      options = list(lengthChange = FALSE, autoWidth = TRUE),
      rownames= FALSE
    )
  })
  
  # selecting rows
  observeEvent(input$AUC_WB_rows_selected,{
    if(!is.null(wbquantResult$WBanalysis) ){
      indexesWB = input$AUC_WB_rows_selected
      AUCdf = wbquantResult$WBanalysis$AUCdf
      
      if(length(indexesWB) > 0){
        wbquantResult$WBanalysis_filtered = AUCdf[indexesWB,]
      }else{
        wbquantResult$WBanalysis_filtered = AUCdf
      }
    }
  })
  observeEvent(input$AUC_WBnorm_rows_selected,{
    if(!is.null(wbquantResult$NormWBanalysis)){
      indexesWB = input$AUC_WBnorm_rows_selected
      AUCdf = wbquantResult$NormWBanalysis$AUCdf
      
      if(length(indexesWB) > 0){
        wbquantResult$NormWBanalysis_filtered = AUCdf[indexesWB,]
        
        choices = paste0( AUCdf[indexesWB,]$SampleName, "; truncated ", AUCdf[indexesWB,]$Truncation)
        selected = input$IdLaneNorm_RelDens
        updateSelectInput("IdLaneNorm_RelDens",
                          session = session,
                          choices = choices,
                          selected = selected)
      }else{
        wbquantResult$NormWBanalysis_filtered = AUCdf
      }
    }
  })
  
  # the relative density and adjusted is calculated
  observeEvent(list(FlagsWBquant$BothUploaded, input$IdLaneNorm_RelDens,input$AUC_WB_rows_selected,input$AUC_WBnorm_rows_selected),{
    table =  wbResult0$AUCdf 
    
    if(!is.null(wbquantResult$WBanalysis_filtered) & !is.null(wbquantResult$NormWBanalysis_filtered)){
      IdLaneNorm_RelDens = input$IdLaneNorm_RelDens
      IdLaneNorm_RelDens = strsplit(IdLaneNorm_RelDens,
                                    split = "; truncated ")[[1]]
      
      tbWBnorm = wbquantResult$NormWBanalysis_filtered %>%
        filter(SampleName ==IdLaneNorm_RelDens[1],
               Truncation == IdLaneNorm_RelDens[2]) %>%
        rename(AUC_Norm = AUC,
               Truncation_Norm = Truncation,
               SampleName_Norm = SampleName)
      
      tbWB = wbquantResult$WBanalysis_filtered
      
      if(!is.null(tbWBnorm) & !is.null(tbWB) & dim(tbWBnorm)[1]==1 ){
        if(!all(table(tbWB$SampleName)==1) ){
          output$LoadingErrorWB <- renderText({"No rows with equal sample name are allowed"})
        }
        else{ # we admit only one SampleName
          table = tbWB
          table$AUC_Norm = tbWBnorm$AUC_Norm
          table$RelDens = table$AUC/table$AUC_Norm
          table = table %>%
            dplyr::select(SampleName, Truncation, AUC, AUC_Norm, RelDens) 
        }
      }
    }
    
    wbquantResult$RelDensitiy = table
    
    output$AUC_RelDens <- renderDT(
      table,
      filter = 'top',
      server = FALSE,
      options = list(lengthChange = FALSE, autoWidth = TRUE),
      rownames= FALSE
    )
    
  })
  
  observeEvent(list(FlagsWBquant$BothUploaded, input$AUC_WB_rows_selected,input$AUC_WBnorm_rows_selected),{
    table = data.frame(SampleName = "-",
                       Truncation = "-", 
                       Truncation_Norm = "-",
                       AUC = "-", 
                       AUC_Norm = "-",
                       AdjRelDens = "-")
    
    if(!is.null(wbquantResult$WBanalysis_filtered) & !is.null(wbquantResult$NormWBanalysis_filtered)){
      
      tbWB = wbquantResult$WBanalysis_filtered
      tbWBnorm = wbquantResult$NormWBanalysis_filtered
      
      if(!all(table(tbWBnorm$SampleName)==1) ){
        output$LoadingErrorNormWB <- renderText({"No rows with equal sample name are allowed"})
      }else if(!all(table(tbWB$SampleName)==1) ){
        output$LoadingErrorWB <- renderText({"No rows with equal sample name are allowed"})
      }
      else{ # we admit only one SampleName
        
        tbWBnorm = tbWBnorm  %>%
          rename(AUC_Norm = AUC,
                 Truncation_Norm = Truncation)
        
        table = merge(tbWBnorm,tbWB, by = "SampleName" ,all = T )
        
        table$AdjRelDens = table$AUC/table$AUC_Norm
        table = table %>% 
          dplyr::select( SampleName, Truncation, Truncation_Norm, AUC, AUC_Norm, AdjRelDens) 
        
        wbquantResult$AdjRelDensitiy = table
        output$AUC_AdjRelDens <- renderDT(
          table ,
          server = FALSE,
          options = list(lengthChange = FALSE, autoWidth = TRUE),
          rownames= FALSE
        )
        
        if(dim(table)[1] > 1 ){
          barPlotAdjRelDens = table %>% 
            mutate(Normalizer = paste0("Sample: ",SampleName ),
                   WB = paste0("Sample: ",SampleName))  %>%
            ggplot() +
            geom_bar(aes(x = SampleName,
                         y = AdjRelDens,
                         fill = Normalizer ),
                     stat = "identity" ) +
            #facet_grid(~WB)+
            theme_bw()
        }else{
          barPlotAdjRelDens = ggplot()
        }
        output$plot_AdjRelDens <- renderPlot({
          barPlotAdjRelDens
        })
      }
    }
  })
  
  #
  toListenWBquant <- reactive({
    reactiveValuesToList(wbquantResult)
  })
  observeEvent(toListenWBquant(),{
    DataAnalysisModule$wbquantResult = reactiveValuesToList(wbquantResult)
  })
  
  ### End WB analysis ####
  
  #### PCR analysis ####
  
  pcrResult = reactiveValues(
                             Initdata = NULL,
                             selectPCRcolumns = NULL,
                             data = NULL,
                             PCRnorm = NULL,
                             BaselineExp = NULL,
                             CompPRC = NULL,
                             NewPCR = NULL)
  pcrResult0 = list(
                    Initdata = NULL,
                    selectPCRcolumns = NULL,
                    data = NULL,
                    PCRnorm = NULL,
                    BaselineExp = NULL,
                    CompPRC = NULL,
                    NewPCR = NULL)
  
  # save everytime there is a change in the results
  PCRresultListen <- reactive({
    reactiveValuesToList(pcrResult)
  })
  observeEvent(PCRresultListen(), {
    DataAnalysisModule$pcrResult = reactiveValuesToList(pcrResult)
  })
  
  ## next buttons
  observeEvent(input$NextQuantif,{
    updateTabsetPanel(session, "SideTabs",
                      selected = "tablesPCR")
  })
  observeEvent(input$NextpcrPlots,{
    updateTabsetPanel(session, "SideTabs",
                      selected = "plotsPCR")
  })
  #
  
  FlagsPCR <- reactiveValues(norm=F, 
                             baseline = F)
  
  observeEvent(input$LoadPCR_Button,{
    
    if( !is.null(pcrResult$Initdata) )
    { ### alert!!! if it is already present! 
      showModal(modalDialog(
        title = "Important message",
        "Do you want to update the RT-PCR data already present?",
        easyClose = TRUE,
        footer= tagList(actionButton("confirmUploadPCR", "Update"),
                        modalButton("Cancel")
        )
      ))
    }
    
    output$LoadingError_PCR <- renderText({
      validate(
        need(!is.null(input$PCRImport) && file.exists(input$PCRImport$datapath) ,
             "Please select an RT-qPCR excel file!!" )
      )
      
      mess = readfile(
        filename = input$PCRImport$datapath,
        type = "Excel"
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]] )
      )
      
      pcrResult$Initdata = mess
      
      updateSelectInput(session,"PCR_gene",
                        choices = c("",colnames(pcrResult$Initdata)),
                        selected = ""
      )
      updateSelectInput(session,"PCR_sample",
                        choices = c("",colnames(pcrResult$Initdata)),
                        selected = ""
      )
      updateSelectInput(session,"PCR_value",
                        choices = c("",colnames(pcrResult$Initdata)),
                        selected = ""
      )
      
      "The RT-qPCR excel has been uploaded  with success"
    })
  })
  observeEvent(input$confirmUploadPCR,{
    
    pcrResult = pcrResult0
    
    FlagsPCR$norm=F 
    FlagsPCR$baseline = F
    
    output$PCRtables <- renderUI({ NULL })
    output$PCRtablesComp <- renderUI({ NULL })
    output$PCRplot <- renderPlot({ NULL })
    
    output$LoadingError_PCR <- renderText({
      validate(
        need(!is.null(input$PCRImport) && file.exists(input$PCRImport$datapath) ,
             "Please select an RT-PCR excel file!!" )
      )
      
      mess = readfile(
        filename = input$PCRImport$datapath,
        type = "Excel"
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]])
      )
      
      pcrResult$Initdata = mess
      
      updateSelectInput(session,"PCR_gene",
                        choices = c("",colnames(pcrResult$Initdata)),
                        selected = ""
      )
      updateSelectInput(session,"PCR_sample",
                        choices = c("",colnames(pcrResult$Initdata)),
                        selected = ""
      )
      updateSelectInput(session,"PCR_value",
                        choices = c("",colnames(pcrResult$Initdata)),
                        selected = ""
      )
      
      removeModal()
      
      "The RDs has been uploaded  with success"
    })
  })
  
  
  observeEvent(list(input$PCR_value,input$PCR_gene,input$PCR_sample),{
    if( !is.null(pcrResult$Initdata) ){
      selectPCRcolumns = c(input$PCR_gene,input$PCR_sample,input$PCR_value)
      selectPCRcolumns = selectPCRcolumns[selectPCRcolumns!= ""]
      
      PCR = pcrResult$Initdata
      colNames = colnames(PCR)
      output$PCRpreview = renderTable({
        if(length(selectPCRcolumns)!=0 ){
          tmp = PCR[,selectPCRcolumns]
          colnames(tmp) = c("Gene", "Sample", "Value")[1:length(colnames(tmp))]
          head(tmp) 
        }
        else
          NULL
      })
      
      if(length(selectPCRcolumns)==3 ){
        tmp = PCR[,selectPCRcolumns]
        colnames(tmp) = c("Gene", "Sample", "Value")[1:length(colnames(tmp))]
        pcrResult$data = tmp
        pcrResult$selectPCRcolumns = selectPCRcolumns
      }else{
        pcrResult$data = NULL
      }
    }
    
  })
  
  observe({
    if(!is.null(pcrResult$data)){
      
      PCR = pcrResult$data
      
      AllGenes = unique(PCR$Gene)
      Exp = unique(PCR$Sample)
      
      updateSelectInput(session, "PCRbaseline",
                        choices = Exp )
      updateCheckboxGroupInput(session,"PCRnorm",
                               choices = AllGenes )
      
    }else{
      updateSelectInput(session, "PCRbaseline",
                        choices = "" )
      updateCheckboxGroupInput(session,"PCRnorm",
                               choices = "" )
    }
  })
  
  observeEvent(input$PCRnorm,{
    pcrResult$PCRnorm = input$PCRnorm
    FlagsPCR$norm = T
  })
  observeEvent(input$PCRbaseline,{
    pcrResult$BaselineExp = input$PCRbaseline
    FlagsPCR$baseline = T
  })
  
  observe({
    if(FlagsPCR$baseline & FlagsPCR$norm & !is.null(pcrResult$data)){
      
      pcrResult$BaselineExp -> BaselineExp
      pcrResult$PCRnorm -> PCRnorm
      pcrResult$data -> PCR
      
      NewPCR = PCR %>%
        group_by(Sample,Gene) %>%
        dplyr::summarise(Mean = mean(Value),
                         Sd = sd(Value)) %>%
        ungroup()
      
      BaselinePCR = NewPCR %>% 
        filter(Sample == BaselineExp) %>%
        rename(BaselineMean=Mean, BaselineSd=Sd) %>%
        dplyr::select(-Sample)
      
      NewPCR = merge(BaselinePCR,NewPCR,all.y = T,by="Gene")
      
      NewPCR = NewPCR %>%
        group_by(Sample,Gene) %>%
        dplyr::summarise(dCt = Mean - BaselineMean,
                         Q = 2^{-dCt},
                         Sd = Sd,
                         Mean = Mean)%>%
        ungroup()
      
      OnePCR = NewPCR %>%
        filter(!Gene %in% PCRnorm)
      
      NormPCR = NewPCR %>%
        filter(Gene %in% PCRnorm ) %>%
        rename(Norm = Gene,
               Norm_dCt = dCt,
               NormQ = Q,
               NormSd = Sd,
               NormMean = Mean)
      
      CompPRC = merge(OnePCR,NormPCR)
      
      CompPRC = CompPRC %>% group_by(Sample,Gene,Norm) %>%
        dplyr::summarise(Qnorm = Q/NormQ,
                         SDddct = sqrt(Sd^2+NormSd^2),
                         SDrq = log(2)*Qnorm*SDddct) %>%
        ungroup()
      
      AllGenes = unique(PCR$Gene)
      
      pcrResult$CompPRC = CompPRC
      pcrResult$NewPCR = NewPCR
      
      output$PCRtables <- renderUI({
        plot_output_list <- lapply(AllGenes, function(i) {
          tablename <- paste("tablename", i, sep="")
          tableOutput(tablename)
        })
        do.call(tagList, plot_output_list)
      })
      output$PCRtablesComp <- renderUI({
        plot_output_list <- lapply(AllGenes[-which(AllGenes %in% PCRnorm)], function(i) {
          tablename <- paste("CompTablename", i, sep="")
          tableOutput(tablename)
        })
        do.call(tagList, plot_output_list)
      })
      output$PCRplot <- renderPlot({
        ggplot(data = CompPRC, aes(x= Gene, y = Qnorm, fill = Sample)) + 
          facet_wrap(~Norm, ncol = 1) +
          geom_bar(stat = "identity",position = "dodge")
        
      })
      
      for (i in AllGenes){
        local({
          my_i <- i
          tablename <- paste("tablename", my_i, sep="")
          output[[tablename]] <- renderTable({
            NewPCR %>% filter(Gene == my_i)
          })
          
          ComparisonPCR = list()
          if(my_i %in% AllGenes[-which(AllGenes %in% PCRnorm)]){
            tablename <- paste("CompTablename", my_i, sep="")
            output[[tablename]] <- renderTable({
              CompPRC %>% 
                filter(Gene == my_i) %>%
                arrange(Norm,Sample)
            })
          }
        })    
      }
      
    }
  })
  
  observe({
    DataAnalysisModule$pcrResult = reactiveValuesToList(pcrResult)
  })
  
  #### END PCR analysis ####
  
  ### ENDOC analysis ####
  
  # next buttons
  observeEvent(input$NextEndocQuantif,{
    updateTabsetPanel(session, "SideTabs",
                      selected = "tablesENDOC")
  })
  #
  
  endocResult = reactiveValues(
                               Initdata= NULL,
                               data = NULL,
                               TablePlot = NULL,
                               dataFinal = NULL,
                               ENDOCcell_TIME = NULL,
                               ENDOCcell_EXP = NULL,
                               MapBaseline = NULL,
                               MapBlanche = NULL)
  
  endocResult0 = list(
                      Initdata= NULL,
                      data = NULL,
                      TablePlot = NULL,
                      dataFinal = NULL,
                      ENDOCcell_TIME = NULL,
                      ENDOCcell_EXP = NULL,
                      MapBaseline = NULL,
                      MapBlanche = NULL)
  
  # save everytime there is a change in the results
  ENDOCresultListen <- reactive({
    reactiveValuesToList(endocResult)
  })
  observeEvent(ENDOCresultListen(), {
    DataAnalysisModule$endocResult = reactiveValuesToList(endocResult)
  })
  
  ##
  FlagsENDOC <- reactiveValues(cellCoo = NULL,
                               AllExp = "",
                               BASEselected = "",
                               BLANCHEselected = "",
                               EXPselected = "",
                               EXPcol = NULL)
  
  observeEvent(input$LoadENDOC_Button,{
    output$LoadingError_ENDOC <- renderText({
      validate(
        need(!is.null(input$ENDOCImport) && file.exists(input$ENDOCImport$datapath) ,
             "Please select an ENDOC excel file!!" )
      )
      
      mess = readfile(
        filename = input$ENDOCImport$datapath,
        type = "Excel",
        allDouble = T,
        colname = F
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]] )
      )
      
      endocResult$Initdata = mess
      
      "The ENDOC excel has been uploaded  with success"
    })
    
    if( !is.null(endocResult$Initdata) )
    { ### alert!!! if it is already present! 
      showModal(modalDialog(
        title = "Important message",
        "Do you want to update the ENDOC data already present?",
        easyClose = TRUE,
        footer= tagList(actionButton("confirmUploadENDOC", "Update"),
                        modalButton("Cancel")
        )
      ))
      
    }
  })
  observeEvent(input$confirmUploadENDOC,{
    
    for(nameList in names(endocResult0)) 
      endocResult[[nameList]] <- endocResult0[[nameList]]
    
    output$LoadingError_ENDOC <- renderText({
      validate(
        need(!is.null(input$ENDOCImport) && file.exists(input$ENDOCImport$datapath) ,
             "Please select an ENDOC excel file!!" )
      )
      
      mess = readfile(
        filename = input$ENDOCImport$datapath,
        type = "Excel"
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]])
      )
      
      endocResult$Initdata = mess
      
      removeModal()
      
      "The RDs has been uploaded  with success"
    })
  })
  observe({
    if( !is.null(endocResult$Initdata) && is.null(endocResult$TablePlot) ){
      ENDOCtb = endocResult$Initdata
      
      ENDOCtb.colors = ENDOCtb
      ENDOCtb.colors[,] = ""
      mENDOC  =cbind(ENDOCtb,ENDOCtb.colors)
      
      cols.keep <- paste0('V',1:length(ENDOCtb[1,])) 
      cols.color <- paste0('Col',1:length(ENDOCtb[1,]))
      
      colnames(mENDOC) = c(cols.keep,cols.color)
      
      ENDOCtb = datatable(mENDOC,
                          filter = 'none',
                          #server = FALSE,
                          selection = list(mode = 'single', target = 'cell'),
                          rownames= FALSE,
                          options = list(
                            lengthChange = FALSE,
                            columnDefs = list(list(targets = cols.color, visible = FALSE))
                          )) %>%
        formatStyle(cols.keep,
                    cols.color,
                    backgroundColor = styleEqual("", 'white'))
      
      ENDOC = ENDOCtb$x$data
      
      output$ENDOCmatrix <- renderDT(
        ENDOCtb,
        #filter = 'none',
        server = FALSE,
        #selection = list(mode = 'single', target = 'cell'),
        #options = list(lengthChange = FALSE ),
        #rownames= FALSE
      )
      
      # ind = expand.grid(0:(length(ENDOC[1,])-1),0:(length(ENDOC[,1])-1))
      # ind$Var3 = paste0(ind$Var1,"_",ind$Var2)
      # NumCells = length(ind$Var3)
      
      ENDOCcell_EXP <- ENDOCcell_TIME <- matrix(
        "",
        nrow = length(ENDOC[,1]),
        ncol = length(ENDOC[1,])
      )
      endocResult$ENDOCcell_EXP <- ENDOCcell_EXP
      endocResult$ENDOCcell_TIME<- ENDOCcell_TIME
      endocResult$TablePlot = ENDOCtb
    }
  })
  observeEvent(input$ENDOCmatrix_cell_clicked,{
    if(length(input$ENDOCmatrix_cell_clicked)!=0){
      cellSelected= as.numeric(input$ENDOCmatrix_cell_clicked)
      FlagsENDOC$cellCoo = cellCoo = c(cellSelected[1],cellSelected[2]+1)
      print(cellCoo)
      print(endocResult$ENDOCcell_TIME[ cellCoo[1],cellCoo[2] ])
      print(endocResult$ENDOCcell_EXP[ cellCoo[1], cellCoo[2] ])
      updateSelectizeInput(inputId = "ENDOCcell_TIME",
                           selected = ifelse(is.null(endocResult$ENDOCcell_TIME[cellCoo[1],cellCoo[2]]),"",endocResult$ENDOCcell_TIME[cellCoo[1],cellCoo[2]])
      )
      updateSelectizeInput(inputId = "ENDOCcell_EXP",
                           selected = ifelse(is.null(endocResult$ENDOCcell_EXP[cellCoo[1],cellCoo[2]]),"",endocResult$ENDOCcell_EXP[cellCoo[1],cellCoo[2]])
      )
    }
  })
  
  observeEvent(input$ENDOCcell_TIME,{
    if(!is.null(endocResult$ENDOCcell_TIME)){
      cellCoo = FlagsENDOC$cellCoo
      endocResult$ENDOCcell_TIME[cellCoo[1],cellCoo[2]] = input$ENDOCcell_TIME
    }
  })
  observeEvent(input$ENDOCcell_EXP,{
    if(!is.null(endocResult$ENDOCcell_EXP)){
      ENDOCtb = endocResult$TablePlot
      cellCoo = FlagsENDOC$cellCoo
      if(!is.null(cellCoo)){
        endocResult$ENDOCcell_EXP[cellCoo[1],cellCoo[2]] = input$ENDOCcell_EXP
        ENDOCtb$x$data[cellCoo[1],paste0("Col",cellCoo[2])] = input$ENDOCcell_EXP
        
        if(! input$ENDOCcell_EXP %in% FlagsENDOC$AllExp){
          FlagsENDOC$AllExp = unique(c(FlagsENDOC$AllExp,input$ENDOCcell_EXP))
          print(FlagsENDOC$AllExp)
        }
        
        EXPcol = rainbow(n = length(FlagsENDOC$AllExp),alpha = 0.4)
        names(EXPcol) = FlagsENDOC$AllExp
        EXPcol[names(EXPcol) == ""] = "white"
          FlagsENDOC$EXPcol = EXPcol
          print(FlagsENDOC$EXPcol)
          cols.color = grep(x = colnames(ENDOCtb$x$data),pattern = "Col",value = T)
          cols.keep = grep(x = colnames(ENDOCtb$x$data),pattern = "V",value = T)
          endocResult$TablePlot = datatable(ENDOCtb$x$data,
                                            filter = 'none',
                                            #server = FALSE,
                                            selection = list(mode = 'single', target = 'cell'),
                                            rownames= FALSE,
                                            options = list(
                                              lengthChange = FALSE,
                                              columnDefs = list(list(targets = cols.color, visible = FALSE))
                                            )) %>%
            formatStyle(cols.keep,
                        cols.color,
                        backgroundColor = styleEqual(names(EXPcol), EXPcol))
          
      }
    }
  })
  
  ## update Baselines checkBox
  observeEvent(c(FlagsENDOC$AllExp,FlagsENDOC$BLANCHEselected),{
    if(length(FlagsENDOC$AllExp) > 1){
      exp = FlagsENDOC$AllExp
      exp = exp[exp != ""]
      
      if(!( length(FlagsENDOC$BLANCHEselected) == 1 && FlagsENDOC$BLANCHEselected == "") )
        exp = exp[!exp %in% FlagsENDOC$BLANCHEselected]
      
      exp_selec = input$ENDOC_baselines
      
      updateCheckboxGroupInput(session,"ENDOC_baselines",
                               choices = exp,
                               selected = exp_selec )
      
      FlagsENDOC$EXPselected = exp
      
    }
  })
  
  observeEvent(c(FlagsENDOC$AllExp,FlagsENDOC$BASEselected),{
    if(length(FlagsENDOC$AllExp) > 1){
      exp = FlagsENDOC$AllExp
      exp = exp[exp != ""]
      
      if(! (length(FlagsENDOC$BASEselected) == 1 && FlagsENDOC$BASEselected == "") )
        exp = exp[!exp %in% FlagsENDOC$BASEselected]
      
      exp_selec = input$ENDOC_blanches
      
      updateCheckboxGroupInput(session,"ENDOC_blanches",
                               choices = exp,
                               selected = exp_selec )
      
      FlagsENDOC$EXPselected = exp
    }
  })
  
  ## select the baselines and blanche
  observeEvent(input$ENDOC_baselines,{
    FlagsENDOC$BASEselected = input$ENDOC_baselines
    FlagsENDOC$EXPselected = FlagsENDOC$AllExp[! FlagsENDOC$AllExp %in% c(FlagsENDOC$BASEselected,FlagsENDOC$BLANCHEselected)]
  },ignoreNULL = F)
  observeEvent(input$ENDOC_blanches,{
    FlagsENDOC$BLANCHEselected = input$ENDOC_blanches
    FlagsENDOC$EXPselected = FlagsENDOC$AllExp[! FlagsENDOC$AllExp %in% c(FlagsENDOC$BASEselected,FlagsENDOC$BLANCHEselected)]
  },ignoreNULL = F)
  
  toListen_endoc <- reactive({
    exp = FlagsENDOC$EXPselected
    exp = exp[exp != ""]
    if(length(exp) > 0 )
    {
      Input_baselEXP = lapply(exp,
                              function(i) input[[paste0("Exp",i)]])
      Input_blEXP = lapply(unique(exp,FlagsENDOC$BASELINEselected),
                           function(i) input[[paste0("blExp",i)]] )
      InputEXP = c(Input_baselEXP,Input_blEXP)
      
      which(sapply(InputEXP, is.null)) -> indexesEXPnull
      if(length(indexesEXPnull) > 0 )
        listReturn = InputEXP[-indexesEXPnull]
      else
        listReturn = InputEXP
    }else{
      listReturn = list()
    }
    
    if(length(listReturn) == 0){
      return(list("Nothing",endocResult$ENDOCcell_TIME,endocResult$ENDOCcell_EXP))
    }else{
      return(c(listReturn,list(endocResult$ENDOCcell_TIME,endocResult$ENDOCcell_EXP)) )
    }
  })
  
  observeEvent(toListen_endoc(),{
    baselines = FlagsENDOC$BASEselected
    baselines = baselines[baselines != ""]
    
    if(toListen_endoc()[[1]] != "Nothing"){
      exp = FlagsENDOC$EXPselected
      exp = exp[exp != ""]
      expNotBlanche = unique(c(exp,baselines))
      
      MapBaseline = do.call(rbind,
                            lapply(exp,function(i){
                              if( length(input[[paste0("Exp",i)]]) > 0 && input[[paste0("Exp",i)]] != ""){
                                data.frame(Exp = i, Baseline = input[[paste0("Exp",i)]])
                              }else{
                                data.frame(Exp = i, Baseline = NA)
                              }
                            })
      ) %>% na.omit()
      
      MapBlanche = do.call(rbind,
                           lapply(expNotBlanche,
                                  function(i){
                                    if( length(input[[paste0("blExp",i)]]) > 0 && input[[paste0("blExp",i)]] != ""){
                                      data.frame(Exp = i, Blanche = input[[paste0("blExp",i)]])
                                    }else{
                                      data.frame(Exp = i, Blanche = NA)
                                    }
                                  })
      ) %>% na.omit()
      
      endocResult$MapBaseline = MapBaseline
      endocResult$MapBlanche = MapBlanche
      
      mat = as.matrix(endocResult$Initdata)
      endocV = expand.grid(seq_len(nrow(mat)), seq_len(ncol(mat))) %>%
        rowwise() %>%
        mutate(values = mat[Var1, Var2])
      matTime =  as.matrix(endocResult$ENDOCcell_TIME)
      endocT = expand.grid(seq_len(nrow(matTime)), seq_len(ncol(matTime))) %>%
        rowwise() %>%
        mutate(time = matTime[Var1, Var2])
      matExp =  as.matrix(endocResult$ENDOCcell_EXP)
      endocE = expand.grid(seq_len(nrow(matExp)), seq_len(ncol(matExp))) %>%
        rowwise() %>%
        mutate(exp = matExp[Var1, Var2])
      endocTot = merge(endocV,merge(endocT,endocE)) %>%
        filter(exp != "")
      
      endocTotAverage = endocTot %>%
        mutate(time = ifelse(exp %in% MapBlanche$Blanche, 0, time)) %>%
        group_by(time, exp) %>%
        summarize(meanValues = mean(values))
      
      # merging exp with blanche for the substraction
      
      endocTot_bl = right_join( endocTotAverage,MapBlanche, 
                                by= c("exp"= "Blanche") )%>%
        rename(BlancheValues = meanValues, Blanche =  exp, exp = Exp )
      
      endocTotAverage = merge( endocTotAverage %>% filter(! exp %in%endocTot_bl$Blanche ),
                               endocTot_bl %>% ungroup() %>% dplyr::select(-time), all.x = T, by = "exp") 
      endocTotAverage = endocTotAverage %>% mutate(meanValues = meanValues - BlancheValues )
      
      # merging exp with baseline
      endocTot_base = merge(MapBaseline, endocTotAverage,
                            by.y = "exp", by.x = "Baseline") %>%
        rename(BaseValues = meanValues) %>% select(-Blanche,-BlancheValues)
      
      endocTot_base = merge(endocTotAverage, endocTot_base, 
                            by.x = c("exp","time"), by.y = c("Exp","time") )
      
      endocResult$data = endocTot
      
      if(length(endocTotAverage[,1]) != 0 ){
        endocmean = endocTot_base %>%
          rename( MeanExperiment = meanValues,
                  MeanBaseline = BaseValues ) %>%
          dplyr::mutate(Quantification = MeanExperiment/MeanBaseline * 100) %>%
          rename(Experiment = exp,Time = time) 
        
        output$ENDOCtables = renderDT(endocmean)
        
        endocResult$dataFinal = endocmean
        
        output$ENDOCplots = renderPlot(
          {
            pl1 = endocmean %>%
              ggplot( aes(x = Time, y = MeanBaseline,
                          col= Baseline, group = Experiment ) )+
              geom_point( )+
              geom_line()+
              theme_bw()+
              labs(x = "Time", col = "Baselines selected",
                   y = "Average Baseline\n quantifications")
            
            pl2 = endocmean %>%
              mutate(ExperimentBaseline = paste0(Experiment,"/",Baseline)) %>%
              ggplot( aes(x = Time, y = Quantification,
                          col= ExperimentBaseline, group = Experiment ) )+
              geom_point()+
              geom_line()+
              theme_bw()+
              labs(x = "Time", col = "Ratio\n Experiment / Baseline",
                   y = "Ratio of the average quantifications\n (as %)")
            
            pl2/pl1
          }
        )
      }else{
        output$ENDOCtables = renderDT(data.frame(Error = "No baseline is associated with the experiment replicants!"))
      }
    }
  })
  
  # here the Exp boxes are updated every time a new experiment is added 
  observeEvent(FlagsENDOC$EXPselected,{
    expToselect = FlagsENDOC$EXPselected
    baselines =  FlagsENDOC$BASEselected
    blanches = FlagsENDOC$BLANCHEselected
    
    expToselect = expToselect[expToselect != ""]
    
    # baselines updating
    output$EndocBaselineSelection <- renderUI({
      select_output_list <- lapply(expToselect[! expToselect %in% baselines],
                                   function(i) {
                                     if(length(input[[paste0("Exp",i)]])>0)
                                       expsel = input[[paste0("Exp",i)]]
                                     else 
                                       expsel = ""
                                     
                                     selectInput(inputId = paste0("Exp",i),
                                                 label = i,
                                                 choices = c("",baselines),
                                                 selected = expsel)
                                   })
      do.call(tagList, select_output_list)
    })
    # blanches updating
    output$EndocBlancheSelection <- renderUI({
      select_output_list <- lapply(unique(c(expToselect,baselines)), function(i) {
        
        if(length(input[[paste0("blExp",i)]])>0)
          expsel = input[[paste0("blExp",i)]]
        else 
          expsel = ""
        
        selectInput(inputId = paste0("blExp",i),
                    label = i,
                    choices = c("",blanches),
                    selected = expsel)
      })
      do.call(tagList, select_output_list)
    })
  })
  
  observeEvent(endocResult$TablePlot, {
    ENDOCtb = endocResult$TablePlot
    output$ENDOCmatrix <- renderDT(
      ENDOCtb,
      server = FALSE
    )
    
    ##### Plot the values selected!
    matTime =  as.matrix(endocResult$ENDOCcell_TIME)
    matExp =  as.matrix(endocResult$ENDOCcell_EXP)
    
    if( !( all(matTime == "")  || all(matExp == "") ) ){
      mat = as.matrix(endocResult$Initdata)
      endocV = expand.grid(seq_len(nrow(mat)), seq_len(ncol(mat))) %>%
        rowwise() %>%
        mutate(values = mat[Var1, Var2])
      endocT = expand.grid(seq_len(nrow(matTime)), seq_len(ncol(matTime))) %>%
        rowwise() %>%
        mutate(time = matTime[Var1, Var2])
      endocE = expand.grid(seq_len(nrow(matExp)), seq_len(ncol(matExp))) %>%
        rowwise() %>%
        mutate(exp = matExp[Var1, Var2])
      endocTot = merge(endocV,merge(endocT,endocE)) %>%
        na.omit() %>%
        filter(time != "",  exp != "") 
      
      endocResult$data = endocTot
      
      output$ENDOCinitplots <- renderPlot(
        ggplot(endocTot, aes(x = time, y=values, col = exp),alpha = 1.4) +
          #geom_boxplot(aes(fill= exp, group = time),alpha = 0.4) +
          geom_point(aes(group = exp)) +
          scale_color_manual(values = FlagsENDOC$EXPcol) + 
          #scale_fill_manual(values = FlagsENDOC$EXPcol) + 
          theme_bw()+
          labs(x = "Times", y = "Values", col = "Exp",fill = "Exp")+
          theme(legend.position = c(0, 1), 
                legend.justification = c(0, 1),
                legend.direction = "vertical",
                legend.background = element_rect(size=0.5,
                                                 linetype="solid",
                                                 colour ="black"))
      )
    }
  })
  
  ### End ENDOC analysis ####
  
  ### ELISA analysis ####
  
  # next buttons
  observeEvent(input$NextElisaQuantif,{
    updateTabsetPanel(session, "SideTabs",
                      selected = "tablesELISA")
  })
  #
  
  elisaResult = reactiveValues(
                               Initdata= NULL,
                               data = NULL,
                               TablePlot = NULL,
                               dataFinal = NULL,
                               ELISAcell_EXP = NULL,
                               ELISAcell_SN = NULL,
                               MapBaseline = NULL,
                               MapBlanche = NULL,
                               Tablestandcurve = NULL,
                               Regression = NULL)
  
  elisaResult0 = list(
                      Initdata= NULL,
                      data = NULL,
                      TablePlot = NULL,
                      dataFinal = NULL,
                      ELISAcell_EXP = NULL,
                      ELISAcell_SN = NULL,
                      MapBaseline = NULL,
                      MapBlanche = NULL,
                      Tablestandcurve = NULL,
                      Regression = NULL)
  
  # save everytime there is a change in the results
  ELISAresultListen <- reactive({
    reactiveValuesToList(elisaResult)
  })
  observeEvent(ELISAresultListen(), {
    DataAnalysisModule$elisaResult = reactiveValuesToList(elisaResult)
  })
  
  ##
  FlagsELISA <- reactiveValues(cellCoo = NULL,
                               AllExp = "",
                               BASEselected = "",
                               STDCselected = "",
                               BLANCHEselected = "",
                               EXPselected = "",
                               EXPcol = NULL)
  
  observeEvent(input$LoadELISA_Button,{
    output$LoadingError_ELISA <- renderText({
      validate(
        need(!is.null(input$ELISAImport) && file.exists(input$ELISAImport$datapath) ,
             "Please select an ELISA excel file!!" )
      )
      
      mess = readfile(
        filename = input$ELISAImport$datapath,
        type = "Excel",
        allDouble = T,
        colname = F
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]] )
      )
      
      elisaResult$Initdata = mess
      
      "The ELISA excel has been uploaded  with success"
    })
    
    if( !is.null(elisaResult$Initdata) )
    { ### alert!!! if it is already present! 
      showModal(modalDialog(
        title = "Important message",
        "Do you want to update the ELISA data already present?",
        easyClose = TRUE,
        footer= tagList(actionButton("confirmUploadELISA", "Update"),
                        modalButton("Cancel")
        )
      ))
      
    }
  })
  observeEvent(input$confirmUploadELISA,{
    removeModal()
    
    for(nameList in names(elisaResult0)) 
      elisaResult[[nameList]] <- elisaResult0[[nameList]]
    
    output$LoadingError_ELISA <- renderText({
      validate(
        need(!is.null(input$ELISAImport) && file.exists(input$ELISAImport$datapath) ,
             "Please select an ELISA excel file!!" )
      )
      
      mess = readfile(
        filename = input$ELISAImport$datapath,
        type = "Excel"
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]])
      )
      
      elisaResult$Initdata = mess
      "The RDs has been uploaded  with success"
    })
  })
  observe({
    if( !is.null(elisaResult$Initdata) && is.null(elisaResult$TablePlot) ){
      ELISAtb = elisaResult$Initdata
      
      ELISAtb.colors = ELISAtb
      ELISAtb.colors[,] = ""
      mELISA  =cbind(ELISAtb,ELISAtb.colors)
      
      cols.keep <- paste0('V',1:length(ELISAtb[1,])) 
      cols.color <- paste0('Col',1:length(ELISAtb[1,]))
      
      colnames(mELISA) = c(cols.keep,cols.color)
      
      ELISAtb = datatable(mELISA,
                          filter = 'none',
                          #server = FALSE,
                          selection = list(mode = 'single', target = 'cell'),
                          rownames= FALSE,
                          options = list(
                            lengthChange = FALSE,
                            scrollX = TRUE,
                            columnDefs = list(list(targets = cols.color, 
                                                   visible = FALSE))
                          )) %>%
        formatStyle(cols.keep,
                    cols.color,
                    backgroundColor = styleEqual("", 'white'))
      
      ELISA = ELISAtb$x$data
      
      output$ELISAmatrix <-renderDataTable({ELISAtb} 
                                           #options = list(scrollX = TRUE)
      )
      # renderDataTable(
      #   ELISAtb,
      #   #filter = 'none',
      #   server = FALSE,
      #   options=list(scrollX=T)
      #   #selection = list(mode = 'single', target = 'cell'),
      #   #options = list(lengthChange = FALSE ),
      #   #rownames= FALSE
      # )
      
      ELISAcell_SN <- ELISAcell_EXP <- matrix(
        "",
        nrow = length(ELISA[,1]),
        ncol = length(ELISA[1,])
      )
      elisaResult$ELISAcell_SN <- ELISAcell_SN
      elisaResult$ELISAcell_EXP<- ELISAcell_EXP
      elisaResult$TablePlot = ELISAtb
    }
  })
  observeEvent(input$ELISAmatrix_cell_clicked,{
    if(length(input$ELISAmatrix_cell_clicked)!=0){
      cellSelected= as.numeric(input$ELISAmatrix_cell_clicked)
      FlagsELISA$cellCoo = cellCoo = c(cellSelected[1],cellSelected[2]+1)
      print(cellCoo)
      print(elisaResult$ELISAcell_EXP[ cellCoo[1],cellCoo[2] ])
      print(elisaResult$ELISAcell_SN[ cellCoo[1], cellCoo[2] ])
      updateSelectizeInput(inputId = "ELISAcell_EXP",
                           selected = ifelse(is.null(elisaResult$ELISAcell_EXP[cellCoo[1],cellCoo[2]]),
                                             "",
                                             elisaResult$ELISAcell_EXP[cellCoo[1],cellCoo[2]])
      )
      updateSelectizeInput(inputId = "ELISAcell_SN",
                           selected = ifelse(is.null(elisaResult$ELISAcell_SN[cellCoo[1],cellCoo[2]]),
                                             "",
                                             elisaResult$ELISAcell_SN[cellCoo[1],cellCoo[2]])
      )
    }
  })
  
  observeEvent(input$ELISAcell_EXP,{
    if(!is.null(elisaResult$ELISAcell_EXP)){
      cellCoo = FlagsELISA$cellCoo
      elisaResult$ELISAcell_EXP[cellCoo[1],cellCoo[2]] = input$ELISAcell_EXP
    }
  })
  observeEvent(input$ELISAcell_SN,{
    if(!is.null(elisaResult$ELISAcell_SN)){
      ELISAtb = elisaResult$TablePlot
      cellCoo = FlagsELISA$cellCoo
      if(!is.null(cellCoo)){
        elisaResult$ELISAcell_SN[cellCoo[1],cellCoo[2]] = input$ELISAcell_SN
        ELISAtb$x$data[cellCoo[1],paste0("Col",cellCoo[2])] = input$ELISAcell_SN
        
        if(! input$ELISAcell_SN %in% FlagsELISA$AllExp){
          FlagsELISA$AllExp = unique(c(FlagsELISA$AllExp,input$ELISAcell_SN))
          print(FlagsELISA$AllExp)
        }
        
        EXPcol = rainbow(n = length(FlagsELISA$AllExp),alpha = 0.4)
        names(EXPcol) = FlagsELISA$AllExp
        EXPcol[names(EXPcol) == ""] = "white"
          FlagsELISA$EXPcol = EXPcol
          print(FlagsELISA$EXPcol)
          cols.color = grep(x = colnames(ELISAtb$x$data),pattern = "Col",value = T)
          cols.keep = grep(x = colnames(ELISAtb$x$data),pattern = "V",value = T)
          elisaResult$TablePlot = datatable(ELISAtb$x$data,
                                            filter = 'none',
                                            #server = FALSE,
                                            selection = list(mode = 'single', target = 'cell'),
                                            rownames= FALSE,
                                            options = list(
                                              scrollX = TRUE,
                                              lengthChange = FALSE,
                                              columnDefs = list(list(targets = cols.color, visible = FALSE))
                                            )) %>%
            formatStyle(cols.keep,
                        cols.color,
                        backgroundColor = styleEqual(names(EXPcol), EXPcol))
          
      }
    }
  })
  
  ## update Baselines checkBox
  observeEvent(c(FlagsELISA$AllExp,FlagsELISA$BASEselected,FlagsELISA$BLANCHEselected),{
    if(length(FlagsELISA$AllExp) > 1){
      exp = FlagsELISA$AllExp
      exp = exp[exp != ""]
      
      bool.tmp = exp %in% unique(c(FlagsELISA$BLANCHEselected,FlagsELISA$BASEselected))
      if( length(bool.tmp) > 0  )
        exp = exp[!bool.tmp]
      
      updateSelectizeInput(session,"ELISA_standcurve",
                           choices = exp,
                           selected = ifelse(FlagsELISA$STDCselected %in% exp,FlagsELISA$STDCselected,"") 
      )
    }
  })
  observeEvent(c(FlagsELISA$AllExp,FlagsELISA$BASEselected,FlagsELISA$STDCselected),{
    if(length(FlagsELISA$AllExp) > 1){
      exp = FlagsELISA$AllExp
      exp = exp[exp != ""]
      
      bool.tmp = exp %in% unique(c(FlagsELISA$STDCselected,FlagsELISA$BASEselected))
      if( length(bool.tmp) > 0  )
        exp = exp[!bool.tmp]
      
      updateCheckboxGroupInput(session,"ELISA_blanches",
                               choices = exp,
                               selected = FlagsELISA$BLANCHEselected )
    }
  })
  observeEvent(c(FlagsELISA$AllExp,FlagsELISA$BLANCHEselected,FlagsELISA$STDCselected),{
    if(length(FlagsELISA$AllExp) > 1){
      exp = FlagsELISA$AllExp
      exp = exp[exp != ""]
      
      bool.tmp = exp %in% unique(c(FlagsELISA$STDCselected,FlagsELISA$BLANCHEselected))
      if( length(bool.tmp) > 0  )
        exp = exp[!bool.tmp]
      
      exp_selec = input$ELISA_baselines
      
      updateCheckboxGroupInput(session,"ELISA_baselines",
                               choices = exp,
                               selected = FlagsELISA$BASEselected )
    }
  })
  
  ## select the baselines, std curves, and blanche
  observeEvent(input$ELISA_baselines,{
    FlagsELISA$BASEselected = input$ELISA_baselines
    FlagsELISA$EXPselected = FlagsELISA$AllExp[! FlagsELISA$AllExp %in% c(FlagsELISA$STDCselected,FlagsELISA$BASEselected,FlagsELISA$BLANCHEselected)]
  },ignoreNULL = F)
  observeEvent(input$ELISA_standcurve,{
    FlagsELISA$STDCselected = input$ELISA_standcurve
    FlagsELISA$EXPselected = FlagsELISA$AllExp[! FlagsELISA$AllExp %in% c(FlagsELISA$STDCselected,FlagsELISA$BASEselected,FlagsELISA$BLANCHEselected)]
  },ignoreNULL = F)
  observeEvent(input$ELISA_blanches,{
    FlagsELISA$BLANCHEselected = input$ELISA_blanches
    FlagsELISA$EXPselected = FlagsELISA$AllExp[! FlagsELISA$AllExp %in% c(FlagsELISA$STDCselected,FlagsELISA$BASEselected,FlagsELISA$BLANCHEselected)]
  },ignoreNULL = F)
  
  toListen_elisa <- reactive({
    exp = FlagsELISA$EXPselected
    exp = exp[exp != ""]
    if(length(exp) > 0 )
    {
      Input_baselEXP = lapply(exp,
                              function(i) input[[paste0("elisa_Exp",i)]])
      Input_blEXP = lapply(unique(exp,FlagsELISA$BASELINEselected),
                           function(i) input[[paste0("elisa_blExp",i)]] )
      InputEXP = c(Input_baselEXP,Input_blEXP)
      
      which(sapply(InputEXP, function(x) 
        ifelse(is.null(x), T, ifelse(x == "", T, F) ) ) ) -> indexesEXPnull
      
      if(length(indexesEXPnull) > 0 )
        listReturn = InputEXP[-indexesEXPnull]
      else
        listReturn = InputEXP
    }else{
      listReturn = list()
    }
    
    if(length(listReturn) == 0){
      return(list("Nothing",elisaResult$ELISAcell_EXP,elisaResult$ELISAcell_SN))
    }else{
      return(c(listReturn,list(elisaResult$ELISAcell_EXP,elisaResult$ELISAcell_SN)) )
    }
  })
  observeEvent(toListen_elisa(),{
    baselines = FlagsELISA$BASEselected
    baselines = baselines[baselines != ""]
    
    if(toListen_elisa()[[1]] != "Nothing" ){
      exp = FlagsELISA$EXPselected
      exp = exp[exp != ""]
      expNotBlanche = unique(c(exp,baselines))
      
      MapBaseline = do.call(rbind,
                            lapply(exp,function(i){
                              if( length(input[[paste0("elisa_Exp",i)]]) > 0 && input[[paste0("elisa_Exp",i)]] != ""){
                                data.frame(Exp = i, Baseline = input[[paste0("elisa_Exp",i)]])
                              }else{
                                data.frame(Exp = i, Baseline = NA)
                              }
                            })
      ) %>% na.omit()
      
      MapBlanche = do.call(rbind,
                           lapply(expNotBlanche,
                                  function(i){
                                    if( length(input[[paste0("elisa_blExp",i)]]) > 0 && input[[paste0("elisa_blExp",i)]] != ""){
                                      data.frame(Exp = i, Blanche = input[[paste0("elisa_blExp",i)]])
                                    }else{
                                      data.frame(Exp = i, Blanche = NA)
                                    }
                                  })
      ) %>% na.omit()
      
      elisaResult$MapBaseline = MapBaseline
      elisaResult$MapBlanche = MapBlanche
      
      mat = as.matrix(elisaResult$Initdata)
      elisaV = expand.grid(seq_len(nrow(mat)), seq_len(ncol(mat))) %>%
        rowwise() %>%
        mutate(values = mat[Var1, Var2])
      matTime =  as.matrix(elisaResult$ELISAcell_EXP)
      elisaT = expand.grid(seq_len(nrow(matTime)), seq_len(ncol(matTime))) %>%
        rowwise() %>%
        mutate(time = matTime[Var1, Var2])
      matExp =  as.matrix(elisaResult$ELISAcell_SN)
      elisaE = expand.grid(seq_len(nrow(matExp)), seq_len(ncol(matExp))) %>%
        rowwise() %>%
        mutate(exp = matExp[Var1, Var2])
      elisaTot = merge(elisaV,merge(elisaT,elisaE)) %>%
        filter(exp != "")
      
      elisaTotAverage = elisaTot %>%
        #mutate(time = ifelse(exp %in% MapBlanche$Blanche, 0, time)) %>%
        group_by(time, exp) %>%
        summarize(meanValues = mean(values))
      
      # merging exp with blanche for the substraction
      
      elisaTot_bl = right_join( elisaTotAverage,MapBlanche, 
                                by= c("exp"= "Blanche") )%>%
        rename(BlancheValues = meanValues, Blanche =  exp, exp = Exp )
      
      elisaTotAverage = merge( elisaTotAverage %>% filter( exp %in%elisaTot_bl$exp ),
                               elisaTot_bl %>% ungroup(),all.x = T, by = c("exp","time") ) 
      elisaTotAverage[is.na(elisaTotAverage[,])] = 0
      elisaTotAverage = elisaTotAverage %>% mutate(meanValues = meanValues - BlancheValues )
      
      # merging exp with baseline
      elisaTot_base = merge(MapBaseline, elisaTotAverage,
                            by.y = "exp", by.x = "Baseline",all = T) %>%
        rename(BaseValues = meanValues) %>% select(-Blanche,-BlancheValues)
      
      elisaTot_base = merge(elisaTotAverage, elisaTot_base, 
                            by.x = c("exp","time"), by.y = c("Exp","time"),
                            all.x = T  )
      
      elisaResult$data = elisaTot
      
      if(length(elisaTot_base[,1]) != 0 && !is.null(elisaResult$Regression) ){
        
        # y = m*x + q
        
        q = elisaResult$Regression$data$coefficients[1]
        m = elisaResult$Regression$data$coefficients[2]
        
        elisamean = elisaTot_base %>%
          rename( MeanExperiment = meanValues,
                  MeanBaseline = BaseValues ) %>%
          dplyr::mutate(Quantification = (MeanExperiment -q)/m ) %>%
          #MeanExperiment/MeanBaseline * 100) %>%
          rename(Experiment = exp,Time = time) 
        
        output$ELISAtables = renderDT(elisamean)
        
        elisaResult$dataFinal = elisamean
        
        output$ELISAplots = renderPlot(
          {
            elisamean %>%
              ggplot( aes(x = Time, y = Quantification,
                          fill= Experiment, group = Experiment ) )+
              geom_bar(position = "dodge",stat = "identity")+
              theme_bw()+
              labs(x = "Time", col = "Experiments",
                   y = "Average quantifications obtained\n from the lm ")
          }
        )
      }else{
        output$ELISAtables = renderDT(data.frame(Error = "No linear model!"))
      }
    }
  })
  
  # here the Exp boxes are updated every time a new experiment is added 
  observeEvent(FlagsELISA$EXPselected,{
    expToselect = FlagsELISA$EXPselected
    baselines =  FlagsELISA$BASEselected
    blanches = FlagsELISA$BLANCHEselected
    
    expToselect = expToselect[expToselect != ""]
    
    # baselines updating
    output$ElisaBaselineSelection <- renderUI({
      select_output_list <- lapply(expToselect[! expToselect %in% baselines],
                                   function(i) {
                                     if(length(input[[paste0("elisa_Exp",i)]])>0)
                                       expsel = input[[paste0("elisa_Exp",i)]]
                                     else 
                                       expsel = ""
                                     
                                     selectInput(inputId = paste0("elisa_Exp",i),
                                                 label = i,
                                                 choices = c("",baselines),
                                                 selected = expsel)
                                   })
      do.call(tagList, select_output_list)
    })
    # blanches updating
    output$ElisaBlancheSelection <- renderUI({
      select_output_list <- lapply(unique(c(expToselect,baselines)), function(i) {
        
        if(length(input[[paste0("elisa_blExp",i)]])>0)
          expsel = input[[paste0("elisa_blExp",i)]]
        else 
          expsel = ""
        
        selectInput(inputId = paste0("elisa_blExp",i),
                    label = i,
                    choices = c("",blanches),
                    selected = expsel)
      })
      do.call(tagList, select_output_list)
    })
  })
  observeEvent(c(elisaResult$TablePlot,elisaResult$ELISAcell_EXP),
               {
                 ELISAtb = elisaResult$TablePlot
                 output$ELISAmatrix <-renderDataTable({ELISAtb})
                 
                 ##### Plot the values selected!
                 matTime =  as.matrix(elisaResult$ELISAcell_EXP)
                 matExp =  as.matrix(elisaResult$ELISAcell_SN)
                 
                 if( !( all(matTime == "")  || all(matExp == "") ) ){
                   mat = as.matrix(elisaResult$Initdata)
                   elisaV = expand.grid(seq_len(nrow(mat)), seq_len(ncol(mat))) %>%
                     rowwise() %>%
                     mutate(values = mat[Var1, Var2])
                   elisaT = expand.grid(seq_len(nrow(matTime)), seq_len(ncol(matTime))) %>%
                     rowwise() %>%
                     mutate(time = matTime[Var1, Var2])
                   elisaE = expand.grid(seq_len(nrow(matExp)), seq_len(ncol(matExp))) %>%
                     rowwise() %>%
                     mutate(exp = matExp[Var1, Var2])
                   elisaTot = merge(elisaV,merge(elisaT,elisaE)) %>%
                     na.omit() %>%
                     filter(time != "",  exp != "") 
                   
                   elisaResult$data = elisaTot
                 }
               })
  
  ## linear regression standard curve
  observeEvent(input$ELISA_standcurve,{
    elisaResult$data -> data
    
    if(input$ELISA_standcurve != ""){
      
      standcurve = data %>%
        filter(exp %in% input$ELISA_standcurve) %>%
        # group_by(exp,time) %>%
        # summarise(AverageMeasures = mean(values)) %>%
        # ungroup() %>%
        select(exp,time,values) %>%
        rename(Measures = values) %>%
        mutate(Concentrations = NaN )
      
      # If nothing changes w..r.t. the already saved table then I keep the old one!
      if(!is.null(elisaResult$Tablestandcurve) && 
         all.equal(elisaResult$Tablestandcurve %>% select(-Concentrations),
                   standcurve  %>% select(-Concentrations) ))
      {
        standcurve =  elisaResult$Tablestandcurve
      }else{
        elisaResult$Tablestandcurve = standcurve
      }
      
      
      output$ELISA_Table_stdcurve <- DT::renderDataTable({
        DT::datatable( standcurve,
                       selection = 'none',
                       editable = list(target = "cell",
                                       disable = list(columns = 0:2) ),
                       options = list(lengthChange = FALSE, autoWidth = TRUE),
                       rownames= FALSE
        )
      })
    } 
  })
  observeEvent(input$ELISA_Table_stdcurve_cell_edit, {
    cells = input$ELISA_Table_stdcurve_cell_edit
    cells$col = cells$col + 1
    elisaResult$Tablestandcurve <- editData( elisaResult$Tablestandcurve ,
                                             cells,
                                             'ELISA_Table_stdcurve')
  })
  observeEvent(input$ELISA_buttonRegression,{
    standcurve = elisaResult$Tablestandcurve
    if(!is.null(standcurve)){
      standcurve = standcurve %>% na.omit()
      
      
      regressionPlot = ggplot(standcurve,aes(Concentrations, Measures)) +
        geom_point() +
        theme_bw()
      
      if(input$regressionType == "Linear"){
        lmStancurve = lm(Measures~Concentrations, data = standcurve)
        
        infoLM = data.frame(x = min(standcurve$Concentrations) + c(1,1),
                            y = max(standcurve$Measures) + c(2,1.75),
                            text = c( paste0("y = ", signif(lmStancurve$coef[[2]], 5), "x + ",signif(lmStancurve$coef[[1]],5 )),
                                      paste0("Adj R2 = ",signif(summary(lmStancurve)$adj.r.squared, 5))) )
        
        regressionPlot =  regressionPlot +
          geom_smooth(method='lm', col = "red") +
          geom_text(data= infoLM,
                    aes(x = x, y = y, label =text ),
                    vjust = "inward", hjust = "inward" )
        
      }else if(input$regressionType == "Quadratic"){
        #this is not implemented
        standcurve$Concentrations2 = standcurve$Concentrations^2
        lmStancurve = lm(Measures~Concentrations+Concentrations2, data = standcurve)
        
        infoLM = data.frame(x = min(standcurve$Concentrations) + c(1,1),
                            y = max(standcurve$Measures) + c(2,1.75),
                            text = c( paste0("y = ", signif(lmStancurve$coef[[3]], 5), "x^2 + ",
                                             signif(lmStancurve$coef[[2]], 5), "x + ",signif(lmStancurve$coef[[1]],5 )),
                                      paste0("Adj R2 = ",signif(summary(lmStancurve)$adj.r.squared, 5))) )
        regressionPlot =  regressionPlot  +
          geom_point() +
          stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1,col="red")+
          geom_text(data= infoLM,
                    aes(x = x, y = y, label =text ),
                    vjust = "inward", hjust = "inward" )
      }
      else if(input$regressionType == "Hyperbola"){
        
        outNLreg = tryCatch(
          {
            nlmStancurve<-nls(
              Measures ~ a*Concentrations/(b+Concentrations), 
              data = standcurve, #%>% group_by(Concentrations) %>% summarise(Measures = mean(Measures)),
              start = list(a = 1,b = 1)
             )
          }, 
          error = function(e){
            return(e)
          })
        
        if(!is.null(outNLreg$mess)){
          nlmStancurve = NULL
          regressionPlot = ggplot()+ geom_text(data = data.frame(x = 1,y =1,text = paste0("Error: ",outNLreg$mess)),
                                               aes(x,y,label = text),color = "red")
        }else{
          nlmStancurve = outNLreg
          coef = nlmStancurve$m$getPars()
          r2 = 1- sum(nlmStancurve$m$resid()^2)/(sum(( mean(standcurve$Measures) - nlmStancurve$m$predict() )^2))
          
          infoLM = data.frame(x = min(standcurve$Concentrations) + c(1,1),
                              y = max(standcurve$Measures) + c(2,1.75),
                              text = c( paste0("y = ", signif(coef["a"], 5), "x / ( ",
                                               signif(coef["b"], 5), " + x ) "),
                                        paste0("R2 = ",signif(r2, 5))) )
          
          dfHyperbola = data.frame(x = seq(min(standcurve$Concentrations),max(standcurve$Concentrations),length.out = 20)) %>%
            mutate(y = (coef["a"]*x/((coef["b"]+x)) ) )
          
          regressionPlot =  regressionPlot  +
            geom_point() +
            geom_line(data = dfHyperbola,aes(x = x,y = y),size = 1,col="red" )+
            geom_text(data= infoLM,
                      aes(x = x, y = y, label =text ),
                      vjust = "inward", hjust = "inward" )
        }
      }
      
      elisaResult$Regression = list(data = lmStancurve, plot = regressionPlot)
      
    }else{
      regressionPlot = ggplot()
    }
    output$ELISAregression <- renderPlot(regressionPlot)
  })
  
  # save everytime there is a change in the results
  ELISAresultListen <- reactive({
    reactiveValuesToList(elisaResult)
  })
  observeEvent(ELISAresultListen(), {
    DataAnalysisModule$elisaResult = reactiveValuesToList(elisaResult)
  })
  
  ### End ELISA analysis ####
  
  ### CYTOTOX analysis ####
  
  # next buttons
  observeEvent(input$NextCytotoxQuantif,{
    updateTabsetPanel(session, "SideTabs",
                      selected = "tablesCYTOTOX")
  })
  #
  
  cytotoxResult = reactiveValues(
                                 Initdata= NULL,
                                 data = NULL,
                                 TablePlot = NULL,
                                 dataFinal = NULL,
                                 CYTOTOXcell_EXP = NULL,
                                 CYTOTOXcell_REP = NULL,
                                 CYTOTOXcell_SN = NULL,
                                 MapBaseline = NULL)
  
  cytotoxResult0 = list(
                        Initdata= NULL,
                        data = NULL,
                        TablePlot = NULL,
                        dataFinal = NULL,
                        CYTOTOXcell_EXP = NULL,
                        CYTOTOXcell_REP = NULL,
                        CYTOTOXcell_SN = NULL,
                        MapBaseline = NULL)
  
  
  # save everytime there is a change in the results
  CYTOTOXresultListen <- reactive({
    reactiveValuesToList(cytotoxResult)
  })
  observeEvent(CYTOTOXresultListen(), {
    DataAnalysisModule$cytotoxResult = reactiveValuesToList(cytotoxResult)
  })
  
  ##
  FlagsCYTOTOX <- reactiveValues(cellCoo = NULL,
                                 AllExp = "",
                                 BASEselected = "",
                                 EXPselected = "",
                                 EXPcol = NULL)
  
  observeEvent(input$LoadCYTOTOX_Button,{
    output$LoadingError_CYTOTOX <- renderText({
      validate(
        need(!is.null(input$CYTOTOXImport) && file.exists(input$CYTOTOXImport$datapath) ,
             "Please select an CYTOTOX excel file!!" )
      )
      
      mess = readfile(
        filename = input$CYTOTOXImport$datapath,
        type = "Excel",
        allDouble = T,
        colname = F,
        colors = T
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]] )
      )
      
      cytotoxResult$Initdata = mess$x
      FlagsCYTOTOX$EXPcol = mess$fill
      cytotoxResult$CYTOTOXcell_SN = mess$SNtable
      
      "The CYTOTOX excel has been uploaded  with success"
    })
    
    if( !is.null(cytotoxResult$Initdata) )
    { ### alert!!! if it is already present! 
      showModal(modalDialog(
        title = "Important message",
        "Do you want to update the CYTOTOX data already present?",
        easyClose = TRUE,
        footer= tagList(actionButton("confirmUploadCYTOTOX", "Update"),
                        modalButton("Cancel")
        )
      ))
      
    }
  })
  observeEvent(input$confirmUploadCYTOTOX,{
    removeModal()
    
    for(nameList in names(cytotoxResult0)) 
      cytotoxResult[[nameList]] <- cytotoxResult0[[nameList]]
    
    output$LoadingError_CYTOTOX <- renderText({
      validate(
        need(!is.null(input$CYTOTOXImport) && file.exists(input$CYTOTOXImport$datapath) ,
             "Please select an CYTOTOX excel file!!" )
      )
      
      mess = readfile(
        filename = input$CYTOTOXImport$datapath,
        type = "Excel"
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]])
      )
      
      cytotoxResult$Initdata = mess
      "The RDs has been uploaded  with success"
    })
  })
  observe({
    if( !is.null(cytotoxResult$Initdata) && is.null(cytotoxResult$TablePlot) ){
      
      tableExcelColored(session = session,
                        Result = cytotoxResult, 
                        FlagsExp = FlagsCYTOTOX,
                        type = "Initialize")
      
      output$CYTOTOXmatrix <-renderDataTable({cytotoxResult$TablePlot})
    }
  })
  observeEvent(input$CYTOTOXmatrix_cell_clicked,{
    if(length(input$CYTOTOXmatrix_cell_clicked)!=0){
      cellSelected= as.numeric(input$CYTOTOXmatrix_cell_clicked)
      FlagsCYTOTOX$cellCoo = cellCoo = c(cellSelected[1],cellSelected[2]+1)
      print(cellCoo)
      print(cytotoxResult$CYTOTOXcell_EXP[ cellCoo[1],cellCoo[2] ])
      print(cytotoxResult$CYTOTOXcell_SN[ cellCoo[1], cellCoo[2] ])
      updateSelectizeInput(inputId = "CYTOTOXcell_EXP",
                           selected = ifelse(is.null(cytotoxResult$CYTOTOXcell_EXP[cellCoo[1],cellCoo[2]]),
                                             "",
                                             cytotoxResult$CYTOTOXcell_EXP[cellCoo[1],cellCoo[2]])
      )
      updateSelectizeInput(inputId = "CYTOTOXcell_SN",
                           selected = ifelse(is.null(cytotoxResult$CYTOTOXcell_SN[cellCoo[1],cellCoo[2]]),
                                             "",
                                             cytotoxResult$CYTOTOXcell_SN[cellCoo[1],cellCoo[2]])
      )
      updateSelectizeInput(inputId = "CYTOTOXcell_REP",
                           selected = ifelse(is.null(cytotoxResult$CYTOTOXcell_REP[cellCoo[1],cellCoo[2]]),
                                             "",
                                             cytotoxResult$CYTOTOXcell_REP[cellCoo[1],cellCoo[2]])
      )
    }
  })
  
  observeEvent(input$CYTOTOXcell_EXP,{
    if(!is.null(cytotoxResult$CYTOTOXcell_EXP)){
      cellCoo = FlagsCYTOTOX$cellCoo
      cytotoxResult$CYTOTOXcell_EXP[cellCoo[1],cellCoo[2]] = input$CYTOTOXcell_EXP
    }
  })
  observeEvent(input$CYTOTOXcell_REP,{
    if(!is.null(cytotoxResult$CYTOTOXcell_REP)){
      cellCoo = FlagsCYTOTOX$cellCoo
      cytotoxResult$CYTOTOXcell_REP[cellCoo[1],cellCoo[2]] = input$CYTOTOXcell_REP
    }
  })
  observeEvent(input$CYTOTOXcell_SN,{
    if(!is.null(cytotoxResult$CYTOTOXcell_SN)){
      CYTOTOXtb = cytotoxResult$TablePlot
      cellCoo = FlagsCYTOTOX$cellCoo
      
      if(!is.null(cellCoo)){
        value.bef = cytotoxResult$CYTOTOXcell_SN[cellCoo[1],cellCoo[2]] 
        value.now = input$CYTOTOXcell_SN
        
        # if the value does not change or it is still "Color " then the matrix is not update
        if( !grep(x = value.bef,pattern = "Color ",value = F) && value.now!=value.bef){
          cytotoxResult$CYTOTOXcell_SN[cellCoo[1],cellCoo[2]] = value.now
          CYTOTOXtb$x$data[cellCoo[1],paste0("Col",cellCoo[2])] = value.now
          
          if(! input$CYTOTOXcell_SN %in% FlagsCYTOTOX$AllExp){
            FlagsCYTOTOX$AllExp = unique(c(FlagsCYTOTOX$AllExp,input$CYTOTOXcell_SN))
            print(FlagsCYTOTOX$AllExp)
          }
          
          ## updating table and colors definition depending on the cell fill 
          tableExcelColored(session = session,
                            Result = cytotoxResult, 
                            FlagsExp = FlagsCYTOTOX,
                            type = "Update")
          #####
          output$CYTOTOXmatrix <-renderDataTable({cytotoxResult$TablePlot})
        }
      }
    }
  })
  
  ## update Baselines checkBox
  observeEvent(FlagsCYTOTOX$AllExp,{
    if(length(FlagsCYTOTOX$AllExp) > 1){
      exp = FlagsCYTOTOX$AllExp
      exp = exp[exp != ""]
      
      exp_selec = input$CYTOTOX_baselines
      
      updateSelectizeInput(session,"CYTOTOX_baselines",
                               choices = exp,
                               selected = FlagsCYTOTOX$BASEselected )
    }
  })
  
  ## select the baselines
  observeEvent(input$CYTOTOX_baselines,{
    FlagsCYTOTOX$BASEselected = input$CYTOTOX_baselines
    FlagsCYTOTOX$EXPselected = FlagsCYTOTOX$AllExp[! FlagsCYTOTOX$AllExp %in% FlagsCYTOTOX$BASEselected]
  },ignoreNULL = F)

  toListen_cytotox <- reactive({
    return( list(cytotoxResult$CYTOTOXcell_EXP,cytotoxResult$CYTOTOXcell_SN,FlagsCYTOTOX$BASEselected) )
  })
  observeEvent(toListen_cytotox(),{
    baselines = FlagsCYTOTOX$BASEselected
    baselines = baselines[baselines != ""]
    if(length(baselines) > 0 )
    {
      CYTOTOXcell_value = data.frame(
        row = c(t(row(cytotoxResult$Initdata))),
        col = c(t(col(cytotoxResult$Initdata))),
        Val = c(t(cytotoxResult$Initdata))
      ) 
      CYTOTOXcell_SN = data.frame(
        row = c(t(row(cytotoxResult$CYTOTOXcell_SN))),
        col = c(t(col(cytotoxResult$CYTOTOXcell_SN))),
        SN = c(t(cytotoxResult$CYTOTOXcell_SN))
      ) 
      CYTOTOXcell_EXP = data.frame(
        row = c(t(row(cytotoxResult$CYTOTOXcell_EXP))),
        col = c(t(col(cytotoxResult$CYTOTOXcell_EXP))),
        EXP = c(t(cytotoxResult$CYTOTOXcell_EXP))
      ) 
      CYTOTOXcell_REP = data.frame(
        row = c(t(row(cytotoxResult$CYTOTOXcell_REP))),
        col = c(t(col(cytotoxResult$CYTOTOXcell_REP))),
        REP = c(t(cytotoxResult$CYTOTOXcell_REP))
      ) 
     
      CYTOTOXcell = merge(merge(merge(CYTOTOXcell_REP,CYTOTOXcell_EXP) , CYTOTOXcell_SN), CYTOTOXcell_value) %>%
        group_by(SN,EXP,REP) %>%
        summarise(MeanV = mean(Val,na.rm = T)) %>%
        ungroup()
      
      CYTOTOXcell_base = CYTOTOXcell %>%
        filter(SN == baselines) %>%
        rename(MeanBaseV = MeanV, Baseline = SN) 
      # if no exp conditions is used for the baseline then we repeat  exp given for the other SN
      # in thius way the same baseline is used for each SN
      if(all(CYTOTOXcell_base$EXP == "") ) {
        CYTOTOXcell_base = do.call("rbind", lapply(unique(CYTOTOXcell$EXP), function(x) CYTOTOXcell_base %>% mutate(EXP = x) ) )
      }
      
      CYTOTOXcell = CYTOTOXcell %>%  filter(SN != baselines)
      CYTOTOXcell = merge(CYTOTOXcell_base,CYTOTOXcell,by= c("REP","EXP"),all.y = T)
      CYTOTOXcell = CYTOTOXcell %>% mutate(Res = (MeanV-MeanBaseV)/(100-MeanBaseV)*100)
      
      cytotoxResult$data =  CYTOTOXcell
      cytotoxResult$dataFinal =  CYTOTOXcell %>%
        select(-Baseline, - MeanBaseV, - MeanV ) %>%
        tidyr::spread(key= "REP", value = "Res") %>%
        rename(`Sample Name` = SN,
               `Experimental Condition` = EXP)
      
      output$CYTOTOXtables = renderDT({
        datatable(
          cytotoxResult$dataFinal,
        rownames= FALSE,
        options = list(
          scrollX = TRUE,
          lengthChange = FALSE,
          dom = 't')
        )
      })
      
      output$CYTOTOXplots = renderPlot({
        CYTOTOXcell %>% ggplot() +
          geom_boxplot(aes(x = as.factor(EXP),y = Res,fill = SN, col = SN),alpha = 0.4) +
          theme_bw() +
          labs(x = "Experimental condition", y= "% Values w.r.t \nthe baseline cell death",
               col="Sample Name",fill="Sample Name")
      })
      
      }
  })
  # save everytime there is a change in the results
  CYTOTOXresultListen <- reactive({
    reactiveValuesToList(cytotoxResult)
  })
  observeEvent(CYTOTOXresultListen(), {
    DataAnalysisModule$cytotoxResult = reactiveValuesToList(cytotoxResult)
  })
  
  ### End CYTOTOX analysis ####
  
  ### Start Statistic ####
  DataStatisticModule = reactiveValues(WB = list(),
                                       PRCC = list(),
                                       ELISA = list(),
                                       ENDOC = list(),
                                       CYTOTOX = list(),
                                       Flag = F)
  
  DataStatisticresultListen <- reactive({
    reactiveValuesToList(DataStatisticModule)
  })
  
  observeEvent(DataStatisticresultListen(),{
    
    if(DataStatisticModule$Flag){
      AnalysisNames = names(DataStatisticModule)[names(DataStatisticModule) != "Flag"]
      Analysis = rep(F,length(AnalysisNames) )
      names(Analysis) = AnalysisNames
      for(j in AnalysisNames)
        Analysis[j] = all(sapply(DataStatisticModule[[j]], is.null))
      
      AnalysisNames = AnalysisNames[!Analysis]
      
      updateSelectizeInput(inputId = "StatAnalysis",
                           choices = c("",AnalysisNames),
                           selected = "")
      
      DataStatisticModule$Flag = F
    }
    
  })
  
  observeEvent(input$StatAnalysis,{
    
    if(input$StatAnalysis != ""){
      DataStatisticModule[[input$StatAnalysis]] -> results
      do.call(rbind,results) -> results
      
      res = resTTest = NULL
      resplot = ggplot()
      
      if(input$StatAnalysis == "WB"){
        resTTest = res = results %>%
          select(DataSet,SampleName,AdjRelDens) %>%
          mutate(SampleName = gsub(pattern = "^[0-9]. ",x = SampleName,replacement = "")) %>%
          tidyr::spread(key = DataSet,value = AdjRelDens ) 
        
        res$Mean = apply(res[,paste(unique(results$DataSet))],1,mean)
        res$Sd = apply(res[,paste(unique(results$DataSet))],1,sd)

        resplot =ggplot(res, aes(x = SampleName,
                                 y = Mean)) + 
          geom_bar(stat="identity", color="black", fill = "#BAE1FF",
                          position=position_dodge()) +
          geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=.2,
                        position=position_dodge(.9)) +
          theme_bw()
        
        combo = expand.grid(resTTest$SampleName,resTTest$SampleName)
        combo = combo[combo$Var1 != combo$Var2, ]
        resTTest = do.call(rbind,
                           lapply(1:dim(combo)[1],function(x){
          sn = combo[x,]
          ttest = t.test(resTTest[resTTest$SampleName == sn$Var1, -1],resTTest[resTTest$SampleName == sn$Var2, -1]) 
          data.frame(Ttest = paste(sn$Var1, " vs ",sn$Var2), 
                     pValue = ttest$p.value,
                     conf.int = paste(ttest$conf.int,collapse = ";")
                    )
          })
        )
        
        }
      
      output$TabStat = renderDT({res})
      output$PlotStat = renderDT({resplot})
      output$TabTTest = renderDT({resTTest})
    }
  })
  
  ### End Statistic ####
  
  ### DATAVERSE ####
  
  observeEvent(input$APIkey,{
    pathOCA <- system.file("Data", package = "OCA")
    
    if(input$APIkey != "") # the last key used is saved
      write(input$APIkey,file = paste0(pathOCA,"/.APIkey"))

  })
  
  DataAnalysisModule0 <- list(wbResult = wbResult0,
                              wbquantResult = wbquantResult0,
                              endocResult = endocResult0,
                              elisaResult = elisaResult0,
                              pcrResult = pcrResult0,
                              cytotoxResult = cytotoxResult0)
  
  observe({
    listDataAnalysisModule = reactiveValuesToList(DataAnalysisModule)
    namesAnalysis = sapply(names(listDataAnalysisModule), function(x){
      if(x == "wbquantResult"){
        if(! identical(DataAnalysisModule[[x]]$NormWBanalysis,DataAnalysisModule0[[x]]$NormWBanalysis) )
           return(x)
        else
          return("")
      }
      else if(!is.null(DataAnalysisModule[[x]]$Initdata) || !identical(DataAnalysisModule[[x]]$Initdata,DataAnalysisModule0[[x]]$Initdata) )
        return(x)
      else
        return("")
      })
    namesAnalysis = namesAnalysis[namesAnalysis!= ""]
    if(length(namesAnalysis)>0)
      updateSelectizeInput(inputId = "selectAnalysis_DV",
                          choices = MapAnalysisNames[namesAnalysis])
  })
  
  observeEvent(input$DataverseUpload_Button,{

    if(input$selectAnalysis_DV != ""){
  
      if(input$Title_DV == "" && input$Author_DV == "" && input$Description_DV == "" &&
         input$AuthorAff_DV== "" && input$ContactN_DV == "" && input$ContactEmail_DV == "")
          output$LoadingError_DATAVERSE = renderText("Error: missing information")
      else{
        # creation of a temporary folder
        tempfolder = paste0(tempdir(check = T),"/OCA")
        system(paste0("mkdir ",tempfolder))
  
        # create the metadata
        result <- fromJSON(file = system.file("docker","metadata.json", package = "OCA") )
        result$dataset_title = input$Title_DV
        result$dataset_description = paste0(input$Description_DV,"\n This dataset has
                                            been obtained using the OCA application,
                                            specifically the module: ", input$selectAnalysis_DV)
        result$author_name = input$Author_DV
        result$author_affiliation= input$AuthorAff_DV
        result$dataset_contact_name = input$ContactN_DV
        result$dataset_contact_email = input$ContactEmail_DV
        # result$subject = as.vector(result$subject)
        write(toJSON(result), file=paste0(tempfolder,"/metadata.json") )
  
        # move the file in the temporary folder
  
        saveExcel(filename = paste0(tempfolder,"/dataset/",
                                    gsub(pattern = " ", 
                                         x = input$selectAnalysis_DV,
                                         replacement = ""),".xlsx"),
                  ResultList = DataAnalysisModule[[ names(MapAnalysisNames[MapAnalysisNames == input$selectAnalysis_DV])]] ,
                  analysis = input$selectAnalysis_DV )
        
        saveRDS(file = paste0(tempfolder,"/dataset/OCA_",
                                         gsub(pattern = " ", 
                                              x = input$selectAnalysis_DV,
                                              replacement = ""),".RDs"))
        # docker run
        docker.run(params = paste0("--volume ", tempfolder,
                   ":/Results/ -d qbioturin/OCA-uploaddataverse python3 main.py /Results/metadata.json /Results/dataset") 
        )
  
      }
    
    }
  })
  
  #initiate_sword_dataset()
  #add_dataset_file()
  #publish_dataset()
  
  #### End DATAVERSE
  
  ### Loading files ####
  UploadDataAnalysisModuleAllFalse  = reactiveValues(FlagALL = F,
                                                     FlagUpdate = F,
                                                     FlagWB = F,
                                                     FlagPRCC = F,
                                                     FlagELISA = F,
                                                     FlagCYTOTOX = F,
                                                     FlagENDOC = F)
  UploadDataAnalysisModule = reactiveValues(FlagALL = F,
                                            FlagUpdate = F,
                                            FlagWB = F,
                                            FlagPRCC = F,
                                            FlagELISA = F,
                                            FlagCYTOTOX = F,
                                            FlagENDOC = F)
  
  
  # upload in the statistic module
  observeEvent(input$loadStatAnalysis_file_Button,{
    output$loadStatAnalysis_Error <- renderText({
      validate(
        need(!is.null(input$loadStatAnalysis_file) && all(file.exists(input$loadStatAnalysis_file$datapath)) ,
             "Please select one RDs file generated throught the Data Analysis module." )
      )
      
      datapaths = input$loadStatAnalysis_file$datapath
      for(dpath in 1:length(datapaths)){
        mess = readRDS(datapaths[dpath])
        
        validate(
          need(all(names(mess) %in% names(DataAnalysisModule)) ||
                 all(names(mess) %in% names(elisaResult)) ||
                 all(names(mess) %in% names(wbquantResult)) || 
                 all(names(mess) %in% names(pcrResult)) ||
                 all(names(mess) %in% names(cytotoxResult)) ||
                 all(names(mess) %in% names(endocResult)) ,
               paste(mess[["message"]],"\n The file must be RDs saved throught the Data Analysis module." ))
        )
        
        DataStatisticModule$Flag = T
        
        if( all(names(mess) %in% names(wbquantResult)) || all(names(mess) %in% names(DataAnalysisModule)) ){
          DataStatisticModule$WB[[dpath]] <- mess$AdjRelDensitiy %>% mutate(DataSet = dpath)
        }else if( all(names(mess) %in% names(pcrResult)) || all(names(mess) %in% names(DataAnalysisModule))){
          DataAnalysisModule$PRCC[[dpath]]  <- mess
        }else if(all(names(mess) %in% names(endocResult)) || all(names(mess) %in% names(DataAnalysisModule))){
          DataAnalysisModule$ENDOC[[dpath]]  <- mess
        }else if(all(names(mess) %in% names(elisaResult)) || all(names(mess) %in% names(DataAnalysisModule))){
          DataAnalysisModule$ELISA[[dpath]]  <- mess
        }else if(all(names(mess) %in% names(cytotoxResult)) || all(names(mess) %in% names(DataAnalysisModule))){
          DataAnalysisModule$CYTOTOX[[dpath]]  <- mess
        }
        
      }
      
      "The RDs files have been uploaded  with success."
      
    })
  })
  
  # general upload in the app
  observeEvent(input$loadAnalysis_Button,{
    output$loadAnalysis_Error <- renderText({
      validate(
        need(!is.null(input$loadAnalysis_file) && all(file.exists(input$loadAnalysis_file$datapath)) ,
             "Please select one RDs file generated throught the Data Analysis module." )
      )
      
      mess = readRDS(input$loadAnalysis_file$datapath)
      
      validate(
        need(all(names(mess) %in% names(DataAnalysisModule)) ||
               all(names(mess) %in% names(elisaResult)) ||
               all(names(mess) %in% names(wbResult)) || 
               all(names(mess) %in% names(pcrResult)) ||
               all(names(mess) %in% names(cytotoxResult)) ||
               all(names(mess) %in% names(endocResult)) ,
             paste(mess[["message"]],"\n The file must be RDs saved throught the Data Analysis module." ))
      )
      
      if(all(names(mess) %in% names(DataAnalysisModule)) ){
        DataAnalysisModule <- mess
        UploadDataAnalysisModule$FlagALL = T
      }
      else if( all(names(mess) %in% names(wbResult)) ){
        DataAnalysisModule$wbResult <- mess
        UploadDataAnalysisModule$FlagWB = T
      }else if( all(names(mess) %in% names(pcrResult)) ){
        DataAnalysisModule$pcrResult <- mess
        UploadDataAnalysisModule$FlagPRCC = T
      }else if(all(names(mess) %in% names(endocResult)) ){
        DataAnalysisModule$endocResult <- mess
        UploadDataAnalysisModule$FlagENDOC = T
      }else if(all(names(mess) %in% names(elisaResult)) ){
        DataAnalysisModule$elisaResult <- mess
        UploadDataAnalysisModule$FlagELISA = T
      }else if(all(names(mess) %in% names(citotoxResult)) ){
        DataAnalysisModule$citotoxResult <- mess
        UploadDataAnalysisModule$FlagCYTOTOX = T
      }
      
      UploadDataAnalysisModule$FlagUpdate = T
      
      "The RDs file has been uploaded  with success."
      
    })
  })
  observeEvent(UploadDataAnalysisModule$FlagUpdate,{
    if(UploadDataAnalysisModule$FlagUpdate){
      
      if(UploadDataAnalysisModule$FlagWB || UploadDataAnalysisModule$FlagALL){
        UploadRDs(Flag = "WB",
                  session = session,
                  output = output,
                  DataAnalysisModule = DataAnalysisModule,
                  Result = wbResult, 
                  FlagsExp = Flags,
                  PanelStructures = PanelStructures)
      }
      else if(UploadDataAnalysisModule$FlagPRCC || UploadDataAnalysisModule$FlagALL){
        UploadRDs(Flag = "PRCC",
                  session = session,
                  output = output,
                  DataAnalysisModule = DataAnalysisModule,
                  Result = prccResult, 
                  FlagsExp = FlagsPRCC)
      }
      else if(UploadDataAnalysisModule$FlagENDOC || UploadDataAnalysisModule$FlagALL){
        UploadRDs(Flag = "ENDOC",
                  session = session,
                  output = output,
                  DataAnalysisModule = DataAnalysisModule,
                  Result = endocResult, 
                  FlagsExp = FlagsENDOC)
      }
      else if(UploadDataAnalysisModule$FlagELISA || UploadDataAnalysisModule$FlagALL){
        
        UploadRDs(Flag = "ELISA",
                  session = session,
                  output = output,
                  DataAnalysisModule = DataAnalysisModule,
                  Result = elisaResult, 
                  FlagsExp = FlagsELISA)
        
      }
      else if(UploadDataAnalysisModule$FlagCYTOTOX || UploadDataAnalysisModule$FlagALL){
        
        UploadRDs(Flag = "CYTOTX",
                  session = session,
                  output = output,
                  DataAnalysisModule = DataAnalysisModule,
                  Result = cytotoxResult, 
                  FlagsExp = FlagsCYTOTOX)
        
      }
      
      UploadDataAnalysisModule = UploadDataAnalysisModuleAllFalse
    }
    
  })
  ### Download files ####
  
  output$downloadButtonExcel_ELISA <- downloadHandler(
    filename = function() {
      paste('ELISAanalysis-', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      saveExcel(filename = file, ResultList=DataAnalysisModule$elisaResult , analysis = "ELISA")
    }
  )
  
  output$downloadButtonExcel_CYTOTOX <- downloadHandler(
    filename = function() {
      paste('CYTOTOXanalysis-', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      saveExcel(filename = file, ResultList=DataAnalysisModule$cytotoxResult , analysis = "Cytotoxicity")
    }
  )
  
  output$downloadButtonExcel_ENDOC <- downloadHandler(
    filename = function() {
      paste('ENDOCanalysis-', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      saveExcel(filename = file, ResultList=DataAnalysisModule$cytotoxResult , analysis = "Endocytosis")
    }
  )
  
  output$downloadRDSwholeAnalysis <- downloadHandler(
    filename = function() {
      paste('DataIntegrationModuleAnalysis-', Sys.Date(), '.RDs', sep='')
    },
    content = function(file) {
      saveRDS(reactiveValuesToList(DataAnalysisModule), file = file)
    }
  )
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      "Report.pdf"
    },
    content = function(file) {
      rmarkdown::render("report.Rmd",
                        output_file = file,
                        params = reactiveValuesToList(DataAnalysisModule) )
    }
  )
  
  output$downloadButton_WB <- downloadHandler(
    filename = function() {
      paste('WBanalysis-', Sys.Date(), '.RDs', sep='')
    },
    content = function(file) {
      results = DataAnalysisModule$wbResult
      saveRDS(results, file = file)
    }
  )
  
  output$downloadButton_WBquant <- downloadHandler(
    filename = function() {
      paste('WBanalysisQuantification-', Sys.Date(), '.RDs', sep='')
    },
    content = function(file) {
      results = DataAnalysisModule$wbquantResult
      saveRDS(results, file = file)
    }
  )
  
  output$downloadButton_PCR <- downloadHandler(
    filename = function() {
      paste('PCRanalysis-', Sys.Date(), '.RDs', sep='')
    },
    content = function(file) {
      results = DataAnalysisModule$pcrResult
      saveRDS(results, file = file)
    }
  )
  
  output$downloadButton_ENDOC <- downloadHandler(
    filename = function() {
      paste('ENDOCanalysis-', Sys.Date(), '.RDs', sep='')
    },
    content = function(file) {
      results = DataAnalysisModule$endocResult
      saveRDS(results, file = file)
    }
  )
  
  
  output$downloadButton_CYTOTOX <- downloadHandler(
    filename = function() {
      paste('CYTOTOXanalysis-', Sys.Date(), '.RDs', sep='')
    },
    content = function(file) {
      results = DataAnalysisModule$cytotoxResult
      saveRDS(results, file = file)
    }
  )
  
  output$downloadButton_ELISA <- downloadHandler(
    filename = function() {
      paste('ELISAanalysis-', Sys.Date(), '.RDs', sep='')
    },
    content = function(file) {
      results = DataAnalysisModule$elisaResult
      saveRDS(results, file = file)
    }
  )
  #### end save files ###
  
  observe({namesAll <<- unique( c(names(wbResult), names(wbquantResult), names(pcrResult), names(endocResult)) )})
  
  return()
  
}
