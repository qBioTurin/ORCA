#shiny.maxRequestSize=1000*1024^2
#shiny.launch.browser = .rs.invokeShinyWindowExternal

Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
APIkey_path = system.file("Data",".APIkey", package = "InteGreat")

#source(system.file("Shiny","AuxFunctions.R", package = "InteGreat"))
# source("./inst/Shiny/AuxFunctions.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  DataAnalysisModule <- reactiveValues(wbResult = NULL,
                                       wbquantResult = NULL,
                                       endocResult = NULL,
                                       elisaResult = NULL,
                                       pcrResult = NULL)
  
  DataIntegrationModule <- reactiveValues(dataLoaded = NULL,
                                          data = NULL,
                                          wbTabs = NULL, 
                                          pcrTabs = NULL,
                                          endocTabs=NULL,
                                          otherTabs = NULL,
                                          otherTabsMean = NULL)
  
  ### WB analysis ####
  # DECLARE REACTIVEVALUES FUNCTION HERE
  PanelData = data.frame(SampleName = character(),
                         xmin = numeric(), ymin = numeric(), 
                         xmax = numeric(), ymax = numeric())
  
  wbResult <- reactiveValues(Normalizer = NULL,
                             Im = NULL,
                             Planes = NULL,
                             TruncatedPanelsValue = NULL,
                             PanelsValue = NULL,
                             Plots = NULL,
                             TruncatedPlots = NULL,
                             pl = NULL,
                             AUCdf=data.frame(SampleName = "-", Truncation = "-", AUC = "-"  ))
  
  wbResultsEmpty <- list(Normalizer = NULL,
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
    DataAnalysisModule$wbResult = wbResultsEmpty
    
    for(j in names(wbResult))
      wbResult[[j]] = wbResultsEmpty[[j]]
    
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
    
    wbResult$AUCdf = wbResultsEmpty$AUCdf
    wbResult$TruncatedPanelsValue = wbResultsEmpty$TruncatedPanelsValue
    wbResult$TruncatedPlots = wbResultsEmpty$TruncatedPlots 
    
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
      table = wbResultsEmpty$AUCdf
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
      table = wbResultsEmpty$AUCdf
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
    table =  wbResultsEmpty$AUCdf 
    
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
        table = tbWB
        table$AUC_Norm = tbWBnorm$AUC_Norm
        table$RelDens = table$AUC/table$AUC_Norm
        table = table %>%
          dplyr::select(SampleName, Truncation, AUC, AUC_Norm, RelDens) 
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
                       SampleName_Norm = "-", 
                       Truncation_Norm = "-",
                       AUC = "-", 
                       AUC_Norm = "-",
                       AdjRelDens = "-")
    
    if(!is.null(wbquantResult$WBanalysis_filtered) & !is.null(wbquantResult$NormWBanalysis_filtered)){
      
      tbWB = wbquantResult$WBanalysis_filtered
      tbWBnorm = wbquantResult$NormWBanalysis_filtered
      
      tbWBnorm = tbWBnorm  %>%
        rename(SampleName_Norm = SampleName,
               AUC_Norm = AUC,
               Truncation_Norm = Truncation)
      
      table = left_join(tbWBnorm,tbWB, by = "SampleName" ,all = T )
      
      table$AdjRelDens = table$AUC/table$AUC_Norm
      table = table %>% 
        dplyr::select( SampleName, Truncation,SampleName_Norm, Truncation_Norm, AUC, AUC_Norm, AdjRelDens) 
    }
    
    wbquantResult$AdjRelDensitiy = table
    output$AUC_AdjRelDens <- renderDT(
      table ,
      server = FALSE,
      options = list(lengthChange = FALSE, autoWidth = TRUE),
      rownames= FALSE
    )
    
    if(dim(table)[1] > 1 ){
      barPlotAdjRelDens = table %>% 
        mutate(Normalizer = paste0("Sample: ",SampleName_Norm ),
               WB = paste0("Sample: ",SampleName))  %>%
        ggplot() +
        geom_bar(aes(x = SampleName,
                     y = AdjRelDens,
                     fill = Normalizer ),
                 stat = "identity" ) +
        facet_grid(~WB)+
        theme_bw()
    }else{
      barPlotAdjRelDens = ggplot()
    }
    output$plot_AdjRelDens <- renderPlot({
      barPlotAdjRelDens
    })
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
  
  pcrResult = reactiveValues(Initdata = NULL,
                             selectPCRcolumns = NULL,
                             data = NULL,
                             PCRnorm = NULL,
                             BaselineExp = NULL,
                             CompPRC = NULL,
                             NewPCR = NULL)
  pcrResult0 = list(Initdata = NULL,
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
  
  endocResult = reactiveValues(Initdata= NULL,
                               data = NULL,
                               TablePlot = NULL,
                               dataFinal = NULL,
                               ENDOCcell_TIME = NULL,
                               ENDOCcell_EXP = NULL,
                               MapBaseline = NULL,
                               MapBlanche = NULL)
  
  endocResult0 = list(Initdata= NULL,
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
  
  elisaResult = reactiveValues(Initdata= NULL,
                               data = NULL,
                               TablePlot = NULL,
                               dataFinal = NULL,
                               ELISAcell_EXP = NULL,
                               ELISAcell_SN = NULL,
                               MapBaseline = NULL,
                               MapBlanche = NULL,
                               Tablestandcurve = NULL,
                               LinearRegression = NULL)
  
  elisaResult0 = list(Initdata= NULL,
                      data = NULL,
                      TablePlot = NULL,
                      dataFinal = NULL,
                      ELISAcell_EXP = NULL,
                      ELISAcell_SN = NULL,
                      MapBaseline = NULL,
                      MapBlanche = NULL,
                      Tablestandcurve = NULL,
                      LinearRegression = NULL)
  
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
      
      if(length(elisaTot_base[,1]) != 0 && !is.null(elisaResult$LinearRegression) ){
        
        # y = m*x + q
        q = elisaResult$LinearRegression$coefficients[1]
        m = elisaResult$LinearRegression$coefficients[2]
        
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
      lmStancurve = lm(Measures~Concentrations, data = standcurve)
      
      infoLM = data.frame(x = min(standcurve$Concentrations) + c(1,1),
                          y = max(standcurve$Measures) + c(2,1.75),
                          text = c( paste0("y = ", signif(lmStancurve$coef[[2]], 5), "x + ",signif(lmStancurve$coef[[1]],5 )),
                                    paste0("Adj R2 = ",signif(summary(lmStancurve)$adj.r.squared, 5))) )
      regressionPlot = ggplot(standcurve,aes(Concentrations, Measures)) +
        geom_point() +
        geom_smooth(method='lm', col = "red") +
        geom_text(data= infoLM,
                  aes(x = x, y = y, label =text ),
                  vjust = "inward", hjust = "inward" )+
        theme_bw()
      
      elisaResult$LinearRegression = lmStancurve
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
  
  
  ### Start Statistic ####
  DataStatisticModule = reactiveValues(WB = list(File1 = NULL, File2 = NULL),
                                       PRCC = list(File1 = NULL, File2 = NULL),
                                       ELISA = list(File1 = NULL, File2 = NULL),
                                       ENDOC = list(File1 = NULL, File2 = NULL))
  
  observeEvent(DataStatisticModule,{
    
    Analysis1 = sapply( sapply(reactiveValuesToList(DataStatisticModule),"[[",1  ), is.null)
    Analysis2 = sapply( sapply(reactiveValuesToList(DataStatisticModule),"[[",2  ), is.null)
    
    # consider the analysis different from NULL
    Analysis1 = sort(names(Analysis1)[!Analysis1])
    Analysis2 = sort(names(Analysis2)[!Analysis2])
    
    if( isTRUE(all.equal(Analysis1,Analysis2)) ){ # there are equal analysis
      updateSelectizeInput(inputId = "StatAnalysis",choices = c("",Analysis2),selected = "")
    }
    
  })
  
  ### End Statistic ####
  
  ### Omics ####
  
  Omics = reactiveValues(Default = NULL, User = NULL, Data = NULL)
  
  # default
  protfile = system.file("Data/Omics","pmic13104Simplified.xlsx", package = "InteGreat")
  if(protfile == ""){
    # In this case it means that we are inside the docker
    protfile = "~/../home/data/Examples/Omics/pmic13104Simplified.xlsx"
  }
  Default = readxl::read_excel(protfile)
  Omics$Default = Default
  Omics$Data = Default %>%
    dplyr::select(`Protein name`,iBAQ) %>%
    rename(Value = iBAQ, Name = `Protein name`)
  
  output$OmicsDefault_table <- renderDT(
    Omics$Default,
    filter = 'top',
    server = FALSE,
    options = list(lengthChange = FALSE,
                   autoWidth = FALSE,
                   columnDefs = list(list(
                     width = "125px",
                     targets = "_all"
                   )) ),
    rownames= FALSE
  )
  
  # user
  observeEvent(input$LoadProt_Button,{
    output$LoadingError_Prot <- renderText({
      validate(
        need(!is.null(input$ProtImport) && all(file.exists(input$ProtImport$datapath)) ,
             "Please select an excel files!!" )
      )
      
      mess = readfile(
        filename = input$ProtImport$datapath,
        type = "Excel"
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             paste(mess[["message"]],"\n all the files must be RDs saved from DataIntegrationModule app." ))
      )
      
      
      colmns = colnames(mess[,sapply(mess, is.numeric)])
      validate(
        need(length(colmns) > 0 ,
             "At least one column should be numeric."
        )
      )
      
      updateSelectInput(inputId = "RescalingColms_User",
                        choices = colmns ) 
      Omics$User = mess
      "The excel file has been uploaded  with success"
    })
  })
  observeEvent(input$ResetProt_Button,{
    Omics$User = NULL
    Omics$Default -> Default
    Omics$Data = Default %>%
      dplyr::select(`Protein name`,iBAQ) %>%
      rename(Value = iBAQ, Name = `Protein name`)
    
  })
  
  output$OmicsUser_table <- renderDT(
    Omics$User,
    filter = 'top',
    server = FALSE,
    selection = list(target = 'column'),
    options = list(lengthChange = FALSE,
                   autoWidth = FALSE,
                   columnDefs = list(list(
                     width = "125px", targets = "_all"
                   )) ),
    rownames= FALSE
  )
  
  observeEvent(input$OmicsUser_table_columns_selected,
               {
                 if(!is.null(Omics$User)){
                   output$LoadingError_Prot <- renderText({
                     
                     validate(
                       need(length(input$OmicsUser_table_columns_selected) == 2 ,
                            "Please select only two columns (one numeric and one character)." )
                     )
                     
                     prot = Omics$User[,input$OmicsUser_table_columns_selected+1]
                     validate(
                       need(all(sapply(prot, class) %in% c("character", "numeric")),
                            "Please select only two columns (one numeric and one character)." )
                     )
                     
                     protType = sapply(prot, class)
                     names(prot)[protType == "numeric"] = "Value"
                     names(prot)[protType == "character"] = "Name"
                     Omics$Data = prot
                   })
                 }
               })
  # rescaling
  observeEvent(input$InputDefault_rescaling,{
    validate(
      need(input$InputDefault_rescaling > 0 ,
           "Please the rescaling factor should be > 0" )
    )
    
    if(!is.null(Omics$Default)){
      Omics$Default$iBAQ = Omics$Default$iBAQ/input$InputDefault_rescaling
    }
    
  })
  observeEvent(input$InputUser_rescaling,{
    validate(
      need(input$InputUser_rescaling > 0 ,
           "Please the rescaling factor should be > 0." )
    )
    
    validate(
      need(length(input$RescalingColms_User)>0,
           "Please select at least one numeric column.")
    )
    
    colms = input$RescalingColms_User
    if(!is.null(Omics$User)){
      Omics$User[,colms] = Omics$User[,colms]/input$InputUser_rescaling
    }
    
  })
  
  ### end Omics ###
  
  ### integration ####
  
  observeEvent(input$LoadIntG_Button,{
    output$LoadingError_IntG <- renderText({
      validate(
        need(!is.null(input$IntGImport) && all(file.exists(input$IntGImport$datapath)) ,
             "Please select one or more RDs files!!" )
      )
      
      mess = readfile(
        filename = input$IntGImport$datapath,
        type = "RDsMulti",
        namesAll = namesAll
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]]
        )
      )
      
      DataIntegrationModule$dataLoaded = mess
      
      
      "The RDs files are uploaded with success"
    })
  })
  
  observeEvent(DataIntegrationModule$dataLoaded,{
    
    data = list(
      wbResult = NULL,
      wbquantResult = NULL,
      endocResult = NULL,
      pcrResult = NULL
    )
    DataIntegrationModule$dataLoaded -> mess
    
    if(!is.null(mess)){
      
      for( x in 1:length(mess))
      {
        if(all(names(mess[[x]]) %in% names(DataAnalysisModule)) )
          data = mess[[x]]
        else if( all(names(mess[[x]]) %in% names(wbResult)) )
          data$wbResult = mess[[x]]
        else if( all(names(mess[[x]]) %in% names(wbquantResult)) )
          data$wbquantResult = mess[[x]]
        else if( all(names(mess[[x]]) %in% names(pcrResult)) )
          data$pcrResult = mess[[x]]
        else if( all(names(mess[[x]]) %in% names(endocResult)))
          data$endocResult = mess[[x]]
      }
      
      DataIntegrationModule$data = data
      ## starting updating the boxes
      prot = Omics$Data
      # removing all the null analysis
      data = data[!sapply(data, is.null)]
      
      namesAnalysis = names(data)
      
      for(n in namesAnalysis){
        subdata = data[[n]]
        print(n)
        if(n == "pcrResult"){
          pcrTabs = subdata$CompPRC 
          updateSelectizeInput(inputId = "SelectGene",
                               choices = unique(pcrTabs$Gene),
                               selected = unique(pcrTabs$Gene)[1])
          
          DataIntegrationModule$pcrTabs = pcrTabs
        }else if(n == "wbquantResult"){
          wbTabs = subdata$RelDensitiy %>%
            dplyr::select(ExpName,Lane,RelDens) %>%
            group_by(ExpName,Lane) %>%
            
            ungroup()
          
          # wbTabs1 = wbTabs %>% dplyr::select(-Mean) %>%
          #   tidyr::spread(Dataset, RelDens)
          # wbTabs2 = wbTabs %>% dplyr::select(-RelDens,-Dataset) %>%
          #   distinct()
          
          DataIntegrationModule$wbTabs = wbTabs #merge(wbTabs1,wbTabs2)
          updatePickerInput(inputId = "Selectprot_wb",
                            choices = c("",unique(prot$Name)),
                            selected = "",session = session)
          
        }else if(n == "endocResult"){
          
        }
      }
      
    }
  })
  
  observeEvent(input$SelectGene,{
    if(!is.null(DataIntegrationModule$pcrTabs) && length(input$SelectGene)>0 && input$SelectGene!=""){
      pcrTabs = DataIntegrationModule$pcrTabs
      SelectedGene = input$SelectGene
      output$tables_IntG_pcr <- renderUI({fluidRow(pcrTab.generation(pcrTabs,SelectedGene)) })
      prot = Omics$Data
      SelInname = paste0("selinp_",SelectedGene)
      updateSelectizeInput(inputId = SelInname,
                           choices = unique(prot$Name),
                           server =T)
      
      local({
        output[[paste0("tab_",SelectedGene)]] <- renderDT({
          pcrTabs %>% filter(Gene == SelectedGene) %>%
            group_by(Sample,Norm) %>%
            dplyr::summarize(Qnorm=Qnorm,
                             Mean = mean(Qnorm),
                             Sd = sd(Qnorm),
                             Norm.Prot = ifelse(length(input[[SelInname]]) > 0,
                                                Mean*prot[prot$Name == input[[SelInname]],"Value"],
                                                Mean)
            ) %>%
            ungroup() %>%
            #tidyr::spread(Dataset,Qnorm) %>%
            arrange(Norm)
        })
      })
    }
  })
  
  output$Tab_IntG_wb <- renderDT({
    DataIntegrationModule$wbTabs
  },
  filter = 'top',
  server = FALSE,
  options = list(lengthChange = FALSE,
                 autoWidth = TRUE),
  rownames= FALSE
  )
  observeEvent(list(input$Selectprot_wb,Omics$Data),{
    if(!is.null(DataIntegrationModule$wbTabs)){
      wbTabs = DataIntegrationModule$wbTabs
      prot = Omics$Data
      if(!is.null(input$Selectprot_wb) && length(input$Selectprot_wb)>0 &&input$Selectprot_wb!= "" ){
        as.numeric(prot[prot$Name == input$Selectprot_wb,"Value"]) -> pr
        wbTabs = wbTabs %>% dplyr::mutate(`Rel.Prot.` = RelDens * pr )
        wbTabs[paste(input$Selectprot_wb)] = pr
      }
      else
        wbTabs = wbTabs[,ifelse("Rel.Prot." %in% colnames(wbTabs),
                                colnames(wbTabs)[!"Rel.Prot." %in% colnames(wbTabs)],
                                colnames(wbTabs) )]
    }
    else{
      wbTabs = NULL
    }
    DataIntegrationModule$wbTabs <- wbTabs 
  })
  #### end integration ###
  
  ### Other integration ####
  observeEvent(input$LoadOther_Button,{
    output$LoadingError_Other <- renderText({
      validate(
        need(!is.null(input$OtherImport) && all(file.exists(input$OtherImport$datapath)) ,
             "Please select one excel files!!" )
      )
      
      mess = readfile(
        filename = input$OtherImport$datapath,
        type = "Excel"
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]]
        )
      )
      
      validate(
        need(all(c("character", "numeric") %in% sapply(mess, class)),
             "Please, the excel file must have two columns: one numeric and one character." )
      )
      
      DataIntegrationModule$otherTabs = mess
      "The excel file has been uploaded  with success"
    })
  })
  
  observe({
    if(!is.null(DataIntegrationModule$otherTabs)){
      
      updateSelectizeInput(inputId = "Selectprot_other",
                           choices = c("",unique(Omics$Data$Name)),
                           selected = "",
                           server = T )
    }
  })
  
  output$Other_table <- renderDT({
    DataIntegrationModule$otherTabs
  },
  filter = 'top',
  server = FALSE,
  selection = list(target = 'column'),
  options = list(lengthChange = FALSE,
                 autoWidth = TRUE),
  rownames= FALSE
  )
  
  output$Other_tableMean <- renderDT({
    DataIntegrationModule$otherTabsMean
  },
  filter = 'top',
  server = FALSE,
  options = list(lengthChange = FALSE,
                 autoWidth = TRUE),
  rownames= FALSE
  )
  
  observeEvent(input$Other_table_columns_selected,{
    if(!is.null(DataIntegrationModule$otherTabs) && !is.null(input$Other_table_columns_selected) && length(input$Other_table_columns_selected)>0){
      
      prot = Omics$Data
      data = DataIntegrationModule$otherTabs
      colmns = colnames(data)[input$Other_table_columns_selected + 1]
      colmns = names(sapply(data[,colmns], class)[ sapply(data[,colmns], class) == "character"])
      
      # check at leat one column selected is character
      if(length(colmns) > 0){
        colmnsNumeric = colnames(data)[-which(colnames(data) %in% colmns)]
        colmnsNumeric = colmnsNumeric[which(sapply(data[,colmnsNumeric], class) %in% "numeric" )]
        
        otherTabs = data %>% 
          tidyr::gather(colmnsNumeric, value = "Value", key= "Numeric Columns") %>%
          group_by_at(c("Numeric Columns",colmns)) %>%
          dplyr::summarise(Mean = mean(Value) ) %>%
          ungroup()
        
        DataIntegrationModule$otherTabsMean = otherTabs
      }
    }
  })
  
  observeEvent(input$Selectprot_other,{
    if(!is.null(DataIntegrationModule$otherTabsMean)){
      if(input$Selectprot_other != ""){
        prot = Omics$Data
        as.numeric(prot[prot$Name == input$Selectprot_other,"Value"]) -> pr
        
        DataIntegrationModule$otherTabsMean$`Rel.Prot.`  = DataIntegrationModule$otherTabsMean$Mean * pr
      }else{
        DataIntegrationModule$otherTabsMean$`Rel.Prot.`  = DataIntegrationModule$otherTabsMean$Mean
      }
    }
  })
  #### End other integration
  
  ### DATAVERSE ####
  
  observeEvent(input$APIkey,{
    
    pathInteGreat <- system.file("Data", package = "InteGreat")
    
    if(input$APIkey != "") # the last key used is saved
      write(input$APIkey,file = paste0(pathInteGreat,"/.APIkey"))
    
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
                                                     FlagENDOC = F)
  UploadDataAnalysisModule = reactiveValues(FlagALL = F,
                                            FlagUpdate = F,
                                            FlagWB = F,
                                            FlagPRCC = F,
                                            FlagELISA = F,
                                            FlagENDOC = F)
  
  
  # upload in the statistic module
  observeEvent(input$loadStatAnalysis_file_Button,{
    output$loadStatAnalysis_Error <- renderText({
      validate(
        need(!is.null(input$loadStatAnalysis_file) && file.exists(input$loadStatAnalysis_file$datapath) ,
             "Please select one RDs file generated throught the Data Analysis module." )
      )
      
      mess = readRDS(input$loadStatAnalysis_file$datapath)
      
      validate(
        need(all(names(mess) %in% names(DataAnalysisModule)) ||
               all(names(mess) %in% names(elisaResult)) ||
               all(names(mess) %in% names(wbResult)) || 
               all(names(mess) %in% names(pcrResult)) || 
               all(names(mess) %in% names(endocResult)) ,
             paste(mess[["message"]],"\n The file must be RDs saved throught the Data Analysis module." ))
      )
      
      if(all(names(mess) %in% names(DataAnalysisModule)) ){
        DataStatisticModule$WB$File1 <- mess
        DataAnalysisModule$PRCC$File1 <- mess
        DataAnalysisModule$ENDOC$File1 <- mess
        DataAnalysisModule$ELISA$File1 <- mess
      }
      else if( all(names(mess) %in% names(wbResult)) ){
        DataStatisticModule$WB$File1 <- mess
      }else if( all(names(mess) %in% names(pcrResult)) ){
        DataAnalysisModule$PRCC$File1 <- mess
      }else if(all(names(mess) %in% names(endocResult)) ){
        DataAnalysisModule$ENDOC$File1 <- mess
      }else if(all(names(mess) %in% names(elisaResult)) ){
        DataAnalysisModule$ELISA$File1 <- mess
      }
      
      "The RDs file has been uploaded  with success."
      
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
