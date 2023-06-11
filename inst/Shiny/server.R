#shiny.maxRequestSize=1000*1024^2
#shiny.launch.browser = .rs.invokeShinyWindowExternal

pcrTab.generation = function(pcrTabs,SelectedGene)
{
  output_list <- column(width = 12,
                        selectizeInput(inputId = paste0("selinp_",SelectedGene),
                                       choices="",
                                       options = list(maxItems = 1),
                                       multiple = F,
                                       label = "Value for the rescaling"),
                        DTOutput(outputId=paste0("tab_",SelectedGene),
                                 width = "80%")
  )
  return(output_list)
}
readfile <- function(filename,type,colname = T, namesAll = namesAll) {
  out <- tryCatch(
    {
      if(type == "RDs"){
        x = readRDS(filename)
        if(! ( all(names(x) %in% namesAll)) )
           return(
            list(message = "The RDs file must be generated from Data Analysis module.",
                 call = "")
          )
        x
      }else if(type == "RDsMulti"){
        x = lapply(filename, readRDS)
        for( i in 1:length(x))
          if(! (all(names(x) %in% namesAll)))
            return(
              list(message = "The RDs file must be generated from Data Analysis module.",
                   call = "")
            )
        x
      }else if(type == "Excel"){
        readxl::read_excel(filename,col_names = colname)
      }else{
        LoadImage(filename)
      }
      
    },
    error=function(cond) {
      # message(cond)
      # Choose a return value in case of error
      return(cond)
    },
    warning=function(cond) {
      # message(cond)
      # Choose a return value in case of warning
      return(cond)
    }
  )    
  return(out)
}

AUCfunction<-function(AUCdf,PanelsValue,bind=T,session = session,Lane=1,AUCdf.new=NULL){
  if(is.null(AUCdf.new)){
    
    if(length(AUCdf[,1])==1 & AUCdf$AUC[1] == "-")
    {
      AUCdf.new <- AUCdf
    }else{
      IDLane = Lane
      AUCdf2 = AUCdf %>% filter(Lane == IDLane)
      
      if(length(AUCdf2[,1]) == 0){
        AUCdf.new = AUCdf2
        AUCdf.new[1,] = rep(NA,length(names(AUCdf2)))
        AUCdf.new$Truncation = "-"
        AUCdf.new$Lane = IDLane
      }else{
        AUCdf.new <- AUCdf2[length(AUCdf2$Truncation),]
      }
    }
  }
  
  PanelsValue.Lane <- PanelsValue[which(PanelsValue$ID == Lane),]
  id <- order(PanelsValue.Lane$Y)
  AUCdf.new$AUC <- round(
    sum(diff(PanelsValue.Lane$Y[id])*rollmean(PanelsValue.Lane$Values[id],2)),
    digits = 4)
  AUCdf.new$Lane <- paste(Lane)
  
  if(length(AUCdf[,1])==1 & AUCdf$AUC[1] == "-")
  {
    A<-AUCdf.new
  }else{
    A<-rbind(AUCdf,AUCdf.new) 
  }
  return(unique(A)) 
}
LoadImage = function(pathImage){
  im <- OpenImageR::readImage(pathImage,as.is = T)
  
  JpegImage = tempfile(fileext = "jpeg")
  # unlink()
  jpeg(file=JpegImage,
       width = dim(im)[2],
       height = dim(im)[1],
       units = "px")
  imageShow(im)
  dev.off()
  
  img <- jpeg::readJPEG(JpegImage, native = F)
  
  return(list(WB = im, RGB = img))
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  DataAnalysisModule <- reactiveValues(wbResult = NULL,
                                       wbquantResult = NULL,
                                       elisaResult = NULL,
                                       pcrResult = NULL)
  
  DataIntegrationModule <- reactiveValues(dataLoaded = NULL,
                                          data = NULL,
                                          wbTabs = NULL, 
                                          pcrTabs = NULL,
                                          elisaTabs=NULL,
                                          otherTabs = NULL,
                                          otherTabsMean = NULL)
  
  ### WB analysis ####
  # DECLARE REACTIVEVALUES FUNCTION HERE
  PanelData = data.frame(Panel = character(),
                         ID = numeric(),
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
                             AUCdf=data.frame(Truncation = "-", AUC = "-", Lane="-"  ))
  
  wbResultsEmpty <- list(Normalizer = NULL,
                         Im = NULL,
                         Planes = NULL,
                         TruncatedPanelsValue = NULL,
                         PanelsValue = NULL,
                         Plots = NULL,
                         TruncatedPlots = NULL,
                         pl = NULL,
                         AUCdf=data.frame(Truncation = "-", AUC = "-", Lane="-"  )
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
      
      "The image is uploaded with success"
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
    removeModal()
    DataAnalysisModule$wbResult = wbResultsEmpty
    
    output$AUC <- renderTable({
      wbResult$AUCdf
    },width = "100%")
    
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
      
      "The image is uploaded with success"
    })
    
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
            PanelStructures$data <- data.frame(Panel = "", ID = 1,vals)
          }else{
            PanelStructures$data <- rbind(PanelStructures$data,
                                          cbind(data.frame(Panel = "",
                                                           ID = nrow(PanelStructures$data)+1),
                                                vals))
          }
        }else{
          PanelStructures$data <- rbind(PanelStructures$data,
                                        cbind(data.frame(Panel = "",
                                                         ID = nrow(PanelStructures$data)+1),
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
      editable = T,#list(target = "column", disable = list(columns = 1:length(PanelStructures$data[1,])) ),
      options = list(lengthChange = FALSE, autoWidth = TRUE),
      rownames= FALSE
      )
    }
    )
  
  # check names Planes
  observeEvent(input$PlanesStructureTable_cell_edit, {
    row  <- input$PlanesStructureTable_cell_edit$row
    PanelStructures$data[row, 1] <- input$PlanesStructureTable_cell_edit$value
    wbResult$Planes = PanelStructures$data
  })
  
  output$AUC <- renderTable({
    wbResult$AUCdf  %>% dplyr::select(Lane,Truncation, AUC)
  },width = "100%")
  observeEvent(list(input$GenLanes,wbResult$Planes),{
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
                                     plane = im[(Nrow-p$ymax):(Nrow-p$ymin),p$xmin:p$xmax]
                                     #imageShow(plane)
                                     GreyPlane = apply(plane,1,"mean")
                                     data.frame(Values = GreyPlane - min(GreyPlane),
                                                ID = paste0(p$ID,". ",p$Panel),
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
    
    output$AUC <- renderTable({
      wbResult$AUCdf
    },width = "100%")
    
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
                        min = min(Plots.Lane$Y),
                        max = max(Plots.Lane$Y),
                        value =min(Plots.Lane$Y) )
      
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
        hline.dat <- data.frame(ID=as.factor(PanelsValue$ID), hl =0)
        hline.dat  <-  hline.dat[ hline.dat$ID == IDlane, ]
        hline.dat$hl <- TruncY
        
        pl <- pl + geom_hline(data=hline.dat,aes(yintercept = hl),linetype="dashed")
      }
      
      output$DataPlot <- renderPlot({pl})
      
      ### AUC calculation of the whole lane without cuts:
      wbResult$AUCdf <- AUCfunction(wbResult$AUCdf,PanelsValue,Lane = IDlane)
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
      lastTrunc = AUCdf %>% 
        group_by(Lane) %>%
        filter(Lane == IDlane, row_number()==n() ) %>%
        ungroup() %>%
        dplyr::select(Truncation) 
      
      if(length(lastTrunc$Truncation) > 0 & lastTrunc$Truncation != "-")
        AUCdf.new$Truncation <- lastTrunc$Truncation
      
      AUCdf.new$Lane <- IDlane
      
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
      AUCdf<-AUCfunction(AUCdf.new=AUCdf.new,wbResult$AUCdf,PanelsValue,Lane = IDlane)
      output$AUC <- renderTable({AUCdf})
      wbResult$AUCdf <- AUCdf
    }
  })
  
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
      
      "The RDs is uploaded with success"
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
      
      choices = paste0(mess$AUCdf$Lane, " with ", mess$AUCdf$Truncation)
      
      updateSelectInput("IdLaneNorm_RelDens",
                        session = session,
                        choices = choices,
                        selected = choices[1])
      
      wbquantResult$NormWBanalysis  = mess
      
      "The RDs is uploaded with success"
    })
    
  })
  
  observe({
    if(!is.null(wbquantResult$WBanalysis) & !is.null(wbquantResult$NormWBanalysis))
      FlagsWBquant$BothUploaded = T
  })
  # update the wb tables
  observe({
    if(is.null(wbquantResult$NormWBanalysis)){
      table = wbResultsEmpty$AUCdf
    }else{
      table = wbquantResult$NormWBanalysis$AUCdf
    }
    output$AUC_WBnorm <- renderDT(
      table, 
      filter = 'top', server = FALSE, 
      selection = "single", 
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
      filter = 'top', server = FALSE, 
      options = list(lengthChange = FALSE, autoWidth = TRUE),
      rownames= FALSE
    )
  })
  
  # removing rows
  observeEvent(input$AUC_WB_rows_selected,{
    if(!is.null(wbquantResult$WBanalysis) & !is.null(wbquantResult$NormWBanalysis)){
      indexesWB = input$AUC_WB_rows_selected
      
      if(!is.null(wbquantResult$WBanalysis_filtered))
        AUCdf = wbquantResult$WBanalysis_filtered
      else
        AUCdf = wbquantResult$WBanalysis$AUCdf
      
      if(length(indexesWB) > 0){
        wbquantResult$WBanalysis_filtered = AUCdf[-indexesWB,]
        output$AUC_WB <- renderDT(
          wbquantResult$WBanalysis_filtered, 
          filter = 'top', server = FALSE, 
          selection = "single", 
          options = list(lengthChange = FALSE, autoWidth = TRUE),
          rownames= FALSE
        )
      }
    }
  })
  observeEvent(input$AUC_WBnorm_rows_selected,{
    if(!is.null(wbquantResult$NormWBanalysis)){
      indexesWB = input$AUC_WBnorm_rows_selected
      
      if(!is.null(wbquantResult$NormWBanalysis_filtered))
        AUCdf = wbquantResult$NormWBanalysis_filtered
      else
        AUCdf = wbquantResult$NormWBanalysis$AUCdf
      
      if(length(indexesWB) > 0){
        choices = paste0(AUCdf$Lane, " with ", AUCdf$Truncation)
        
        selected = input$IdLaneNorm_RelDens
        updateSelectInput("IdLaneNorm_RelDens",
                          session = session,
                          choices = choices,
                          selected = selected)
        
        wbquantResult$NormWBanalysis_filtered = AUCdf[-indexesWB,]
        
        output$AUC_WBnorm <- renderDT(
          wbquantResult$NormWBanalysis_filtered,
          filter = 'top', server = FALSE,
          selection = "single",
          options = list(lengthChange = FALSE, autoWidth = TRUE),
          rownames= FALSE
        )
      }
    }
  })
  
  # resetting the tables
  
  # the relative density and adjusted is calculated
  observeEvent(list(FlagsWBquant$BothUploaded, input$IdLaneNorm_RelDens,input$AUC_WB_rows_selected,input$AUC_WBnorm_rows_selected),{
    table =  wbResultsEmpty$AUCdf 
    
    if(!is.null(wbquantResult$WBanalysis) & !is.null(wbquantResult$NormWBanalysis)){
      IdLaneNorm_RelDens = input$IdLaneNorm_RelDens
      IdLaneNorm_RelDens = strsplit(IdLaneNorm_RelDens,split = " with ")[[1]]
      
      if(!is.null(wbquantResult$NormWBanalysis_filtered))
        tbWBnorm = wbquantResult$NormWBanalysis_filtered
      else
        tbWBnorm = wbquantResult$NormWBanalysis$AUCdf
      
      tbWBnorm = tbWBnorm %>%
        filter(Truncation == IdLaneNorm_RelDens[2],
               Lane ==IdLaneNorm_RelDens[1]) %>%
        rename(AUC_Norm = AUC,
               Truncation_Norm = Truncation,
               Lane_Norm = Lane)
      
      if(!is.null(wbquantResult$WBanalysis_filtered))
        tbWB = wbquantResult$WBanalysis_filtered
      else
        tbWB = wbquantResult$WBanalysis$AUCdf
      
      if(!is.null(tbWBnorm) & !is.null(tbWB) & dim(tbWBnorm)[1]==1 ){
        table = tbWB
        table$AUC_Norm = tbWBnorm$AUC_Norm
        table$RelDens = table$AUC/table$AUC_Norm
        table$ExpName = "To set"
        table = table %>% dplyr::select(ExpName, Lane, Truncation, AUC, AUC_Norm, RelDens)
      }
    }
    
    wbquantResult$RelDensitiy = table
    
    output$AUC_RelDens <- renderDT(
      table,
      filter = 'top',
      server = FALSE,
      editable = list(target = "column", disable = list(columns = 1:length(table[1,])) ),
      options = list(lengthChange = FALSE, autoWidth = TRUE),
      rownames= FALSE
    )
    
  })
  observeEvent(list(FlagsWBquant$BothUploaded, input$AUC_WB_rows_selected,input$AUC_WBnorm_rows_selected),{
    table =  wbResultsEmpty$AUCdf 
    
    if(!is.null(wbquantResult$WBanalysis) & !is.null(wbquantResult$NormWBanalysis)){
      
      if(!is.null(wbquantResult$WBanalysis_filtered))
        tbWB = wbquantResult$WBanalysis_filtered
      else
        tbWB = wbquantResult$WBanalysis$AUCdf
      
      if(!is.null(wbquantResult$NormWBanalysis_filtered))
        tbWBnorm = wbquantResult$NormWBanalysis_filtered
      else
        tbWBnorm = wbquantResult$NormWBanalysis$AUCdf
      
      tbWBnorm = tbWBnorm  %>%
        rename(AUC_Norm = AUC,
               Truncation_Norm = Truncation)
      
      if(!is.null(tbWBnorm) & !is.null(tbWB) ){
        table = merge(tbWBnorm,tbWB, by = "Lane",all = T )
        
        table$AdjRelDens = table$AUC/table$AUC_Norm
        table$ExpName = "To set"
        table = table %>% dplyr::select(ExpName, Lane, Truncation, Truncation_Norm, AUC, AUC_Norm, AdjRelDens)
      }
    }
    
    wbquantResult$AdjRelDensitiy = table
    output$AUC_AdjRelDens <- renderDT(
      table,
      server = FALSE,
      editable = list(target = "column", disable = list(columns = 1:length(table[1,])) ),
      options = list(lengthChange = FALSE, autoWidth = TRUE),
      rownames= FALSE
    )
    
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
      
      "The RT-qPCR excel is uploaded with success"
    })
  })
  observeEvent(input$confirmUploadPCR,{
    removeModal()
    
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
      
      "The RDs is uploaded with success"
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
                               ELISAcell_TIME = NULL,
                               ELISAcell_EXP = NULL,
                               MapBaseline = NULL)
  
  elisaResult0 = list(Initdata= NULL,
                      data = NULL,
                      TablePlot = NULL,
                      dataFinal = NULL,
                      ELISAcell_TIME = NULL,
                      ELISAcell_EXP = NULL,
                      MapBaseline = NULL)
  
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
        colname = F
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]] )
      )
      
      elisaResult$Initdata = mess
      
      "The ELISA excel is uploaded with success"
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
      "The RDs is uploaded with success"
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
                            columnDefs = list(list(targets = cols.color, visible = FALSE))
                          )) %>%
        formatStyle(cols.keep,
                    cols.color,
                    backgroundColor = styleEqual("", 'white'))
      
      ELISA = ELISAtb$x$data
      
      output$ELISAmatrix <- renderDT(
        ELISAtb,
        #filter = 'none',
        server = FALSE,
        #selection = list(mode = 'single', target = 'cell'),
        #options = list(lengthChange = FALSE ),
        #rownames= FALSE
      )
      
      # ind = expand.grid(0:(length(ELISA[1,])-1),0:(length(ELISA[,1])-1))
      # ind$Var3 = paste0(ind$Var1,"_",ind$Var2)
      # NumCells = length(ind$Var3)
      
      ELISAcell_EXP <- ELISAcell_TIME <- matrix(
        "",
        nrow = length(ELISA[,1]),
        ncol = length(ELISA[1,])
      )
      elisaResult$ELISAcell_EXP <- ELISAcell_EXP
      elisaResult$ELISAcell_TIME<- ELISAcell_TIME
      elisaResult$TablePlot = ELISAtb
    }
  })
  observeEvent(input$ELISAmatrix_cell_clicked,{
    if(length(input$ELISAmatrix_cell_clicked)!=0){
      cellSelected= as.numeric(input$ELISAmatrix_cell_clicked)
      FlagsELISA$cellCoo = cellCoo = c(cellSelected[1],cellSelected[2]+1)
      print(cellCoo)
      print(elisaResult$ELISAcell_TIME[ cellCoo[1],cellCoo[2] ])
      print(elisaResult$ELISAcell_EXP[ cellCoo[1], cellCoo[2] ])
      updateSelectizeInput(inputId = "ELISAcell_TIME",
                           selected = ifelse(is.null(elisaResult$ELISAcell_TIME[cellCoo[1],cellCoo[2]]),"",elisaResult$ELISAcell_TIME[cellCoo[1],cellCoo[2]])
      )
      updateSelectizeInput(inputId = "ELISAcell_EXP",
                           selected = ifelse(is.null(elisaResult$ELISAcell_EXP[cellCoo[1],cellCoo[2]]),"",elisaResult$ELISAcell_EXP[cellCoo[1],cellCoo[2]])
      )
    }
  })
  
  observeEvent(input$ELISAcell_TIME,{
    if(!is.null(elisaResult$ELISAcell_TIME)){
      cellCoo = FlagsELISA$cellCoo
      elisaResult$ELISAcell_TIME[cellCoo[1],cellCoo[2]] = input$ELISAcell_TIME
    }
  })
  observeEvent(input$ELISAcell_EXP,{
    if(!is.null(elisaResult$ELISAcell_EXP)){
      ELISAtb = elisaResult$TablePlot
      cellCoo = FlagsELISA$cellCoo
      if(!is.null(cellCoo)){
        elisaResult$ELISAcell_EXP[cellCoo[1],cellCoo[2]] = input$ELISAcell_EXP
        ELISAtb$x$data[cellCoo[1],paste0("Col",cellCoo[2])] = input$ELISAcell_EXP
        
        if(! input$ELISAcell_EXP %in% FlagsELISA$AllExp){
          FlagsELISA$AllExp = unique(c(FlagsELISA$AllExp,input$ELISAcell_EXP))
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
  observeEvent(FlagsELISA$AllExp,{
    if(length(FlagsELISA$AllExp) > 1){
      exp = FlagsELISA$AllExp
      exp = exp[exp != ""]
      updateCheckboxGroupInput(session,"ELISA_baselines",
                               choices = exp )
      
      FlagsELISA$EXPselected = exp
      
    }
  })
  
  observeEvent(input$ELISA_baselines,{
    FlagsELISA$BASEselected = input$ELISA_baselines
    FlagsELISA$EXPselected = FlagsELISA$AllExp[! FlagsELISA$AllExp %in% FlagsELISA$BASEselected]
  })
  
  toListen <- reactive({
    exp = FlagsELISA$EXPselected
    exp = exp[exp != ""]
    if(length(exp) > 0 )
    {
      InputEXP = lapply(exp, function(i) input[[paste0("Exp",i)]])
      which(sapply(InputEXP, is.null)) -> indexesEXPnull
      if(length(indexesEXPnull) > 0 )
        listReturn = InputEXP[-indexesEXPnull]
      else
        listReturn = InputEXP
    }else{
      listReturn = list()
    }
    
    if(length(listReturn) == 0){
      return(list("Nothing",elisaResult$ELISAcell_TIME,elisaResult$ELISAcell_EXP))
    }else{
      return(c(listReturn,list(elisaResult$ELISAcell_TIME,elisaResult$ELISAcell_EXP)) )
    }
  })
  
  observeEvent(toListen(),{
    baselines = FlagsELISA$BASEselected
    baselines = baselines[baselines != ""]
    
    if(toListen()[[1]] != "Nothing"){
      exp = FlagsELISA$EXPselected
      exp = exp[exp != ""]
      
      MapBaseline = do.call(rbind,
                            lapply(exp,function(i){
                              if( length(input[[paste0("Exp",i)]]) > 0 && input[[paste0("Exp",i)]] != ""){
                                data.frame(Exp = i, Baseline = input[[paste0("Exp",i)]])
                              }else{
                                data.frame(Exp = i, Baseline = NA)
                              }
                            })
      )
      
      elisaResult$MapBaseline = MapBaseline
      
      MapBaseline = MapBaseline %>% na.omit()
      
      mat = as.matrix(elisaResult$Initdata)
      elisaV = expand.grid(seq_len(nrow(mat)), seq_len(ncol(mat))) %>%
        rowwise() %>%
        mutate(values = mat[Var1, Var2])
      matTime =  as.matrix(elisaResult$ELISAcell_TIME)
      elisaT = expand.grid(seq_len(nrow(matTime)), seq_len(ncol(matTime))) %>%
        rowwise() %>%
        mutate(time = matTime[Var1, Var2])
      matExp =  as.matrix(elisaResult$ELISAcell_EXP)
      elisaE = expand.grid(seq_len(nrow(matExp)), seq_len(ncol(matExp))) %>%
        rowwise() %>%
        mutate(exp = matExp[Var1, Var2])
      elisaTot = merge(elisaV,merge(elisaT,elisaE)) %>%
        filter(exp != "")
      
      MapBaseline = merge(MapBaseline,elisaTot,by.y = "exp", by.x = "Baseline")%>% 
        mutate(BaseValues = mean(values)) %>% 
        dplyr::select(Baseline,Exp,BaseValues) 
      
      elisaTot = merge(elisaTot,MapBaseline, by.x = c("exp"), by.y = c("Exp"))
      
      elisaResult$data = elisaTot
      
      if(length(elisaTot[,1]) != 0 ){
        elisamean = elisaTot %>%
          ungroup()%>%
          dplyr::select(Baseline,BaseValues,exp,values,time) %>%
          group_by(exp,time,Baseline) %>%
          dplyr::summarise(MeanExperiment = mean(values),
                           MeanBaseline = mean(BaseValues),
                           Quantification = MeanExperiment/MeanBaseline * 100) %>%
          na.omit() %>%
          ungroup() %>%
          rename(Experiment = exp,Time = time)
        
        output$ELISAtables = renderDT(elisamean)
        
        elisaResult$dataFinal = elisamean
        
        output$ELISAplots = renderPlot(
          elisamean%>%
            ggplot(aes(x = Time, y = Quantification, col=Experiment,group = Experiment))+
            geom_point()+
            geom_line()+
            theme_bw()+
            labs(x = "Time", y = "Quantifications")
        )
      }else{
        output$ELISAtables = renderDT(data.frame(Error = "No baseline is associated with the experiment replicants!"))
      }
    }
  })
  
  
  # here the Exp boxes are updated every time a new experiment is added 
  observeEvent(FlagsELISA$EXPselected,{
    expToselect = FlagsELISA$EXPselected
    baselines =  FlagsELISA$BASEselected
    
    expToselect = expToselect[expToselect != ""]
    
    output$ElisaBaselineSelection <- renderUI({
      select_output_list <- lapply(expToselect, function(i) {
        
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
  })
  
  observeEvent(elisaResult$TablePlot, {
    ELISAtb = elisaResult$TablePlot
    output$ELISAmatrix <- renderDT(
      ELISAtb,
      server = FALSE
    )
    
    ##### Plot the values selected!
    matTime =  as.matrix(elisaResult$ELISAcell_TIME)
    matExp =  as.matrix(elisaResult$ELISAcell_EXP)
    
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
      
      output$ELISAinitplots <- renderPlot(
        ggplot(elisaTot, aes(x = time, y=values, col = exp),alpha = 1.4) +
          #geom_boxplot(aes(fill= exp, group = time),alpha = 0.4) +
          geom_point(aes(group = exp)) +
          scale_color_manual(values = FlagsELISA$EXPcol) + 
          #scale_fill_manual(values = FlagsELISA$EXPcol) + 
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
  
  ### End ELISA analysis ####
  
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
      "The excel file is uploaded with success"
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
      elisaResult = NULL,
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
      else if( all(names(mess[[x]]) %in% names(elisaResult)))
        data$elisaResult = mess[[x]]
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
        
      }else if(n == "elisaResult"){
        
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
      "The excel file is uploaded with success"
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
  ### End other integration
  
  ### Loading files ####
  UploadDataAnalysisModuleAllFalse  = reactiveValues(FlagUpdate = F,
                                                     FlagWB = F,
                                                     FlagPRCC = F,
                                                     FlagELISA = F)
  UploadDataAnalysisModule = reactiveValues(FlagUpdate = F,
                                            FlagWB = F,
                                            FlagPRCC = F,
                                            FlagELISA = F)
  
  observeEvent(input$loadAnalysis_Button,{
    output$loadAnalysis_Error <- renderText({
      validate(
        need(!is.null(input$loadAnalysis_file) && all(file.exists(input$loadAnalysis_file$datapath)) ,
             "Please select one RDs file generated throught the Data Analysis module." )
      )
      
      mess = readRDS(input$loadAnalysis_file$datapath)
      
      validate(
        need(all(names(mess) %in% names(wbResult)) || all(names(mess) %in% names(pcrResult)) || all(names(mess) %in% names(elisaResult)) ,
             paste(mess[["message"]],"\n The file must be RDs saved throught the Data Analysis module." ))
      )
      
      if( all(names(mess) %in% names(wbResult)) ){
        DataAnalysisModule$wbResult <- mess
        UploadDataAnalysisModule$FlagWB = T
      }else if( all(names(mess) %in% names(pcrResult)) ){
        DataAnalysisModule$pcrResult <- mess
        UploadDataAnalysisModule$FlagPRCC = T
      }else if(all(names(mess) %in% names(elisaResult)) ){
        DataAnalysisModule$elisaResult <- mess
        UploadDataAnalysisModule$FlagELISA = T
      }
      
      UploadDataAnalysisModule$FlagUpdate = T
      
      "The RDs file is uploaded with success."
      
    })
  })
  
  observeEvent(UploadDataAnalysisModule$FlagUpdate,{
    if(UploadDataAnalysisModule$FlagUpdate){
      
      if(UploadDataAnalysisModule$FlagWB){
        
        for(nameList in names(DataAnalysisModule$wbResult)) 
          wbResult[[nameList]] <- DataAnalysisModule$wbResult[[nameList]]
        
        # update image WB
        if(!is.null(wbResult$Im)){
          im = wbResult$Im$RGB
          output$TifPlot2 <- renderPlot({
            plot(c(1,dim(im)[2]),c(1,dim(im)[1]), type='n',ann=FALSE)
            rasterImage(im,1,1,dim(im)[2],dim(im)[1])
            if (nrow(wbResult$Planes) > 0) {
              r <- wbResult$Planes
              rect(r$xmin, r$ymin, r$xmax, r$ymax, border = "red")
            }
          })
          
          output$PlanesStructureTable <- renderDT(
            wbResult$Planes,
            server = FALSE,
            editable = list(target = "column",
                            disable = list(columns = 1:length(wbResult$Planes[1,])) ),
            options = list(lengthChange = FALSE, autoWidth = TRUE),
            rownames= FALSE
          )
          
          PanelStructures$data = wbResult$Planes
          
        }
        
        # update lanes
        
        if(!is.null(wbResult$PanelsValue)){
          updateSelectInput(session, "LaneChoice",
                            choices = unique(wbResult$PanelsValue$ID),
                            selected = 0
          )
        }
        
        if(!is.null(wbResult$TruncatedPlots)){
          output$DataPlot <- renderPlot({wbResult$TruncatedPlots})
          Flags$LanesCut = T
        }else if(!is.null(wbResult$Plots)){
          output$DataPlot <- renderPlot({wbResult$Plots})
          Flags$LanesCut = T
        }
        
        output$AUC <- renderTable({
          wbResult$AUCdf  %>% dplyr::select(Lane,Truncation, AUC)
        },width = "100%")
        
        # change pannel
        updateTabsetPanel(session, "SideTabs",
                          selected = "grey")
        
      }else if(UploadDataAnalysisModule$FlagPRCC){
        
        for(nameList in names(DataAnalysisModule$pcrResult)) 
          pcrResult[[nameList]] <- DataAnalysisModule$pcrResult[[nameList]]
        
        choices = ""
        selected = rep("",3)
        
        if(!is.null(pcrResult$Initdata)){
          choices = c("",colnames(pcrResult$Initdata))
        }
        
        if(!is.null(pcrResult$selectPCRcolumns)){
          selected = pcrResult$selectPCRcolumns
        }
        
        updateSelectInput(session,"PCR_gene",
                          choices = choices,
                          selected = selected[1]
        )
        updateSelectInput(session,"PCR_sample",
                          choices = choices,
                          selected = selected[2]
        )
        updateSelectInput(session,"PCR_value",
                          choices = choices,
                          selected = selected[3]
        )
        
        
        if(!is.null(pcrResult$PCRnorm))
          FlagsPCR$norm = T
        
        if(!is.null(pcrResult$PCRbaseline))
          FlagsPCR$baseline = T
        
        # change pannel
        updateTabsetPanel(session, "SideTabs",
                          selected = "uploadPCR")
        
        
      }else if(UploadDataAnalysisModule$FlagELISA){
        
        for(nameList in names(DataAnalysisModule$elisaResult)) 
          elisaResult[[nameList]] <- DataAnalysisModule$elisaResult[[nameList]]
        
        if(!is.null(elisaResult$TablePlot)){
          output$ELISAmatrix <- renderDT(
            elisaResult$TablePlot,
            server = FALSE
          )
        }
        
        if(!is.null(elisaResult$ELISAcell_TIME)){
          
          updateSelectizeInput(inputId = "ELISAcell_TIME",
                               choices = unique(c(elisaResult$ELISAcell_TIME))
          )
          
          updateSelectizeInput(inputId = "ELISAcell_EXP",
                               choices = unique(c(elisaResult$ELISAcell_EXP))
          )
          
          FlagsELISA$AllExp = unique(c(elisaResult$ELISAcell_EXP))
        }
        
        # change pannel
        updateTabsetPanel(session, "SideTabs",
                          selected = "uploadELISA")
        
      }
      UploadDataAnalysisModule = UploadDataAnalysisModuleAllFalse
    }
    
  })
  ### Download files ####
  
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
  
  observe({namesAll <<- unique( c(names(wbResult), names(wbquantResult), names(pcrResult), names(elisaResult)) )})
  
return()
  
  }
