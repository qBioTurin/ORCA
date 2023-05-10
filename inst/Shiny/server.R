shiny.maxRequestSize=1000*1024^2
shiny.launch.browser = .rs.invokeShinyWindowExternal

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
readfile <- function(filename,type,colname = T) {
  out <- tryCatch(
    {
      if(type == "RDs"){
        x = readRDS(filename)
        if(!length(x) %in% c(6,9) )
          return(
            list(message = "The RDs file must be generated from InteGreat app.",
                 call = "")
          )
        x
      }else if(type == "RDsMulti"){
        x = lapply(filename, readRDS)
        for( i in 1:length(x))
          if(length(x[[i]]) != 6 )
            return(
              list(message = "The RDs file must be generated from InteGreat app.",
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
      AUCdf.new <- AUCdf[length(AUCdf$Truncation),]
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
  AllResult <- reactiveValues(wbResult = NULL,
                              WBanalysis = NULL,
                              NormWBanalysis = NULL,
                              RappWBanalysis = NULL,
                              elisaResult = NULL,
                              pcrResult = NULL)
  
  InteGreat<- reactiveValues(data = NULL,
                             wbTabs = NULL, 
                             pcrTabs = NULL,
                             elisaTabs=NULL,
                             otherTabs = NULL,
                             otherTabsMean = NULL)
  
  ### WB analysis ####
  # DECLARE REACTIVEVALUES FUNCTION HERE
  PanelData = data.frame(Panel_ID = numeric(),
                         xmin = numeric(), ymin = numeric(), 
                         xmax = numeric(), ymax = numeric())
  
  wbResult <- list(Normalizer = NULL,
                   Im = NULL,
                   Planes = NULL,
                   TruncatedPanelsValue = NULL,
                   PanelsValue = NULL,
                   Plots = NULL,
                   TruncatedPlots = NULL,
                   pl = NULL,
                   AUCdf=data.frame(Truncation = "-", AUC = "-", Lane="-"  ))
  
  observeEvent(input$saveWBButton,{
    AllResult$wbResult = wbResult
    })
  
  EmptyRes <- reactiveValues(wbResults0 = wbResult)
  
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
      AllResult$wbResult$Im = mess
      
      updateTabsetPanel(session, "SideTabs",
                        selected = "plane")
      
      "The image is uploaded with success"
    })
    
    if( !is.null(AllResult$wbResult$Im) )
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
    AllResult$wbResult = EmptyRes$wbResults0
    
    output$AUC <- renderTable({
      AllResult$wbResult$AUCdf
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
      AllResult$wbResult$Im = mess
      
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
      AllResult$wbResult$Im -> ListIm 
      im = ListIm$RGB
      
      output$TifPlot  <- renderPlot({ imageShow(im) })
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
            PanelStructures$data <- data.frame(Panel_ID = 1,vals)
          }else{
            PanelStructures$data <- rbind(PanelStructures$data,
                                          cbind(data.frame(Panel_ID = nrow(PanelStructures$data)+1),
                                                vals))
          }
        }else{
          PanelStructures$data <- rbind(PanelStructures$data,
                                        cbind(data.frame(Panel_ID = nrow(PanelStructures$data)+1),
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
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
    
  })
  
  output$PlanesStructureTable <- renderTable({
    PanelStructures$data
  },width = "100%")
  
  output$AUC <- renderTable({
    AllResult$wbResult$AUCdf
  },width = "100%")
  
  observeEvent(input$GenLanes,{
    if(NumberOfPlanes$N >1){
      AllResult$wbResult$Planes = round(PanelStructures$data)
      print(PanelStructures$data)
      Flags$LanesCut= T
    }else{
      Flags$LanesCut= F
    }
    
    if(Flags$LanesCut)
    {
      
      updateTabsetPanel(session, "SideTabs",
                        selected = "grey")
      
      im = AllResult$wbResult$Im$WB
      PanelData = AllResult$wbResult$Planes
      
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
                                                ID = paste("Lane",i), Y = 1:length(GreyPlane) )
                                   },
                                   im,PanelData)
      )
      
      
      pl<-ggplot(PanelsValue, aes(x =Y,y=Values)) +
        geom_line() + theme_bw() +
        facet_wrap(~ID)
      
      AllResult$wbResult$PanelsValue <- PanelsValue
      AllResult$wbResult$Plots <- pl
      
      updateSelectInput(session, "LaneChoice",
                        choices = unique(PanelsValue$ID),
                        selected = 0
      )
      
      output$DataPlot <- renderPlot({pl})
      
    }
  })
  
  observeEvent(c(input$LaneChoice),{
    if(Flags$LanesCut & !is.null(AllResult$wbResult$PanelsValue))
    {
      if(!is.null(AllResult$wbResult$TruncatedPanelsValue ))
      {
        pl <- AllResult$wbResult$TruncatedPlots    
        AllResult$wbResult$TruncatedPanelsValue -> PanelsValue
      }
      else{
        AllResult$wbResult$PanelsValue -> PanelsValue
        pl<-AllResult$wbResult$Plots
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
    if(!is.null(AllResult$wbResult$PanelsValue))
    {
      if(!is.null(AllResult$wbResult$TruncatedPanelsValue ))
      {
        pl <- AllResult$wbResult$TruncatedPlots    
        AllResult$wbResult$TruncatedPanelsValue -> PanelsValue
      }
      else{
        AllResult$wbResult$PanelsValue -> PanelsValue
        pl<-AllResult$wbResult$Plots
      }
      
      IDlane = input$LaneChoice
      Flags$IDlane <- IDlane
      
      if(Flags$CutTab=="V")
      {
        MinTrunc<-input$truncV[1]
        MaxTrunc<-input$truncV[2]
        
        vline.dat <- data.frame(ID=as.factor(rep(PanelsValue$ID,2)), vl =0)
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
      AllResult$wbResult$AUCdf <- AUCfunction(AllResult$wbResult$AUCdf,PanelsValue,Lane = IDlane)
    }  
    
  })
  
  observeEvent(c(input$actionButton_TruncV,input$actionButton_TruncH),{
    
    if( !is.null(AllResult$wbResult$PanelsValue))
    {
      Flags$IDlane -> IDlane
      if(!is.null(AllResult$wbResult$TruncatedPanelsValue ))
      {
        pl <- AllResult$wbResult$TruncatedPlots    
        AllResult$wbResult$TruncatedPanelsValue -> PanelsValue
      }
      else{
        AllResult$wbResult$PanelsValue -> PanelsValue
        pl<-AllResult$wbResult$Plots
      }
      
      AllResult$wbResult$AUCdf -> AUCdf
      AUCdf.new <- AUCdf[length(AUCdf$Truncation),]
      lastTrunc = AUCdf %>% filter(Lane == IDlane, row_number()==n() ) %>% dplyr::select(Truncation)
      if(length(lastTrunc$Truncation) > 0 & lastTrunc$Truncation != "-")
        AUCdf.new$Truncation <- lastTrunc$Truncation
      
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
      
      AllResult$wbResult$TruncatedPanelsValue <- PanelsValue
      AllResult$wbResult$TruncatedPlots <- pl
      output$DataPlot <- renderPlot({pl})
      AUCdf<-AUCfunction(AUCdf.new=AUCdf.new,AllResult$wbResult$AUCdf,PanelsValue,Lane = IDlane)
      output$AUC <- renderTable({AUCdf})
      AllResult$wbResult$AUCdf <- AUCdf
    }
  })
  
  observeEvent(input$actionButton_ResetPlanes,{
    
    AllResult$wbResult$AUCdf = EmptyRes$wbResults0$AUCdf
    AllResult$wbResult$TruncatedPanelsValue = EmptyRes$wbResults0$TruncatedPanelsValue
    
    output$AUC <- renderTable({
      AllResult$wbResult$AUCdf
    },width = "100%")
    output$DataPlot <- renderPlot({AllResult$wbResult$Plots})
  })
  
  output$downloadButton_saveRes <- downloadHandler(
    filename = function() {
      paste('WBanalysis-', Sys.Date(), '.RDs', sep='')
    },
    content = function(file) {
      results = AllResult$wbResult
      saveRDS(results, file = file)
    }
  )
  
  # quantification WB
  observeEvent(input$actionB_loadingWB,{
    
    output$LoadingErrorWB <- renderText({
      validate(
        need(!is.null(input$WBImport) && file.exists(input$WBImport$datapath) ,
             "Please select a RDs file!!" )
      )
      
      mess = readfile(
        filename = input$WBImport$datapath,
        type = "RDs"
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]])
      )
      
      AllResult$WBanalysis = mess
      
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
        type = "RDs"
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             mess[["message"]])
      )
      
      AllResult$NormWBanalysis = mess
      
      "The RDs is uploaded with success"
    })
    
  })
  
  observe({
    if(is.null(AllResult$NormWBanalysis)){
      table = EmptyRes$wbResults0$AUCdf
    }else{
      table = AllResult$NormWBanalysis$AUCdf
    }
    output$AUC_WBnorm <- renderDT(
      table, 
      filter = 'top', server = FALSE, 
      options = list(lengthChange = FALSE, autoWidth = TRUE),
      rownames= FALSE
    )
  })
  observe({
    
    if(is.null(AllResult$WBanalysis)){
      table = EmptyRes$wbResults0$AUCdf
    }else{
      table = AllResult$WBanalysis$AUCdf
    }
    output$AUC_WB <- renderDT(
      table,
      filter = 'top', server = FALSE, 
      options = list(lengthChange = FALSE, autoWidth = TRUE),
      rownames= FALSE
    )
  })
  observe({
    
    if(!is.null(AllResult$WBanalysis) & !is.null(AllResult$NormWBanalysis)){
      indexesWB = input$AUC_WB_rows_selected 
      indexesWBnorm = input$AUC_WBnorm_rows_selected 
      
      if(length(indexesWB) > 0)
        tbWB = AllResult$WBanalysis$AUCdf[indexesWB,]
      else
        tbWB = NULL
      
      if(length(indexesWBnorm) > 0)
        tbWBnorm = AllResult$NormWBanalysis$AUCdf[indexesWBnorm,] %>% rename(AUC_Norm = AUC,Truncation_Norm = Truncation)
      else
        tbWBnorm = NULL 
      
      print(tbWBnorm)
      print(tbWB)
      
      if(!is.null(tbWBnorm) & !is.null(tbWB)){
        table = merge(tbWBnorm, tbWB, by  = "Lane",all = T)
        table$Rel.Norm. = table$AUC/table$AUC_Norm
        table$ExpName = "To set"
        table = table %>% dplyr::select(ExpName, Lane, Truncation_Norm, Truncation, AUC_Norm, AUC, Rel.Norm.)
      }else{
        table = EmptyRes$wbResults0$AUCdf 
      }
      
    }else{
      table = EmptyRes$wbResults0$AUCdf 
    }
    
    AllResult$RappWBanalysis = table
    
    output$AUC_rapp <- renderDT(
      table,
      filter = 'top',
      server = FALSE,
      editable = list(target = "column", disable = list(columns = 2:7) ),
      options = list(lengthChange = FALSE, autoWidth = TRUE),
      rownames= FALSE
    )
    
  })
  
  ### End WB analysis ####
  
  
  #### PCR analysis ####
  
  pcrResult0 = pcrResult = reactiveValues(data = NULL,
                                         PCRnorm = NULL,
                                         BaselineExp = NULL,
                                         CompPRC = NULL,
                                         NewPCR = NULL)
  
  observeEvent(input$savePCRButton,{
    AllResult$pcrResult = reactiveValuesToList(pcrResult, all.names = T) 
  })
  
  FlagsPCR <- reactiveValues(Initdata= NULL, norm=F, baseline = F)
  observeEvent(input$LoadPCR_Button,{
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
      
      FlagsPCR$Initdata = mess
      
      "The RT-qPCR excel is uploaded with success"
    })
    
    if( !is.null(FlagsPCR$Initdata) )
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
  })
  observeEvent(input$confirmUploadPCR,{
    removeModal()
    pcrResult = pcrResult0
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
      
      FlagsPCR$Initdata = mess
      
      "The RDs is uploaded with success"
    })
  })
  observe({
    updateSelectizeInput(session,"selectPCRcolumns",
                         choices = colnames(FlagsPCR$Initdata),
                         selected = colnames(FlagsPCR$Initdata)[1]
    )
  })
  
  
  observeEvent(input$selectPCRcolumns,{
    
    if( !is.null(FlagsPCR$Initdata) ){
      PCR = FlagsPCR$Initdata
      colNames = colnames(PCR)
      output$PCRpreview = renderTable({
        if(length(input$selectPCRcolumns)!=0 ){
          tmp = PCR[,input$selectPCRcolumns]
          colnames(tmp) = c("Gene", "Sample", "Value")[1:length(colnames(tmp))]
          head(tmp) 
        }
        else
          NULL
      })
      
      if(length(input$selectPCRcolumns)==3){
        tmp = PCR[,input$selectPCRcolumns]
        colnames(tmp) = c("Gene", "Sample", "Value")[1:length(colnames(tmp))]
        pcrResult$data = tmp
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
    if(FlagsPCR$baseline & FlagsPCR$norm){
      
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
      
      print(CompPRC)
      
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
    AllResult$pcrResult = reactiveValuesToList(pcrResult)
  })
  
  #### END PCR analysis ####
  
  ### ELISA analysis ####
  elisaResult = elisaResult0 = reactiveValues(data = NULL,
                                              ELISAcell_TIME = NULL,
                                              ELISAcell_EXP = NULL,
                                              MapBaseline = NULL)
  
  observeEvent(input$saveElisaButton,{
    AllResult$elisaResult = reactiveValuesToList(elisaResult, all.names = T) 
  })
  
  
  FlagsELISA <- reactiveValues(Initdata= NULL,
                               cellCoo = NULL,
                               EXPselected = "",
                               EXPcol = NULL,
                               expToselect="")
  
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
      
      FlagsELISA$Initdata = mess
      
      "The ELISA excel is uploaded with success"
    })
    
    if( !is.null(FlagsELISA$Initdata) )
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
    elisaResult = elisaResult0
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
      
      FlagsELISA$Initdata = mess
      "The RDs is uploaded with success"
    })
  })
  observe({
    if( !is.null(FlagsELISA$Initdata) && is.null(elisaResult$data) ){
      ELISAtb = FlagsELISA$Initdata
      
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
      elisaResult$data = ELISAtb
    }
  })
  observeEvent(input$ELISAmatrix_cell_clicked,{
    if(length(input$ELISAmatrix_cell_clicked)!=0){
      cellSelected= as.numeric(input$ELISAmatrix_cell_clicked)
      FlagsELISA$cellCoo = cellCoo = c(cellSelected[1],cellSelected[2]+1)
      print(cellCoo)
      print(elisaResult$ELISAcell_TIME[ cellCoo[1],cellCoo[2] ])
      print(elisaResult$ELISAcell_EXP[ cellCoo[1], cellCoo[2] ])
      updateTextInput(inputId = "ELISAcell_TIME",
                      value = ifelse(is.null(elisaResult$ELISAcell_TIME[cellCoo[1],cellCoo[2]]),"",elisaResult$ELISAcell_TIME[cellCoo[1],cellCoo[2]])
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
      ELISAtb = elisaResult$data
      cellCoo = FlagsELISA$cellCoo
      elisaResult$ELISAcell_EXP[cellCoo[1],cellCoo[2]] = input$ELISAcell_EXP
      ELISAtb$x$data[cellCoo[1],paste0("Col",cellCoo[2])] = input$ELISAcell_EXP
      
      if(! input$ELISAcell_EXP %in% FlagsELISA$EXPselected){
        FlagsELISA$EXPselected = unique(c(FlagsELISA$EXPselected,input$ELISAcell_EXP))
        print(FlagsELISA$EXPselected)
      }
      
      EXPcol = rainbow(n = length(FlagsELISA$EXPselected),alpha = 0.4)
      names(EXPcol) = FlagsELISA$EXPselected
      EXPcol[names(EXPcol) == ""] = "white"
        FlagsELISA$EXPcol = EXPcol
        print(FlagsELISA$EXPcol)
        cols.color = grep(x = colnames(ELISAtb$x$data),pattern = "Col",value = T)
        cols.keep = grep(x = colnames(ELISAtb$x$data),pattern = "V",value = T)
        elisaResult$data = datatable(ELISAtb$x$data,
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
  })
  
  ## dynamic select input ELISA
  toListen <- reactive({
    
    InputEXP = lapply(FlagsELISA$expToselect, function(i) input[[paste0("Exp",i)]])
    InputEXP = InputEXP[-which(sapply(InputEXP, is.null))]
    
    if(length(InputEXP) > 0){
      listReturn = list( FlagsELISA$ChangeFromSelection  )
    }
    else{
      listReturn = InputEXP
      listReturn[["ChangeFromSelection"]] = FlagsELISA$ChangeFromSelection 
    }

    listReturn
    
  })
               
  observeEvent(toListen(),{
    if(length(FlagsELISA$EXPselected) > 1){
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
      
      exp_toDelete = do.call( rbind,
        lapply(exp,function(i){
          if( length(input[[paste0("Exp",i)]]) > 0 && input[[paste0("Exp",i)]] != ""){
            input[[paste0("Exp",i)]]
          }
        })
      )
      
      if(length(exp_toDelete) == 0)
        exp_toDelete = ""
      
      FlagsELISA$expToselect = exp[!exp%in%exp_toDelete]
    }
  })
  
  observeEvent( FlagsELISA$expToselect ,{
    expToselect = FlagsELISA$expToselect
    print(expToselect)
    output$ElisaBaselineSelection <- renderUI({
      select_output_list <- lapply(expToselect, function(i) {
        
        if(length(input[[paste0("Exp",i)]])>0)
          expsel = input[[paste0("Exp",i)]]
        else 
          expsel = ""
        
        exp = FlagsELISA$EXPselected
        exp = exp[exp != ""]
        
        selectInput(inputId = paste0("Exp",i),
                    label = i,
                    choices = c("",exp[exp!=i]),
                    selected = expsel)
      })
      do.call(tagList, select_output_list)
    })
  })
  
  observeEvent(elisaResult$data, {
    ELISAtb = elisaResult$data
    output$ELISAmatrix <- renderDT(
      ELISAtb,
      server = FALSE
    )
    
    ##### Plot the vlaues selected!
    matTime =  as.matrix(elisaResult$ELISAcell_TIME)
    matExp =  as.matrix(elisaResult$ELISAcell_EXP)
    
    if( !( all(matTime == "")  || all(matExp == "") ) ){
      mat = as.matrix(FlagsELISA$Initdata)
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
      
      output$ELISAinitplots <- renderPlot(
        ggplot(elisaTot, aes(x = time, y=values, col = exp, group = exp),alpha = 1.4) +
          geom_point() +
          # geom_line(linetype = "dashed") +
          scale_color_manual(values = FlagsELISA$EXPcol) + 
          theme_bw()+
          labs(x = "Times", y = "Values", col = "Exp")+
          theme(legend.position = c(0, 1), 
                legend.justification = c(0, 1),
                legend.direction = "vertical",
                legend.background = element_rect(size=0.5,
                                                 linetype="solid",
                                                 colour ="black"))
      )
    }
  })
  
  observeEvent(input$MeanCalcELISA_Button, {
    
    if(!is.null(elisaResult$MapBaseline)){
      MapBaseline = elisaResult$MapBaseline %>% na.omit()
      
      mat = as.matrix(FlagsELISA$Initdata)
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
      elisaTot = merge(elisaV,merge(elisaT,elisaE)) %>% filter(exp != "")
      
      MapBaseline = merge(MapBaseline,elisaTot,by.y = "exp", by.x = "Baseline") %>% 
        dplyr::select(Baseline,Exp,values,time) %>%
        rename(BaseValues = values)
      
      elisaTot = merge(elisaTot,MapBaseline, by.x = c("exp","time"), by.y = c("Exp","time"))
      
      if(length(elisaTot[,1]) != 0 ){
        elisamean = elisaTot %>%
          group_by(exp,time) %>%
          dplyr::summarise(MeanV = mean(values)/BaseValues * 100,
                           MedianV = median(values)/BaseValues * 100 ) %>%
          na.omit()
        
        output$ELISAtables = renderDT(elisamean)
        
        output$ELISAplots = renderPlot(
          elisamean%>%
            ggplot()+
            geom_line(aes(x = time, y = MeanV, col=exp,group = exp))+
            theme_bw()
        )
      }else{
        output$ELISAtables = renderDT(data.frame(Error = "No baseline is associated with the experiment replicants, or the time do not match!"))
      }

      
    }
    
  })
  
  
  
  ### End ELISA analysis ####
  
  ### Proteomic ####
  
  Proteomic = reactiveValues(Default = NULL, User = NULL, Data = NULL)
  
  # default
  protfile = system.file("Data/Proteomic","pmic13104Simplified.xlsx", package = "GelAnalyser")
  # protfile = "~/Desktop/GIT/R_packages_project/GelAnalyser/inst/Data/Proteomic/pmic13104Simplified.xlsx"
  if(protfile == ""){
    # In this case it means that we are inside the docker
    protfile = "~/../home/data/Examples/Proteomic/pmic13104Simplified.xlsx"
  }
  Default = readxl::read_excel(protfile)
  Proteomic$Default = Default
  Proteomic$Data = Default %>%
    dplyr::select(`Protein name`,iBAQ) %>%
    rename(Value = iBAQ, Name = `Protein name`)
  
  output$ProteomicDefault_table <- renderDT(
    Proteomic$Default,
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
             paste(mess[["message"]],"\n all the files must be RDs saved from InteGreat app." ))
      )
      
      
      colmns = colnames(mess[,sapply(mess, is.numeric)])
      validate(
        need(length(colmns) > 0 ,
             "At least one column should be numeric."
        )
      )
      
      updateSelectInput(inputId = "RescalingColms_User",
                        choices = colmns ) 
      Proteomic$User = mess
      "The excel file is uploaded with success"
    })
  })
  observeEvent(input$ResetProt_Button,{
    Proteomic$User = NULL
    Proteomic$Default -> Default
    Proteomic$Data = Default %>%
      dplyr::select(`Protein name`,iBAQ) %>%
      rename(Value = iBAQ, Name = `Protein name`)
    
  })
  
  output$ProteomicUser_table <- renderDT(
    Proteomic$User,
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
  
  observeEvent(input$ProteomicUser_table_columns_selected,
               {
                 if(!is.null(Proteomic$User)){
                   output$LoadingError_Prot <- renderText({
                     
                     validate(
                       need(length(input$ProteomicUser_table_columns_selected) == 2 ,
                            "Please select only two columns (one numeric and one character)." )
                     )
                     
                     prot = Proteomic$User[,input$ProteomicUser_table_columns_selected+1]
                     validate(
                       need(all(sapply(prot, class) %in% c("character", "numeric")),
                            "Please select only two columns (one numeric and one character)." )
                     )
                     
                     protType = sapply(prot, class)
                     names(prot)[protType == "numeric"] = "Value"
                     names(prot)[protType == "character"] = "Name"
                     Proteomic$Data = prot
                   })
                 }
               })
  # rescaling
  observeEvent(input$InputDefault_rescaling,{
    validate(
      need(input$InputDefault_rescaling > 0 ,
           "Please the rescaling factor should be > 0" )
    )
    
    if(!is.null(Proteomic$Default)){
      Proteomic$Default$iBAQ = Proteomic$Default$iBAQ/input$InputDefault_rescaling
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
    if(!is.null(Proteomic$User)){
      Proteomic$User[,colms] = Proteomic$User[,colms]/input$InputUser_rescaling
    }
    
  })
  
  ### end proteomic ###
  
  ### integration ####
  observeEvent(input$LoadIntG_Button,{
    output$LoadingError_IntG <- renderText({
      validate(
        need(!is.null(input$IntGImport) && all(file.exists(input$IntGImport$datapath)) ,
             "Please select one or more RDs files!!" )
      )
      
      mess = readfile(
        filename = input$IntGImport$datapath,
        type = "RDsMulti"
      )
      
      validate(
        need(!setequal(names(mess),c("message","call")) ,
             paste(mess[["message"]],"\n all the files must be RDs saved from InteGreat app." ))
      )
      
      InteGreat$data = mess
      "The RDs files are uploaded with success"
    })
  })
  
  observe({
    if(!is.null(InteGreat$data)){
      prot = Proteomic$Data
      data = InteGreat$data
      names(data) = paste0("DataSet",1:length(data))
      
      namesAnalysis = c("pcrResult","RappWBanalysis","elisaResult")
      
      for(n in namesAnalysis){
        subdata = lapply(data,"[[",n)
        subdata = subdata[!sapply(subdata, is.null)]
        
        if(length(subdata)>0){
          if(n == "pcrResult"){
            pcrTabs = do.call("rbind",lapply(1:length(subdata),function(x){
              subdata[[x]]$CompPRC$Dataset =  names(subdata)[[x]]
              subdata[[x]]$CompPRC
            })
            )
            updateSelectizeInput(inputId = "SelectGene",
                                 choices = unique(pcrTabs$Gene),
                                 selected = unique(pcrTabs$Gene)[1])
            
            InteGreat$pcrTabs = pcrTabs
          }else if(n == "RappWBanalysis"){
            wbTabs = do.call("rbind",lapply(1:length(subdata),function(x){
              subdata[[x]]$Dataset =  names(subdata)[[x]]
              subdata[[x]] %>% dplyr::select(ExpName,Lane,Dataset,Rel.Norm.) %>%
                group_by(ExpName,Lane) %>%
                dplyr::mutate(Mean = mean(Rel.Norm., na.omit = T)) %>%
                ungroup()
            })
            )
            wbTabs1 = wbTabs %>% dplyr::select(-Mean) %>%
              tidyr::spread(Dataset, Rel.Norm.)
            wbTabs2 = wbTabs %>% dplyr::select(-Rel.Norm.,-Dataset) %>%
              distinct()
            
            InteGreat$wbTabs = merge(wbTabs1,wbTabs2)
            updateSelectizeInput(inputId = "Selectprot_wb",
                                 choices = c("",unique(prot$Name)),
                                 selected = "",
                                 server = T )
          }else{
            
          }
        }
      }
    }
  })
  
  observeEvent(input$SelectGene,{
    if(!is.null(InteGreat$pcrTabs) && length(input$SelectGene)>0 && input$SelectGene!=""){
      pcrTabs = InteGreat$pcrTabs
      SelectedGene = input$SelectGene
      output$tables_IntG_pcr <- renderUI({fluidRow(pcrTab.generation(pcrTabs,SelectedGene)) })
      prot = Proteomic$Data
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
    InteGreat$wbTabs
  },
  filter = 'top',
  server = FALSE,
  options = list(lengthChange = FALSE,
                 autoWidth = TRUE),
  rownames= FALSE
  )
  observeEvent(input$Selectprot_wb,{
    if(!is.null(InteGreat$wbTabs)){
      wbTabs = InteGreat$wbTabs
      prot = Proteomic$Data
      if(!is.null(input$Selectprot_wb) && length(input$Selectprot_wb)>0 &&input$Selectprot_wb!= "" ){
        as.numeric(prot[prot$Name == input$Selectprot_wb,"Value"]) -> pr
        wbTabs = wbTabs %>% dplyr::mutate(`Rel.Prot.` = Mean * pr )
      }
      else
        wbTabs = wbTabs[,ifelse("Rel.Prot." %in% colnames(wbTabs),
                                colnames(wbTabs)[!"Rel.Prot." %in% colnames(wbTabs)],
                                colnames(wbTabs) )]
    }
    else{
      wbTabs = NULL
    }
    InteGreat$wbTabs <- wbTabs 
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
      
      InteGreat$otherTabs = mess
      "The excel file is uploaded with success"
    })
  })
  
  observe({
    if(!is.null(InteGreat$otherTabs)){
      
      updateSelectizeInput(inputId = "Selectprot_other",
                           choices = c("",unique(Proteomic$Data$Name)),
                           selected = "",
                           server = T )
    }
  })
  
  output$Other_table <- renderDT({
    InteGreat$otherTabs
  },
  filter = 'top',
  server = FALSE,
  selection = list(target = 'column'),
  options = list(lengthChange = FALSE,
                 autoWidth = TRUE),
  rownames= FALSE
  )
  
  output$Other_tableMean <- renderDT({
    InteGreat$otherTabsMean
  },
  filter = 'top',
  server = FALSE,
  options = list(lengthChange = FALSE,
                 autoWidth = TRUE),
  rownames= FALSE
  )
  
  observeEvent(input$Other_table_columns_selected,{
    if(!is.null(InteGreat$otherTabs) && !is.null(input$Other_table_columns_selected) && length(input$Other_table_columns_selected)>0){
      
      prot = Proteomic$Data
      data = InteGreat$otherTabs
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
        
        InteGreat$otherTabsMean = otherTabs
      }
    }
  })
  
  observeEvent(input$Selectprot_other,{
    if(!is.null(InteGreat$otherTabsMean)){
      if(input$Selectprot_other != ""){
        prot = Proteomic$Data
        as.numeric(prot[prot$Name == input$Selectprot_other,"Value"]) -> pr
        
        InteGreat$otherTabsMean$`Rel.Prot.`  = InteGreat$otherTabsMean$Mean * pr
      }else{
        InteGreat$otherTabsMean$`Rel.Prot.`  = InteGreat$otherTabsMean$Mean
      }
    }
  })
  ### Ed other integration
  
  ### Save files ####
  
  output$downloadRDSwholeAnalysis <- downloadHandler(
    filename = function() {
      paste('InteGreatAnalysis-', Sys.Date(), '.RDs', sep='')
    },
    content = function(file) {
      saveRDS(reactiveValuesToList(AllResult), file = file)
    }
  )
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      "Report.pdf"
    },
    content = function(file) {
      rmarkdown::render("report.Rmd",
                        output_file = file,
                        params = reactiveValuesToList(AllResult) )
    }
  )
  
  #### end save files ###
  
  
  
}
