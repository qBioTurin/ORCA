readfile <- function(filename,type) {
  out <- tryCatch(
    {
      if(type == "RDs"){
        readRDS(filename)
      }else if(type == "Excel"){
        readxl::read_excel(filename)
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
  AUC <- sum(diff(PanelsValue.Lane$Y[id])*rollmean(PanelsValue.Lane$Values[id],2))
  AUCdf.new$AUC <- AUC
  AUCdf.new$Lane <- paste(Lane)
  
  if(length(AUCdf[,1])==1 & AUCdf$AUC[1] == "-")
  {
    A<-AUCdf.new
  }else{
    A<-rbind(AUCdf,AUCdf.new) 
  }
  
  return(unique(A)) 
  #output$AUC <- renderTable({AUCdf2})
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
                              elisaResult = NULL,
                              pcrResult = NULL)
  
  ##### WB analysis
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
  
  AllResult$wbResult = wbResult
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
        need(!is.character(mess[[1]]) ,
             mess[[1]] )
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
        need(!is.character(mess[[1]]) ,
             mess[[1]] )
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
      AUCdf<-AUCfunction(AllResult$wbResult$AUCdf,PanelsValue,Lane = IDlane)
      AllResult$wbResult$AUCdf <- AUCdf
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
      
      if(Flags$CutTab=="V")
      {
        MinTrunc<-input$truncV[1]
        MaxTrunc<-input$truncV[2]
        AUCdf.new$Truncation <- paste("X = [", MinTrunc," ; ", MaxTrunc ,"]")
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
        AUCdf.new$Truncation <- paste("Y = ", TruncY)
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
    output$LoadingError <- renderText({
      validate(
        need(!is.null(input$WBImport) && file.exists(input$WBImport$datapath) ,
             "Please select a tif file!!" )
      )
      
      mess = readfile(
        filename = input$WBImport$datapath,
        type = "RDs"
      )
      
      validate(
        need(!is.character(mess[[1]]) ,
             mess[[1]] )
      )
      
      AllResult$WBanalysis = mess
      
      "The RDs is uploaded with success"
    })
    
  })
  observeEvent(input$actionB_loadingNormWB,{
    output$LoadingErrorNormWB <- renderText({
      validate(
        need(!is.null(input$NormWBImport) && file.exists(input$NormWBImport$datapath) ,
             "Please select a tif file!!" )
      )
      
      mess = readfile(
        filename = input$NormWBImport$datapath,
        type = "RDs"
      )
      
      validate(
        need(!is.character(mess[[1]]) ,
             mess[[1]] )
      )
      
      AllResult$NormWBanalysis = mess
      
      "The RDs is uploaded with success"
    })
    
  })
  
  observe({
    if(is.null(AllResult$WBanalysis)){
      table = EmptyRes$wbResults0$AUCdf
    }else{
      table = AllResult$WBanalysis$AUCdf
    }
    output$AUC_WB <- renderTable({
      table
    },width = "100%")
  })
  observe({
    if(is.null(AllResult$NormWBanalysis)){
      table = EmptyRes$wbResults0$AUCdf
    }else{
      table = AllResult$NormWBanalysis$AUCdf
    }
    output$AUC_WBnorm <- renderTable({
      table
    },width = "100%")
  })
  
  #### END WB analysis #####
  
  #### PCR analysis
  
  pcrResult = pcrResult = reactiveValues(data = NULL,
                                         PCRnorm = NULL,
                                         BaselineExp = NULL,
                                         CompPRC = NULL,
                                         NewPCR = NULL)
  
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
        need(!is.character(mess[[1]]) ,
             mess[[1]] )
      )

      FlagsPCR$Initdata = mess
      
      "The RDs is uploaded with success"
    })
  })
  
  observe({
    print("HERE updateSelectizeInput ")
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
        select(-Sample)
      
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
  
  #### END PCR analysis
  
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
  
}
