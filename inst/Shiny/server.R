
AUCfunction<-function(AUCdf,PanelsValue,bind=T,session = session,Lane=1,AUCdf.new=NULL){
  if(is.null(AUCdf.new)){
    if(length(AUCdf[,1])==1 & AUCdf$AUC[1] == "0")
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
  AUCdf.new$Lane <- Lane
  
  if(length(AUCdf[,1])==1 & AUCdf$AUC[1] == "0")
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
                              elisaResult = NULL,
                              pcrResult = NULL)
  
  ##### WB analysis
  # DECLARE REACTIVEVALUES FUNCTION HERE
  PanelData = data.frame(Panel_ID = numeric(),
                         xmin = numeric(), ymin = numeric(), 
                         xmax = numeric(), ymax = numeric())

  wbResult <- reactiveValues(Im = NULL,
                             Planes = NULL,
                             PanelsValue = NULL,
                             Plots = NULL,
                             TruncatedPlots = NULL,
                             pl = NULL,
                             IDlane = 0,
                             AUCdf=data.frame(Truncation = "no", AUC = "0", Lane=0  ))
  
  Flags <- reactiveValues( ShowTif = F, LanesCut = F,
                           CutTab="V")
  prev_vals <- NULL
  PanelStructures <- reactiveValues(data = PanelData )
  NumberOfPlanes <- reactiveValues(N = 0)
  PlaneSelected <- reactiveValues(First = NULL)
  
  observeEvent(c(input$LoadingTif),{
    output$LoadingError <- renderText({
      validate(
        need(!is.null(input$imImport) ,
             "Please select a tif file!!" )
      )
      Flags$ShowTif <- TRUE
      ""
    })
    
    if( !is.null(wbResult$Im) )
    { ### alert!!! if it is already present! 
      showModal(modalDialog(
        title = "Important message",
        "Do you want to update the WB data already present?",
        easyClose = TRUE,
        footer= tagList(actionButton("confirmUpload", "Update"),
                        modalButton("Cancel")
        )
      ))
      
      observeEvent(input$confirmUpload,{
        removeModal()
        wbResult <- reactiveValues( Im = NULL,
                                   Planes = NULL,
                                   PanelsValue = NULL,
                                   Plots = NULL,
                                   TruncatedPlots = NULL,
                                   pl = NULL,
                                   IDlane = 0,
                                   AUCdf=data.frame(Truncation = "no", AUC = "0", Lane=0  ))
        Flags$ShowTif <- TRUE
      })
    }
  })
  
  observe({
    if(Flags$ShowTif)
    {
      ListIm = LoadImage(input$imImport$datapath)
      im = ListIm$RGB
      wbResult$Im = ListIm 
      
      output$TifPlot  <- renderPlot({ imageShow(im) })
      output$TifPlot2 <- renderPlot({ 
        #plot(1:(max(dim(im))), type='n')
        plot(c(1,dim(im)[2]),c(1,dim(im)[1]), type='n',ann=FALSE)
        rasterImage(im,1,1,dim(im)[2],dim(im)[1])
        
        if (nrow(PanelStructures$data) > 0) {
          r <- PanelStructures$data
          rect(r$xmin, r$ymin, r$xmax, r$ymax, border = "red")
        }
        
      })#, height = dim(im)[2], width = dim(im)[1])
      
      observeEvent(input$panelSelect_button,{
        NumberOfPlanes$N = NumberOfPlanes$N + 1
        e <- input$plot_brush
        if (!is.null(e)) {
          vals <- data.frame(xmin = round(e$xmin, 1),
                             ymin = round(e$ymin, 1),
                             xmax = round(e$xmax, 1),
                             ymax = round(e$ymax, 1))
          
          if (!identical(vals,prev_vals))  #We dont want to change anything if the values havent changed.
          {
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
      
    }
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
    wbResult$AUCdf
    },width = "100%")
  
  observeEvent(input$GenLines,{
    if(NumberOfPlanes$N >1){
      wbResult$Planes = round(PanelStructures$data)
      print(PanelStructures$data)
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
                                     data.frame(Values = GreyPlane, ID = i, Y = 1:length(GreyPlane) )
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
  
  observeEvent(c(input$LaneChoice),{
    if(Flags$LanesCut & !is.null(wbResult$PanelsValue))
    {
      wbResult$PanelsValue -> PanelsValue
      Plots.Lane <- PanelsValue[which(PanelsValue$ID == input$LaneChoice),]
      colnames(Plots.Lane) = c("Y","ID","X")
      min(Plots.Lane$X)
      
      cat(input$LaneChoice,"\n")
      updateSliderInput(session,"truncX",
                        min = min(Plots.Lane$X),
                        max = max(Plots.Lane$X),
                        value = c(min(Plots.Lane$X), max(Plots.Lane$X) ) ) 
      updateSliderInput(session,"truncV1",
                        min = min(Plots.Lane$X),
                        max = max(Plots.Lane$X),
                        value = c(min(Plots.Lane$X), max(Plots.Lane$X) ) ) 
      updateSliderInput(session,"truncH1",
                        min = min(Plots.Lane$Y),
                        max = max(Plots.Lane$Y),
                        value =min(Plots.Lane$Y) )
      
    }
    
  } )  
  
  observe({  Flags$CutTab <- input$tabs })
  
  observeEvent(c(input$truncX,input$truncH1,input$LaneChoice), {
    if(!is.null(wbResult$PanelsValue))
    {
      wbResult$PanelsValue -> PanelsValue
      IDlane =as.numeric(input$LaneChoice)
      
      wbResult$IDlane <- IDlane
      
      pl<-wbResult$Plots
      
      if(Flags$CutTab=="V")
      {
        MinTrunc<-input$truncX[1]
        MaxTrunc<-input$truncX[2]
        
        vline.dat <- data.frame(ID=as.factor(rep(PanelsValue$ID,2)), vl =0)
        vline.dat  <-  vline.dat[ vline.dat$ID == IDlane, ]
        vline.dat$vl <- c(MinTrunc,MaxTrunc)
        
        pl <- pl + geom_vline(data=vline.dat,aes(xintercept=vl),linetype="dashed")
      }else if(Flags$CutTab=="H")
      {
        TruncY<-input$truncH1[1]
        hline.dat <- data.frame(ID=as.factor(PanelsValue$ID), hl =0)
        hline.dat  <-  hline.dat[ hline.dat$ID == IDlane, ]
        hline.dat$hl <- TruncY
        
        pl <- pl + geom_hline(data=hline.dat,aes(yintercept = hl),linetype="dashed")
      }
      
      output$DataPlot <- renderPlot({pl})
      
      # ### AUC calculation:
      AUCdf<-AUCfunction(wbResult$AUCdf,PanelsValue,Lane = IDlane)
      wbResult$AUCdf <- AUCdf
    }  
    
  })
  
  observeEvent(c(input$TruncateDataV,input$TruncateDataH),{
    
    if( !is.null(wbResult$PanelsValue))
    {
      wbResult$IDlane -> IDlane
      wbResult$PanelsValue -> PanelsValue
      
      wbResult$AUCdf -> AUCdf
      AUCdf.new <- AUCdf[length(AUCdf$Truncation),]
      
      if(Flags$CutTab=="V")
      {
        MinTrunc<-input$truncX[1]
        MaxTrunc<-input$truncX[2]
        AUCdf.new$Truncation <- paste("X = [", MinTrunc," ; ", MaxTrunc ,"]")
        PanelsValue<- PanelsValue[!((PanelsValue$Y < MinTrunc | PanelsValue$Y > MaxTrunc) & PanelsValue$ID == IDlane),]
        PanelsValue$Values[PanelsValue$ID == IDlane] <- PanelsValue$Values[PanelsValue$ID == IDlane] -min(PanelsValue$Values[PanelsValue$ID == IDlane]) 
        
      }else if(Flags$CutTab=="H")
      {
        TruncY<-input$truncH1[1]
        PanelsValue <- PanelsValue[!(PanelsValue$Values<TruncY & PanelsValue$ID == IDlane),]
        PanelsValue$Values[PanelsValue$ID == IDlane] <- PanelsValue$Values[PanelsValue$ID == IDlane] - TruncY
        AUCdf.new$Truncation <- paste("Y = ", TruncY)
      }
      
      wbResult$TruncatedPanelsValue <- PanelsValue
      pl<-wbResult$Plots
      
      output$DataPlot <- renderPlot({pl})
      AUCdf<-AUCfunction(AUCdf.new=AUCdf.new,wbResult$AUCdf,PanelsValue,Lane = IDlane)
      output$AUC <- renderTable({AUCdf})
      wbResult$AUCdf <- AUCdf
    }
  })
  
  #### END WB analysis #####
  
  #### PCR analysis
  
  pcrResult <- reactiveValues(data = NULL)
  
  FlagsPCR <- reactiveValues(DataLoaded = F, norm=F, baseline = F)
  
  observeEvent(c(input$LoadPCR_Button),{
    output$LoadingError_PCR <- renderText({
      validate(
        need(!is.null(input$PCRImport) ,
             "Please select an RT-PCR excel file!!" )
      )
      FlagsPCR$DataLoaded <- TRUE
      ""
    })
    
    if( !is.null(pcrResult$data) )
    { ### alert!!! if it is already present! 
      showModal(modalDialog(
        title = "Important message",
        "Do you want to update the RT-PCR data already present?",
        easyClose = TRUE,
        footer= tagList(actionButton("confirmUploadPCR", "Update"),
                        modalButton("Cancel")
        )
      ))
      
      observeEvent(input$confirmUploadPCR,{
        removeModal()
        pcrResult <- reactiveValues()
        Flags$DataLoaded <- TRUE
      })
    }
  })
  
  observe({
    if(FlagsPCR$DataLoaded){
      library(readxl)
      PCR <- read_excel(input$PCRImport$datapath)
      pcrResult$data = PCR
      
      AllGenes = unique(PCR$Target)
      Exp = unique(PCRexample$Sample)
      
      updateSelectInput(session, "PCRbaseline",
                        choices = Exp )
      updateCheckboxGroupInput(session,"PCRnorm",
                               choices = AllGenes )
      
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
        group_by(Sample,Target) %>%
        dplyr::summarise(Mean = mean(Cq),
                         Sd = sd(Cq)) %>%
        ungroup()
      
      BaselinePCR = NewPCR %>% 
        filter(Sample == BaselineExp) %>%
        rename(BaselineMean=Mean, BaselineSd=Sd) %>%
        select(-Sample)
      
      NewPCR = merge(BaselinePCR,NewPCR,all.y = T,by="Target")
      
      NewPCR = NewPCR %>%
        group_by(Sample,Target) %>%
        dplyr::summarise(dCt = Mean - BaselineMean,
                         Q = 2^{-dCt},
                         Sd = Sd,
                         Mean = Mean)%>%
        ungroup()
      
      OnePCR = NewPCR %>%
        filter(!Target %in% PCRnorm)
      
      NormPCR = NewPCR %>%
        filter(Target %in% PCRnorm ) %>%
        rename(Norm = Target,
               Norm_dCt = dCt,
               NormQ = Q,
               NormSd = Sd,
               NormMean = Mean)
      
      CompPRC = merge(OnePCR,NormPCR)
      
      CompPRC = CompPRC %>% group_by(Sample,Target,Norm) %>%
        dplyr::summarise(Qnorm = Q/NormQ,
                         SDddct = sqrt(Sd^2+NormSd^2),
                         SDrq = log(2)*Qnorm*SDddct) %>%
        ungroup()
      
      print(CompPRC)
      
      AllGenes = unique(PCR$Target)
         
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
      output$PCRplots <- renderPlot({
        CompPRC %>%
          ggplot(aes(x= Target, y = Qnorm, fill = Sample)) + 
          facet_wrap(~Norm,ncol = 1) +
          geom_bar(stat = "identity",position = "dodge")

      },width = "100%")
        
      
      for (i in AllGenes){
        local({
          my_i <- i
          tablename <- paste("tablename", my_i, sep="")
          output[[tablename]] <- renderTable({
            NewPCR %>% filter(Target == my_i)
          })
          
          ComparisonPCR = list()
          if(my_i %in% AllGenes[-which(AllGenes %in% PCRnorm)]){
            tablename <- paste("CompTablename", my_i, sep="")
            output[[tablename]] <- renderTable({
              CompPRC %>% 
                filter(Target == my_i) %>%
                arrange(Norm,Sample)
            })
          }
        })    
      }
      
    }
  })


  #### END PCR analysis
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      "Report.pdf"
    },
    content = function(file) {
      rmarkdown::render("report.Rmd",output_file = file, params = reactiveValuesToList(wbResult) )
    }
  )
  
}
