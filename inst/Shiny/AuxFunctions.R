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
readfile <- function(filename,type,colname = T, namesAll = namesAll, allDouble = F) {
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
        x = readxl::read_excel(filename,col_names = colname)
        if(allDouble){
          # we check the presence of not double values to set as NA
          xstr = which(sapply(1:dim(x)[1], function(i) !is.double(x[[i]])) )
          if(length(xstr)>0)
            for(i in xstr)
              x[[i]] = as.double(x[[i]])
        }
        return(x)
      }else{
        LoadImage(filename)
        #imageShow(r2g)
      }
      
    },
    error=function(cond) {
      # message(cond)
      # Choose a return value in case of error
      return(cond)
    }
    # warning=function(cond) {
    #   # message(cond)
    #   # Choose a return value in case of warning
    #   return(cond)
    # }
  )    
  return(out)
}

AUCfunction<-function(AUCdf,PanelsValue,bind=T,session = session,SName="1",AUCdf.new=NULL){
  if(is.null(AUCdf.new)){
    
    if(length(AUCdf[,1])==1 & AUCdf$AUC[1] == "-")
    {
      AUCdf.new <- AUCdf
    }else{
      AUCdf2 = AUCdf %>% filter(SampleName == SName)
      
      if(length(AUCdf2[,1]) == 0){
        AUCdf.new = AUCdf2
        AUCdf.new[1,] = rep(NA,length(names(AUCdf2)))
        AUCdf.new$Truncation = "-"
        AUCdf.new$SampleName = SName
      }else{
        AUCdf.new <- AUCdf2[length(AUCdf2$Truncation),]
      }
    }
  }
  
  PanelsValue.Lane <- PanelsValue[which(PanelsValue$ID == SName),]
  id <- order(PanelsValue.Lane$Y)
  AUCdf.new$AUC <- round(
    sum(diff(PanelsValue.Lane$Y[id])*rollmean(PanelsValue.Lane$Values[id],2)),
    digits = 4)
  AUCdf.new$SampleName <- paste(SName)
  
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
  im = rgb_2gray(im)
  
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

UploadRDs = function(Flag, session, output,
                     DataAnalysisModule,
                     Result, FlagsExp,PanelStructures=NULL){
  if(Flag == "WB"){
    
    for(nameList in names(DataAnalysisModule$wbResult)) 
      Result[[nameList]] <- DataAnalysisModule$wbResult[[nameList]]
    
    # update image WB
    if(!is.null(Result$Im)){
      im = Result$Im$WB
      output$TifPlot2 <- renderPlot({
        plot(c(1,dim(im)[2]),c(1,dim(im)[1]), type='n',ann=FALSE)
        rasterImage(im,1,1,dim(im)[2],dim(im)[1])
        if (nrow(Result$Planes) > 0) {
          r <- Result$Planes
          rect(r$xmin, r$ymin, r$xmax, r$ymax, border = "red")
        }
      })
      
      output$PlanesStructureTable <- renderDT(
        Result$Planes,
        server = FALSE,
          editable = list(target = "cell", 
                          disable = list(columns = 1:4)),
        options = list(lengthChange = FALSE, autoWidth = TRUE),
        rownames= FALSE
      )
      
      PanelStructures$data = Result$Planes
      
    }
    
    # update lanes
    
    if(!is.null(Result$PanelsValue)){
      updateSelectInput(session = session, "LaneChoice",
                        choices = unique(Result$PanelsValue$ID),
                        selected = ""
      )
    }
    
    if(!is.null(Result$TruncatedPlots)){
      output$DataPlot <- renderPlot({Result$TruncatedPlots})
      FlagsExp$LanesCut = T
    }else if(!is.null(Result$Plots)){
      output$DataPlot <- renderPlot({Result$Plots})
      FlagsExp$LanesCut = T
    }
    
    output$AUC <- renderDT({
      Result$AUCdf %>% 
        dplyr::select(SampleName,Truncation, AUC)
    },
    selection = 'none',
    #session = session,
    rownames= FALSE
    )
    
    # output$AUC <- renderTable({
    #   wbResult$AUCdf  %>% dplyr::select(Lane,Truncation, AUC)
    # },width = "100%")
    
    # change pannel
    updateTabsetPanel(session = session, "SideTabs",
                      selected = "grey")
    
  }
  else if(Flag == "PRCC"){
    
    for(nameList in names(DataAnalysisModule$pcrResult)) 
      Result[[nameList]] <- DataAnalysisModule$pcrResult[[nameList]]
    
    choices = ""
    selected = rep("",3)
    
    if(!is.null(Result$Initdata)){
      choices = c("",colnames(Result$Initdata))
    }
    
    if(!is.null(Result$selectPCRcolumns)){
      selected = Result$selectPCRcolumns
    }
    
    updateSelectInput(session = session,"PCR_gene",
                      choices = choices,
                      selected = selected[1]
    )
    updateSelectInput(session = session,"PCR_sample",
                      choices = choices,
                      selected = selected[2]
    )
    updateSelectInput(session = session,"PCR_value",
                      choices = choices,
                      selected = selected[3]
    )
    
    
    if(!is.null(Result$PCRnorm))
      FlagsExp$norm = T
    
    if(!is.null(Result$PCRbaseline))
      FlagsExp$baseline = T
    
    # change pannel
    updateTabsetPanel(session = session, "SideTabs",
                      selected = "uploadPCR")
    
    
  }
  else if(Flag == "ENDOC"){
    
    for(nameList in names(DataAnalysisModule$endocResult)) 
      Result[[nameList]] <- DataAnalysisModule$endocResult[[nameList]]
    
    if(!is.null(Result$TablePlot)){
      output$ENDOCmatrix <- renderDT(
        Result$TablePlot,
        server = FALSE
      )
    }
    
    if(!is.null(Result$ENDOCcell_TIME)){
      
      updateSelectizeInput(inputId = "ENDOCcell_TIME",session = session,
                           choices = unique(c(Result$ENDOCcell_TIME))
      )
      
      updateSelectizeInput(inputId = "ENDOCcell_EXP",session = session,
                           choices = unique(c(Result$ENDOCcell_EXP))
      )
      
      FlagsExp$AllExp = unique(c(Result$ENDOCcell_EXP))
    }
    
    # change pannel
    updateTabsetPanel(session = session, "SideTabs",
                      selected = "uploadENDOC")
    
  }
  else if(Flag == "ELISA"){
    
    for(nameList in names(DataAnalysisModule$elisaResult)) 
      Result[[nameList]] <- DataAnalysisModule$elisaResult[[nameList]]
    
    if(!is.null(Result$TablePlot)){
      output$ELISAmatrix <- renderDT(
        Result$TablePlot,
        server = FALSE
      )
    }
    
    if(!is.null(Result$ELISAcell_EXP)){
      
      updateSelectizeInput(inputId = "ELISAcell_EXP",
                           session = session,
                           choices = unique(c(Result$ELISAcell_EXP))
      )
      
      updateSelectizeInput(inputId = "ELISAcell_SN",
                           session =session,
                           choices = unique(c(Result$ELISAcell_SN))
      )
      
      FlagsExp$AllExp = unique(c(Result$ELISAcell_SN))
    }
    if(!is.null(Result$MapBaseline)){
      FlagsExp$BASEselected = unique(Result$MapBaseline$Baseline)
    }
    if(!is.null(Result$MapBlanche)){
      FlagsExp$BLANCHEselected = unique(Result$MapBlanche$Blanche)
    }
    if(!is.null(Result$Tablestandcurve)){
      output$ELISA_Table_stdcurve <- DT::renderDataTable({
        DT::datatable( Result$Tablestandcurve,
                       selection = 'none',
                       editable = list(target = "cell",
                                       disable = list(columns = 0:2) ),
                       options = list(lengthChange = FALSE, autoWidth = TRUE),
                       rownames= FALSE
        )
      })
      
      if(length(FlagsExp$AllExp) > 1){
        exp = FlagsExp$AllExp
        exp = exp[exp != ""]
        
        bool.tmp = exp %in% unique(c(FlagsExp$BLANCHEselected,FlagsExp$BASEselected))
        if( length(bool.tmp) > 0  )
          exp = exp[!bool.tmp]
        
        updateSelectizeInput(session = session,
                             inputId = "ELISA_standcurve",
                             choices = exp,
                             selected = unique(Result$Tablestandcurve$exp) )
        
        FlagsExp$STDCselected= unique(Result$Tablestandcurve$exp)
      }
    }
    
    ### updating check boxes
    if(!is.null(FlagsExp$BLANCHEselected)){
      exp = FlagsExp$AllExp
      exp = exp[exp != ""]
      
      bool.tmp = exp %in% unique(c(FlagsExp$STDselected,FlagsExp$BASEselected))
      if( length(bool.tmp) > 0  )
        exp = exp[!bool.tmp]
      
      updateCheckboxGroupInput(session = session,
                               inputId = "ELISA_blanches",
                               choices = exp,
                               selected = unique(FlagsExp$BLANCHEselected) )
    }
    if(!is.null(FlagsExp$BASEselected)){
      exp = FlagsExp$AllExp
      exp = exp[exp != ""]
      
      bool.tmp = exp %in% unique(c(FlagsExp$STDselected,FlagsExp$BLANCHEselected))
      if( length(bool.tmp) > 0  )
        exp = exp[!bool.tmp]
      
      updateCheckboxGroupInput(session = session,
                               inputId = "ELISA_baselines",
                               choices = exp,
                               selected = unique(FlagsExp$BASEselected) )
    }
    ###
    if(!is.null(Result$LinearRegression)){
      Result$LinearRegression -> lmStancurve
      Result$Tablestandcurve -> standcurve
      infoLM = data.frame(x = min(standcurve$Concentrations) + c(1,1),
                          y = max(standcurve$Measures) + c(2,1.75),
                          text = c( paste0("y = ", signif(lmStancurve$coef[[2]], 5), "x + ",signif(lmStancurve$coef[[1]],5 )),
                                    paste0("Adj R2 = ",signif(summary(lmStancurve)$adj.r.squared, 5))) )
      
      output$ELISAregression <- renderPlot(
        ggplot(standcurve,aes(Concentrations, Measures)) +
          geom_point() +
          geom_smooth(method='lm', col = "red") +
          geom_text(data= infoLM,
                    aes(x = x, y = y, label =text ),
                    vjust = "inward", hjust = "inward" )+
          theme_bw()
      )
    }
    
    # change pannel
    updateTabsetPanel(session = session, "SideTabs",
                      selected = "uploadELISA")
    
  }
}



saveExcel = function(filename,ResultList,analysis){
  
  if(analysis == "ELISA"){
    ## Create a new workbook
    wb <- createWorkbook("ELISA")
    
    ## initial data
    addWorksheet(wb,"TablePlot")
    writeDataTable(wb, sheet = "TablePlot", ResultList[["TablePlot"]]$x$data)
    
    ## Linear regression analysis
    addWorksheet(wb,"standard curve")
    standcurve = ResultList[["Tablestandcurve"]]
    lmStancurve = ResultList[["LinearRegression"]]
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
    print(regressionPlot)
    writeDataTable(wb,standcurve, sheet="standard curve")
    insertPlot(wb = wb,  sheet="standard curve",
               startCol=dim(standcurve)[2]+ 2)
    
    ## Anlaysis
    addWorksheet(wb,"Analysis")
    writeDataTable(wb,ResultList[["dataFinal"]], sheet="Analysis")
    print(ResultList[["dataFinal"]] %>%
            ggplot( aes(x = Time, y = Quantification,
                        fill= Experiment, group = Experiment ) )+
            geom_bar(position = "dodge",stat = "identity")+
            theme_bw()+
            labs(x = "Time", col = "Experiments",
                 y = "Average quantifications obtained\n from the lm "))
    insertPlot(wb = wb,  sheet="Analysis",
               startCol=dim(ResultList[["dataFinal"]] )[2]+ 2)
  }
  
  ## Save it
  saveWorkbook(wb, filename)
  
}