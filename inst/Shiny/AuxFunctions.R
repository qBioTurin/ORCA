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
readfile <- function(filename,type,colname = T, namesAll = namesAll, allDouble = F, colors = F) {
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
          xstr = which(sapply(1:dim(x)[2], function(i) !is.double(x[[i]])) )
          if(length(xstr)>0)
            for(i in xstr)
              x[[i]] = as.double(x[[i]])
        }
        
        if(colors){
          wb =loadWorkbook(filename)
          sheetName = wb$sheet_names[1]
          
          l = lapply(wb$styleObjects, function(x){
            if(x$sheet == sheetName){
              if( all(areColors( paste0("#",unname(x$style$fill$fillFg)))) ){
                color = paste0("#",unname(x$style$fill$fillFg))
              }else{
                color = randomcoloR::randomColor(1)
              }
              df = data.frame(row = x$rows, col = x$cols,
                         fill = ifelse(length(x$style$fill$fillFg) > 0 , 
                                       color,
                                       "white" ) )
            }
          })
          l = do.call(rbind, l[lengths(l) > 0])
          SN = table(l$fill)
          l$SN = paste0("Color ", match(l$fill ,names(SN)) )
          
          tb.SN = matrix("",nrow = max(l$row),ncol = max(l$col))
          
          for(j in 1:ncol(tb.SN)){
            fill_col = l %>% filter(col == j)
            tb.SN[fill_col$row,j] = fill_col$SN
          }
          
          col = l %>% select(fill,SN) %>% distinct()
          vectcol = col$fill
          names(vectcol) = col$SN
          
          return(list(x = x, SNtable = tb.SN, fill =vectcol ))
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

areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE)
  })
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
    if(!is.null(Result$Regression)){
      Result$Regression$data -> lmStancurve
      Result$Tablestandcurve -> standcurve
      
      output$ELISAregression <- renderPlot(
        Result$Regression$plot
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
    lmStancurve = ResultList[["Regression"]]$data
    print(ResultList[["Regression"]]$plot)
    
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
  else if(analysis == "Cytotoxicity"){
    ## Create a new workbook
    wb <- createWorkbook("CYTOTOX")
    
    ## initial data
    addWorksheet(wb,"TablePlot")
    writeDataTable(wb, sheet = "TablePlot", ResultList[["TablePlot"]]$x$data)
    
    ## Analysis
    addWorksheet(wb,"Results Analysis")
    data = ResultList[["data"]]
    finaldata = ResultList[["dataFinal"]]
    print(
      data %>% ggplot() +
            geom_boxplot(aes(x = as.factor(EXP), y = Res, fill = SN, col = SN),alpha = 0.4) +
            theme_bw() +
            labs(x = "Experimental condition", y= "% Values w.r.t \nthe baseline cell death",
                 col="Sample Name",fill="Sample Name")
          )
    writeDataTable(wb,finaldata, sheet="Results Analysis")
    insertPlot(wb = wb,  sheet="Results Analysis",
               startCol=dim(finaldata)[2]+ 2)
  }
  else if(analysis =="Endocytosis"){
    ## Create a new workbook
    wb <- createWorkbook("ENDOC")
    
    ## initial data
    addWorksheet(wb,"TablePlot")
    writeDataTable(wb, sheet = "TablePlot", ResultList[["TablePlot"]]$x$data)
    
    ## Analysis
    addWorksheet(wb,"Results Analysis")
    finaldata = ResultList[["dataFinal"]]
    writeDataTable(wb,finaldata, sheet="Results Analysis")
  }
  else if(analysis =="RT-qPCR"){
    ## Create a new workbook
    wb <- createWorkbook("RTqPCR")
    
    ## initial data
    addWorksheet(wb,"Table")
    writeDataTable(wb, sheet = "Table", ResultList[["Initdata"]])

    ## Norm Analysis
    addWorksheet(wb,"Norm PRC")
    writeDataTable(wb,ResultList[["NewPCR"]], sheet="Norm PRC")
    
    print(ResultList[["plotPRC"]])
    insertPlot(wb = wb,  sheet="Norm PRC",
               startCol=dim(ResultList[["NewPCR"]])[2]+ 2)
    
    ## Comp Analysis
    # addWorksheet(wb,"Comparison PRC")
    # writeDataTable(wb,ResultList[["CompPRC"]], sheet="Comparison PRC")
    # print( ggplot(data =  ResultList[["CompPRC"]],
    #               aes(x= Gene, y = Qnorm, fill = Sample)) + 
    #          facet_wrap(~Norm, ncol = 1) +
    #          geom_bar(stat = "identity",position = "dodge")
    #        )
    # 
    # insertPlot(wb = wb,  sheet="Comparison PRC",
    #            startCol=dim(ResultList[["CompPRC"]] )[2]+ 2)
  
  }
  else if(analysis =="WB"){
    ## Create a new workbook
    wb <- createWorkbook("WB")
    
    ### initial data
    addWorksheet(wb,"WBimage")
    ResultList[["Im"]] -> ListIm 
    im = ListIm$RGB
    
    plot(c(1,dim(im)[2]),c(1,dim(im)[1]), type='n',ann=FALSE)
    rasterImage(im,1,1,dim(im)[2],dim(im)[1])
    insertPlot(wb = wb,  sheet="WBimage")
    
    ### Analysis
    addWorksheet(wb,"Plot")
    print(ResultList[["Plots"]])
    insertPlot(wb = wb,  sheet="Plot")
    
    addWorksheet(wb,"Truncated Plot")
    print(ResultList[["TruncatedPlots"]])
    insertPlot(wb = wb,  sheet="Truncated Plot")
    
    addWorksheet(wb,"AUC")
    finaldata = ResultList[["AUCdf"]]
    writeDataTable(wb,finaldata, sheet="AUC")
    
  }
  else if(analysis =="WB comparison"){
    ## Create a new workbook
    wb <- createWorkbook("WB comparison")

    ## initial data
    addWorksheet(wb,"Normalizer WB")
    writeDataTable(wb, sheet = "Normalizer WB", ResultList[["NormWBanalysis_filtered"]])
    
    addWorksheet(wb,"WB")
    writeDataTable(wb, sheet = "WB", ResultList[["WBanalysis_filtered"]])
    
    ### Analysis
    
    addWorksheet(wb,"RelDensitiy")
    writeDataTable(wb, sheet = "RelDensitiy", ResultList[["RelDensitiy"]])
    
    addWorksheet(wb,"AdjRelDensitiy")
    writeDataTable(wb, sheet = "AdjRelDensitiy", ResultList[["AdjRelDensitiy"]])
    
  } 
  
  ## Save it
  saveWorkbook(wb, filename)
  
}

tableExcelColored = function(session, output,Result, FlagsExp, type){
  
  if(type == "Initialize"){
    ExpDataTable = Result$Initdata
    
    if(is.null(FlagsExp$EXPcol)){
      ExpDataTable.colors = matrix("",nrow = nrow(ExpDataTable),ncol=ncol(ExpDataTable))
    }else{
      ExpDataTable.colors = Result[[grep(x=names(Result), pattern = "cell_SN", value = T)]]
    }
    completeExpDataTable = cbind(ExpDataTable,ExpDataTable.colors)
    
    cols.keep <- paste0('V',1:length(ExpDataTable[1,])) 
    cols.color <- paste0('Col',1:length(ExpDataTable[1,]))
    colnames(completeExpDataTable) = c(cols.keep,cols.color)
    
    if(is.null(FlagsExp$EXPcol)){
      EXPcol = ""
      names(EXPcol) = "white"
    }else{
      EXPcol = FlagsExp$EXPcol
    }
    
    ExpDataTable = datatable(completeExpDataTable,
                          filter = 'none',
                          #server = FALSE,
                          selection = list(mode = 'single', target = 'cell'),
                          rownames= FALSE,
                          options = list(
                            lengthChange = FALSE,
                            scrollX = TRUE,
                            scrollY = TRUE,
                            columnDefs = list(list(targets = cols.color, 
                                                   visible = FALSE))
                          )) %>%
      formatStyle(cols.keep,
                  cols.color,
                  backgroundColor = styleEqual(names(EXPcol), EXPcol) )
    
    cell_SN <- ExpDataTable.colors
    cell_EXP <- cell_REP <- matrix(
      "",
      nrow = length(ExpDataTable$x$data[,1]),
      ncol = length(ExpDataTable$x$data[1,])
    )
    
    Result[[grep(x=names(Result),pattern = "cell_SN", value = T)]] <- cell_SN
    Result[[grep(x=names(Result),pattern = "cell_EXP", value = T)]]<- cell_EXP
    Result[[grep(x=names(Result),pattern = "cell_REP", value = T)]]<- cell_REP
    Result$TablePlot = ExpDataTable
  }
  else if(type == "Update"){
    ColorsSN = rainbow(n = 50,alpha = 0.5)[sample(50,x=1:50,replace = F)]
    
    EXPcol = FlagsExp$EXPcol
    if(is.null(EXPcol)){
      EXPcol = ColorsSN[1:c(FlagsExp$AllExp)]
      names(EXPcol) = FlagsExp$AllExp
      EXPcol[names(EXPcol) == ""] = "white"
      FlagsExp$EXPcol = EXPcol
    }else{
      SNnew = FlagsExp$AllExp[! FlagsExp$AllExp %in%  names(EXPcol)]
      if(length(SNnew)>0){
        colNew = ColorsSN[! ColorsSN %in% EXPcol][1:length(SNnew)]
        names(colNew) = SNnew
        EXPcol = c(EXPcol,colNew)
        EXPcol[names(EXPcol) == ""] = "white"
        FlagsExp$EXPcol = EXPcol
      }
    }
    ExpDataTable = Result$TablePlot$x$data
    completeExpDataTable = cbind(Result$Initdata,Result$CYTOTOXcell_SN)
    colnames(completeExpDataTable) = colnames(ExpDataTable)
    cols.color = grep(x = colnames(ExpDataTable),pattern = "Col",value = T)
    cols.keep = grep(x = colnames(ExpDataTable),pattern = "V",value = T)
    Result$TablePlot = datatable(completeExpDataTable,
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