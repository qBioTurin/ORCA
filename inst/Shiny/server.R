AUCfunction<-function(AUCdf,Plots,bind=T,session = session,Lane=0,AUCdf.new=NULL){
  if(is.null(AUCdf.new)){
    if(length(AUCdf[,1])==1 & AUCdf$AUC[1] == "0")
    {
      AUCdf.new <- AUCdf
    }else{
      AUCdf.new <- AUCdf[length(AUCdf$Truncation),]
    }
  }

  Plots.Lane <- Plots[which(Plots$ID == Lane),]
  id <- order(Plots.Lane$X)
  AUC <- sum(diff(Plots.Lane$X[id])*rollmean(Plots.Lane$Y[id],2))
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # DECLARE REACTIVEVALUES FUNCTION HERE
  rResult <- reactiveValues( Plots = NULL,
                             TruncatedPlots = NULL,
                             pl = NULL,
                             CutTab="V",
                             IDlane = 0,
                             AUCdf=data.frame(Truncation = "no", AUC = "0", Lane=0  ))
  
  Flags <- reactiveValues( ContinueLoadingData = F)

observeEvent(c(input$LoadingTxt),{
  
  output$LoadingError <- renderText({
    validate(
      need(!is.null(input$txtImport) , 
           "Please select a txt file!!" )
    )
  
  Flags$ContinueLoadingData <- TRUE
  ""
  })
  if(Flags$ContinueLoadingData)
    {
  dati <- read.csv(input$txtImport$datapath, header=FALSE, sep=";")

  colnames(dati)<- c("X","Y","Value","ID")
  Plots.tmp <- dati[dati$Value==0,]
  na.omit(Plots.tmp)->Plots.tmp
  nLanes<- length(unique(Plots.tmp$ID))
  
  NewPlos.list=lapply(1:nLanes,function(i)
  {
    Plots.tmp[which(Plots.tmp$ID==i-1),]->p
    p <- p[p$Y != max(p$Y),]
    p$Y <- abs(p$Y - max(p$Y) )
    # plot(p$X,p$Y,type = "l")
    return(p)
  })
  Plots <- do.call("rbind",NewPlos.list)
  
  rResult$Plots <-Plots
  
  pl<-ggplot(Plots,aes(X,Y))+
    geom_line()+
    facet_wrap(~ID)
  
  updateSelectInput(session, "LaneChoice",
                    choices = 1:nLanes-1,
                    selected = 0
  )
  
  output$DataPlot <- renderPlot({pl})
  }

  })

observeEvent(c(input$LaneChoice),{
  if(Flags$ContinueLoadingData & !is.null(rResult$Plots))
  {
    rResult$Plots -> Plots
  Plots.Lane <- Plots[which(Plots$ID == input$LaneChoice),]
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
  if(!is.null(rResult$Plots))
  {
    rResult$Plots -> Plots
    IDlane =as.numeric(input$LaneChoice)
    
    rResult$IDlane <- IDlane
    
    pl<-ggplot(Plots,aes(X,Y))+geom_line()+
      facet_wrap(~ID)
    
    if(Flags$CutTab=="V")
      {
        MinTrunc<-input$truncX[1]
        MaxTrunc<-input$truncX[2]
        
        vline.dat <- data.frame(ID=as.factor(rep(Plots$ID,2)), vl =0)
        vline.dat  <-  vline.dat[ vline.dat$ID == IDlane, ]
        vline.dat$vl <- c(MinTrunc,MaxTrunc)
        
        pl <- pl + geom_vline(data=vline.dat,aes(xintercept=vl),linetype="dashed")
      }else if(Flags$CutTab=="H")
      {
        TruncY<-input$truncH1[1]
        hline.dat <- data.frame(ID=as.factor(Plots$ID), hl =0)
        hline.dat  <-  hline.dat[ hline.dat$ID == IDlane, ]
        hline.dat$hl <- TruncY
        
         pl <- pl + geom_hline(data=hline.dat,aes(yintercept = hl),linetype="dashed")
      }
    
    output$DataPlot <- renderPlot({pl})
    
    # ### AUC calculation:
    # AUCdf<-AUCfunction(rResult$AUCdf,Plots,Lane = IDlane)
    # 
    # output$AUC <- renderTable({AUCdf})
    # rResult$AUCdf <- AUCdf
  
  }  

})

observeEvent(c(input$TruncateDataV,input$TruncateDataH),{

    if( !is.null(rResult$Plots))
    {
      rResult$IDlane -> IDlane
      rResult$Plots -> Plots
      
      rResult$AUCdf -> AUCdf
      AUCdf.new <- AUCdf[length(AUCdf$Truncation),]
      
      if(Flags$CutTab=="V")
      {
        MinTrunc<-input$truncX[1]
        MaxTrunc<-input$truncX[2]
        AUCdf.new$Truncation <- paste("X = [", MinTrunc," ; ", MaxTrunc ,"]")
        Plots<- Plots[!((Plots$X < MinTrunc | Plots$X > MaxTrunc) & Plots$ID == IDlane),]
        Plots$Y[Plots$ID == IDlane] <- Plots$Y[Plots$ID == IDlane] -min(Plots$Y[Plots$ID == IDlane]) 
        
        }else if(Flags$CutTab=="H")
        {
          TruncY<-input$truncH1[1]
          Plots <- Plots[!(Plots$Y<TruncY & Plots$ID == IDlane),]
          Plots$Y[Plots$ID == IDlane] <- Plots$Y[Plots$ID == IDlane] - TruncY
          AUCdf.new$Truncation <- paste("Y = ", TruncY)
        }
      
      rResult$TruncatedPlots <- Plots
      pl<-ggplot(Plots,aes(X,Y))+geom_line()+facet_wrap(~ID)

      output$DataPlot <- renderPlot({pl})
      AUCdf<-AUCfunction(AUCdf.new=AUCdf.new,rResult$AUCdf,Plots,Lane = IDlane)
      output$AUC <- renderTable({AUCdf})
      rResult$AUCdf <- AUCdf
    }
  })

output$downloadData <- downloadHandler(
  filename = function() {
    "Report.pdf"
  },
  content = function(file) {
    rmarkdown::render("report.Rmd",output_file = file, params = reactiveValuesToList(rResult) )
  }
)

}
