# Run from the merged project root. Includes the existing application smoke tests.
source('tests/manual/smoke-app.R')

make_flow_data <- function() {
  mat <- cbind(X=as.double(1:8), Y=rep(2,8))
  flowCore::flowSet(list(A=flowCore::flowFrame(mat), B=flowCore::flowFrame(mat)))
}
polygon_points <- function(xmin,xmax) {
 lapply(seq_len(4), function(i) list(x=c(xmin,xmax,xmax,xmin)[i],y=c(0,0,4,4)[i]))
}
testthat::test_that('raw FCS import handles missing, invalid and valid files', {
 shiny::testServer(server, {
  do.call(session$setInputs,defaults)
  session$setInputs(LoadRawFACS_Button=1)
  testthat::expect_false(session$isClosed())
  path <- tempfile(fileext='.fcs')
  writeLines('invalid FCS',path)
  session$setInputs(rawFACSImport=upload(path),LoadRawFACS_Button=2)
  testthat::expect_false(session$isClosed())
  testthat::expect_null(rawfacsResult$Initdata)
  flowCore::write.FCS(make_flow_data()[[1]],path)
  session$setInputs(rawFACSImport=upload(path),LoadRawFACS_Button=3)
  testthat::expect_false(session$isClosed())
  testthat::expect_s4_class(rawfacsResult$Initdata,'flowSet')
  testthat::expect_equal(nrow(flowCore::exprs(rawfacsResult$Initdata[[1]])),8L)
 })
})
testthat::test_that('polygon hierarchy reuse respects parents and retains all samples', {
 shiny::testServer(server, {
  do.call(session$setInputs,defaults)
  rawfacsResult$Initdata <- make_flow_data()
  session$setInputs(facs_sampleSelector='A',facs_xChannel='X',facs_yChannel='Y',facs_hierarchySelector='None')
  facsSelections$polygonPoints <- polygon_points(0,3.5)
  session$setInputs(facs_selectionName='Parent',facs_confirmSelection=1)
  testthat::expect_false(session$isClosed())
  testthat::expect_equal(facsSelections$selections$A$Parent$n_cells,3L)
  facsSelections$polygonPoints <- polygon_points(2.5,6.5)
  session$setInputs(facs_hierarchySelector='Parent',facs_selectionName='Child',facs_confirmSelection=2)
  testthat::expect_equal(facsSelections$selections$A$Child$n_cells,1L)
  copied <- applySelectionsToSample(facsSelections$selections$A,'B',rawfacsResult$Initdata)
  testthat::expect_equal(copied$Parent$n_cells,3L)
  testthat::expect_equal(copied$Child$n_cells,1L)
  testthat::expect_equal(copied$Child$statistic,100/3)
  facsSelections$selections$B <- copied
  session$flushReact()
  session$setInputs(NextFacsHierarchicalGating=1)
  testthat::expect_false(session$isClosed())
  testthat::expect_equal(FlagsFACS$data$Name,c('A','B'))
  session$setInputs(FACScell_1='Parent')
  testthat::expect_equal(FlagsFACS$data$Parent,c('37.50%','37.50%'))
  exported <- tempfile(fileext='.xlsx')
  saveExcel(exported, rawfacsResult$tableSelections, analysis='RAWFACS')
  testthat::expect_equal(nrow(openxlsx::read.xlsx(exported)),6L)
 })
})
testthat::test_that('FACS statistics returns tabular results and tolerates missing groups', {
 shiny::testServer(server, {
  do.call(session$setInputs,defaults)
  facsResult$dataFinal <- data.frame(Name=letters[1:6], Gate=paste0(c(10,12,15,35,38,40),'%'))
  facsResult$ExpConditionDF <- data.frame(Name=letters[1:6],ExpCondition='')
  session$setInputs(FACSstatButton=1)
  testthat::expect_false(session$isClosed())
  facsResult$ExpConditionDF$ExpCondition <- rep(c('control','treated'),each=3)
  session$setInputs(FACSstatButton=2)
  testthat::expect_false(session$isClosed())
  testthat::expect_s3_class(facsResult$StatDF,'data.frame')
  testthat::expect_equal(facsResult$StatDF$Gate,'Gate')
  testthat::expect_equal(facsResult$StatDF$pValue,t.test(c(10,12,15),c(35,38,40))$p.value)
 })
})
testthat::test_that('both BCA ratio conventions are available', {
 shiny::testServer(server, {
  do.call(session$setInputs,defaults)
  bcaResult$dataFinal <- data.frame(Ug=c(10,20))
  session$setInputs(BCA_UGvalue_init='5',BCA_UGvalue='10',confirmBCA_UGvalue=1)
  testthat::expect_equal(bcaResult$dataFinal[['10/UgBaseline']],c(5,2.5))
  session$setInputs(BCA_ratio_direction='baseline_over_target',confirmBCA_UGvalue=2)
  testthat::expect_equal(bcaResult$dataFinal[['UgBaseline/10']],c(.2,.4))
  testthat::expect_false(session$isClosed())
 })
})
testthat::test_that('cytotoxicity keeps separate plots and the combined export plot', {
 shiny::testServer(server, {
  do.call(session$setInputs,defaults)
  p <- ggplot(data.frame(x=c('A','B'),y=c(1,2)),aes(x,y))+geom_col()
  cytotoxResult$individualPlots <- list(Plot1=p,Plot2=p)
  cytotoxResult$resPlot <- p+p
  session$setInputs(CYTOTOX_plot_customize_selection='Plot 1',Customize_plot_CYTOTOX=1)
  testthat::expect_false(session$isClosed())
  testthat::expect_s3_class(cytotoxResult$resPlot,'patchwork')
 })
})
