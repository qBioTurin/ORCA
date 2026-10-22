# Run from the repository root: Rscript tests/manual/smoke-app.R
suppressWarnings(suppressPackageStartupMessages(pkgload::load_all(quiet = TRUE)))
suppressPackageStartupMessages(source('inst/Shiny/ui.R'))
source('inst/Shiny/server.R')
doc <- xml2::read_html(as.character(htmltools::renderTags(ui)$html))
defaults <- list(SideTabs = 'Home')
for (node in xml2::xml_find_all(doc, '//input[@id]')) {
 id <- xml2::xml_attr(node, 'id'); type <- xml2::xml_attr(node, 'type')
 if(is.na(type)) type <- 'text'
 if(type %in% c('file','radio')) next
 value <- xml2::xml_attr(node, 'value')
 defaults[[id]] <- if(type == 'checkbox') !is.na(xml2::xml_attr(node,'checked')) else if(is.na(value)) '' else if(type == 'number') as.numeric(value) else value
}
for(node in xml2::xml_find_all(doc, '//select[@id]')) {
 opts <- xml2::xml_find_all(node, './option[@selected]')
 if(!length(opts)) opts <- xml2::xml_find_all(node, './option')[1]
 value <- xml2::xml_attr(opts, 'value'); value <- value[!is.na(value)]
 defaults[[xml2::xml_attr(node,'id')]] <- if(length(value)) value else ''
}
for(node in xml2::xml_find_all(doc, '//input[@type="radio"][@checked]')) defaults[[xml2::xml_attr(node,'name')]] <- xml2::xml_attr(node,'value')

upload <- function(path) {
 path <- normalizePath(path)
 data.frame(name=basename(path), datapath=path, size=file.info(path)$size, type='application/octet-stream')
}
testthat::test_that('all tabs can be opened before loading data', {
 shiny::testServer(server, {
  do.call(session$setInputs, defaults)
  tabs <- xml2::xml_attr(xml2::xml_find_all(doc, '//div[contains(@class,"tab-pane")][@id]'), 'id')
  for(tab in sub('^shiny-tab-', '', tabs)) {
   session$setInputs(SideTabs=tab)
   testthat::expect_false(session$isClosed(), info=tab)
   nodes <- xml2::xml_find_all(doc, paste0('//div[@id="shiny-tab-',tab,'"]//*[@id][contains(@class,"-output")]'))
   for(id in xml2::xml_attr(nodes, 'id')) {
    problem <- tryCatch({output[[id]]; NULL}, shiny.silent.error=function(e) NULL,
      error=function(e) if(startsWith(conditionMessage(e), "The test referenced an output that hasn't been defined yet:")) NULL else conditionMessage(e))
    testthat::expect_null(problem, info=paste(tab,id,problem))
   }
  }
  cat('Navigation checked:', length(tabs), 'tabs\n')
 })
})
for(module in c('PCR', 'ELISA', 'ENDOC', 'CYTOTOX', 'IF', 'FACS', 'BCA')) {
 testthat::test_that(paste(module, 'handles a missing upload'), {
  shiny::testServer(server, {
   do.call(session$setInputs, defaults)
   do.call(session$setInputs, setNames(list(1), paste0('Load',module,'_Button')))
   testthat::expect_false(session$isClosed())
  })
 })
}
examples <- list(PCR='PCR/PCRexample.xls', ELISA='ELISA/ELISA.xlsx', ENDOC='Endocytosis/Endocytosis.xlsx', CYTOTOX='Cytotoxicity/Citotoxicity.xlsx', IF='IF/Itga5Itgb1Fn1nMa1.xlsx')
results <- c(PCR='pcrResult', ELISA='elisaResult', ENDOC='endocResult', CYTOTOX='cytotoxResult', IF='ifResult')
for(module in names(examples)) {
 testthat::test_that(paste(module, 'loads the example data'), {
  shiny::testServer(server, {
   do.call(session$setInputs, defaults)
   do.call(session$setInputs, setNames(list(upload(file.path('inst/Data',examples[[module]]))), paste0(module,'Import')))
   do.call(session$setInputs, setNames(list(1), paste0('Load',module,'_Button')))
   testthat::expect_false(session$isClosed())
   testthat::expect_true(NROW(get(results[[module]])$Initdata)>0)
  })
 })
}
testthat::test_that('FACS loads and gates even when the gating tab was already opened', {
 shiny::testServer(server, {
  do.call(session$setInputs, defaults)
  session$setInputs(SideTabs='tablesFACS')
  session$setInputs(FACSImport=upload('inst/Data/FACS/Linfociti Exp#1_100123_Linfo.xls'))
  session$setInputs(LoadFACS_Button=1)
  testthat::expect_false(session$isClosed())
  testthat::expect_true(NROW(FlagsFACS$data)>0)
  session$setInputs(FACScell_1='Cells')
  testthat::expect_equal(FlagsFACS$data$Cells[1], '89.40%')
  session$setInputs(FACScell_2='LIve cells')
  testthat::expect_equal(FlagsFACS$data[['LIve cells']][1], '17.30%')
  session$setInputs(selectBaseGate='Start', SaveFACSanalysis=1)
  testthat::expect_false(session$isClosed())
  testthat::expect_equal(facsResult$dataFinal[[2]][1], '15.47%')
  testthat::expect_false(exists('levelObservers', envir=.GlobalEnv, inherits=FALSE))
 })
})

testthat::test_that('Western blot loads the example TIFF', {
 shiny::testServer(server, {
  do.call(session$setInputs, defaults)
  session$setInputs(imImport=upload('inst/Data/WB/WB1/prove_nero_PPFIA1.tif'), LoadingTif=1)
  testthat::expect_false(session$isClosed())
  testthat::expect_true(length(wbResult$Im$RGB)>0)
 })
})

testthat::test_that('the app entry point works without RStudio', {
 app <- source('inst/Shiny/app.R')$value
 testthat::expect_s3_class(app, 'shiny.appobj')
 old_dir <- getwd()
 tryCatch({
  setwd('inst/Shiny')
  testthat::expect_s3_class(source('app.R')$value, 'shiny.appobj')
 }, finally=setwd(old_dir))
})

testthat::test_that('the package launcher works without RStudio', {
 launcher <- new.env(parent=globalenv())
 source('R/runORCA.R', local=launcher)
 launcher$runApp <- function(app, host, port) {
  testthat::expect_s3_class(app, 'shiny.appobj')
  testthat::expect_equal(host, '127.0.0.1')
  testthat::expect_equal(port, 3838)
 }
 launcher$ORCA.run()
})
