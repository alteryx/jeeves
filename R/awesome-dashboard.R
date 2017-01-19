getTestDir <- function(d){
  dirs <- dirNames()
  test_dir <- file.path(d, dirs$extras, 'Tests')
  if (!dir.exists(test_dir)){
    test_dir <- file.path(d, 'Supporting_Macros', 'tests')
    if (!dir.exists(test_dir)){
      test_dir <- d
    }
  }
  return(test_dir)
}

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = " "
  )
}

parseLog <- function(result){
  r2 <- stringr::str_split(tail(result, 1), '\\s+with\\s+')[[1]]
  status <- ifelse(is.null(attr(result, 'status')), 0, attr(result, 'status'))
  r3 <- list(
    status = if(status <= 1) ":smile:" else ":rage:",
    time = stringr::str_match(r2[1], "^Finished in (.*)")[,2],
    message = ifelse(is.na(r2[2]), "", r2[2]),
    log = paste(result, collapse = '\n')
  )
}

cleanupLog <- function(x){
  lowMemoryWarning <- "The machine is running low on available physical memory"
  y <- strsplit(x, "\\n")[[1]]
  y <- y[!grepl(lowMemoryWarning, y)]
  paste(y, collapse = '\n')
}


processTestResults <- function(d){
  testDir <- getTestDir(d)
  f <- file.path(testDir, '_testResults.rds')
  results <- readRDS(f)
  d <- plyr::ldply(results, function(r){
    r$log <- cleanupLog(enc2utf8(as.character(r$log)))
    r2 <- strsplit(r$log, "\\n")[[1]]
    warnings <- length(grep("^Warning", r2))
    errors <- length(grep("^Error", r2))
    r3 <- stringr::str_split(tail(r2, 1), '\\s+with\\s+')[[1]]
    time <- stringr::str_match(r3[1], "^Finished in (.*)")[,2]
    # log <- c(
    #   grep("^Error", r2, value = TRUE),
    #   grep("^Warning", r2, value = TRUE)
    # )
    # log <- if (length(log) > 0){
    #   paste(log, collapse = "<br>")
    # } else {
    #   ""
    # }
    # log <- paste(c(log, rep("", 4)), collapse = "<br>")
    r2[grepl("^Warning", r2)] <- sprintf(
      "<b class='text-warning'>%s</b>",
      r2[grepl("^Warning", r2)]
    )
    r2[grepl("^Error", r2)] <- sprintf(
      "<b class='text-danger'>%s</b>",
      r2[grepl("^Error", r2)]
    )
    log <- paste(r2, collapse = "<br>")
    status <- if (errors > 0) "&#x274C;" else "&#9989;"
    data.frame(
      category = basename(d),
      name = r$name,
      status = status,
      time = time,
      warnings = if (warnings > 0) warnings else "",
      errors = if (errors > 0) errors else "",
      timestamp = format(file.mtime(f), '%d-%b-%y %H:%M'),
      log = log
    )
  })
  names(d) <- sapply(names(d), .simpleCap, USE.NAMES = F)
  return(d)
}

makeTestResultsTable <- function(testDir){
  d1 <- processTestResults(testDir)
  d1$Errors <- as.numeric(as.character(d1$Errors))
  d1$Warnings <- as.numeric(as.character(d1$Warnings))
  d2 <- plyr::arrange(d1, plyr::desc(Errors), plyr::desc(Warnings), Category)
  d2 <- cbind(id = 1:NROW(d2), d2)
  d2$Errors <- sapply(d2$Errors, function(x){
    if (is.na(x)) {return("")}
    if (x > 0) sprintf("<span class = 'badge bg-red'>%s</span>", x) else x
  })
  d2$Warnings <- sapply(d2$Warnings, function(x){
    if (is.na(x)) {return("")}
    if (x > 0) sprintf("<span class = 'badge bg-orange'>%s</span>", x) else x
  })
  myTests <- DT::datatable(d2,
    rownames = FALSE,
    extensions = c('Buttons', 'Responsive'),
    options = list(iDisplayLength = 10),
    style = 'bootstrap',
    width = '100%',
    height = if (NROW(d2) > 10) 550 else NULL,
    class = c('stripe', 'hover', 'cell-border'),
    escape = FALSE
  )
  fdRowBox(width = 12, title = d2$Category[1], myTests)
}



makePages <- function(dirs){
  lapply(dirs, function(d){
    fdPage(makeTestResultsTable(d), id = basename(d), display = FALSE)
  })
}
  
makeSidebar <- function(dirs){
  icons_ <- toolIcons()
  menuItems <- lapply(dirs, function(d){
    r <- processTestResults(d)
    e <- sum(r$Errors != "")
    w <- sum(r$Warnings != "")
    icon_ <- icons_[[as.character(r$Category[1])]]
    fdMenuItem(addBadges(basename(d), w, e),  
      icon = fdIcon(icon_), 
      pageName = basename(d)
    )
  })
  summaryMenuItem <- fdMenuItem("Summary", icon = fdIcon("th"), 
    pageName = 'summary')
  rpackagesMenuItem <- fdMenuItem("R Packages", icon = fdIcon("credit-card"),
    pageName = 'rpackages'                                
  )
  do.call(fdSidebarMenu, c(
    list(summaryMenuItem), menuItems, list(rpackagesMenuItem))
  )
}


addBadges <- function(x, w, e){
  tagList(
    tags$span(x),
    tags$span(class = 'pull-right-container',
      if (e > 0) tags$small(class = 'label pull-right bg-red', e),
      if (w > 0) tags$small(class = 'label pull-right bg-orange', w)
    )
  )
}
  
makeSummaryPage <- function(allDirs){
  ayxVersion <- extractAlteryxVersion(
    processTestResults(allDirs[1])$Log[1]
  )
  allTestResults = plyr::ldply(allDirs, function(d){
    r <- processTestResults(d)
    data.frame(
      Category = r$Category[1],
      `Total` = NROW(r),
      `Errors` = sum(r$Errors != ""),
      `Warnings` = sum(r$Warnings != "")
    )
  })
  allTestResults <- plyr::arrange(allTestResults, plyr::desc(Errors))
  allTestResults$Errors = sapply(allTestResults$Errors, function(x){
    if (x > 0) sprintf("<span class = 'badge bg-red'>%s</span>", x) else x
  })
  d2 <- DT::datatable(allTestResults,
    rownames = FALSE,
    extensions = c('Buttons', 'Responsive'),
    options = list(iDisplayLength = 15),
    style = 'bootstrap',
    width = '100%',
    height = if (NROW(allTestResults) > 10) 550 else NULL,
    class = c('stripe', 'hover', 'cell-border'),
    escape = FALSE
  )
  fdPage(id = 'summary', display = TRUE,
    fdRow(
      fdInfoBox('Alteryx Version', ayxVersion, width = 6,
        color = 'blue', icon = fdIcon('chart-line', lib = 'entypo')),
      fdInfoBox('R Version', as.character(getRversion()), width = 6,
        color = 'green', icon = fdIcon('star', lib = 'entypo'))
    ),
    fdRowBox(width = 12, title = 'Summary of Tests', d2)
  )
}

makeRPackagePage <- function(){
  d <- do.call(rbind, unname(getAllPackageDetails()))
  d2 <- DT::datatable(d,
    rownames = FALSE,
    extensions = c('Buttons', 'Responsive'),
    options = list(iDisplayLength = 15),
    style = 'bootstrap',
    width = '100%',
    height = if (NROW(d) > 10) 550 else NULL,
    class = c('stripe', 'hover', 'cell-border'),
    escape = FALSE
  )
  fdPage(id = 'rpackages', display = FALSE,
    fdRowBox(title = 'R Packages', d2, width = 12)       
  )
}


makeTestDashboard <- function(d){
  requireNamespace('flightdeck')
  about <- tags$a(fdIcon('info-circle'), 'About')
  about <- tags$li(fdModal(about, paste0(
      "This dashboard was autogenerated using",
      "[jeeves](http://github.com/alteryx/jeeves) and",
      "[flightdeck](http://alteryx.github.io/flightdeck)"
    ), title = 'About'))
  fdBoard(
    fdHeader(title = 'Predictive Tools Tests', about),
    fdSidebar(makeSidebar(d)),
    do.call(fdBody, c(
      list(makeSummaryPage(d)), makePages(d), list(makeRPackagePage()))
    )
  )
}

extractAlteryxVersion <- function(log){
  strsplit(
    strsplit(
      strsplit(as.character(log), "<br>")[[1]][1], 
      "<a9>"
    )[[1]][1],
    'Version'
  )[[1]][2]
}


#' Make awesome test dashboard
#' 
#' @param svnDir path to svn directory
#' @export
#' @import flightdeck
makeAwesomeDashboard <- function(svnDir = getOption('alteryx.svndir')){
  allDirs <- getAllSamplesAndTestDirs(svnDir)
  makeTestDashboard(allDirs)
}

#' @inheritParams makeAwesomeDashboard
getAllSamplesAndTestDirs <- function(svnDir = getOption('alteryx.svndir')){
  sampleDirs <- dir(getSamplesDir(), full.names = TRUE)[1:3]
  pluginDirs <- file.path(svnDir, 'QA',
    c('Linear_Regression', 'Logistic_Regression', 'Decision_Tree')
  )
  allDirs <- c(sampleDirs, pluginDirs)
  return(allDirs)
}

#' Run all samples and QA tests.
#' 
#' @inheritParams makeAwesomeDashboard
#' @export
runAllSamplesAndTests <- function(svnDir =  getOption('alteryx.svndir')){
  allDirs <- getAllSamplesAndTestDirs(svnDir)
  pluginTestResults <- plyr::llply(allDirs, runTests2, 
    build_doc = FALSE, .progress = 'text'
  )
}

toolIcons <- function(){
  list(
    Data_Artistry = 'database',
    Predictive_Analytics = 'line-chart',
    Prescriptive_Analytics = 'magic',
    Linear_Regression = 'line-chart',
    Logistic_Regression = 'flask',
    Decision_Tree = 'tree'
  )
}
