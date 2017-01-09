#' Create a dashboard of test results
#' 
#' @param plugins plugins to get test results for
#' @export
createTestDashboard <- function(plugins){
  if (is.null(rootDir)){
    rootDir <- file.path(getOption('dev.dir'), 'dev', 'Predictive_Tools')
  }
  plugins <- c('Linear_Regression', 'Logistic_Regression', 'Decision_Tree')
  d1 <- plyr::ldply(plugins, function(x){
    f <- file.path(rootDir, x, 'Extras', 'Tests', '_testResults.rds')
    results <- readRDS(f)
    r2 <- plyr::ldply(results, function(r){as.data.frame(r)})
    r2$log <- sapply(enc2utf8(as.character(r2$log)), function(x){
      y <- strsplit(x, "\\n")[[1]]
      #y <- y[!grepl(low_memory, y)]
      warnings <- grepl("^Warning", y)
      y[warnings] <- sprintf("<span class='text-warning'>%s</span>", y[warnings])
      paste(y, collapse = "<br>")
    })
    r2$status <- ifelse(r2$status == ":smile:", "&#9989;", "&#x274C;")
    tool <- x
    r2 <- cbind(tool = tool, r2, 
      timestamp = format(file.mtime(f), '%d-%b-%y %H:%M')
    )
  })
  # TOFIX
  # this line causes R CMD CHECK to emit a note
  # since plyr::arrange uses NSE and status and tool are
  # seen as global objects.
  d2 <- plyr::arrange(d1, plyr::desc(status), tool)
  d2 <- cbind(id = 1:NROW(d2), d2)
  myTests <- DT::datatable(d2,
    rownames = FALSE,
    extensions = c('Buttons', 'Responsive'),
    options = list(iDisplayLength = 15),
    style = 'bootstrap',
    width = '100%',
    height = if (NROW(d2) > 10) 550 else NULL,
    class = c('stripe', 'hover', 'cell-border'),
    escape = FALSE
  )

  flightdeck::fdBoard(
    flightdeck::fdHeader(title = 'Predictive Tests'),
    flightdeck::fdSidebar(),
    flightdeck::fdBody(
      flightdeck::fdRowBox(myTests, width = 12)
    )
  )
}
