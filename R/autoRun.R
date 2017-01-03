#' Auto run tests on a new build of designer.
#' 
#' This function runs all samples and tests for plugins.
#' @param plugins names of plugins to test.
#' @param downloadInstallers boolean indicating if installers should be downloaded.
#' @param installAlteryx boolean indicating if Alteryx should be installed.
#' @param downloadDir directory to which installers should be downloaded to
#' @param ... additional arguments to pass to \code{\link{downloadInstallers}}
#' @export
#' @examples
#' # Run tests on OSR
#' \dontrun{
#' autoRun(
#'   plugins, downloadDir = downloadDir, 
#'   downloadInstallers = TRUE, installAlteryx = TRUE,
#'   rInstaller = 'RInstaller'
#' )
#' # Run tests on MRC
#'   downloadDir <- "."
#'   autoRun(
#'     plugins, downloadDir = downloadDir, downloadInstallers = TRUE, 
#'     installAlteryx = FALSE, rInstaller = 'RREInstaller'
#'   )
#' }
#' @details 
#' \itemize{
#'   \item Dont leave Alteryx open when you run this function.
#'   \item Make sure you run it from a version of R not installed with Alteryx.
#' }
autoRun <- function(plugins, downloadInstallers = FALSE, downloadDir, 
    installAlteryx = FALSE, ...){
  controlPanelDir <- file.path(downloadDir, paste0('ControlPanel', rpois(1, 1231)))
  testDir <- file.path(controlPanelDir, 'tests')
  
  if (downloadInstallers){
    # Download Latest Designer and Predictive installers
    message("Downloading latest predictive installer")
    installers <- downloadInstallers(to = downloadDir, ...)
    
    # Install Designer and Predictive Tools Optionall
    if (installAlteryx){
      message("Installing Alteryx and Predictive Tools")
      installAlteryx(installers)
    } else {
      installers$ayxInstaller <- NULL
      message("Installing Predictive Tools")
      installAlteryx(installers)
    }
  }
  
  message("Creating test directory at ", controlPanelDir)
  dir.create(controlPanelDir)
  dir.create(testDir)
  
  # Test Samples
  message("Testing Predictive Analytics Samples...")
  samplesDir <- file.path(
    getOption('alteryx.path'), 'R-3.3.2', 'plugin', 'Samples', 'Predictive_Analytics'
  )
  results <- runWorkflows(samplesDir)
  saveRDS(results, file.path(testDir, 'sampleTestResults.rds'))
  
  # Download and Test Plugins
  message('Downloading predictive plugins from github...')
  pluginFolders <- sapply(plugins, downloadPluginFromGithub, to = controlPanelDir)
  
  message('Starting tests for plugins ...')
  for (plugin in pluginFolders){
    withr::with_dir(plugin, {
      message("Running tests in ", plugin)
      results <- runTests2()
    })
    testResults <- paste(basename(plugin), 'TestResults', '.rds', sep = "")
    message("Saving results to ", testResults)
    saveRDS(results, file.path(testDir, testResults))
  }
  
  d1 <- plyr::ldply(list.files(testDir, full.names = TRUE), function(f){
    results <- readRDS(f)
    r2 <- plyr::ldply(results, function(r){as.data.frame(r[-5])})
    r2$status <- ifelse(r2$status == ":smile:", "&#9989;", "&#x274C;")
    tool <- gsub("TestResults", "", tools::file_path_sans_ext(basename(f)))
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

downloadPluginFromGithub <- function(name, to){
  dlPath <- sprintf("http://github.com/alteryx/%s/archive/master.zip", name)
  tf <- file.path(to, paste0(name, '.zip'))
  message("Downloading ", dlPath, ' to ', tf)
  downloader::download(dlPath, tf)
  unzip(tf, exdir = to)
  udir <- file.path(to, sprintf('%s-master', name))
  file.rename(udir, gsub("-master", "", udir))
  return(gsub("-master", "", udir))
}