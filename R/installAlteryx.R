#' Copy predictive macros, samples and plugins from SVN
#' 
#' @param to directory to copy files to
#' @export
copyAlteryxRPlugin <- function(to = NULL){
  if (is.null(to)){
    to <- file.path(getOption('dev.dir'), 'dev', 'AlteryxRPlugin')
  }
  pluginDir <- file.path(getOption('alteryx.svndir'), 'Alteryx', 'Plugins',
    'AlteryxRPlugin'
  )
  files_to_copy <- list.files(pluginDir, full.names = T, 
      pattern = 'Macros|Samples|HtmlPlugins')
  if (!(file.exists(to))) {
    message("Creating directory ", to, "...")
    dir.create(to, recursive = TRUE)
  }
  message("Copying files to ", to, "...")
  file.copy(files_to_copy, to, recursive = TRUE)
}

runFromWindows <- function(){
  if(.Platform$OS.type != "windows"){
    stop("Please run this function from Windows", call. = FALSE)
  }
}


#' Download Installers from the Build Repo
#' 
#' @param buildRepo path to the build repo.
#' @param to directory to download the installers to. defaults to working dir.
#' @param buildDir the build directory. 
#' @param branch string indicating the branch. defaults to Predictive_Dev.
#' @param type string indicating installer type. it should be one of 'Server',
#'   'Gallery', NonAdmin' or ''.
#' @export
downloadInstallers <- function(buildRepo = "\\\\DEN-IT-FILE-07\\BuildRepo", 
   to = ".", buildDir = NULL, branch = "Predictive_Dev", type = 'Server',
   rInstaller = 'RInstaller'){
  runFromWindows()
  to <- normalizePath(to)
  if (is.null(buildDir)){
    message("No buildDir specified. So defaulting to latest.")
    builds <-  dir(buildRepo, pattern = branch, full = TRUE)
    buildDir <- tail(builds, 1)
  }
  message("Build Directory is ", buildDir)
  ayxInstaller <- list.files(
    file.path(buildDir, 'Alteryx'), pattern = type, full.names = TRUE
  )
  message("Downloading ", ayxInstaller)
  file.copy(ayxInstaller, to)
  rInstaller <- list.files(
    file.path(buildDir, 'R'), pattern = rInstaller, full.names = TRUE
  )
  message("Downloading ", rInstaller)
  file.copy(rInstaller, to)
  list(
    ayxInstaller = file.path(to, basename(ayxInstaller)), 
    rInstaller = file.path(to, basename(rInstaller))
  )
}

#' Install Alteryx
#' 
#' This function installs Alteryx and optionally, the Predictive Tools using the
#' command line SDK. It does a silent install skipping through all dialogs and
#' accepting all defaults.
#' @param installers list of paths to named installers.
#' @export
installAlteryx <- function(installers){
  for (inst in installers){
    withr::with_dir(dirname(inst), {
      r <- plyr::llply(basename(inst), function(installer){
        message("Installing ", basename(installer))
        install_cmd <- paste(basename(installer), '/s')
        message('Running ', install_cmd)
        system(install_cmd)
      })
    })
  }
}

# List all installed packages
listInstalledPackages <- function(){
  rdirs <- getAyxSvnRDirs()
  readmeFile = file.path(rdirs$installer, "Readme.txt")
  pkgs <- readLines(readmeFile, warn = FALSE)
  ayxPkgs <- grep("^Alteryx", pkgs, value = TRUE)
  list(
    cran = setdiff(pkgs, ayxPkgs),
    alteryx = ayxPkgs
  )
}

#' Write RPluginIni
#' 
#' The \code{RPluginSettings.ini} file enables Alteryx to find the R engine to
#' work with. This function allows one to customize the ini file and support
#' working with R installs that are not provided by the predictive installer.
#' @param revo boolean indicating if xdf inputs are to be supported.
#' @param replace boolean indicating if the existing file should be overwritten.
#' @export
writeRPluginIni <- function(revo = FALSE, replace = FALSE){
  RpluginIni <- file.path(getOption('alteryx.path'), 'Settings',
   'RPluginSettings.ini'
  )
  l <- c(
    RVersion = paste(R.version$major, R.version$minor, sep = "."),
    RExePath = normalizePath(R.home()),
    RevolutionRinstalled = as.character(revo)
  )
  contents <- c(
    '[Settings]',
    paste(names(l), l, sep = "=")
  )
  if (revo){
    message('Copying XDF macros and samples ...')
    copyXDFFiles()
  }
  if (replace){
    message('Writing new RpluginSettings.ini')
    writeLines(contents, con = RpluginIni)
  } else {
    return(contents)
  }
}

#' Install all needed packages that are missing
#'
#' @export
installAllPackages <- function(dev = TRUE){
  runFromWindows()
  cranPkgs <- listInstalledPackages()$cran
  existing_packages <- row.names(installed.packages())
  needed_packages <- cranPkgs[!(cranPkgs %in% existing_packages)]
  if (length(.libPaths()) == 1) {
    lib <- .libPaths()
  } else {
    lib <- .libPaths()[2]
  }
  if (length(needed_packages) > 0){
    message("Installing packages ")
    message(paste(needed_packages, collapse = "\n"))
    install.packages(needed_packages)
  }
  ayxPackages <- c("AlteryxSim", "AlteryxPredictive",
   "AlteryxPrescriptive", "AlteryxRDataX",  "AlteryxRviz")
  ayxPackages <- file.path(getOption('dev.dir'), 'dev',
    'AlteryxRPackage', ayxPackages)
  library(devtools)
  withr::with_libpaths(lib, {
    lapply(ayxPackages, install)
  })
}

#' Install all packages
#' 
#' 
#' @export
installAllPackages2 <- function(branch = 'Predictive_Dev', buildDir = NULL,
    ayxRepo = 'https://alteryx.github.io/drat',
    buildRepo = "\\\\DEN-IT-FILE-07\\BuildRepo"){
  runFromWindows()
  requiredPkgs <- unlist(listInstalledPackages(), use.names = F)
  requiredPkgs <- requiredPkgs[requiredPkgs != 'AlteryxRDataX']
  existing_packages <- row.names(installed.packages())
  needed_packages <- requiredPkgs[!(requiredPkgs %in% existing_packages)]
  if (length(.libPaths()) == 1) {
    lib <- .libPaths()
  } else {
    lib <- .libPaths()[2]
  }
  message("Installing AlteryxRDataX...")
  if (is.null(buildDir)){
    builds <-  dir(buildRepo, pattern = branch, full = TRUE)
    buildDir <- tail(builds, 1)
  }
  RDataX <- list.files(file.path(buildDir, 'R'), pattern = 'AlteryxRDataX_', 
    full.names = TRUE)
  install.packages(RDataX, repos = NULL)
  if (length(needed_packages) > 0){
    options(repos = c(CRAN = getOption("repos"), Alteryx = ayxRepo))
    message("Installing missing packages from CRAN...")
    message(paste(needed_packages, collapse = "\n"))
    install.packages(needed_packages)
  } else {
    message("Updating R Packages")
    ayxPkgs <- grep("^Alteryx", requiredPkgs, value = TRUE)
    install.packages(ayxPkgs)
    update.packages() 
  }
}

#' Update R installation
#' 
#' @export
updateRInstallation <- function(){
  message('Installing missing R packages...')
  installAllPackages()
  message("Updating RPluginSettings.ini...")
  writeRPluginIni(replace = TRUE)
}

#' Copy XDF files from SVN
#'
#' @param svnDir svn directory to copy from
#' 
copyXDFFiles <- function(svnDir = getOption('alteryx.svndir'), 
    rVersion = '3.2.3'){
  xdf_macros <- file.path(svnDir, 'Alteryx', 'Plugins', 'AlteryxRPlugin', 
    'XDF_Macros')
  xdf_samples <- file.path(svnDir, 'Alteryx', 'Plugins', 'AlteryxRPlugin', 
    'XDF_Samples')
  copy_dir <- function (from, to) {
    if (!(file.exists(to))) {
      dir.create(to, recursive = TRUE)
    }
    message("Copying files to ", to, "...")
    file.copy(list.files(from, full.names = T), to, recursive = TRUE)
  }
  pluginDir <- file.path(getOption('alteryx.path'), 
    paste0('R-', rVersion), 'plugin')
  copy_dir(xdf_macros, file.path(pluginDir, 'Macros', 'XDF_Macros'))
  copy_dir(xdf_samples, file.path(pluginDir, 'Samples', 'XDF_Samples'))
}
