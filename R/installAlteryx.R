#' Download Installers 
#' 
#' Downloads installers automatically from the build repository.
#' @param buildRepo path to the build repo.
#' @param to directory to download the installers to. defaults to working dir.
#' @param buildDir the build directory. 
#' @param branch string indicating the branch. defaults to Predictive_Dev.
#' @param type string indicating installer type. it should be one of 'Server',
#'   'Gallery', NonAdmin' or ''.
#' @param rInstaller string indicating R installer to download. it should be one
#'   of 'RInstaller' or 'RREInstaller'.
#' @export
downloadInstallers <- function(buildRepo = "\\\\DEN-IT-FILE-07\\BuildRepo", 
   to = ".", buildDir = NULL, branch = "Predictive_Dev", type = 'Server',
   rInstaller = 'RInstaller'){
  runFromWindows()
  to <- normalizePath(to)
  if (is.null(buildDir)){
    message("No buildDir specified. So defaulting to latest.")
    builds <-  dir(buildRepo, pattern = branch, full.names = TRUE)
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
#' Install Alteryx and optionally, the Predictive Tools using the
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

#' Copy predictive macros, samples and plugins from SVN
#' 
#' @param to directory to copy files to.
#' @param svnDir path to svn branch.
#' @export
copyAlteryxRPlugin <- function(to = NULL, svnDir = getOption('alteryx.svndir')){
  if (is.null(to)){
    to <- file.path(getOption('dev.dir'), 'dev', 'AlteryxRPlugin')
  }
  pluginDir <- file.path(svnDir, 'Alteryx', 'Plugins', 'AlteryxRPlugin')
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

#' List of all installed packages used in Alteryx's Predictive tools
#'
#' A vector of all added packages in an SVN installation of a version of R.
#' This is done by reading the Readme.txt file in the /3rdParty/R/Installer
#' directory. The packages are in a named list identifying those from CRAN
#' and those from Alteryx
#'
#' @param svnDir Path to the local copy of a SVN branch of Alteryx.
#' @param rVersion The version of R to use as the basis of package installation.
#'   For a completely new version of R, this will likely be the last version.
#' @export
listInstalledPackages <- function(svnDir = getOption('alteryx.svndir'),
                                  rVersion = NULL) {
  rdirs <- getAyxSvnRDirs(svnDir = svnDir, rVersion = rVersion)
  readmeFile = file.path(rdirs$installer, "Readme.txt")
  pkgs <- readLines(readmeFile, warn = FALSE)
  ayxPkgs <- c(grep("^Alteryx", pkgs, value = TRUE), "flightdeck")
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

#' Install All Needed Packages
#'

#'
#' @param dev Boolean indicating if dev versions of packages should be installed.
#' @param rVersion The version of R to use as the basis of package installation.
#'  This optional argument will typically be used when moving to a new version of
#'  R, when the natural source of packages were those packages in the current
#'  version of R used by Alteryx.
#' @export
installAllPackages <- function(dev = TRUE, rVersion = NULL){
  runFromWindows()
  cranPkgs <- listInstalledPackages(rVersion = rVersion)$cran
  existing_packages <- row.names(installed.packages())
  needed_packages <- cranPkgs[!(cranPkgs %in% existing_packages)]
  if (length(.libPaths()) == 1) {
    lib <- .libPaths()
  } else {
    lib <- .libPaths()[2]
  }
  # Install any needed R packages
  if (length(needed_packages) > 0){
    message("Installing packages ")
    message(paste(needed_packages, collapse = "\n"))
    install.packages(needed_packages)
  }
  ayxPackages <- c("AlteryxSim", "AlteryxPredictive",
   "AlteryxPrescriptive", "AlteryxRDataX",  "AlteryxRviz")
  # The full paths to the binary packages to be installed. This is based on
  # installing the packages from a local directory
  ayxPackages <- file.path(getOption('dev.dir'), 'dev',
    'AlteryxRPackage', ayxPackages)
  requireNamespace('devtools')
  install_ <- devtools::install
  withr::with_libpaths(lib, {
    lapply(ayxPackages, install_)
  })
}

#' Install All Needed Packages V2
#'
#' Alteryx packages, with the exception of AlteryxRDataX, are installed from
#' the Alteryx drat repo on GitHub, while AlteryxRDataX is installed from
#' either the binary installer of the package of the most recent nightly
#' build of the specified branch, or from a local a local directory. The local
#' directory option would allow for an installation of AlteryxRDataX from
#' source.
#' 
#' @param branch string indicating svn branch.
#' @param buildDir build directory.
#' @param ayxRepo string indicating cran-like repo for alteryx packages
#' @param buildRepo build repo.
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
  if (is.null(buildDir)) {
    # Determine the most recent build for the desired branch
    builds <-  dir(buildRepo, pattern = branch, full.names = TRUE)
    buildDir <- tail(builds, 1)
  }
  # The path to the *binary* installer from the most recent build of the branch
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
    # The line below may not work as expected, since it does not have a
    # specified repository, and default repositories have not been specified,
    # via the use of options(), unless it is assumed the user had already
    # done this.
    install.packages(ayxPkgs)
    update.packages() 
  }
}

#' Install CRAN Packages needed for Alteryx predictive tools
#'
#' The function can be used to install needed CRAN packages for the predictive
#' tools to either a user's development R installation or the R installation in
#' the user's local copy of the SVN repository of an Alteryx development
#' branch. NOTE: To use this function, the R session being used must be running
#' in administrator mode to allow for appropriate read/write permissions.
#'
#' @param currentRVersion The current version of R being used by Alteryx's
#'  predictive tools.
#' @param installation One of "dev" or "svn". In the case of "dev", the
#'  needed CRAN packages are installed into the system library of the user's
#'  development installation of R. When "svn" is selected, then the packages
#'  are installed to the system library of the R installation located in the
#'  user's local copy of the relevant SVN repository. The development R
#   version and the one in the local copy of the SVN repository must match.
#'  The respository's path is determined by the alteryx.svndir global options
#'  setting.
#' @param repos The CRAN repository to use for package installation. The
#'  default is https://cloud.r-project.org.
#' @export
install_CRAN_pkgs <- function(currentRVersion,
                              installation = c("dev", "svn"),
                              repos = "https://cloud.r-project.org") {
  installation <- match.arg(installation)
  # Stop Mac users from harming themselves
  runFromWindows()
  # Bootstrap the process using the packages associated with the current
  # version of R being used
  curPkgs_l <- listInstalledPackages(rVersion = currentRVersion)
  # Get the set of dependencies that match the current packages used. This
  # is needed to determine any new dependencies
  allCranDeps_vc <- miniCRAN::pkgDep(curPkgs_l$cran, suggests = FALSE)
  # The set of dependencies will include recommended packages which will
  # already be installed, the lines below find these packages and removes
  # the from the set of packages to install 
  pkgPriority_vc <- installed.packages()[, "Priority"]
  pkgPriority_vc[is.na(pkgPriority_vc)] <- "optional"
  recoPkgs_vc <- names(pkgPriority_vc[pkgPriority_vc == "recommended"])
  cranPkgs_vc <- allCranDeps_vc[!(allCranDeps_vc %in% recoPkgs_vc)]
  cranPkgs_vc <-
    cranPkgs_vc[!(cranPkgs_vc %in% row.names(installed.packages()))]
  # Address the installation type
  installPlace_sc <- if (installation == "dev") {
                      "development R installation.\n"
                    } else {
                      "copy of the SVN repository.\n"
                    }
  libLoc_sc <- if (installation == "dev") {
                 .libPaths()[1]
               } else {
                 getAyxSvnRDirs()$lib
               }
  # Install the packages
  msg_sc <- paste("Installing",
                  length(cranPkgs_vc),
                  "CRAN packages to the local",
                  installPlace_sc)
  cat(msg_sc)
  curPkgs_vc <- installed.packages(lib.loc = libLoc_sc)
  while (!all(cranPkgs_vc %in% curPkgs_vc)) {
    missPkgs_vc <- cranPkgs_vc[!(cranPkgs_vc %in% curPkgs_vc)]
    install.packages(missPkgs_vc, lib = libLoc_sc, repos = repos)
    curPkgs_vc <- installed.packages(lib.loc = libLoc_sc)
  }
  cranPkgs_vc
}

#' Install Alteryx R packages
#'
#' The function can be used to install Altery's R packages to either a
#' user's development R installation or the R installation in the user's
#' local copy of the SVN repository of an Alteryx development branch.
#' NOTE: To use this function, the R session being used must be running
#' in administrator mode to allow for appropriate read/write permissions.
#'
#' @param installation One of "dev" or "svn". In the case of "dev", the
#'  needed CRAN packages are installed into the system library of the user's
#'  development installation of R. When "svn" is selected, then the packages
#'  are installed to the system library of the R installation located in the
#'  user's local copy of the relevant SVN repository. The development R
#   version and the one in the local copy of the SVN repository must match.
#'  The respository's path is determined by the alteryx.svndir global options
#'  setting.
#' @param dataXPath The local full path to an appropriate binary installer of
#'  the AlteryxRDataX package. If its value is NULL, then no attempt will be
#'  made to install the package.
#' @param useGitHub Install the Alteryx predictive packages other than
#'  AlteryxRDataX from Alteryx's CRAN like repository on GitHub at
#'  https://alteryx.github.io/drat. The default is FALSE.
#' @param ayxDepend A character vector of CRAN packages that Alteryx packages
#'  depend on, but are not a dependency of other CRAN packages.
#' @export
install_Alteryx_pkgs <- function(installation = c("dev", "svn"),
                                 dataXPath = NULL,
                                 useGitHub = FALSE,
                                 ayxDepend = NULL) {
  installation <- match.arg(installation)
  # Stop Mac users from harming themselves
  runFromWindows()
  # Address the installation type
  installPlace_sc <- if (installation == "dev") {
                      "to the local development R installation"
                    } else {
                      "to the local copy of the SVN repository"
                    }
  libLoc_sc <- if (installation == "dev") {
                 .libPaths()[1]
               } else {
                 getAyxSvnRDirs()$lib
               }
  # Address dependencies for Alteryx package, but not the other CRAN packages
  if (length(ayxDepend) > 0) {
    install.packages(ayxDepend,
                     repos = "https://cloud.r-project.org",
                     lib = libLoc_sc)
  }
  # Install AlteryxRDataX if dataXPath is not NULL
  if (!is.null(dataXPath)) {
    message(paste0("Installing AlteryxDataX to ", installPlace_sc, "."))
    install.packages(dataXPath, repos = NULL, lib = libLoc_sc)
  }
  # Install Alteryx R packages other than AlteryxRDataX
  ayxPackages_vc <- c("AlteryxSim",
                      "flightdeck",
                      "AlteryxRviz",
                      "AlteryxPredictive",
                      "AlteryxPrescriptive")
  msg1 <- paste0("Installing Alteryx packages other than AlteryxRDataX to ",
                 installPlace_sc,
                 ".")
  message(paste0(msg1))
  if (useGitHub) {
    withr::with_libpaths(libLoc_sc, {
      install.packages(ayxPackages_vc,
                       repos = "https://alteryx.github.io/drat",
                       lib = libLoc_sc)})
  } else {
    fullPaths_vc <- paste0(getOption("alteryx.svndir"),
                           "/Alteryx/Plugins/AlteryxRPackage/",
                           ayxPackages_vc)
    requireNamespace('devtools')
    install_ <- devtools::install
    withr::with_libpaths(libLoc_sc, {
      lapply(fullPaths_vc, install_)
    })
  }
  c(ayxDepend, ayxPackages_vc)
}

#' Install All Packages Needed for Alteryx Predictive Tools
#'
#' The function can be used to install Altery's R packages to either a
#' user's development R installation or the R installation in the user's
#' local copy of the SVN repository of an Alteryx development branch.
#' NOTE: To use this function, the R session being used must be running
#' in administrator mode to allow for appropriate read/write permissions.
#'
#' @param installation One of "dev" or "svn". In the case of "dev", the
#'  needed CRAN packages are installed into the system library of the user's
#'  development installation of R. When "svn" is selected, then the packages
#'  are installed to the system library of the R installation located in the
#'  user's local copy of the relevant SVN repository. The development R
#   version and the one in the local copy of the SVN repository must match.
#'  The respository's path is determined by the alteryx.svndir global options
#'  setting.
#' @param readmeManifest A logical flag indicating whether the Readme file
#'  and the manifest file are saved after installing all the
#'  needed package. This is only relevant for installing packages into the SVN
#'  R installation.
#' @param dataXPath The local full path to an appropriate binary installer of
#'  the AlteryxRDataX package. If its value is NULL, then no attempt will be
#'  made to install the package.
#' @param repos The CRAN repository to use for package installation. The
#'  default is https://cloud.r-project.org.
#' @param useGitHub Install the Alteryx predictive packages other than
#'  AlteryxRDataX from Alteryx's CRAN like repository on GitHub.
#'  The default is FALSE.
#' @param ayxDepend A character vector of CRAN packages that Alteryx packages
#'  depend on, but are not a dependency of other CRAN packages. Currently this
#'  is just the package stringr.
#' @export
install_all_pkgs <- function(currentRVersion,
                             installation = c("dev", "svn"),
                             readmeManifest = TRUE,
                             dataXPath = NULL,
                             repos = "https://cloud.r-project.org",
                             useGitHub = FALSE,
                             ayxDepend = "stringr") {
  installedCranPkgs_vc <- install_CRAN_pkgs(currentRVersion = currentRVersion,
                                            installation = installation,
                                            repos = repos)
  installedAyxPkgs_vc <- install_Alteryx_pkgs(installation = installation,
                                              dataXPath = dataXPath,
                                              useGitHub = useGitHub,
                                              ayxDepend = ayxDepend)
  if (readmeManifest && installation == "svn") {
    svnR_l <- jeeves:::getAyxSvnRDirs()
    # The readme file
    allPkgs_vc <- c(installedCranPkgs_vc, installedAyxPkgs_vc)
    allPkgs_vc <- allPkgs_vc[order(allPkgs_vc)]
    readmeFile = file.path(svnR_l$installer, "Readme.txt")
    writeLines(allPkgs_vc, readmeFile)
    # The manifest file
    man1_mc <- summary(packageStatus(lib.loc = svnR_l$lib,
                       repositories = "https://cran.cnr.berkeley.edu"))
    man2_mc <-
      man1_mc$inst[, c("Package", "Version", "Status", "Priority", "Built")]
    rownames(man2_mc) <- NULL
    write.csv(man2_mc,
              file = file.path(svnR_l$installer, "../Scripts", "packages.csv"),
              row.names = F)
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
#' @param svnDir svn directory to copy from.
#' @param rVersion string indicating version of R.
copyXDFFiles <- function(svnDir = getOption('alteryx.svndir'), 
    rVersion = getRversion()){
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
