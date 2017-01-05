#' Update SVN with R package or HtmlPlugin from Github
#' 
#' @param name name of the package or plugin to update.
#' @param svnDir path to the root of the svn branch directory.
#' @export
#' @section Update Plugin from Github:
#' This function tries to automate the following manual sequence of steps.
#' \enumerate{
#'   \item Download zip file from github.
#'   \item Unzip it and rename it (remove the -master suffix).
#'   \item Delete the Extras subfolder contained in it.
#'   \item Copy the folder over to SVN.
#' }
#' @examples 
#' # Set path to your SVN directory using options(alteryx.svndir = <path>)
#' \dontrun{
#'   updatePackageFromGithub('AlteryxPredictive')
#'   updatePluginFromSvn('Linear_Regression')
#' }
updatePackageFromGithub <- function(name, svnDir = getOption('alteryx.svndir')){
  if (is.null(svnDir) || !dir.exists(svnDir)){
    stop("Invalid SVN directory specified.")
  }
  ayxPackagesDir <- file.path(svnDir, 'Alteryx', 'Plugins', 'AlteryxRPackage')
  dlPath <- sprintf("http://github.com/alteryx/%s/archive/master.zip", name)
  tempDir <- tempdir()
  tf <- file.path(tempDir, paste0(name, '.zip'))
  message("Download ", dlPath, ' to ', tf)
  downloader::download(dlPath, tf)
  unzip(tf, exdir = tempDir)
  udir <- file.path(tempDir, sprintf('%s-master', name))
  toDir <- file.path(ayxPackagesDir, name)
  if (!dir.exists(toDir)){
    dir.create(toDir, recursive = TRUE)
    message('This looks like a new package. So jeeves is adding a build script.')
    addBuildScript(ayxPackagesDir, name)
  }
  message('Copying package files')
  file.copy(list.files(udir, full.names = TRUE), toDir, recursive = TRUE)
  return(file.path(ayxPackagesDir, name))
}

#' @rdname updatePackageFromGithub
#' @inheritParams updatePackageFromGithub
#' @export
updatePluginFromGithub <- function(name, svnDir = getOption('alteryx.svndir')){
  if (is.null(svnDir) || !dir.exists(svnDir)){
    stop("Invalid SVN directory specified.")
  }
  dlPath <- sprintf("http://github.com/alteryx/%s/archive/master.zip", name)
  tempDir <- tempdir()
  tf <- file.path(tempDir, paste0(name, '.zip'))
  message("Downloading ", dlPath, ' to ', tf)
  downloader::download(dlPath, tf)
  unzip(tf, exdir = tempDir)
  udir <- file.path(tempDir, sprintf('%s-master', name))
  file.rename(udir, gsub("-master", "", udir))
  pluginDir <- file.path(tempDir, name)
  on.exit(unlink(pluginDir, recursive = TRUE))
  message("Copying plugin to ", svnDir)
  with_dir_(pluginDir, {
    copyHtmlPlugin(ayxDir = getAyxSvnDirs(svnDir = svnDir))
    if (dir.exists('assets')){
      assetDir <- file.path(
        ayxDir = getAyxSvnDirs(svnDir = svnDir)$htmlplugin, 
        name, 'assets'
      )
      if (!dir.exists(assetDir)) dir.create(assetDir, recursive = TRUE)
      file.copy(
        list.files(file.path(".", 'assets'), full.names = T), 
        assetDir, 
        recursive = TRUE
      )
    }
    message('Copying Tests to QA Folder')
    file.copy(
      list.files(file.path('.', 'Extras', 'Tests'), full.names = TRUE),
      file.path(svnDir, 'QA', name),
      recursive = TRUE
    )
  })
}

addBuildScript <- function(ayxPackagesDir, name){
  batFileContents <- readLines(
    system.file('templates', 'pkg-build-batchfile.bat', package = 'jeeves')
  )
  batFile <- file.path(ayxPackagesDir, paste0('Build', name, '.bat'))
  batFileContents <- gsub('__PKG__', name, batFileContents)
  writeLines(batFileContents, batFile)
}

#' Get new dependencies for a package
#' 
#' @param pkg package to get new dependencies for
#' @export
getNewDependencies <- function(pkg){
  options(repos =  c(
    CRAN = 'http://cran.rstudio.org', 
    Alteryx = 'http://alteryx.github.io/drat'
  ))
  ayxRDirs <- getAyxSvnRDirs()
  pkgLibDir <- ayxRDirs$lib
  readme <- readLines(file.path(ayxRDirs$installer, 'Readme.txt'), warn = FALSE)
  deps <- miniCRAN::pkgDep(pkg, suggests = FALSE)
  depsToInstall <- setdiff(deps, c(pkg, readme))
}

