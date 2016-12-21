#' Update SVN with R package or HtmlPlugin from Github
#' 
#' @param name name of the package or plugin to update.
#' @param svnDir path to the root of the svn directory.
#' @export
#' @examples 
#' updatePackageFromGithub('AlteryxPredictive')
#' updatePluginFromSvn('Linear_Regression')
updatePackageFromGithub <- function(name, svnDir = getOption('alteryx.svndir')){
  if (is.null(svnDir) || !dir.exists(svnDir)){
    stop("Invalid SVN directory specified.")
  }
  ayxPackagesDir <- file.path(svnDir, 'Alteryx', 'Plugins', 'AlteryxRPackage')
  if (dir.exists(file.path(ayxPackagesDir, name))){
    stop("Please delete the existing package on SVN before proceeding.")
  } else {
    dlPath <- sprintf("http://github.com/alteryx/%s/archive/master.zip", name)
    tempDir <- tempdir()
    tf <- file.path(tempDir, paste0(name, '.zip'))
    message("Download ", dlPath, ' to ', tf)
    downloader::download(dlPath, tf)
    unzip(tf, exdir = ayxPackagesDir)
    file.rename(
      file.path(ayxPackagesDir, paste0(name, '-master')), 
      file.path(ayxPackagesDir, name)
    )
    return(file.path(ayxPackagesDir, name))
  }
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
  })
}
