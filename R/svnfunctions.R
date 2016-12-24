#' Install packages to R library in SVN
#' 
#' 
#' @param srcPkg path to source package
#' @param install whether or not to actually install dependencies
#' @export
installToSvn <- function(srcPkg, to_install = TRUE, rVersion = '3.2.3'){
  srcPkg <- normalizePath(srcPkg)
  rdirs <- getAyxSvnRDirs(rVersion = rVersion)
  svnLibDir <- rdirs$lib
  d <- read.dcf(file.path(srcPkg, "DESCRIPTION"))
  imports <- gsub("\\n", "", strsplit(d[,'Imports'], ",")[[1]])
  ayxPackages <- c("AlteryxPrescriptive", "AlteryxRDataX", "AlteryxRviz")
  imports <- setdiff(imports, ayxPackages)
  deps <- miniCRAN::pkgDep(imports, suggests = FALSE)
  installed <- rownames(installed.packages(svnLibDir))
  depsToInstall <- setdiff(deps, installed)
  install <- devtools::install
  if (to_install){
    withr::with_libpaths(svnLibDir, {
      if (length(depsToInstall) > 0){
        message(
          "Installing following dependencies from CRAN\n  ",
          paste(depsToInstall, collapse = '\n  ')
        )
        install.packages(depsToInstall, svnLibDir)
      }
      message(
        "Installing from source: ", basename(normalizePath(srcPkg))
      )
      install(srcPkg, lib = svnLibDir, dependencies = FALSE)
    }, action = 'prefix')
  }
  list(dependencies = deps, toInstall = depsToInstall)
}


#' Save manifest of R packages installed.
#' 
#' A detailed manifest of all R package installed in the SVN directory specified
#' by \code{svnDir} is built and saved to 
#' \code{3rdParty\\R\\Scripts\\packages.csv}. A plain list of R packages
#' installed is also saved to \code{3rdParty\\R\\Installer\\Readme.txt} Note
#' that both the manifest and the Readme are built based on packages installed
#' in the R library in the SVN directory and hence should not be manipulated
#' manually.
#' @param string indicating version of R
#' @export
saveManifest <- function(
    svnDir = getOption("alteryx.svndir"),
    rVersion = '3.2.3'){
  rdirs <- getAyxSvnRDirs(rVersion = rVersion)
  svnLibDir = rdirs$lib
  d3 <- summary(packageStatus(svnLibDir))
  d4 <- d3$inst[,c('Package', 'Version', 'Status', 'Priority', 'Built')]
  rownames(d4) <- NULL
  message("Updating package manifest on SVN...")
  write.csv(
    d4, 
    file = file.path(rdirs$installer, "../Scripts", "packages.csv"), 
    row.names = F
  )
  message("Updating package readme on SVN...")
  saveReadme()
}

#' Save readme
#' 
#' @param save whether or not to save the readme
#' @export
saveReadme <- function(save = TRUE, rVersion = '3.2.3'){
  rdirs <- getAyxSvnRDirs(rVersion = rVersion)
  readmeFile = file.path(rdirs$installer, "Readme.txt")
  svnLibDir = rdirs$lib
  pkgs <- summary(packageStatus(svnLibDir))$inst
  pkgs <- pkgs[is.na(pkgs[,"Priority"]),]
  pkgs <- pkgs[pkgs$Package != "translations",]
  ayxPackages <- c("AlteryxPrescriptive", "AlteryxRDataX", "AlteryxRviz")
  allPkgs <- unique(unname(sort(c(pkgs$Package, ayxPackages))))
  if (save){
    writeLines(allPkgs, readmeFile)
  } else {
    return (allPkgs)
  }
}

#' Get Alteryx SVN Directories
#' 
#' 
#' @param svnDir svn directory
#' @export
getAyxSvnDirs <- function(svnDir = getOption("alteryx.svndir")){
  if (!dir.exists(svnDir)){
    message('Your SVN location is set to ', svnDir)
    message("But I can't locate it :-( .")
    stop('Please set your SVN dir using options(alteryx.svndir = <path/to/svn>)')
  }
  list(
    htmlplugin = file.path(svnDir, "Alteryx", "Plugins", "AlteryxRPlugin", 
      "HtmlPlugins"),
    macro = NULL
  )
}

getAyxSvnPath <- function(to, svnDir = getOption("alteryx.svndir")){
  Plugins <- file.path(svnDir, "Alteryx", "Plugins")
  AlteryxRPlugin <- file.path(svnDir, Plugins, 'AlteryxRPlugin')
  HtmlPlugins <- file.path(AlteryxRPlugin, 'HtmlPlugins')
  paths <- c(Plugins = Plugins, AlteryxRPlugin = AlteryxRPlugin, 
    HtmlPlugins = HtmlPlugins
  )
  unname(paths[to])
}

#' @export
copyHtmlPluginToSvn <- function(pluginDir = "."){
  copyHtmlPlugin(ayxDir = getAyxSvnDirs())
}

#' Get Alteryx R SVN Directories
#' 
#' 
#' @param svnDir svn directory
#' @param rVersion optional argument to specify an R version.
#' @return a named list containing paths to the R library and
#'   \code{3rdParty//R//Installer} on SVN
#' @export
getAyxSvnRDirs <- function(svnDir = getOption("alteryx.svndir"), rVersion = NULL){
  if (is.null(rVersion)) {
    rver <- R.Version()
    rVersion = paste(rver$major, rver$minor, sep = ".")
  }
  rPath <- file.path(svnDir, "3rdParty", "R")
  list(
    lib = file.path(rPath, 'R_Installed_Files', 
      paste0('R-', rVersion), 'library'
    ),
    installer = file.path(rPath, 'Installer')
  )
}

#' @export
svnInstallPackages <- function(pkg,
    svnDir = getOption("alteryx.svndir"), 
    rVersion = paste(R.version$major, R.version$minor, sep = '.')){
  rPath <- getAyxSvnRDirs(svnDir, rVersion)
  withr::with_libpaths(rPath$lib, {
    install.packages(pkg, lib = rPath$lib)
  })
  saveReadme(rVersion = rVersion)
  saveManifest(rVersion = rVersion)
  return(file.path(rPath$lib, pkg))
}