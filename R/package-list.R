#' Make Html Page for list of packages.
#' 
#' 
#' @param svnDir svn directory
#' @param saveTo file to save page to
#' @export
makeHtmlPageOfPackages <- function(
    svnDir = getOption('alteryx.svndir'),
    saveTo = NULL){
  pkgs <- getAllPackageDetails(svnDir = svnDir)
  section_ <- function(d, title){
    tagList(tags$h4(title), tags$ul(apply(d, 1, function(x){
        tags$li(tags$b(x['Package']), tags$span(paste(": ", x['Title'])))
    })))
  }
  pkgPage <- tags$div(
    section_(pkgs$Alteryx, "Alteryx R Packages"), 
    section_(pkgs$CRAN, "CRAN Packages")
  )
  if (is.null(saveTo)) {
    htmltools::browsable(pkgPage)
  } else {
    htmltools::save_html(pkgPage, saveTo)
  }
}

getAllPackageDetails <- function(svnDir = getOption('alteryx.svndir')){
  rdirs <- getAyxSvnRDirs(svnDir = svnDir)
  pkgs <- installed.packages(lib.loc = rdirs$lib)
  readme <- readLines(file.path(rdirs$installer, 'Readme.txt'), warn = F)
  
  pkgs_for_html <- as.data.frame(pkgs[pkgs[,'Package'] %in% readme,])
  
  cranPkgs <- as.data.frame(t(sapply(rownames(pkgs_for_html), 
    packageDescription, lib.loc = rdirs$lib, 
    fields = c('Package', 'Title', 'Version'), USE.NAMES = FALSE
  )))
  
  svnDir <- getOption('alteryx.svndir')
  ayxRpkgDir <- file.path(svnDir, 'Alteryx', 'Plugins', 'AlteryxRPackage')
  ayxRpkgsDesc <- file.path(list.dirs(ayxRpkgDir, recursive = F)[-7], 'DESCRIPTION')
  ayxPkgs <- plyr::ldply(ayxRpkgsDesc, function(x){
    d <- read.dcf(x)
    data.frame(
      Package = d[,'Package'],
      Title = d[,'Description'],
      Version = d[,'Version'],
      stringsAsFactors = FALSE
    )
  })
  ayxPkgs <- plyr::arrange(ayxPkgs, Package)
  list(Alteryx = ayxPkgs, CRAN = cranPkgs)
}
