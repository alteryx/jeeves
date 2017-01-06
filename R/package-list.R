#' Make Html Page for list of packages.
#' 
#' 
#' @param svnDir svn directory
#' @param saveTo file to save page to
#' @export
makeHtmlPageOfPackages <- function(
    svnDir = getOption('alteryx.svndir'),
    saveTo = NULL){
  rdirs <- getAyxSvnRDirs(svnDir = svnDir)
  pkgs <- installed.packages(lib.loc = rdirs$lib)
  readme <- readLines(file.path(rdirs$installer, 'Readme.txt'), warn = F)
  
  pkgs_for_html <- as.data.frame(pkgs[pkgs[,'Package'] %in% readme,])
  
  cranPkgs <- as.data.frame(t(sapply(rownames(pkgs_for_html), packageDescription, 
    lib.loc = rdirs$lib, fields = c('Package', 'Title'), USE.NAMES = FALSE
  )))
  
  svnDir <- getOption('alteryx.svndir')
  ayxRpkgDir <- file.path(svnDir, 'Alteryx', 'Plugins', 'AlteryxRPackage')
  ayxRpkgsDesc <- file.path(list.dirs(ayxRpkgDir, recursive = F)[-7], 'DESCRIPTION')
  ayxPkgs <- plyr::ldply(ayxRpkgsDesc, function(x){
    d <- read.dcf(x)
    data.frame(
      Package = d[,'Package'],
      Description = d[,'Description']
    )
  })
  ayxPkgs <- arrange(ayxPkgs, Package)
  pkgPage <- div(
    tags$h4("Alteryx R Packages"), 
    tags$ul(
      apply(ayxPkgs, 1, function(x){
        tags$li(tags$b(x['Package']), tags$span(paste(": ", x['Title'])))
      })
    ),
    h4("CRAN Packages"), 
    tags$ul(
      apply(cranPkgs, 1, function(x){
        tags$li(tags$b(x['Package']), tags$span(paste(": ", x['Title'])))
      })
    )
  )
  if (is.null(saveTo)) {
    htmltools::browsable(pkgPage)
  } else {
    htmltools::save_html(pkgPage, saveTo)
  }
}
