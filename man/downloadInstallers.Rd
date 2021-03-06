% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/installAlteryx.R
\name{downloadInstallers}
\alias{downloadInstallers}
\title{Download Installers}
\usage{
downloadInstallers(buildRepo = "\\\\\\\\DEN-IT-FILE-07\\\\BuildRepo",
  to = ".", buildDir = NULL, branch = "Predictive_Dev", type = "Server",
  rInstaller = "RInstaller")
}
\arguments{
\item{buildRepo}{path to the build repo.}

\item{to}{directory to download the installers to. defaults to working dir.}

\item{buildDir}{the build directory.}

\item{branch}{string indicating the branch. defaults to Predictive_Dev.}

\item{type}{string indicating installer type. it should be one of 'Server',
'Gallery', NonAdmin' or ''.}

\item{rInstaller}{string indicating R installer to download. it should be one
of 'RInstaller' or 'RREInstaller'.}
}
\description{
Downloads installers automatically from the build repository.
}
