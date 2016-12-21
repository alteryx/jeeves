#' ## Read
#' 
#' The first step is to read configuration and inputs that stream in from
#' Alteryx. For this code to be portable, we need to provide defaults that will
#' be used when the R code is not run inside an Alteryx workflow.
#'
#' ### Configuration
## DO NOT MODIFY: Auto Inserted by Jeeves ----
library(AlteryxPredictive)

{{{ config }}}

##----

#' ### Defaults
#' 
#' These defaults are used when the R code is run outside Alteryx
defaults <- list(
  d1 = data.frame(carat = runif(100), price = runif(100)*100)
)

#' ### Inputs
#' 
#' This is a named list of all inputs that stream into the R tool.
inputs <- list(
  d1 = read.Alteryx2("#1", default = defaults$d1)
)


#' ## Process
#' 
#' The next step is to process the payload (configuration and inputs) and return
#' outputs. The `process` function needs to be pure (no side effects) so that we
#' can also write unit tests in R.
#'
#' It is advisable to to write small, simple helper functions and 
#' string them together to construct the `process` function. These helper
#' functions should be thrown into an R package so that they can be documented
#' and unit tested.

process <- function(config, inputs){
  require(ggplot2)
  d <- inputs$d1
  out1 <- d
  out2 <- ggplot(data = d, aes(x = carat)) +
    geom_histogram(bins = config$numBins, fill = config$barColor)
  list(out1 = out1, out2 = out2)
}

outputs <- process(config, inputs)

#' ## Write 
#' 
#' The final step is to write outputs to the Alteyx stream. We use the `output`
#' object generated previously and then write them out to the desired output

write.Alteryx2(outputs$out1, 1)
AlteryxGraph2(outputs$out2, 3)
