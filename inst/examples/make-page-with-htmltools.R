library(htmltools)
library(jeeves)
config <- getConfigFromLayout('../../Predictive_Tools/Decision_Tree')


jvPage <- function(..., id){
  div(class = 'page', id = paste0('page-', id), ...)
}

jvHeader <- function(...){
  div(class = 'fd-header', ...)
}

jvContent <- function(...){
  div(class = 'content',
    div(class = 'content-padded', ...)    
  )
}


jvPage(id = 'basic',
  jvHeader('Home'),
  jvContent(
    config$`Model Name`,
    config$`Y Var`,
    config$`X Vars`
  )
)
