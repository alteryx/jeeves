library(htmltools)
library(jeeves)
config <- getConfigFromLayout('../../Predictive_Tools/Decision_Tree')

pluginDir <- '../../Predictive_Tools/Decision_Tree'

getPathToMacro <- function(pluginDir = "."){
  dirs <- jeeves:::dirNames()
  pluginName <- basename(normalizePath(pluginDir))
  file.path(pluginDir, 
    dirs$macros, 
    sprintf("%s.yxmc", pluginName)
  )
}

extractOverrides <- function(pluginDir = "."){
  dirs <- jeeves:::dirNames()
  ov <- file.path(pluginDir, dirs$extras, "Gui", "overrides.yaml")
  if (file.exists(ov)){
    yaml::yaml.load_file(ov)
  } else {
    NULL
  }
}

extractConfigurationWithOverrides <- function(pluginDir = "."){
 yxmc <- getPathToMacro(pluginDir)
 x1 <- extractConfiguration(yxmc)
 ov <- extractOverrides(pluginDir)
 if (!is.null(ov)) x1 <- modifyList(x1, ov)
 x1b <- lapply(seq_along(x1), function(i){
   x1[[i]]$id = names(x1)[i]
   x1[[i]]
 })
 names(x1b) <- names(x1)
 x1
}

getItemsOfType <- function(itemType = 'ToggleBar', pluginDir = ".", toJSON = TRUE){
  config <- extractConfigurationWithOverrides(pluginDir)
  isToggleBar <- function(d){d$type == itemType}
  items <- lapply(Filter(isToggleBar, d), function(d){
    unname(as.vector(names(d$values)))
  })
  if (toJSON){
    jsonlite::toJSON(items, auto_unbox = TRUE, pretty = TRUE)
  } else {
    items
  }
}

getItemsOfType('ToggleBar', pluginDir = pluginDir)
getItemsOfType('RadioGroup', pluginDir = pluginDir)




ov <- file.path(pluginDir, dirs$extras, "Gui", "overrides.yaml")
if (file.exists(ov)){
  overrides <- yaml::yaml.load_file(ov)
}
if (!is.null(overrides)){
  x1 <- modifyList(x1, overrides)
}
x1b <- lapply(seq_along(x1), function(i){
  x1[[i]]$id = names(x1)[i]
  x1[[i]]
})
names(x1b) <- names(x1)


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
    do.call(tagList, config[c('Model Name', 'Y Var', 'X Vars')])
  )
)
