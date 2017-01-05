#' Make a section
#' 
#' @param section section
#' @param config config
#' @export
makeSection <- function(section, config){
  sectionItem <- lapply(section[[1]], makeSubSection, config)
  do.call(jvTabPage, c(sectionItem, list(title = names(section)[1])))
}

#' Make a subsection
#' 
#' @param subsection subsection
#' @param config config
#' @param ... additional arguments
#' @export
makeSubSection <- function(subsection, config, ...){
  UseMethod('makeSubSection')
}

#' @export
makeSubSection.character <- function(subsection, config, ...){
  config[subsection]
}

#' @export
makeSubSection.list <- function(subsection, config, ...){
  accordionItems <- lapply(subsection[[1]], function(s){
    jvAccordionItem(title = names(s), config[s[[1]]])
  })
  do.call(jvAccordion, 
    c(accordionItems, list(id = paste0('subsection-', names(subsection)[1])))
  )
}

#' Write layout from a yaml spec file
#' 
#' @param pluginDir path to root folder of the plugin.
#' @param displayRules list of display rules to specify.
#' @param ... arguments to pass to jvMakeDataItemsToInitialize.
#' @export
writeLayoutFromYamlSpec <- function(pluginDir = ".", displayRules = c(), ...){
  cfg <- renderPluginWidgets(pluginDir, wrapInDiv = TRUE)
  config <- setNames(
    lapply(names(cfg), function(x){
      div(id = paste0('div-', makeHtmlId(x)), HTML(sprintf("{{ `%s` }}", x)))
    }), names(cfg)
  )
  layoutFile = file.path(pluginDir, 'Extras', 'Gui', 'layout.yml')
  spec <- yaml::yaml.load_file(layoutFile)
  
  page1 <- spec[[1]]
  setupPage <- jvSetupPage(title = names(spec)[1], config[page1])
  page2 <- spec[[2]]
  customizePageContent <- do.call(
    jvTabbedContent, lapply(spec[[2]], makeSection, cfg)
  )
  
  customizePage <- jvCustomizePage(title = names(spec)[2], customizePageContent)
  
  ui <- tagList(setupPage, customizePage)
  di <- jvMakeDataItemsToInitialize(pluginDir = pluginDir, ...)
  jsonDisplayRules <- jsonlite::toJSON(
    displayRules, auto_unbox = TRUE, pretty = TRUE
  )
  myPage <- tagList(ui, tags$script(
    makeJsVariable(di, 'items'), 
    makeJsVariable(jsonDisplayRules, 'displayRules')
  ))
  writeLines(as.character(myPage), file.path(pluginDir, 'Extras/Gui/layout.html'))
}

#' Write layout file from a YAML spec
#' 
#' @param pluginDir path to plugin
#' @export
writeLayoutFromYamlSpec2 <- function(pluginDir = "."){
  cfg <- renderPluginWidgets(pluginDir, wrapInDiv = TRUE)
  config <- setNames(
    lapply(names(cfg), function(x){
      div(id = paste0('div-', makeHtmlId(x)), HTML(sprintf("{{ `%s` }}", x)))
    }), names(cfg)
  )
  layoutFile = file.path(pluginDir, 'Extras', 'Gui', 'layout.yml')
  spec <- yaml::yaml.load_file(layoutFile)
  
  page1 <- spec[[1]]
  setupPage <- jvSetupPage(title = names(spec)[1], config[page1])
  page2 <- spec[[2]]
  customizePageContent <- do.call(
    jvTabbedContent, lapply(spec[[2]], makeSection, cfg)
  )
  
  customizePage <- jvCustomizePage(title = names(spec)[2], customizePageContent)
  
  ui <- tagList(setupPage, customizePage)
  dataItems <- spec$dataItems
  di <- jvMakeDataItemsToInitialize(pluginDir = pluginDir, 
    curPage = dataItems$curPage, curTab = dataItems$curTab, 
    curToggle = dataItems$curToggle
  )
  jsonDisplayRules <- jsonlite::toJSON(
    spec$displayRules, auto_unbox = TRUE, pretty = TRUE
  )
  myPage <- tagList(ui, tags$script(
    makeJsVariable(di, 'items'), 
    makeJsVariable(jsonDisplayRules, 'displayRules')
  ))
  writeLines(as.character(myPage), file.path(pluginDir, 'Extras/Gui/layout.html'))
}
