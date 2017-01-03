#' Create a page
#' 
#' @param ... elements to pass
#' @param id html id
#' @export
jvPage <- function(..., id){
  div(class = 'page', id = paste0('page-', id), ...)
}

#' Create a header
#' 
#' @param ... elements to pass to the header
#' @export
jvHeader <- function(...){
  div(class = 'fd-header', ...)
}

#' Create a content div
#' 
#' @param ... elements to pass.
#' @param .list same as ..., but passed as a list.
#' @export
jvContent <- function(..., .list = NULL){
  items = c(list(...), .list)
  div(class = 'content',
    div(class = 'content-padded', items)    
  )
}

#' Create tabs
#'
#' @inheritParams jvContent
#' @export
jvTabs <- function(..., .list = NULL){
  items = c(list(...), .list)
  div(class = 'tabs', id = 'header', items)
}

#' Create a tab nav item
#' 
#' 
#' @inheritParams jvPage
#' @param active boolean indicating if the tab item should be active.
#' @export
jvTabNavItem <- function(..., id, active = FALSE){
  cl = if (active) 'tab active' else 'tab'
  div(class = cl, `data-page` = id, id = id, ...)
}

#' Create a tab page
#' 
#' @param title the title for the tab page
#' @param ... elements to add as tabs
#' @param id html id of the tab page
#' @export
jvTabPage <- function(title, ..., id = makeHtmlId(title)){
  div(class = 'tabpage', 
    id = paste0('tabpage-', id),
    `data-id` = id,
    `data-title` = title,
    ...
  )
}

#' Create tabbed content
#' 
#' @param ... elements to create tabs from
#' @param selected boolean indicating which tab should be active
#' @param .list same as ..., but provided as a list
#' @export
jvTabbedContent <- function(..., selected = NULL, .list = NULL){
  tabContent <- c(list(...), .list)
  tabIds <- lapply(tabContent, function(tp){tp$attribs$`data-id`})
  if (is.null(selected)) selected <- tabIds[1]
  tabNav <- lapply(tabContent, function(tp){
    jvTabNavItem(
      tp$attribs$`data-title`, 
      id = tp$attribs$`data-id`,
      style = sprintf("width:%s%%", round(100/length(tabContent), 2)),
      active = (tp$attribs$`data-id` == selected)
    )
  })
  tabContent <- lapply(tabContent, function(d){
    if (d$attribs$`data-id` == selected) {
      d$attribs$class <- paste(d$attribs$class, 'active')
    }
    return(d)
  })
  tagList(
    jvTabs(tabNav),
    jvContent(tabContent)
  )
}

#' Create the setup page
#' 
#' @param title tile of the page
#' @param ... elements to build the page from
#' @export
jvSetupPage <- function(title, ...){
  customizeButton <- tags$button(class = 'switch', 
    id = 'switch-to-customize', 
    `data-page` = 'Customize',
    "Customize",
    tags$i(class = 'en-right-open-big pull-right')
  )
  jvPage(id = 'basic',
    jvHeader(title),
    jvContent(..., customizeButton)
  )
}

#' Create the customize page
#'
#'
#' @inheritParams jvSetupPage
#' @export
jvCustomizePage <- function(title, ...){
  backButton <- tags$i(class = 'en-left-open-big switch', 
    id = 'switch-to-home', `data-page` = 'Home'
  )
  jvPage(id = 'customize', style='display:none;',
    jvHeader(backButton, title), 
    ...
  )
}

#' Create an accordion
#' 
#' @param ... elements to build the accordion div.
#' @export
jvAccordion <- function(...){
  div(class = 'dt-accordion', ...)
}

#' Create an accordion item
#' 
#' @param title the title for the accordion item
#' @param ... elements to build the accordion item
#' @param id html id for the accordion item
#' @export
jvAccordionItem <- function(title, ..., id = makeHtmlId(title)){
  tagList(
    h4(class = 'accordion-toggle', title, id = id),
    div(class = 'accordion-content', ...)
  )
}


#' Create data items to initialize
#' 
#' @param pluginDir path to plugin directory
#' @param curPage id of page to be active by default
#' @param curTab id of tab to be active by default
#' @param curToggle id of toggle to be active by default
#' @param ... other data items to be initialized before load
#' @export
jvMakeDataItemsToInitialize <- function(pluginDir = ".", curPage, 
    curTab = NULL, curToggle = NULL, ...){
  items <- list(
    itemsToInitialize = Filter(Negate(is.null), list(
      curPage = curPage,
      curTab = curTab,
      curToggle = curToggle,
      ...
    )),
    toggleBarItems = getItemsOfType(
      'ToggleBar', pluginDir = pluginDir, toJSON = F
    ),
    radioItems = getItemsOfType(
      'RadioGroup', pluginDir = pluginDir, toJSON = F
    )
  )
  jsonlite::toJSON(items, auto_unbox = TRUE, pretty = TRUE)
}

#' Make a valid html id
#' 
#' @param x string to sanitize and convert to a valid html id
#' @export
makeHtmlId <- function(x){
  x <- gsub(".", "-", make.names(x), fixed = TRUE)
  x <- gsub("_", "-", x, fixed = TRUE)
  tolower(x)
}

#' Make a javascript variable
#' 
#' @param x json object
#' @param name name to provide for the javascript variable
#' @export
makeJsVariable <- function(x, name){
  d <- sprintf('
var %s = %s
', name, HTML(x))
  return(d)
}

getPathToMacro <- function(pluginDir = "."){
  dirs <- dirNames()
  pluginName <- basename(normalizePath(pluginDir))
  file.path(pluginDir, 
    dirs$macros, 
    sprintf("%s.yxmc", pluginName)
  )
}

extractOverrides <- function(pluginDir = "."){
  dirs <- dirNames()
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

getItemsOfType <- function(itemType = 'ToggleBar', pluginDir = ".", 
    toJSON = TRUE){
  config <- extractConfigurationWithOverrides(pluginDir)
  isToggleBar <- function(d){d$type == itemType}
  items <- lapply(Filter(isToggleBar, config), function(d){
    unname(as.vector(names(d$values)))
  })
  if (toJSON){
    jsonlite::toJSON(items, auto_unbox = TRUE, pretty = TRUE)
  } else {
    items
  }
}
