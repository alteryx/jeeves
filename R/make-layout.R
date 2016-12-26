#' @export
jvPage <- function(..., id){
  div(class = 'page', id = paste0('page-', id), ...)
}

#' @export
jvHeader <- function(...){
  div(class = 'fd-header', ...)
}

#' @export
jvContent <- function(..., .list = NULL){
  items = c(list(...), .list)
  div(class = 'content',
    div(class = 'content-padded', items)    
  )
}

#' @export
jvTabs <- function(..., .list = NULL){
  items = c(list(...), .list)
  div(class = 'tabs', id = 'header', items)
}

#' @export
jvTabNavItem <- function(..., id, active = FALSE){
  cl = if (active) 'tab active' else 'tab'
  div(class = cl, `data-page` = id, id = id, ...)
}

#' @export
jvTabPage <- function(title, ..., id = makeHtmlId(title)){
  div(class = 'tabpage', 
    id = paste0('tabpage-', id),
    `data-id` = id,
    `data-title` = title,
    ...
  )
}

#' @export
jvTabbedContent <- function(..., selected = NULL, .list = NULL){
  tabContent <- c(list(...), .list)
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

#' @export
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

#' @export
jvAccordion <- function(...){
  div(class = 'dt-accordion', ...)
}

#' @export
jvAccordionItem <- function(title, ..., id = makeHtmlId(title)){
  tagList(
    h4(class = 'accordion-toggle', title, id = id),
    div(class = 'accordion-content', ...)
  )
}


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

#' @export
makeHtmlId <- function(x){
  x <- gsub(".", "-", make.names(x), fixed = TRUE)
  x <- gsub("_", "-", x, fixed = TRUE)
  tolower(x)
}

#' @export
makeJsVariable <- function(x, name){
  d <- sprintf('
var %s = %s
', name, HTML(x))
  return(d)
}
