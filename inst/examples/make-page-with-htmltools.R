library(htmltools)
library(jeeves)
pluginDir <- '.'
widgets <- renderPluginWidgets(pluginDir, wrapInDiv = TRUE)

home <- jvSetupPage('Setup', 
  widgets$model.name, widgets$select.target, widgets$select.predictors
)

modelTabItems <- tagList(
  widgets$use.weights, widgets$select.weights, widgets$`LabelGroup (93)`, 
  widgets$`LabelGroup (106)`, widgets$t.df
)

accordionTabItems = jvAccordion(
  jvAccordionItem('Fraction', widgets$bag.fraction, widgets$train.fraction),
  jvAccordionItem('Hyperparameters', widgets$interaction.depth, widgets$n.trees)
)

tabContent <- jvTabbedContent(
  jvTabPage(title = 'Model', modelTabItems),
  jvTabPage(title = 'Plots', p('Plots')),
  jvTabPage(title = 'Accordions', accordionTabItems),
  selected = 'Model'
)

customize <- jvCustomizePage('Customize', tabContent)

ui <- tagList(home, customize)
di <- jvMakeDataItemsToInitialize(curPage = 'Home', 
  curTab = 'model', curToggle = 'fraction'
)


myPage <- tagList(
  ui, makeJsVariable(di, 'items')
)
writeLines(as.character(myPage), 'Extras/Gui/layout.html')
updatePlugin()


