library(jeeves)
displayRules = list(
  "subsection-rpart" = c("model.algorithm", "rpart"),
  "subsection-c50" = c("model.algorithm", "c50")
)
writeLayoutFromYamlSpec(displayRules = displayRules,
  curPage = 'Home', curTab = 'model', curToggle = 'model-and-sampling-weights'
)
updatePlugin()