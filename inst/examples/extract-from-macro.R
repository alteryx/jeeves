library(jeeves)
yxmc <- system.file("templates", "sample1.yxmc", package = 'jeeves')

# Extract Question Constatns
cat(extractQuestionConstants(yxmc))

# Extract Configuration
cat(extractConfiguration(yxmc, asYaml = TRUE))

# Extract Annotation from Configuration
extractAnnotationConfig(yxmc)

# Extract Annotation from Inputs
extractAnnotationInput(yxmc)

# Extract Annotation from Outputs
extractAnnotationOutput(yxmc)

# Extract Macro Constants
extractMacroConstants(yxmc = yxmc)


# Extract Icon
extractIcon(yxmc)
