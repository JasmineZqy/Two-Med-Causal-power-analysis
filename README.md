# Two-Med-Causal-Power-Analysis

Shiny App: Power Analysis for Two-Mediator Causal Mediation Models

The online app can be accessed here: [https://yourname.shinyapps.io/TwoMedCausalApp/](https://yourname.shinyapps.io/TwoMedCausalApp/)  


The app can also be run locally using the following code in **R**:

```r
# Install required packages
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "purrr", "stringr","mvtnorm"))

# Run the app (if cloned from GitHub)
shiny::runApp("path/to/downloaded/folder")

# Or run the app directly from the R console
shiny::runGitHub("Two-Med-Causal-Power-Analysis", "JasmineZqy")
