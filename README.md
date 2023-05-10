# InitGreat

It is R package that is able to.....

### Required installed packages
The following R packages must be installed:
....

```
install.packages(c( "ggplot2", "shinydashboard", "shinyWidgets", "shiny", "dashboardthemes",
"dplyr","OpenImageR","knitr","zoo","shinythemes","readxl","DT","shinyjs"))
```

### How to install ....
To install it you can use  **devtools**:

```
install.packages("devtools")
library(devtools)
install_github("qBioTurin/GelAnalyser", ref="main",dependencies=TRUE)
```
To run the Shiny application:

```
GelAnalyser::displayGel()
```
