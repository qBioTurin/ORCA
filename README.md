# GelAnalyser

It is R package that is able to.....

### Required installed packages
The following R packages must be installed:
....

```
install.packages(c( "ggplot2", "RColorBrewer", "shinyWidgets", "viridis", "dashboardthemes"))
```

### How to install ....
To install it you can use  **devtools**:

```
install.packages("devtools")
library(devtools)
install_github("qBioTurin/GelAnalyser", ref="master",dependencies=TRUE)
```
To run the Shiny application:

```
GelAnalyser::displayGel()
```
