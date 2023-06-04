# InitGreat

The **InteGreat** workflow provides an exhaustive platform where scientists
can experience the discovery process from the analysis of a single datum to
the creation of a complex computational model.

InteGreat consists of two modules: **Data Analysis** and **Model Integration**. 

1. The Data Analysis module includes tools specifically developed or adapted for the elaboration of raw Western Blot (WB), Reverse Transcription-quantitative PCR (RT-qPCR) and Enzyme-Linked ImmunoSorbent Assay (ELISA) experiments.

2. The Model Integration module supports scientists in the process of integration of lab data resulting from any type of experiment into a computational model.



## Required installed packages
The following R packages must be installed:
....

```
install.packages(c( "ggplot2", "shinydashboard", "shinyWidgets", "shiny", "dashboardthemes",
"dplyr","OpenImageR","knitr","zoo","shinythemes","readxl","DT","shinyjs"))
```

## How to install

To install it you can use  **devtools**:

```
install.packages("devtools")
library(devtools)
install_github("qBioTurin/InteGreat", ref="main",dependencies=TRUE)
```

## How to run 

To run the Shiny application:

```
InteGreat::InteGreat.run()
```
