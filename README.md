# InitGreat

**OCA**: a cellular biologist’s toolbox for data analysis.

It provides an exhaustive platform where scientists can analyze raw:
Western Blot (WB),
Reverse Transcription-quantitative PCR (RT-qPCR),
Enzyme-Linked ImmunoSorbent Assay (ELISA),
Endocytosis and,
Cytotoxicity experiments.

## Required installed packages

The following R packages must be installed:

```
install.packages(c( "rjson", "openxlsx", "patchwork", "ggplot2", "shinydashboard", "shinyWidgets", "shiny", "dashboardthemes", "dplyr","OpenImageR","knitr","zoo", "shinythemes","readxl","DT","shinyjs"))
```

## How to install

To install it you can use  **devtools**:

```
install.packages("devtools")
library(devtools)
install_github("qBioTurin/OCA", ref="main",dependencies=TRUE)
```

## How to run 

To run the Shiny application:

```
OCA::OCA.run()
```

# Docker

You need to have docker installed on your machine, for more info see this document:
https://docs.docker.com/engine/installation/.

To download all the docker images exploited by **OCA** you can use:

```
library(OCA)
downloadContainers()
```

and the docker images are free available at the following [link](https://hub.docker.com/r/qbioturin/).

## How to run the application with docker

To run the OCA application through its docker image, it is possible to use the R function:

```
library(OCA)
docker.application.run()
```


## Dataverse communication

The uploading of the analysis can be done by exploiting the panel in the application called *Dataverse*. In this context, an image docker c



