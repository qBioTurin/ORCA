# ORCA: Omni Reproducible Cell Analysis

<p align="center">
  <img src="./inst/Shiny/www/images/ORCAlogo.png" alt="ORCA Logo" width="300">
</p>

**A cellular biologist’s toolbox for data analysis.**

ORCA provides an exhaustive platform where scientists can analyze raw data from:

- Western Blot (WB)
- Reverse Transcription-quantitative PCR (RT-qPCR)
- Enzyme-Linked ImmunoSorbent Assay (ELISA)
- Endocytosis
- Cytotoxicity
- Immunofluorescence
- Flow Cytometry analysis experiments

## Required Installed Packages

The following R packages must be installed:

```r
install.packages(c("rjson", "openxlsx", "patchwork", "ggplot2", "shinydashboard", 
                   "shinyWidgets", "shiny", "dashboardthemes", "dplyr", "OpenImageR", 
                   "knitr", "zoo", "shinythemes", "readxl", "DT", "shinyjs"))
```

## How to Install

To install ORCA, you can use **devtools**:

```r
install.packages("devtools")
devtools::install_github("qBioTurin/ORCA", ref="main", dependencies=TRUE)
```

## How to Run

To run the Shiny application:

```r
ORCA::ORCA.run()
```

## Docker

You need to have Docker installed on your machine. For more information, see the [Docker installation guide](https://docs.docker.com/engine/installation/).

To download all the Docker images used by **ORCA**, you can use:

```r
library(ORCA)
downloadContainers()
```

The Docker images are freely available at the following [link](https://hub.docker.com/r/qbioturin/).

## How to Run the Application with Docker

To run the ORCA application through its Docker image, use the following R function:

```r
library(ORCA)
docker.application.run()
```

## Dataverse Communication

The uploading of the analysis can be done using the *Dataverse* panel in the application. In this context, a Docker image can be utilized.

<p align="center">
  <img src="./inst/Shiny/www/images/Logo_QBio.png" alt="QBio Logo" width="200">
</p>
