# syntax=docker/dockerfile:1
FROM rocker/r-ver:4.5.2

ENV S6_VERSION=v2.1.0.2
ENV RSTUDIO_VERSION=daily
ENV DEFAULT_USER=rstudio
ENV PANDOC_VERSION=default
ENV PATH=/usr/lib/rstudio-server/bin:$PATH

RUN /rocker_scripts/install_rstudio.sh
RUN /rocker_scripts/install_pandoc.sh

# Install system libraries before compiling any R packages that need them.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-gnutls-dev \
    libssl-dev \
    libz-dev \
  libpng-dev \
  libtiff5-dev \
  libxml2-dev \
  libfontconfig1-dev \
  cmake \
  pkg-config \
  libglpk-dev \
  libglpk40 \
  && rm -rf /var/lib/apt/lists/*


# install R packages required - using latest CRAN mirror
RUN R -e 'install.packages(c(\
              "rjson", \ 
              "openxlsx", \
              "shiny", \
              "shinydashboard", \
              "ggplot2", \
              "zoo", \
              "knitr", \
              "shinythemes", \
              "OpenImageR", \
              "dplyr", \
              "shinyWidgets", \
              "DT", \
              "shinyjs",\
              "readxl", \
              "DT", \
              "shinyjs",\
              "randomcoloR",\
              "plotly"\
            ), \
            repos="https://cloud.r-project.org"\
          )'

RUN R -e 'install.packages(c("ggraph", "tidyverse", "xml2", "visNetwork", "shinyFiles"))'
RUN R -e 'install.packages("colourpicker")'
RUN R -e 'install.packages(c("shinyalert","shinybusy"))'

RUN R -e 'install.packages("ggpubr")'

RUN R -e 'install.packages(c("patchwork","BiocManager"))'
RUN R -e 'install.packages("remotes")'

RUN R -e 'install.packages("BH")'
RUN R -e 'BiocManager::install(c("ggcyto", "flowCore" ))'

# Create data directory
RUN mkdir ./home/data; chmod 777 ./home/data
#COPY ./Data/ ./home/data/Examples/


RUN R -e 'install.packages("shinyAce")'

RUN R -e 'remotes::install_github("qBioTurin/ORCA", ref="v3.0-(on-going)")'

# expose port
EXPOSE 3838

# Define default command.
# CMD ["bash"]
# run app on container start
CMD R -e ' ORCA::ORCA.run(inDocker = TRUE)'
