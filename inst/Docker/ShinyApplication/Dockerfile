FROM rocker/r-ver:4.2.1

ENV S6_VERSION=v2.1.0.2
ENV RSTUDIO_VERSION=daily
ENV DEFAULT_USER=rstudio
ENV PANDOC_VERSION=default
ENV PATH=/usr/lib/rstudio-server/bin:$PATH

RUN /rocker_scripts/install_rstudio.sh
RUN /rocker_scripts/install_pandoc.sh

# system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libz-dev \
    libpng-dev\
    libtiff5-dev
  

# install R packages required 
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
              "plotly",\
              "flowCore"\
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2022-08-01"\
          )'

RUN R -e 'install.packages(c("ggraph","tidyverse","xml2", "visNetwork", "shinyFiles"))'
RUN R -e 'install.packages("colourpicker")'
RUN R -e 'install.packages(c("shinyalert","shinybusy"))'

RUN apt-get update && apt-get install -y  cmake

RUN R -e 'install.packages("ggpubr")'

RUN apt-get update && apt-get install -y libfontconfig1-dev
RUN apt-get update && apt-get install -y libxml2-dev 
RUN R -e 'install.packages("ggraph")'
RUN R -e 'install.packages("tidyverse")'

RUN R -e 'install.packages(c("patchwork","BiocManager"))'
RUN R -e 'install.packages("remotes")'

RUN R -e 'remotes::install_github("qBioTurin/ORCA", ref="main"); '
# copy the app directory into the image

COPY ./inst/Shiny/* ./app/
# Create data directory
RUN mkdir ./home/data; chmod 777 ./home/data
#COPY ./Data/ ./home/data/Examples/

# expose port
EXPOSE 3838

# Define default command.
# CMD ["bash"]
# run app on container start
CMD R -e ' ORCA::ORCA.run(TRUE)'
