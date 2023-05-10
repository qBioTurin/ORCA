FROM rocker/shiny:4.0.5

# system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev
  

# install R packages required 
RUN R -e 'install.packages(c(\
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
              "readxl" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2022-08-01"\
          )'

# date > marker
COPY marker /dev/null

# copy the app directory into the image
COPY ./Shiny/* ./app/

# Create data directory
RUN mkdir ./home/data; chmod 777 ./home/data
COPY ./Data/ ./home/data/Examples/

# expose port
EXPOSE 3838

# Define default command.
#CMD ["bash"]
# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
