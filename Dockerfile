
## Start rocker r image
FROM rocker/r-ver:3.4.4

MAINTAINER "Sam Abbott" contact@samabbott.co.uk

## Get libs required by packages
RUN apt-get update && \
	apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    && apt-get clean

## Install R packages - MRAN
RUN Rscript -e 'install.packages(c("shiny", "shinydashboard", "shinyBS", "tidyverse", "rmarkdown"))'

ADD . home/thepebblegame

WORKDIR  home/thepebblegame

EXPOSE 3838

## Create log file
CMD R -e 'shiny::runApp(port = 3838)'