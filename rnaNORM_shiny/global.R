library(readr)
library(ggplot2)
library(ggthemes)  
library(shiny)
library(plotly)
library(shinyWidgets)
library(shinyjs)
library(future)
library(promises)
library(shinycssloaders)
library(DT)

if (future::supportsMulticore()) {
  future::plan(future::multicore)
} else {
  future::plan(future::multisession)
}


# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)
