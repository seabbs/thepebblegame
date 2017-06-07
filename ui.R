## Load packages
library(shiny)
library(tidyverse)
library(plotly)

## Source functions
source("pebble_game.R")

shinyUI(fluidPage(

  # Application title
  titlePanel("The Pebble Game: Understanding Epidemics and Vaccination"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("r0",
                  "Reproduction no.:",
                  min = 0,
                  max = 50,
                  value = 3),
      sliderInput("no_in_first_gen",
                  "No. in first generation:",
                  min = 1,
                  max = 50,
                  value = 1),
      sliderInput("prop_vac",
                  "Proportion vaccinated:",
                  min = 0,
                  max = 1,
                  value = 0.6),
      sliderInput("population",
                  "No. of pebbles:",
                  min = 1,
                  max = 1000,
                  value = 100),
      sliderInput("simulations",
                  "No. of simulations:",
                  min = 1,
                  max = 1000,
                  value = 10),
      selectInput("sumstat", 
                  "Summary statistic to plot:",
                  list(`No. of pebbles` = 
                         "`No. of pebbles`",
                       `Cumulative no. of pebbles` = 
                         "`Cumulative no. of pebbles`")
      ),
      submitButton("Apply changes")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("pebble_plot")
    )
  )
))
