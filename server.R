#Load packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

## Source functions
source("pebble_game.R")

## Stop spurious warnings
options(warn = -1)

shinyServer(function(input, output) {

  output$pebble_plot <- renderPlotly({
    ## Run simulations, summarise and plot see pebble_game.R
    plot <- sim_then_plot_pebble_game(r0 = input$r0, 
                                      no_in_first_gen = input$no_in_first_gen,
                                      prop_vac = input$prop_vac, 
                                      population = input$population,
                                      simulations = input$simulations,
                                      y = input$sumstat)
    ggplotly(plot) %>% layout(autosize = TRUE)
  })

})
