#Load packages
library(shiny)
library(tidyverse)
library(plotly)

## Source functions
source("pebble_game.R")

## Stop spurious warnings
options(warn = -1)

shinyServer(function(input, output) {

  output$pebble_plot <- renderPlotly({

    ## Run simulations
     pebble_sims <- multi_sim_pebble_game(r0 = input$r0, 
                                          no_in_first_gen = input$no_in_first_gen,
                                          prop_vac = input$prop_vac, 
                                          population = input$population,
                                          simulations = input$simulations)
     
    ## Summarise simulations
     pebble_sims_sum <- pebble_sims %>% 
       summarise_pebble_game_sim
     
    ## Plot summarised simulations
     plot_pebbles(pebble_sims_sum, y = input$sumstat)
  })

})
