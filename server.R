#Load packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(prettypublisher)

## Source functions
source("pebble_game.R")

## Stop spurious warnings
options(warn = -1)

shinyServer(function(input, output) {

  sim_sum <- reactive({
    sim <- multi_sim_pebble_game(r0 = input$r0, 
                                 no_in_first_gen = input$no_in_first_gen,
                                 prop_vac = input$prop_vac, 
                                 population = input$population,
                                 simulations = input$simulations) %>%
      summarise_pebble_game_sim
    
    return(sim)
  })
  
  output$pebble_plot <- renderPlot({
    ## Run simulations, summarise and plot see pebble_game.R
    plot <- sim_sum() %>%
      plot_pebbles(y = input$sumstat)
    
  #  ggplotly(plot) %>% layout(autosize = TRUE)
    plot
  })

  output$pebble_table <- renderTable({
    ## generate summary df
    sim_sum() %>% summary_table
  })
  
  ## Set up downloadable scripts
  output$downloadData1 <- downloadHandler(filename = "pebble_game.R",
                                          content = function(file) {
                                            file.copy("pebble_game.R", file, overwrite = TRUE)
                                            }
                                          )
  output$downloadData2 <- downloadHandler(filename = "ui.R",
                                          content = function(file) {
                                            file.copy("ui.R", file, overwrite = TRUE)
                                            }
                                          )
  output$downloadData3 <- downloadHandler(filename = "server.R",
                                          content = function(file) {
                                            file.copy("server.R", file, overwrite = TRUE)
                                            }
                                          )
  
})
