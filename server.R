#Load packages
source("load_packages.R")

## Source functions
source("pebble_game.R")

## Stop spurious warnings
options(warn = - 1)

shinyServer(function(input, output) {

  ## Simulation the game
  ## Play the game simulation
  sim_sum <- reactive({
    ## Depend on play button
    input$play_button
    
    ## Sim
    sim <- multi_sim_pebble_game(r0 = input$r0, 
                                 no_in_first_gen = input$no_in_first_gen,
                                 prop_vac = input$prop_vac, 
                                 population = input$population,
                                 simulations = input$simulations) %>%
      summarise_pebble_game_sim(simulations = input$simulations,
                                population = input$population,
                                prop_vac = input$prop_vac)
    
    return(sim)
  })
  
  ## Primary disease simulation
  prim_sim <- reactive({
    ## depends on comparision button press
    input$compare_button
    
    prim_sim <- multi_sim_pebble_game(r0 = input$r0_prim, 
                                 no_in_first_gen = input$no_in_first_gen_com,
                                 prop_vac = input$prop_vac_com, 
                                 population = input$population_com,
                                 simulations = input$simulations_com) %>%
      summarise_pebble_game_sim(simulations = input$simulations_com,
                                population = input$population_com,
                                prop_vac = input$prop_vac_com)
    
    return(prim_sim)
  })
  
  ## Secondary disease simulation
  sec_sim <- reactive({
    ## depends on comparision button press
    input$compare_button
    
    sec_sim <- multi_sim_pebble_game(r0 = input$r0_sec, 
                                 no_in_first_gen = input$no_in_first_gen_com,
                                 prop_vac = input$prop_vac_com, 
                                 population = input$population_com,
                                 simulations = input$simulations_com) %>%
      summarise_pebble_game_sim(simulations = input$simulations_com,
                                population = input$population_com,
                                prop_vac = input$prop_vac_com)
    
    return(sec_sim)
  })
  
  ## serve results table
  ## Play the game simulation
  output$results_table <- renderDataTable({
    sim_sum()
  },
  options = list(
    pageLength = 10)
  )
  
  ##Primary
  output$prim_results_table <- renderDataTable({
    prim_sim()
  },
  options = list(
    pageLength = 10)
  )
  
  ##Secondary
  output$sec_results_table <- renderDataTable({
    sec_sim()
  },
  options = list(
    pageLength = 10)
  )
  
  ## Serve results plot - playing the game
  output$pebble_plot <- renderPlot({
    ## Run simulations, summarise and plot see pebble_game.R
    plot <- sim_sum() %>%
      plot_pebbles(y = input$sumstat)
    
    plot
  })

  ## Serve primary disease plot
  output$prim_plot <- renderPlot({
    ## Run simulations, summarise and plot see pebble_game.R
    plot <- prim_sim() %>%
      plot_pebbles(y = input$sumstat_com)
    
    plot
  })
  ## Serve secondary disease plot
  output$sec_plot <- renderPlot({
    ## Run simulations, summarise and plot see pebble_game.R
    plot <- sec_sim() %>%
      plot_pebbles(y = input$sumstat_com,
                   colour = "firebrick2")
    
    plot
  })
  
  ## Serve results plot for comparing disease
  output$com_plot <- renderPlot({
    ## Bind data
    com_sim <- prim_sim() %>% 
      mutate(Disease = "Primary") %>% 
      bind_rows(sec_sim() %>% 
                  mutate(Disease = "Secondary"))
    ## Run simulations, summarise and plot see pebble_game.R
    plot <- com_sim %>%
      plot_pebbles_compare(y = input$sumstat_com)
    
    plot
  })
  
  ## Summary data tables
  
  ## Play the game
  output$pebble_table <- renderTable({
    ## generate summary df
    sim_sum() %>% summary_table(population = input$population,
                                prop_vac = input$prop_vac
                                )
  })
  
  ## Primary disease
  output$prim_sum_tab <- renderTable({
    ## generate summary df
    prim_sim() %>% summary_table(population = input$population_com,
                                 prop_vac = input$prop_vac_com
    )
  })
  
  ## Secondary disease
  output$sec_sum_tab <- renderTable({
    ## generate summary df
    sec_sim() %>% summary_table(population = input$population_com,
                                prop_vac = input$prop_vac_com
    )
  })
  
  ## Set up downloadbale data
  
  ## Play the game
  output$downloadDatatable <- downloadHandler(filename = "pebble_game_sim_results.csv",
                                              content = function(file) {
                                                write.csv(sim_sum(), file)
                                              }
  )
  
  ## Primary disease
  output$downloadPrimDatatable <- downloadHandler(filename = "pebble_game_prim_disease_sim_results.csv",
                                              content = function(file) {
                                                write.csv(prim_sim(), file)
                                              }
  )
  
  ## Secondary disease
  output$downloadSecDatatable <- downloadHandler(filename = "pebble_game_sec_disease_sim_results.csv",
                                              content = function(file) {
                                                write.csv(sec_sim(), file)
                                              }
  )
  
  
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
