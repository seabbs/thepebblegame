### Pebble game outline
library(tidyverse)

### Input
## Number of simulations - slider
## R0 - slider 
## Proportion Vaccinated - slider
## Population - slider

### Output 
## Graph of Simulation Trends over time
## Summary stats of Trends over time

## Function to estimate the number of cases in a generation
no_in_gen <- function(df, vac_status = 'no') {
  df %>% 
    filter(generation == max(generation, na.rm = TRUE), 
           vaccinated %in% vac_status) %>% 
    nrow
} 

## Function to run a pebble game simulation
gen_pebble_game <- function(df, generation_no, r0, no_in_first_gen) {
  ##define sampled population
  df_sampled <- df %>% 
    filter(!is.na(generation))
  
  ## Calculated generation number
  if (generation_no > 1) {
    no_in_generation <- df_sampled %>% 
      no_in_gen
  } else {
    no_in_generation <- no_in_first_gen
  }
  
  ## Restrict to unsampled pebbles
  df_unsampled <- df %>% 
    filter(is.na(generation))
  
  ## Sample pebbles
  no_sample <- no_in_generation * r0
  
  if (no_sample < length(df_unsampled$id)) {
    pebble_sample <- df_unsampled$id %>% 
      sample(no_sample, replace = FALSE)
  }else {
    pebble_sample <- df_unsampled$id
  }
  
  
  df_unsampled <- df_unsampled %>% 
    mutate(generation = generation %>% 
             replace(id %in% pebble_sample, generation_no)
    )
  
  ##Combine held back population with sampled population
  df <- df_sampled %>% 
    bind_rows(df_unsampled)
  
  return(df)
}

##Function to run a single simulation of the pebble game
pebble_game <- function(r0, 
                        no_in_first_gen,
                        prop_vac, 
                        population,
                        verbose) {
  ## number vaccinated
  no_vac <- round(prop_vac * population, digits = 0)
  
  ## create population in a data frame
  pebbles <- data_frame(id = seq(1, population, 1), 
                        vaccinated = c(rep("yes", no_vac), 
                                       rep("no", population - no_vac)),
                        generation = NA
                        )
  
  ## Define first generation
  generation_no <- 1
  no_left_to_infect <- population - no_vac
  no_in_generation <- 1
  
  ## Repeatedly sample pebbles
  while (no_left_to_infect > 0 & no_in_generation > 0) {
    ## Update pebbles
    pebbles <- pebbles %>% 
      gen_pebble_game(generation_no = generation_no, 
                      r0 = r0,
                      no_in_first_gen =  no_in_first_gen)
    
    ## Chec if there are any unvaccinated pebbles left
    no_left_to_infect <- pebbles %>% 
      filter(is.na(generation),
             vaccinated %in% "no") %>% 
      nrow
    
    ## Check if there are any unvaccinated pebbles
    ## in the current generation
    no_in_generation <- pebbles %>% 
      no_in_gen
    
    ## Advance generation number
    generation_no <- generation_no + 1
    
    if(verbose) {
      message("The generation number is:",  generation_no)
    }
  
  }

  return(pebbles)
}


## Function to run multiple simulations of the pebble game
multi_sim_pebble_game <- function(r0, 
                                  no_in_first_gen,
                                  prop_vac, 
                                  population,
                                  simulations,
                                  verbose) {
  
  df <- map_df(1:simulations, function(sim) {
    if (verbose) {
      message("Starting simulation number:", sim)
    }

    df <- pebble_game(r0, 
                      no_in_first_gen,
                      prop_vac, 
                      population,
                      verbose) %>% 
            mutate(simulation = sim)
    return(df)
  }
  ) 
      
 return(df)
}

## generate summaries by generation for data
summarise_pebble_game_sim <- function(df) {
  df <- df %>% 
    filter(vaccinated %in% "no",
           !is.na(generation)) %>% 
    group_by(simulation, generation) %>% 
    count %>% 
    mutate(cumsum = cumsum(n)) %>%
    rename(Generation = generation,
           Simulation = simulation,
           `No. of pebbles` = n,
           `Cumulative no. of pebbles` = cumsum)
  
  return(df)
}


## Make comparision ggplot to plotly plot
plot_pebbles <- function(df, y) {
  
  df %>% 
    ggplot(aes_string(x = "Generation", y = y)) +
    geom_point(alpha = 0.2, 
               colour = "dodgerblue2", 
               aes(group = Simulation)) +
    geom_line(alpha = 0.2, 
              colour = "dodgerblue2", 
              aes(group = Simulation)) +
    geom_smooth(method = "loess", alpha = 0.6) + 
    theme_minimal() -> plot
  
  return(plot)
}

## Wrap everything into a wrapper function for portability
sim_then_plot_pebble_game <- function(r0 = 3, 
                                      no_in_first_gen = 1,
                                      prop_vac = 0.6, 
                                      population = 1000,
                                      simulations = 100,
                                      verbose = FALSE,
                                      y = "`No. of pebbles`") {
  plot <- multi_sim_pebble_game(r0 = r0, 
                                no_in_first_gen = no_in_first_gen,
                                prop_vac = prop_vac, 
                                population = population,
                                simulations = simulations,
                                verbose = verbose) %>% 
    summarise_pebble_game_sim %>% 
    plot_pebbles(y = y)
  return(plot)
}

## Summary of functions
## df <- multi_sim_pebble_game(r0 = 3, no_in_first_gen = 1,prop_vac = 0.6, population = 1000,simulations = 100, verbose = FALSE) 
## df_count <- df %>% summarise_pebble_game_sim
## plot_pebbles(df_count, y = "`No. of pebbles`") %>% ggplotly
## plot_pebbles(df_count, y = "`Cumulative no. of pebbles`") %>% ggplotly
