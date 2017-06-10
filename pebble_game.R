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
    
    ## Check if there are any unvaccinated pebbles left
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
                                  verbose = FALSE) {
  
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
 ## clean up table
  df <- df %>% 
    mutate(generation = as.integer(generation))
 return(df)
}

## generate summaries by generation for data - count
summarise_pebble_game_sim <- function(df, simulations) {
  df_count <- df %>% 
    filter(vaccinated %in% "no",
           !is.na(generation)) %>% 
    group_by(simulation, generation) %>% 
    count
 
  ## Detect simmulations that have no intial cases
  ## Add these to the data frame with a generation number of 1, and n = 0
  df_count <- df_count %>% 
    bind_rows(data_frame(simulation = 1:simulations,
                                      generation = 1,
                                      n = 0) %>% 
    mutate(simulation = simulation %>% 
             replace(simulation %in% unique(df_count$simulation), NA)
           ) %>% 
    na.omit
    )
  
  ## Added in generations for which no pebbles were infected
  zero_generations <- df_count %>%
    group_by(simulation) %>% 
    filter(generation == max(generation)) %>%
    do(data_frame(simulation = .$simulation,
            generation = seq(.$generation + 1, max(df_count$generation) + 1, 1),
            n = 0
            )
    )
    
  ## Calculate cumulative sum
  df_cum <- df_count %>% 
    bind_rows(zero_generations) %>% 
    group_by(simulation) %>% 
    mutate(cumsum = cumsum(n)) %>%
    rename(Generation = generation,
           Simulation = simulation,
           `No. of pebbles` = n,
           `Cumulative no. of pebbles` = cumsum)
  
  return(df_cum)
}


## Make ggplot for playing the game 
plot_pebbles <- function(df, y, colour =  "dodgerblue2") {
  
  df %>% 
    ggplot(aes_string(x = "Generation", y = y)) +
    geom_point(alpha = 0.2, 
               colour = colour, 
               aes(group = Simulation)) +
    geom_line(alpha = 0.2, 
              colour = colour, 
              aes(group = Simulation)) +
    geom_smooth(method = "loess", alpha = 0.6, colour = colour) + 
    theme_minimal() -> plot
  
  return(plot)
}

##  Make a ggplot for comparing diseases
plot_pebbles_compare <- function(df, y) {
  
  df %>% 
    ggplot(aes_string(x = "Generation", y = y, colour = "Disease", group = "interaction(Disease, Simulation)")) +
    geom_point(alpha = 0.2) +
    geom_line(alpha = 0.2) +
    geom_smooth(method = "loess", alpha = 0.6, aes(group = Disease)) +
    scale_colour_manual(values=c("dodgerblue2", "firebrick2")) +
    theme_minimal() +
    theme(legend.position = "bottom") -> plot
  
  return(plot)
}

## Add a summary statistic
add_sum_stat <- function(df, stat_vect, sum_measure) {
  df <- df %>% 
    add_row(`Summary Measure` = sum_measure,
            Mean = stat_vect %>% 
              mean,
            Median = stat_vect %>% 
              median,
            `25%` = stat_vect %>% 
              quantile(probs = 0.25),
            `75%` = stat_vect %>% 
              quantile(probs = 0.75))
  
  return(df)
}
## Make table of summary data
## mean, medium, CI of generations reached
## 
summary_table <- function(df, 
                          population,
                          prop_vac) {
  ## Set up dataframe
  sum_tab <- data_frame(`Summary Measure` = NA, Mean = NA, Median = NA, `25%` = NA, `75%` = NA)
  
  ## No. in a generation
  sum_tab <- sum_tab %>% 
    add_sum_stat(stat_vect = df$`No. of pebbles`, 
                 sum_measure = "No. in a generation")
  
  ## no. of generations
  no_of_generations <- df %>% 
    group_by(Simulation) %>%
    filter(`No. of pebbles` > 0) %>%  
    summarise(no_of_gen = max(Generation))
  
  sum_tab <- sum_tab %>% 
    add_sum_stat(stat_vect = no_of_generations$no_of_gen, 
                 sum_measure = "No. of generations")
  
  ## Totol no. of infected pebbles
  total_no_infected_pebbles <- df %>% 
    group_by(Simulation) %>% 
    summarise(total_infect = max(`Cumulative no. of pebbles`))
  
  sum_tab <- sum_tab %>% 
    add_sum_stat(stat_vect = total_no_infected_pebbles$total_infect, 
                 sum_measure = "Total no. of infected")
  
  ## Number vaccinated
  no_unvac <- round((1 - prop_vac) * population, digits = 0)
  
  ## Calculate percentage of susceptible pop that are infected
  ## Tidy results
  sum_tab <- sum_tab %>%
    mutate_each(funs(as.character(as.integer(round(., digits = 0)))), Mean, Median, `25%`, `75%`) %>% 
    bind_rows(sum_tab %>%
                filter(`Summary Measure` %in% "Total no. of infected") %>% 
                mutate_each(funs(paste0(round(. / no_unvac * 100, digits = 0), "%")), Mean, Median, `25%`, `75%`) %>% 
                mutate(`Summary Measure` = "Percentage of unvaccinated infected")
    )

  ## Clear first row
  sum_tab <- sum_tab %>% na.omit
  
  return(sum_tab)
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
## df <- multi_sim_pebble_game(r0 = 3, no_in_first_gen = 1,prop_vac = 0.6, population = 1000, simulations = 100, verbose = FALSE) 
## df_count <- df %>% summarise_pebble_game_sim
## plot_pebbles(df_count, y = "`No. of pebbles`") %>% ggplotly
## plot_pebbles(df_count, y = "`Cumulative no. of pebbles`") %>% ggplotly
