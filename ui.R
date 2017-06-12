## Load packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(tidyverse)
library(rmarkdown)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id = "tabs",
              menuItem("Play the game", tabName="pebble_game", icon=icon("line-chart"), selected=TRUE),
              menuItem("Compare diseases", tabName = "disease_com", icon = icon("random")),
              menuItem("How to play the game", tabName = "readme", icon=icon("mortar-board")),
              menuItem("Code",  icon = icon("code"),
                       menuSubItem("Github", href = "https://github.com/seabbs/thepebblegame", icon = icon("github")),
                       menuSubItem("pebble_game.R", tabName = "pebble_game_code", icon = icon("angle-right")),
                       menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                       menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))
              )
  ),
  hr(),
  helpText("Developed by ", a("Sam Abbott, ", href="http://samabbott.co.uk"), 
           a("Bristol Infectious Disease Dynamics, ", href="http://www.bristol.ac.uk/social-community-medicine/research/groups/bidd/"), "University of Bristol", style="padding-left:1em; padding-right:1em;position:absolute; bottom:1em; ")
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "readme",
            withMathJax(), 
            includeMarkdown("README.md")
    ),
    tabItem(tabName = "pebble_game",
            fluidRow(
              tags$head(includeScript("google-analytics.js")),
              column(width = 4, 
                     box( width = NULL,
                          tipify(sliderInput("r0",
                                      "Reproduction no.:",
                                      min = 1,
                                      max = 50,
                                      value = 3),
                                 title = "The basic reproduction rate (R0) is used to measure the transmission potential of a disease. It is thought of as the number of secondary infections produced by a typical case of an infection in a population that is totally susceptible."
                                 ),
                          tipify(sliderInput("no_in_first_gen",
                                      "No. in first generation:",
                                      min = 1,
                                      max = 50,
                                      value = 1),
                                 title = "The number of infected pebbles introduced into the population. Does altering this have any effect on the epidemic dynamics?"),
                          tipify(sliderInput("prop_vac",
                                      "Proportion vaccinated:",
                                      min = 0,
                                      max = 1,
                                      value = 0.6),
                                 title = "The proportion of the population that have been vaccinated, the number that are vaccinated can be calulated by multiplying the proportion vaccinated by the population. Try to find the vaccination thresold at which an epidemic cannot occur."),
                          tipify(radioButtons("population",
                                              label = "No. of pebbles:",
                                              choices = c(50, 100, 250, 500, 1000),
                                              selected = 100,
                                              inline = TRUE),
                                 title = "How many pebbles do you want to simulate? Does this alter the trend, or does it just alter the epidemic size? Note: A high population will take longer to compute."
                                 ),
                          tipify(radioButtons("simulations",
                                      label = "No. of simulations:",
                                      choices = c(1, 10, 50, 100, 500, 1000),
                                      selected = 10,
                                      inline = TRUE),
                                 title = "How many times to repeat the game. What is the effect of increasing this? Note: A high number of simulations will take longer to compute"
                          ),
                          tipify(selectInput("sumstat", 
                                      "Summary statistic to plot:",
                                      list(`Cumulative no. of pebbles` = 
                                             "`Cumulative no. of pebbles`",
                                           `No. of pebbles` = 
                                             "`No. of pebbles`",
                                           `Percentage (%) of unvaccinated infected` = 
                                             "`Percentage (%) of unvaccinated infected`")
                          ),
                          title = "The cumulative number of pebbles (i.e. the rolling total) gives the clearest picture of the final epidemic size. The number of pebbles (in each generation) shows how the epidemic evolves over time. The percentage of unvaccinated infected allows diseases to be compared more easily."
                          ),
                          tipify(actionButton("play_button", "Simulate", 
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 title = "What effect does repeating the game with the same parameters have?"
                          ),
                          title = 'Game Parameters', 
                          status = "primary", solidHeader = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE
                     )),
              column(width = 8,
                     box(width = NULL, 
                         tipify(plotOutput("pebble_plot"),
                                title = "Plot of the simulations of the game, overlayed with the mean and 95% confidence intervals. What effect does varying the parameters have?"),
                     collapsible = TRUE,
                     title = "Plot",
                     status = "primary", 
                     solidHeader = FALSE
                     ),
                     tabBox( width = NULL,
                             title = "Tables",
                             side = "right",
                             tabPanel(title = "Summary Statistics",
                                      id = "tabletab1",
                                      tipify(tableOutput("pebble_table"),
                                             title = "Summary statistics for the pebble game. How does varying the parameters effect these? Do these summary measures match what you see on the plot above?",
                                             placement = "top"
                                      )
                             ),
                             tabPanel(title = "Simulation Data",
                                      id = "tabletab2",
                                      tipify(dataTableOutput("results_table"),
                                             title = "Simulation results from playing the pebble game, with your specified parameters. Download this with the button below and see what patterns you can find.",
                                             placement = "top"
                                      ),
                                      downloadButton('downloadDatatable', 'Download')
                                      )
                     )
                     )
            )
    ),
    tabItem(tabName = "disease_com",
            fluidRow(
              column(width = 4, 
                     box( width = NULL,
                          tipify(sliderInput("r0_prim",
                                      "Primary disease R0:",
                                      min = 1,
                                      max = 50,
                                      value = 3),
                                 title = "The basic reproduction rate (R0) is used to measure the transmission potential of a disease. It is thought of as the number of secondary infections produced by a typical case of an infection in a population that is totally susceptible."
                          ),
                          tipify(sliderInput("r0_sec",
                                      "Secondary disease R0:",
                                      min = 1,
                                      max = 50,
                                      value = 12),
                                 title = "The basic reproduction rate (R0) is used to measure the transmission potential of a disease. It is thought of as the number of secondary infections produced by a typical case of an infection in a population that is totally susceptible."
                          ),
                          tipify(sliderInput("no_in_first_gen_com",
                                      "No. in first generation:",
                                      min = 1,
                                      max = 50,
                                      value = 1),
                                 title = "The number of infected pebbles introduced into the population. Does altering this have any effect on the epidemic dynamics?"),
                          tipify(sliderInput("prop_vac_com",
                                      "Proportion vaccinated:",
                                      min = 0,
                                      max = 1,
                                      value = 0.6),
                                 title = "The proportion of the population that have been vaccinated, the number that are vaccinated can be calulated by multiplying the proportion vaccinated by the population. Try to find the vaccination thresold at which an epidemic cannot occur."),
                          tipify(radioButtons("population_com",
                                      label = "No. of pebbles:",
                                      choices = c(50, 100, 250, 500, 1000),
                                      selected = 100,
                                      inline = TRUE),
                                 title = "How many pebbles do you want to simulate? Does this alter the trend, or does it just alter the epidemic size? Note: A high population will take longer to compute."
                          ),
                          tipify(radioButtons("simulations_com",
                                              label = "No. of simulations:",
                                              choices = c(1, 10, 50, 100, 500, 1000),
                                              selected = 10,
                                              inline = TRUE),
                                 title = "How many times to repeat the game. What is the effect of increasing this? Note: A high number of simulations will take longer to compute"
                          ),
                          tipify(selectInput("sumstat_com", 
                                      "Summary statistic to plot:",
                                      list(`Cumulative no. of pebbles` = 
                                             "`Cumulative no. of pebbles`",
                                           `No. of pebbles` = 
                                             "`No. of pebbles`",
                                           `Percentage (%) of unvaccinated infected` = 
                                             "`Percentage (%) of unvaccinated infected`")
                          ),
                          title = "The cumulative number of pebbles (i.e. the rolling total) gives the clearest picture of the final epidemic size. The number of pebbles (in each generation) shows how the epidemic evolves over time. The percentage of unvaccinated infected allows diseases to be compared more easily."
                          ),
                          tipify(actionButton("compare_button", "Simulate", 
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 title = "What effect does repeating the game with the same parameters have?"
                          ),
                          title = "Disease Parameters", 
                          status = "primary", solidHeader = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE
                     )),
              column(width = 8,
                     tabBox(width = NULL, 
                         title = "Plots",
                         side = "right",
                         tabPanel(
                           title = "Comparision",
                           id = "tabletab1",
                           tipify(plotOutput("com_plot"),
                                  title = "Comparision plot of simulations of the game for both diseases, overlayed with the mean and 95% confidence intervals. Compare the difference between the trend line gradients, what does this mean?")
                         ),
                         tabPanel(
                           title = "Primary Disease",
                           id = "tabletab2",
                           tipify(plotOutput("prim_plot"),
                                  title = "Plot of the simulations of the game for the primary disease, overlayed with the mean and 95% confidence intervals. What effect does varying the other parameters have?")
                         ),
                         tabPanel(
                           title = "Secondary Disease",
                           id = "tabletab3",
                           tipify(plotOutput("sec_plot"),
                                  title = "Plot of the simulations of the game for the primary disease, overlayed with the mean and 95% confidence intervals. What effect does varying the other parameters have?")
                           )
                         ),
                     tabBox( width = NULL,
                             title = "Primary Disease",
                             side = "right",
                             tabPanel(title = "Summary Table",
                                      tipify(tableOutput("prim_sum_tab"), 
                                             title = "Summary statistics for the pebble game, played with the primary disease. How does varying the parameters effect these? Do these summary measures match what you see on the plot above and how do they correspond with those from the secondary disease?",
                                             placement = "top"
                                      ),
                                      id = "tabletab1"
                                      ),
                             tabPanel(title = "Simulation Table",
                                      id = "tabletab2",
                                      tipify(dataTableOutput("prim_results_table"),
                                             title = "Simulation results from playing the pebble game with the primary disease, and your specified parameters. Download this with the button below and see what patterns you can find.",
                                             placement = "top"
                                      ),
                                      downloadButton("downloadPrimDatatable", "Download"))
                     ),
                     tabBox( width = NULL,
                             title = "Secondary Disease",
                             side = "right",
                             tabPanel(title = "Summary Table",
                                      tipify(tableOutput("sec_sum_tab"),
                                             title = "Summary statistics for the pebble game, played with the secondary disease. How does varying the parameters effect these? Do these summary measures match what you see on the plot above and how do they correspond with those from the primary disease?",
                                             placement = "top"
                                      ),
                                      id = "tabletab1"
                             ),
                             tabPanel(title = "Simulation Table",
                                      id = "tabletab2",
                                      tipify(dataTableOutput("sec_results_table"),
                                             title = "Simulation results from playing the pebble game with the secondary disease, and your specified parameters. Download this with the button below and see what patterns you can find.",
                                             placement = "top"
                             ),
                                      downloadButton("downloadSecDatatable", "Download")
                             )
                     )
              )
            )
            ),
    tabItem(tabName = "pebble_game_code",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Simulation Functions",                
                 downloadButton('downloadData1', 'Download'),
                 br(),br(),
                 pre(includeText("pebble_game.R"))
            )
    ),
    tabItem(tabName = "ui",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="UI",
                 downloadButton('downloadData2', 'Download'),
                 br(),br(),
                 pre(includeText("ui.R"))
            )
    ),
    tabItem(tabName = "server",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Server",
                 downloadButton('downloadData3', 'Download'),
                 br(),br(),
                 pre(includeText("server.R"))
            )
    )
  )
)

dashboardPage(
  dashboardHeader(title = "The Pebble Game"),
  sidebar,
  body
)