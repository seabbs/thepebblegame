## Load packages
library(shiny)
library(shinydashboard)
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
              column(width = 4, 
                     box( width = NULL,
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
                                      value = 50),
                          sliderInput("simulations",
                                      "No. of simulations:",
                                      min = 1,
                                      max = 1000,
                                      value = 10),
                          selectInput("sumstat", 
                                      "Summary statistic to plot:",
                                      list(`Cumulative no. of pebbles` = 
                                             "`Cumulative no. of pebbles`",
                                           `No. of pebbles` = 
                                             "`No. of pebbles`")
                          ),
                          actionButton("play_button", "Apply changes",
                                       icon("paper-plane"), 
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
                          title = 'Game Parameters', 
                          status = "primary", solidHeader = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE
                     )),
              column(width = 8,
                     box(width = NULL, 
                         plotOutput("pebble_plot"),
                         collapsible = TRUE,
                         title = "Plot",
                         footer = "Plot of each simulated game, overlayed with a trend line.",
                         status = "primary", 
                         solidHeader = FALSE),
                     tabBox( width = NULL,
                             title = "Tables",
                             side = "right",
                             tabPanel(title = "Summary Statistics",
                                      id = "tabletab1",
                                      tableOutput("pebble_table"),
                                      footer = "Summary statistics for the pebble game."
                             ),
                             tabPanel(title = "Simulation Data",
                                      id = "tabletab2",
                                      dataTableOutput("results_table"),
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
                          sliderInput("r0_prim",
                                      "Primary disease R0:",
                                      min = 0,
                                      max = 50,
                                      value = 3),
                          sliderInput("r0_sec",
                                      "Secondary disease R0:",
                                      min = 0,
                                      max = 50,
                                      value = 12),
                          sliderInput("no_in_first_gen_com",
                                      "No. in first generation:",
                                      min = 1,
                                      max = 50,
                                      value = 1),
                          sliderInput("prop_vac_com",
                                      "Proportion vaccinated:",
                                      min = 0,
                                      max = 1,
                                      value = 0.6),
                          sliderInput("population_com",
                                      "No. of pebbles:",
                                      min = 1,
                                      max = 1000,
                                      value = 100),
                          sliderInput("simulations_com",
                                      "No. of simulations:",
                                      min = 1,
                                      max = 1000,
                                      value = 10),
                          selectInput("sumstat_com", 
                                      "Summary statistic to plot:",
                                      list(`Cumulative no. of pebbles` = 
                                             "`Cumulative no. of pebbles`",
                                           `No. of pebbles` = 
                                             "`No. of pebbles`")
                          ),
                          actionButton("compare_button", "Apply changes",
                                       icon("paper-plane"), 
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          title = 'Disease Parameters', 
                          status = "primary", solidHeader = FALSE,
                          collapsible = TRUE,
                          collapsed = TRUE
                     )),
              column(width = 8,
                     box(width = NULL, 
                         collapsible = TRUE,
                         title = "Comparision Plot",
                         plotOutput("com_plot"),
                         footer = "Plot of each simulated disease, overlayed with a trend line.",
                         status = "primary", 
                         solidHeader = FALSE),
                     tabBox( width = NULL,
                             title = "Primary Disease",
                             side = "right",
                             tabPanel(title = "Summary Table",
                                      tableOutput("prim_sum_tab"), 
                                      id = "tabletab1",
                                      footer = "Summary statistics for the primary disease." ),
                             tabPanel(title = "Simulation Table",
                                      id = "tabletab2",
                                      dataTableOutput("prim_results_table"),
                                      downloadButton("downloadPrimDatatable", "Download"))
                     ),
                     tabBox( width = NULL,
                             title = "Secondary Disease",
                             side = "right",
                             tabPanel(title = "Summary Table",
                                      tableOutput("sec_sum_tab"),
                                      id = "tabletab1",
                                      footer = "Summary statistics for the primary disease."
                             ),
                             tabPanel(title = "Simulation Table",
                                      id = "tabletab2",
                                      dataTableOutput("sec_results_table"),
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