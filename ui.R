## Load packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)


sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Play the game", tabName="pebble_game", icon=icon("line-chart"), selected=TRUE),
              menuItem("How to play the game", tabName = "readme", icon=icon("mortar-board")),
              menuItem("Code",  icon = icon("file-text-o"),
                       menuSubItem("Github", href = "https://github.com/seabbs/thepebblegame", icon = icon("github")),
                       menuSubItem("pebble_game.R", tabName = "pebble_game_code", icon = icon("angle-right")),
                       menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                       menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))
              ),
              menuItem("About", tabName = "about", icon = icon("question"))
  ),
  hr()
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "readme",
            withMathJax(), 
            includeMarkdown("README.Rmd")
    ),
    tabItem(tabName = "pebble_game",
            fluidRow(
              column(width = 4, 
                     tabBox( width = NULL,
                             tabPanel(h5("Game Parameters"),
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
                             )
                     )),
              column(width = 8,
                     box(width = NULL, plotlyOutput("pebble_plot", height = "auto", width = "auto"), collapsible = TRUE,
                           title = "Plot", status = "primary", solidHeader = TRUE)
              ))
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
    ),
    tabItem(tabName = "about",
            includeMarkdown("about.Rmd")
    )
  )
)

dashboardPage(
  dashboardHeader(title = "The Pebble Game"),
  sidebar,
  body
)