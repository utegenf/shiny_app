library(shinyWidgets)
library(shiny)
library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
# ------------------
# Main title section
# ------------------

ui <- navbarPage(
  "SuicideRate",
  theme = shinytheme("flatly"),
  tabPanel(
    "Main",
    # App title ----
    titlePanel(div(
      windowTitle = "SuicideRateSG",
      img(src = "sg0.jpg", width = "100%", class = "bg"),
    )),
    
    tags$br(),

