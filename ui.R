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
      img(src = "suicide.jpeg", width = "100%", class = "bg"),
    )),
    
    tags$br(),
 
 tabsetPanel(
      type = "tabs",
      tabPanel(
        "Summary",
        ################################################
        #### Panel: Main>Summary>Tables & Pie Chart ####
        ################################################
        
        # ------------------
        # ranking $ pie chart section
        # ------------------
        
        sidebarLayout(
          sidebarPanel(
            h3("Data by Year"),
            tags$br(),
            selectInput(
              "checkYear",
              "Select Year",
              choices = list("2019", "2018", "2017"),
              selected = "2018"
            )
          ),
          
          mainPanel(
            tabsetPanel(
              type = "tabs",
              tabPanel("Ranking", tableOutput("datahead")),
            ),
            tags$br(),
            tags$br(),
          )
        ),
        tags$hr(),
