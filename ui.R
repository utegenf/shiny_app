library(shinyWidgets)
library(shiny)
library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
# ------------------
# Main title section
# ------------------

ui <- navbarPage(
  "Anime Suggestions",
  theme = shinytheme("flatly"),
  tabPanel(
    "Main",
    # App title ----
    titlePanel(div(
      windowTitle = "AnimeSG",
      img(src = "wallpaper.jpeg", width = "100%", class = "bg"),
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
              choices = list("1997", "1998", "2000", "2001", "2002", "2004", "2008", "2010", "2018", "2020", "2021"),
              selected = "2021"
            )
          ),
          
          mainPanel(
            tabsetPanel(
              type = "tabs",
              tabPanel("Score", tableOutput("datahead")),
            ),
            tags$br(),
            tags$br(),
          )
        ),
        tags$hr(),
        
        
        sidebarLayout(
          sidebarPanel(
            # ------------------
            # Data overview filters
            # ------------------
            
            h3("Data Overview"),
            tags$br(),
            setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
            sliderInput(
              "durationRange",
              label = "Duration per episode/min",
              min = 10,
              max = 50,
              value = c(10, 50)
            ),
            setSliderColor(c("e67e22 ", "#e67e22"), c(1, 2)),
            sliderInput(
              "episodeRange",
              label = "Number of episodes",
              min = 3,
              max = 1800,
              value = c(3, 1800)
            ),
            selectInput(
              "checkYearGroup",
              "Select Year",
              choices = data$year,
              selected = "2018",
              multiple = TRUE
            ),
            
            checkboxGroupInput("checkYear", label = "Select Year",
                              choices = list("1997", "1998", "2000", "2001", "2002", "2004", "2008", "2010", "2018", "2020", "2021"),
                             selected = list("1997", "1998", "2000", "2001", "2002", "2004", "2008", "2010", "2018", "2020", "2021"), inline = TRUE),
            
            actionButton("actionDT", "Filter", class = "btn btn-warning"),
          ),
          mainPanel(
            h3("Browse All"),
            tags$br(),
            dataTableOutput("myTable"),
            tags$br(),
            tags$br(),
          )
        ),
        tags$hr(),
      ),
   
        
      ################################################
      #### Panel: Main>Details                    ####
      ################################################
      
      tabPanel(
        "Details By Producers",
        h3("Highest scored animes by genre/producer", align = "center"),
        br(),
        div(style = "display:vertical-align:center;center-align",
            fluidRow(
              column(
                4,
                selectInput(
                  "detailproducers",
                  label = "Select STUDIO production",
                  choices = unique(data$Producers),
                  selected = "DAX Production",
                  width = 400
                ),
              ),
              column(
                4,
                selectInput(
                  "detailgenre",
                  "Select Genre",
                  choices = "",
                  selected = "",
                  width = 400
                )
              ),
              column(4,
                     column(
                       8,
                       selectInput(
                         "detailRating",
                         "Select Rating",
                         choices = "",
                         selected = "",
                         width = 400
                       )
                     ),
                     column(
                       4,
                       tags$br(),
                       actionButton("detailFilter", "Filter", class = "btn btn-warning btn-sm")
                     ))
            )),
        
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),
        
        fluidRow(
          column(4, tableOutput("detailTable")),
          column(4, h5("Average Score", align="center"), plotOutput(outputId = "detailPlot", height = "300px")),
        ),
        
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br()
      )
    )
  ),
  
)
