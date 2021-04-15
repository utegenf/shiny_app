# load the required packages
library(shiny)
require(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(ggthemes)
library(knitr)
library(DT)



recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)



#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Anime Recommendation")

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Summary", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Details", tabName= "details", badgeLabel = "filter", badgeColor = "green",icon = icon("send",lib='glyphicon')
  )
)

)


body <- dashboardBody(
  picture <- titlePanel(div(
    img(src = "wallpaper.jpg", width = "100%", class = "bg")
  )),
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
fluidRow(
   valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
),
fluidRow(
  box(
      h4("Data by Year"),
      selectInput(
        "checkYear",
        "Select Year",
        choices = list("1999", "2001", "2004",
                       "2006", "2007", "2009", "2010", "2012", "2015", "2020", "2021"),
        selected = "2021"
    ),
    title = "Top Scored Animes"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,htmlOutput("topscored", height = "400px")

  )

  ,box(
    title = "Popular Genres"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("commongenre", height = "400px")
  )
)
),
tabItem(tabName = "details",
        h3("Producers' median score by Year", align = "center"),
        br(),
        div(style = "display:vertical-align:center;center-align",
            fluidRow(
              column(
                4,
                selectInput(
                  "detailProducers",
                  label = "Select Producers",
                  choices = unique(recommendation$Producers),
                  selected = "Marvelous",
                  width = 400
                )
              ),
              column(
                4,
                selectInput(
                  "detailGenre",
                  "Select Genre",
                  choices = "",
                  selected = "Action",
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
                         selected = "R - 17+ (violence & profanity)",
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
          status = "primary",
          column(4,htmlOutput("detailTable")),
          column(4, h3("Median Score", align="center"), plotOutput(outputId = "detailPlot", height = "400px", width = "700px"))

        ),

        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),

        sidebarLayout(
          sidebarPanel(
            # ------------------
            # Data overview filters
            # ------------------

            h3("Data Overview"),
            tags$br(),
            setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
            sliderInput(
              "incomeRange",
              label = "Episode Range",
              min = 3,
              max = 1818,
              value = c(3, 1818)
            ),
            setSliderColor(c("e67e22 ", "#e67e22"), c(1, 2)),
            sliderInput(
              "employRange",
              label = "Duration Range(in minute)",
              min = 1,
              max = 50,
              value = c(1, 50)
            ),
            selectInput(
              "checkYearGroup",
              "Select Year",
              choices =unique(recommendation$Year),
              selected = "2018",
              multiple = TRUE
            ),

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
)
)
)


ui <- dashboardPage(header, sidebar, body, skin='blue')



# create the server functions for the dashboard
opts <- list(
  language = list(url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/English.json"),
  pageLength = 30,
  searchHighlight = TRUE,
  orderClasses = TRUE,
  columnDefs = list(list(
    targets = c(1, 6), searchable = FALSE
  ))
)
server <- function(input, output,session) {

  total.revenue <- recommendation %>% summarise(value = mean(Score)) %>% filter(value==max(value))
  sales.account <- recommendation %>% summarise(value = sum(number)) %>% filter(value==max(value))
  prof.prod <- recommendation %>% summarise(value = unique(Genre)) %>% filter(value==max(value))


  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(sales.account$value, format="d", big.mark=' ')
      ,paste('Total number of Anime',sales.account$number)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")


  })

  output$value2 <- renderValueBox({
    valueBox(
      formatC(total.revenue$value, format="d", big.mark=',')
      ,paste('The average score of each out of 10',total.revenue$Score)
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")

  })

  output$value3 <- renderValueBox({
    valueBox(
      formatC(prof.prod$value, format="d", big.mark=',')
      ,paste('The most common genre',prof.prod$Genre)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")

  })


  output$topscored <- renderText({
    recommendation %>%
      filter(Year == input$checkYear) %>%
      group_by(Name) %>%
      select(Name,
             Score,
             Year) %>%
      summarise_all(funs(mean)) %>%
      arrange(desc(Score)) %>%
      kable(
        "html",
        col.names = c(
          "Name",
          "Score",
          "Year"
        )
      ) %>%
      kable_styling(c("striped", "hover"), full_width = T)%>%
      kableExtra::scroll_box(width = "100%", height = "283px")

  })

  # ----------------
  # pie plot section
  # ----------------

  output$commongenre <- renderPlot({
    colmap <-
      c(
        "#bdb2ff",
        "#ffc6ff",
        "#fffffc",
        "#33658A",
        "#3a506b",
        "#577590",
        "#43aa8b",
        "#90be6d",
        "#f9c74f",
        "#f8961e",
        "#f3722c",
        "#f94144",
        "#ffadad",
        "#ffd6a5",
        "#fdffb6",
        "#caffbf",
        "#a8dadc"
      )

    recommendation %>%
      group_by(Genre) %>%
      tally(number) %>%
      ggplot(aes(x = "", y = n, fill = Genre)) +
      geom_bar(
        stat = "identity",
        width = 1,
        color = "black",
        size = 1
      ) +
      theme_void() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 14)) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c(colmap))

  })
  observeEvent(
    input$detailProducers,
    updateSelectInput(
      session,
      "detailGenre",
      "Select Genre",
      choices = unique(recommendation$Genre[recommendation$Producers == input$detailProducers])
    )
  )
  observeEvent(
    input$detailGenre,
    updateSelectInput(
      session,
      "detailRating",
      "Select Rating",
      choices = unique(recommendation$Rating[recommendation$Genre == input$detailGenre &
                                     recommendation$Producers == input$detailProducers])
    )
  )

  detailTB <- eventReactive(input$detailRating,
                            {
                              recommendation %>%
                                filter(
                                    Producers == input$detailProducers &
                                    Genre == input$detailGenre &
                                    Rating == input$detailRating
                                ) %>%
                                select(c(
                                  "Year",
                                  "Score"
                                ))
                            })

  output$detailTable <- renderText({
    input$detailFilter

    isolate({
      detailTB() %>%
        arrange(desc(Year)) %>%
    kable(
      "html",
      col.names = c(
        "Year",
        "Median Score"
      ))%>%
      kable_styling(c("striped", "hover"), full_width = T)%>%
      kableExtra::scroll_box(width = "100%", height = "460px")

  })
})

  output$detailPlot <- renderPlot({
    input$detailFilter

    isolate({
      ggplot(detailTB(), aes(x = Year, y = Score)) +
        geom_smooth(
          mapping = aes(linetype = "r2"),
          method = "lm",
          formula = y ~ x + log(x),
          se = FALSE,
          color = "#bdd5ea",
          linetype = "dashed",
          size = 2,
          alpha = 0.5
        ) +
        geom_line(aes(y = Score),
                  size = 2,
                  color = "#2c3e50") +
        geom_point(
          aes(x = Year, y = Score),
          size = 7,
          shape = 21,
          colour = "white",
          fill = "#fca311",
          stroke = 5
        ) +

        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )

  })

})

  yearGroup <- reactive({
    input$actionDT
    isolate(return(recommendation[recommendation$Year %in% input$checkYearGroup, ]))
  })


  filtered_DT <- reactive({
    input$actionDT
    isolate({
      minEpisode <- input$incomeRange[1]
      maxEpisode <- input$incomeRange[2]
      minDuration <- input$employRange[1]
      maxDuration <- input$employRange[2]
    })

    yearGroup() %>%
      filter(Episodes > minEpisode,
             Episodes < maxEpisode) %>%
      filter(Duration > minDuration,
             Duration < maxDuration) %>%
      select(7, 2, 3, 4, 6, 10)
  })

  # render DT:

  output$myTable <- renderDataTable({
    filtered_DT() %>%
      datatable(
        .,
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = T),
        colnames = c(
          "Year",
          "Name",
          "Score",
          "Genre",
          "Number of Episodes",
          "Duration per episode"
        )
      )

  })
}

shinyApp(ui, server)
