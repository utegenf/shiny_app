library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

library(rsconnect)
library(shinythemes)

##########################################
####   Attaching datasets             ####
##########################################

data <- read.csv("dataset.csv")

## Setting datatables view

opts <- list(
  language = list(url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/English.json"),
  pageLength = 30,
  searchHighlight = TRUE,
  orderClasses = TRUE,
  columnDefs = list(list(
    targets = c(1, 6), searchable = FALSE
  ))
)
server <- function(session, input, output) {
  ################################################
  #### Panel: Main>Summary>Tables & Pie Chart ####
  ################################################
  
  # ----------------
  # Summary section
  # ----------------
  
  output$datahead <- renderPrint({
    data %>%
      filter(release_year == input$checkYear) %>%
      group_by(country) %>%
      select(country,
             female,
             male) %>%
      summarise_all(funs(mean)) %>%
      mutate_if(is.numeric, round, 0) %>%
      arrange(desc(female)) %>%
      kable(
        "html",
        col.names = c(
          "Top 5 countries",
          "Female (Avg %)",
          "Male (Avg %)"
        )
      ) %>%
      kable_styling(c("striped", "hover"), full_width = T)
    
  })
  
