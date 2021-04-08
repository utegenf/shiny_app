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
      group_by(score) %>%
      select(name,
             score,
             genre) %>%
      summarise_all(funs(mean)) %>%
      mutate_if(is.numeric, round, 0) %>%
      arrange(desc(score)) %>%
      kable(
        "html",
        col.names = c(
          "Top scored animes",
          "Average score out of 10",
          "Type of genre"
        )
      ) %>%
      kable_styling(c("striped", "hover"), full_width = T)
    
  })
  
  output$piePlot <- renderPlot({
    colmap <-
      c(
        "#bdb2ff",
        # NUS
        "#ffc6ff",
        # NTU
        "#fffffc",
        # SMU
        "#33658A",
        # SIT
        "#3a506b",
        # SUTD
        "#577590",
        # SUSS
        "#43aa8b",
        # NIE
        "#90be6d",
        # SP
        "#f9c74f",
        # NP
        "#f8961e",
        # TP
        "#f3722c",
        # NAYANG POLY
        "#f94144",
        # RP
        "#ffadad",
        # NAFA DEG
        "#ffd6a5",
        # LAS DEG
        "#fdffb6",
        # NAFA DIP
        "#caffbf",
        # NAFA DEG
        "#a8dadc"  # ITE
      )
    
    data %>%
      filter(year == input$checkYear) %>%
      group_by(animes) %>%
      tally(genre) %>%
      ggplot(aes(x = "", y = n, fill = genre)) +
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
      scale_fill_manual(values = c(colmap)) +
      labs(title = "Most common genres")
    
  })
  
  observeEvent(
    input$detailProducers,
    updateSelectInput(
      session,
      "detailProducers",
      "Select School",
      choices = unique(data$school[data$university == input$detailUniversity])
    )
  )
  observeEvent(
    input$detailSchool,
    updateSelectInput(
      session,
      "detailMajor",
      "Select Program",
      choices = unique(data$degree[data$school == input$detailSchool &
                                     data$university == input$detailUniversity])
    )
  )
  
  detailTB <- eventReactive(input$detailMajor,
                            {
                              data %>%
                                filter(
                                  school == input$detailSchool &
                                    university == input$detailUniversity &
                                    degree == input$detailMajor
                                ) %>%
                                select(c(
                                  "year",
                                  "basic_monthly_median",
                                  "employment_rate_ft_perm"
                                ))
                              
                            })
  
  output$detailTable <- renderPrint({
    input$detailFilter
    
    isolate({
      detailTB() %>%
        data.frame() %>%
        kable("html",
              col.names = c("Year", "Median Montly Income",
                            "Fulltime Employment Rate")) %>%
        kable_styling(c("striped", "hover"), full_width = F)
    })
  })
  
  # median income plot:
  
  output$detailPlot <- renderPlot({
    input$detailFilter
    
    isolate({
      ggplot(detailTB(), aes(x = year, y = basic_monthly_median)) +
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
        geom_line(aes(y = basic_monthly_median),
                  size = 2,
                  color = "#2c3e50") +
        geom_point(
          aes(x = year, y = basic_monthly_median),
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
  
  output$detailPlotem <- renderPlot({
    input$detailFilter
    
    isolate({
      ggplot(detailTB(), aes(x = year, y = employment_rate_ft_perm)) +
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
        geom_line(aes(y = employment_rate_ft_perm),
                  size = 2,
                  color = "#2c3e50") +
        geom_point(
          aes(x = year, y = employment_rate_ft_perm),
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
  
  ################################################
  #### Panel: Documentation                   ####
  ################################################
  
  getPageDoc <- function() {
    return(includeHTML("gesrmarkdown.html"))
  }
  output$doc <- renderUI({
    getPageDoc()
  })
  
  
  ################################################
  #### Panel: About                           ####
  ################################################
  
  getPageAbo <- function() {
    return(includeHTML("about.html"))
  }
  output$abo <- renderUI({
    getPageAbo()
  })
  
  
}
  
