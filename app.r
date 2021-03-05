library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(here)
options(scipen = 999)

#Add dataframe named gap.csv#
gap <- read.csv(here("gap.csv"), stringsAsFactors = FALSE)
yrrange <- seq(1952,2007,5)

ui <- fluidPage(titlePanel('Countries- Timeline of GDP per Capita vs Average Life Expectancy'),
                sidebarLayout(
                  sidebarPanel(
                    fluidRow(
                      radioButtons("mode", "Select Mode:", choices = list("Exploding Bubbles" ="bubbles", "Trail" = "trail" ))
                    ),
                    fluidRow(
                      htmlOutput("instructions")
                    ),
                    fluidRow(
                      selectInput("xaxis", "Select X-Axis parameter:", c("Life Expectancy" = "lifeExp","Population" = "pop","Gdp Per Capita"="gdpPercap"))
                    ),
                    fluidRow(
                      selectInput("yaxis", "Select Y-Axis parameter:", c("Life Expectancy" = "lifeExp","Population" = "pop","Gdp Per Capita"="gdpPercap"), selected = "gdpPercap")
                    ),
                    fluidRow(
                    selectInput("bsize", "Select Bubble size parameter:", c("Life Expectancy" = "lifeExp","Population" = "pop","Gdp Per Capita"="gdpPercap"), selected = "pop")
                    ),
                    fluidRow(
                      sliderInput("xchange", "Change in X axis:", min=0.2, max= 2, step = 0.2, value = 1)
                    ),
                    fluidRow(
                      sliderInput("ychange", "Change in Y axis:", min=0.2, max= 2, step = 0.2, value = 1 )
                    ),
                    fluidRow(
                      selectInput("yearchange", "Apply change from:", choices = yrrange)
                    )
                  ),
                  mainPanel(
                    #output animated plot
                    fluidRow(
                      column(width = 10, plotlyOutput("coolplot", height = 750))
                    )
                  )
                )
              )

server <- function(input, output, session) {

   gap0 <- reactive({
     gaptemp <- gap
     xax <- input$xaxis
     xch <- as.numeric(input$xchange)
     yearch <- as.numeric(input$yearchange)
     gaptemp[as.numeric(gaptemp$year) >= yearch , xax] <- gaptemp[as.numeric(gaptemp$year) >= yearch , xax ] * xch
     yax <- input$yaxis
     ych <- as.numeric(input$ychange)
     gaptemp[as.numeric(gaptemp$year) >= yearch , yax] <- gaptemp[as.numeric(gaptemp$year) >= yearch , yax ] * ych
     return(gaptemp)
   })
  output$coolplot <- renderPlotly({
    #show only continents
    if(input$mode  == "bubbles") {
      if(is.null(event_data("plotly_click"))) { 
        gap1 <- filter(gap0(), is.na(country))
        p <- ggplot(gap1, aes(get(input$xaxis), get(input$yaxis) , color = continent)) +
        geom_point(aes(size = get(input$bsize), frame = year, ids = continent)) +
        scale_x_log10()
        p <- ggplotly(p + labs(x =
                                 if(input$xaxis=="pop") {
                                   "Population"
                                 } else if(input$xaxis=="lifeExp") {
                                   "Average Life Expectancy"
                                 } else {
                                   "GDP Per Capita (USD)"
                                 },
                               y =
                                 if(input$yaxis=="pop") {
                                   "Population"
                                 } else if(input$yaxis=="lifeExp") {
                                   "Average Life Expectancy"
                                 } else {
                                   "GDP Per Capita (USD)"
                                 }
                              )
                      )
      }
      else {
        #show countries when clicked
        d <- event_data("plotly_click")
        click.cont <- switch(d$curveNumber+1, "Africa" , "Americas" , "Asia" , "Europe" , "Oceania")
        gap2 <- filter(gap0(), continent %in% click.cont)
        gap2 <- rbind(gap2, filter(gap0(), is.na(country)))
        p <- ggplot(gap2, aes(get(input$xaxis), get(input$yaxis), color = continent)) +
          geom_point(aes(size = get(input$bsize), frame = year, ids = country)) +
          scale_x_log10()
        p <- ggplotly(p + labs(x =
                                 if(input$xaxis=="pop") {
                                   "Population"
                                 } else if(input$xaxis=="lifeExp") {
                                   "Average Life Expectancy"
                                 } else {
                                   "GDP Per Capita (USD)"
                                 },
                               y =
                                 if(input$yaxis=="pop") {
                                   "Population"
                                 } else if(input$yaxis=="lifeExp") {
                                   "Average Life Expectancy"
                                 } else {
                                   "GDP Per Capita (USD)"
                                 }
                              )
                  )
      }
    }
    else {
      gap3 <- filter(gap0(), is.na(country))
      
      accumulate_by <- function(dat, var) {
        var <- lazyeval::f_eval(var, dat)
        lvls <- plotly:::getLevels(var)
        dats <- lapply(seq_along(lvls), function(x) {
          cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
        })
        dplyr::bind_rows(dats)
      }
      gap3 <- gap3 %>%
        accumulate_by(~year)
      
      p <- ggplot(gap3, aes(get(input$xaxis), get(input$yaxis), , colour = continent)) + 
        geom_line(aes(group = continent,  ids = continent, frame = frame, year = year)) +
        geom_point(aes(ids = continent, size = get(input$bsize), frame = year)) +
        scale_x_log10()
      p <- ggplotly(p + labs(x =
                               if(input$xaxis=="pop") {
                                 "Population"
                               } else if(input$xaxis=="lifeExp") {
                                 "Average Life Expectancy"
                               } else {
                                 "GDP Per Capita (USD)"
                               },
                             y =
                               if(input$yaxis=="pop") {
                                 "Population"
                               } else if(input$yaxis=="lifeExp") {
                                 "Average Life Expectancy"
                               } else {
                                 "GDP Per Capita (USD)"
                               }
                            )
                  )
    }
  })
  #print text for info
  output$instructions <- renderUI({
    if(input$mode == "bubbles")
      HTML(paste("Single click on a continent to get corresponding countries <br/>" , "Double click empty space to return graph to default <br/> <br/> <br/>"))
    else
      HTML(paste("<br/> <br/> <br/>"))
    })
}  
shinyApp(ui = ui, server = server)

#reference -- https://github.com/ropensci/plotly/issues/957
#reference -- https://plot.ly/r/cumulative-animations/