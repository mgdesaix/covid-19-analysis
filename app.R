library(shiny)
library(tidyverse)
library(urbnmapr)
library(gridExtra)

#######################################################################################
################      Get Initial Data     ###########################################
#######################################################################################

# Get COVID data from NYT's github page
county.url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
covid.counties <- read_csv(url(county.url))

state.url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
covid.states <- read_csv(url(state.url))

# Get county data from urbnmapr
counties <- urbnmapr::counties %>%
  mutate(fips = county_fips)

# Get state data from urbnmapr
states <- urbnmapr::states %>%
  mutate(fips = state_fips)


# State polygons

full.covid.states <- covid.states %>%
  filter(date == max(covid.states$date)) %>%
  right_join(states, by = "fips") %>%
  mutate(log_cases = log10(cases))

# County polygons

full.covid.counties <- covid.counties %>%
  filter(date == max(covid.counties$date)) %>%
  right_join(counties, by = "fips")

# U.S. summary
daily.sum <- covid.states %>%
  group_by(date) %>%
  summarise(fullcases = sum(cases),
            fulldeaths = sum(deaths))

# U.S. new per day

daily.new <- data.frame(
  "date" = daily.sum$date[2:nrow(daily.sum)],
  "fullcases" = daily.sum[2:nrow(daily.sum),2] - daily.sum[1:(nrow(daily.sum)-1), 2],
  "fulldeaths" = daily.sum[2:nrow(daily.sum),3] - daily.sum[1:(nrow(daily.sum)-1), 3]
)
  
total.cases <- daily.sum$fullcases[nrow(daily.sum)]
total.deaths <- daily.sum$fulldeaths[nrow(daily.sum)]
percent.deaths <- round(total.deaths / total.cases * 100, 3) 
today <- max(covid.states$date)

#######################################################################################

ui <- fluidPage(
  titlePanel("Spread of Coronavirus"),
      
      tabsetPanel(type = "tabs",
                  tabPanel("Main Page",
                           sidebarLayout(
                             
                             # Sidebar panel for inputs
                             sidebarPanel(
                               width = 2,
                               
                               # Input: Slider for day
                               sliderInput(inputId = "date",
                                           label = "Date:",
                                           min = min(covid.states$date),
                                           max = max(covid.states$date),
                                           value = max(covid.states$date))
                               ),
                             mainPanel(
                               h3("Coronavirus Data Visualization"),
                               textOutput("introduction"),
                               uiOutput("github_url"),
                               uiOutput("data_url"),
                               h3("Cases by U.S. Counties"),
                               plotOutput(outputId = "USmap"),
                               h3("Cumulative Number of Cases and Deaths"),
                               plotOutput(outputId = "USgraphs_cumul"),
                               h3("Daily Addition of Cases and Deaths"),
                               plotOutput(outputId = "USgraphs_day")
                             )
                             )
                           ),
                  tabPanel("State Analyses",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("state", "States:",
                                           choices = unique(full.covid.states$state)),
                               # Input: Slider for day
                               sliderInput(inputId = "date2",
                                           label = "Date:",
                                           min = min(covid.states$date),
                                           max = max(covid.states$date),
                                           value = max(covid.states$date))
                             ),
                             mainPanel(
                               plotOutput(outputId = "Statemap"),
                               plotOutput(outputId = "Stategraphs")
                             )
                           ))
      ) # close tabsetPanel
) # close fluidPage


server <- function(input, output) {
  data_url <- a("Click here for NYT data", href = "https://github.com/nytimes/covid-19-data")
  output$data_url <- renderUI({
    tagList(data_url)
  })
  
  github_url <- a("Click here for code", href = "https://github.com/mgdesaix/covid-19-analysis")
  output$github_url <- renderUI({
    tagList(github_url)
  })
  
  output$introduction <- renderText({
    paste("Here are some basic visualizations of data on coronavirus cases in the United States. There have been", 
          total.cases, "cases and ", total.deaths, "deaths as of ", today, ". Hopefully I will find time to update this with additional meaninful analyses.
          On this page you will find summaries of data for the entire country and you can explore state-by-state summaries on the following tab.
          I would like to point out that I have provided raw numbers, as well as log-transformed (base 10) values as well (i.e. log10(10) = 1 and log10(1000) = 3).
          I find the log-transformed data provides a meaningful comparison when looking at exponential growth between vastly differing quantities, as is the case
          in the number of reported cases and deaths.  Feel free to contact me at mgdesaix@gmail.com if you have any questions or would like to see specific analyses.
          You can find the code at my github page and the data are provided by the New York Times." )
  })
  
  output$USmap <- renderPlot({
    
    slider.date <- input$date
    
    # This is the geom_point() data for county cases
    county_location_cases <- covid.counties %>%
      filter(date == slider.date) %>%
      right_join(counties, by = "fips") %>%
      mutate(log_cases = log10(cases)) %>%
      mutate(log_casesFull = replace_na(log_cases, 0)) %>%
      group_by(fips) %>%
      summarise(mean_cases = mean(cases),
                mean_logcases = mean(log_casesFull),
                county_lat = mean(c(max(lat), min(lat))), 
                county_long = mean(c(max(long), min(long))),
                state = unique(state)) %>%
      filter(mean_cases != "NA")
    
    
    size_breaks <- c(1,10,50, 100, 500, 1000)
    
    full.covid.states %>%
      ggplot() +
      geom_polygon(aes(long, lat, group = group), 
                   fill="gray95", 
                   color = "gray70",
                   size = 0.1) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      geom_point(data = county_location_cases, 
                 aes(x = county_long, y = county_lat, size = mean_cases), 
                 color = "red4", 
                 fill = adjustcolor("red1", alpha.f = 0.25),
                 shape = 21) +
      scale_size(breaks = size_breaks, 
                 range = c(.5,25),
                 name = "Cases") +
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.4)
      )
  })
  
  output$USgraphs_cumul <- renderPlot({
    
    p1 <- daily.sum %>%
      ggplot() +
      geom_point(aes(x = date, y = fullcases, color = "Cases")) +
      geom_point(aes(x = date, y = fulldeaths, color = "Deaths")) +
      xlab("Date") +
      ylab("Cases and Deaths") +
      theme_bw() +
      theme(legend.position = "none")
    
    p2 <- daily.sum %>%
      ggplot() +
      geom_point(aes(x = date, y = log10(fullcases), color = "Cases")) +
      geom_point(aes(x = date, y = log10(fulldeaths), color = "Deaths")) +
      xlab("Date") +
      ylab("Cases and Deaths (Log10)") +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.position = "right")
    
    grid.arrange(p1, p2, ncol = 2)
    
  })
  
  output$USgraphs_day <- renderPlot({
    
    p1 <- daily.new %>%
      ggplot() +
      geom_point(aes(x = date, y = fullcases, color = "Cases")) +
      geom_point(aes(x = date, y = fulldeaths, color = "Deaths")) +
      xlab("Date") +
      ylab("Cases and Deaths") +
      theme_bw() +
      theme(legend.position = "none")
    
    p2 <- daily.new %>%
      ggplot() +
      geom_point(aes(x = date, y = log10(fullcases), color = "Cases")) +
      geom_point(aes(x = date, y = log10(fulldeaths), color = "Deaths")) +
      xlab("Date") +
      ylab("Cases and Deaths (Log10)") +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.position = "right")
    
    grid.arrange(p1, p2, ncol = 2)
    
  })
  
  output$Statemap <- renderPlot({
    
    slider.date <- input$date2
    input.state <- input$state
    
    # This is the geom_point() data for county cases
    county_location_cases <- covid.counties %>%
      filter(date == slider.date,
             state == input.state) %>%
      right_join(counties, by = "fips") %>%
      mutate(log_cases = log10(cases)) %>%
      mutate(log_casesFull = replace_na(log_cases, 0)) %>%
      group_by(fips) %>%
      summarise(mean_cases = mean(cases),
                mean_logcases = mean(log_casesFull),
                county_lat = mean(c(max(lat), min(lat))), 
                county_long = mean(c(max(long), min(long))),
                state = unique(state)) %>%
      filter(mean_cases != "NA")
    
    
    size_breaks <- c(1,10,50, 100, 500, 1000)
    
    full.covid.counties %>%
      filter(state_name == input.state) %>%
      ggplot() +
      geom_polygon(aes(long, lat, group = group), 
                   fill="gray95", 
                   color = "gray70",
                   size = 0.1) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      geom_point(data = county_location_cases, 
                 aes(x = county_long, y = county_lat, size = mean_cases), 
                 color = "red4", 
                 fill = adjustcolor("red1", alpha.f = 0.25),
                 shape = 21) +
      scale_size(breaks = size_breaks, 
                 range = c(.5,25),
                 name = "Cases") +
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "right"
      )
  })
  
  output$Stategraphs <- renderPlot({
    
    input.state <- input$state
    
    # State summary
    state.sum <- covid.counties %>%
      filter(state == input.state) %>%
      group_by(date) %>%
      summarise(fullcases = sum(cases),
                fulldeaths = sum(deaths))
      
    
    p1 <- state.sum %>%
      ggplot() +
      geom_point(aes(x = date, y = fullcases, color = "Cases")) +
      geom_point(aes(x = date, y = fulldeaths, color = "Deaths")) +
      xlab("Date") +
      ylab("Cases and Deaths") +
      theme_bw() +
      theme(legend.position = "none")
    
    p2 <- state.sum %>%
      ggplot() +
      geom_point(aes(x = date, y = log10(fullcases), color = "Cases")) +
      geom_point(aes(x = date, y = log10(fulldeaths), color = "Deaths")) +
      xlab("Date") +
      ylab("Cases and Deaths (Log10)") +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.position = "right")
    
    grid.arrange(p1, p2, ncol = 2)
    
  })
}

shinyApp(ui = ui, server = server)














