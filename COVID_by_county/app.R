library(shiny)
library(tidyverse)
library(zoo)

#Grab the raw data from the NY Times

counties <- read_csv(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", 
    col_types = cols(
        date = col_date(format = ""),
        county = col_character(),
        state = col_character(),
        fips = col_double(),
        cases = col_double(),
        deaths = col_double()
    )
)

states <- counties %>% select(state) %>% unique() %>% arrange(state)

#Calculate the total number of cases and deaths in the US

usa <- counties %>%
    group_by(date) %>%
    summarise("all_cases" = sum(cases),
              "all_deaths" = sum(deaths)) %>%
    mutate("new_deaths" = all_deaths - lag(all_deaths, default = 0),
           "new_cases" = all_cases - lag(all_cases, default = 0)) %>%
    mutate("weekly_mean_deaths" = rollmean(new_deaths, 7, na.rm = TRUE, 
                                           fill = 0, align = "right")) %>%
    mutate("weekly_mean_cases" = rollmean(new_cases, 7, na.rm = TRUE, 
                                          fill = 0, align = "right"))

usa_deaths <- usa$all_deaths[nrow(usa)]

usa_cases <- usa$all_cases[nrow(usa)]

#Define shared style

# Define UI for application

ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Data by State and County"),
    tags$p("I wanted a better visualization of local COVID-19 data than I've 
    been able to find, particularly in terms of the distribution of cases and 
    deaths as they change over time. The charts below track new cases and deaths 
    in the state and county of your choice, as well as in the United States as a 
    whole."),
    
    tags$p("All data come from the New York Times' ongoing",
           
    tags$a(href = "https://github.com/nytimes/covid-19-data", "repository"),
    "of COVID-19 cases and deaths in the United States."),
    
    tags$p("It's hard to know how to interpret these numbers since there are 
    major known unknowns. Neither confirmed cases nor deaths can be said to be 
    reliable counts of the true numbers."),
    
    tags$p("Data current as of",
    format(Sys.time(), '%B %d, %Y')),
    tags$hr(),

    # Sidebar with dropdown menus to choose state and county 
    
    sidebarLayout(
        sidebarPanel(
            selectInput("state",
                        label = "State:",
                        choices = states$state,
                        selected = "Georgia"
            ),
            uiOutput("state_counties"),
            tags$p(textOutput("text_data")),
            tags$p("In the United States, there have been",
                   usa_deaths,
                   "deaths and",
                   usa_cases,
                   "cases."),
            tags$img(width = "100%",
                     height = "auto",
                     src = "23312.jpg")
            ),

        # Show a plot of the generated cases
        mainPanel(
           tags$h3(textOutput("county")),
           plotOutput("county_cases"),
           plotOutput("county_deaths"),
           tags$h3(textOutput("state")),
           plotOutput("state_cases"),
           plotOutput("state_deaths"),
           tags$h3("United States"),
           plotOutput("usa_cases"),
           plotOutput("usa_deaths")
        )
    )
)

# Define server logic required to render plot

server <- function(input, output) {

    #create plots for county
    
    output$county_cases <- renderPlot({
        counties %>%
            filter(state == input$state) %>%
            filter(county == input$county) %>%
            group_by(date) %>%
            summarise("all_cases" = sum(cases)) %>%
            mutate("new_cases" = all_cases - lag(all_cases, default = 0)) %>%
            mutate("weekly_mean_cases" = rollmean(new_cases, 7, na.rm = TRUE, 
                                                  fill = 0, align = "right")) %>%
            
            ggplot() +
            geom_col(mapping = aes(x = date, y = new_cases, 
                                   fill = "new daily cases")) +
            geom_line(mapping = aes(x = date, y = weekly_mean_cases,
                                    color = "7-day average")) +
            labs(title = paste("New Cases in", input$county),
                 caption = "Data from The New York Times",
                 x = "Date",
                 y = "New Cases") +
            scale_color_manual(
                values = c("7-day average" = "#800014")) +
            scale_fill_manual(
                values= c("new daily cases" = "gray40")) +
            theme(legend.title = element_blank(),
                  legend.position = "top") +
            scale_x_date(limits = as.Date(c("2020-03-01", Sys.time())))
        
    })
    
    output$county_deaths <- renderPlot({
        counties %>%
            filter(state == input$state) %>%
            filter(county == input$county) %>%
            group_by(date) %>%
            summarise("all_deaths" = sum(deaths)) %>%
            mutate("new_deaths" = all_deaths - lag(all_deaths, default = 0)) %>%
            mutate("weekly_mean_deaths" = rollmean(new_deaths, 7, na.rm = TRUE, 
                                                   fill = 0, align = "right")) %>%
            ggplot() +
            geom_col(mapping = aes(x = date, y = new_deaths, 
                                   fill = "new daily deaths")) +
            geom_line(mapping = aes(x = date, y = weekly_mean_deaths,
                                    color = "7-day average")) +
            labs(title = paste("New Deaths in", input$county),
                 caption = "Data from The New York Times",
                 x = "Date",
                 y = "New Deaths") +
            scale_color_manual(
                values = c("7-day average" = "gray40")) +
            scale_fill_manual(
                values= c("new daily deaths" = "#800014")) +
            theme(legend.title = element_blank(),
                  legend.position = "top") +
            scale_x_date(limits = as.Date(c("2020-03-01", Sys.time())))
        
    })    
    
    
#Create plots for state        
    
    output$state_cases <- renderPlot({
        counties %>%
            filter(state == input$state) %>%
            group_by(date) %>%
            summarise("all_cases" = sum(cases)) %>%
            mutate("new_cases" = all_cases - lag(all_cases, default = 0)) %>%
            mutate("weekly_mean_cases" = rollmean(new_cases, 7, na.rm = TRUE, 
                                                  fill = 0, align = "right")) %>%
            ggplot() +
            geom_col(mapping = aes(x = date, y = new_cases,
                                   fill = "new daily cases")) +
            geom_line(mapping = aes(x = date, y = weekly_mean_cases, 
                                    color = "7-day average")) +
            labs(title = paste("New Cases in", input$state),
                 caption = "Data from The New York Times",
                 x = "Date",
                 y = "New Cases") +
            scale_color_manual(
                values = c("7-day average" = "#800014")) +
            scale_fill_manual(
                values= c("new daily cases" = "gray40")) +
            theme(legend.title = element_blank(),
                  legend.position = "top") +
            scale_x_date(limits = as.Date(c("2020-03-01", Sys.time())))
    })
    
    output$state_deaths <- renderPlot({
        counties %>%
            filter(state == input$state) %>%
            group_by(date) %>%
            summarise("all_deaths" = sum(deaths)) %>%
            mutate("new_deaths" = all_deaths - lag(all_deaths, default = 0)) %>%
            mutate("weekly_mean_deaths" = rollmean(new_deaths, 7, na.rm = TRUE, 
                                                   fill = 0, align = "right")) %>%
        ggplot() +
            geom_col(mapping = aes(x = date, y = new_deaths, 
                                   fill = "new daily deaths")) +
            geom_line(mapping = aes(x = date, y = weekly_mean_deaths,
                                    color = "7-day average")) +
            labs(title = paste("New Deaths in", input$state),
                 caption = "Data from The New York Times",
                 x = "Date",
                 y = "New Deaths") +
            scale_color_manual(
                values = c("7-day average" = "gray40")) +
            scale_fill_manual(
                values= c("new daily deaths" = "#800014")) +
            theme(legend.title = element_blank(),
                  legend.position = "top") +
            scale_x_date(limits = as.Date(c("2020-03-01", Sys.time())))
    })
    

    #create plots for USA
    
    output$usa_cases <- renderPlot({
        ggplot(usa) +
            geom_col(mapping = aes(x = date, y = new_cases, 
                                   fill = "new daily cases")) +
            geom_line(mapping = aes(x = date, y = weekly_mean_cases,
                                    color = "7-day average")) +
            labs(title = "New Cases in the United States",
                 caption = "Data from The New York Times",
                 x = "Date",
                 y = "New Cases") +
            scale_color_manual(
                values = c("7-day average" = "#800014")) +
            scale_fill_manual(
                values= c("new daily cases" = "gray40")) +
            theme(legend.title = element_blank(),
                  legend.position = "top") +
            scale_x_date(limits = as.Date(c("2020-03-01", Sys.time())))
    })
    
    output$usa_deaths <- renderPlot({
        ggplot(usa) +
            geom_col(mapping = aes(x = date, y = new_deaths, 
                                   fill = "new daily deaths")) +
            geom_line(mapping = aes(x = date, y = weekly_mean_deaths,
                                    color = "7-day average")) +
            labs(title = "New Deaths in the United States",
                 caption = "Data from The New York Times",
                 x = "Date",
                 y = "New Deaths") +
            scale_color_manual(
                values = c("7-day average" = "gray40")) +
            scale_fill_manual(
                values= c("new daily deaths" = "#800014")) +
            theme(legend.title = element_blank(),
                  legend.position = "top") +
            scale_x_date(limits = as.Date(c("2020-03-01", Sys.time())))
    })
    
    
    #create reactive county dropdown menu
    
    output$state_counties <- renderUI({
        counties_by_state <- counties %>%
            filter(state == input$state) %>%
            arrange(county)
        selectInput("county",
                    label = "County:",
                    choices = counties_by_state$county,
                    selected = 1
        )
    })
    
    
    #Create text output about total values
    
    output$text_data <- renderText({
        current_state <- counties %>% 
            filter(state == input$state)
        current_state <- current_state %>% 
            group_by(date) %>% 
            summarise(sum(deaths))
        total_state_deaths <- current_state$`sum(deaths)`[nrow(current_state)]
        
        current_state <- counties %>% 
            filter(state == input$state)
        current_state <- current_state %>% 
            group_by(date) %>% 
            summarise(sum(cases))
        total_state_cases <- current_state$`sum(cases)`[nrow(current_state)]
        
        current_county <- counties %>%
            filter(state == input$state & county == input$county)
        current_county <- current_county %>%
            group_by(date) %>%
            summarise(sum(deaths))
        total_county_deaths <- current_county$`sum(deaths)`[nrow(current_county)]
        
        current_county <- counties %>%
            filter(state == input$state & county == input$county)
        current_county <- current_county %>%
            group_by(date) %>%
            summarise(sum(cases))
        total_county_cases <- current_county$`sum(cases)`[nrow(current_county)]
        
        paste("There have been ",
              total_county_deaths,
              " deaths out of ",
              total_county_cases,
              " cases in ",
              input$county,
              ". ",
              input$state,
              " has seen ",    
              total_state_deaths,
              " deaths out of ",
              total_state_cases,
              " cases.",
              sep = "")
    })
    output$state <- renderText(input$state)
    output$county <-renderText(input$county)    
        
}

# Run the application 
shinyApp(ui = ui, server = server)
