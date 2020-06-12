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

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Data by State and County"),

    # Sidebar with dropdown menus to choose state and county 
    sidebarLayout(
        sidebarPanel(
            selectInput("state",
                        label = "State:",
                        choices = states$state,
                        selected = "Georgia"
            ),
            uiOutput("state_counties")
            ),

        # Show a plot of the generated cases
        mainPanel(
           plotOutput("state_cases"),
           plotOutput("state_deaths"),
           plotOutput("county_cases"),
           plotOutput("county_deaths")
        )
    )
)

# Define server logic required to render plot
server <- function(input, output) {

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
            geom_col(mapping = aes(x = date, y = new_cases)) +
            geom_line(mapping = aes(x = date, y = weekly_mean_cases)) +
            labs(title = "New Cases in State",
                 caption = "Data from The New York Times",
                 x = "Date",
                 y = "New Cases")
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
            geom_col(mapping = aes(x = date, y = new_deaths)) +
            geom_line(mapping = aes(x = date, y = weekly_mean_deaths)) +
            labs(title = "New Deaths in State",
                 caption = "Data from The New York Times",
                 x = "Date",
                 y = "New Deaths")
    })
    
#create reactive county dropdown menu
        
    output$state_counties <- renderUI({
        counties_by_state <- counties %>%
            filter(state == input$state) %>%
            arrange(county)
        selectInput("county",
                    label = "County:",
                    choices = counties_by_state$county
        )
    })

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
            geom_col(mapping = aes(x = date, y = new_cases)) +
            geom_line(mapping = aes(x = date, y = weekly_mean_cases)) +
            labs(title = "New Cases in County",
                 caption = "Data from The New York Times",
                 x = "Date",
                 y = "New Cases")
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
            geom_col(mapping = aes(x = date, y = new_deaths)) +
            geom_line(mapping = aes(x = date, y = weekly_mean_deaths)) +
            labs(title = "New Deaths in County",
                 caption = "Data from The New York Times",
                 x = "Date",
                 y = "New Deaths")
    })    
        
}

# Run the application 
shinyApp(ui = ui, server = server)
