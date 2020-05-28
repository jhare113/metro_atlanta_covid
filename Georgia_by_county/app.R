library(shiny)
library(tidyverse)

#Grab the raw data from the NY Times

counties <- read_csv(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", 
    col_types = cols(
        date = col_date(format = ""),
        county = col_character(),
        state = col_character(),
        fips = col_character(),
        cases = col_double(),
        deaths = col_double()
    ))
#Filter down to Georgia counties

Georgia <- counties %>%
    filter(state == "Georgia") %>%
    arrange(county)
    

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Georgia COVID-19 Data by County"),

    # Sidebar with dropdown to choose county 
    sidebarLayout(
        sidebarPanel(
            selectInput("county",
                        label = "County:",
                        choices = Georgia$county
                        )
        ),

        # Show a plot of the generated cases
        mainPanel(
           plotOutput("cases"),
           plotOutput("deaths")
        )
    )
)

# Define server logic required to render plot
server <- function(input, output) {

    output$cases <- renderPlot({
        Georgia %>%
            filter(county == input$county) %>%
        ggplot(mapping = aes(x = date, y = cases)) +
            geom_point() +
            geom_line()+
            labs(title = "Total Cases in County",
                 caption = "Data from The New York Times",
                 x = "Date",
                 y = "Total Cases")
    })
    output$deaths <- renderPlot({
        Georgia %>%
            filter(county == input$county) %>%
            ggplot(mapping = aes(x = date, y = deaths)) +
            geom_point() +
            geom_line()+
            labs(title = "Total Deaths in County",
                 caption = "Data from The New York Times",
                 x = "Date",
                 y = "Total Deaths")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
