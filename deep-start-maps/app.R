#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(janitor)
library(plotly)
library(shinythemes)
library(stringr)
library(tidyverse)

location_data <- read_csv("USRUS nuclear weapons info.csv", col_types = cols(
    Country = col_character(),
    `Base/location` = col_character(),
    Region = col_character(),
    `Weapon system` = col_character(),
    Remarks = col_character(),
    Type = col_character(),
    Lat = col_double(),
    Long = col_double()
)) %>% clean_names

location_data <- location_data %>% mutate(side = if_else(country == "Russia", "Russia", "United States"))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),

    # Application title
    titlePanel("Arms Control Negotiation Academy"),
        sidebarPanel(width = 2,
                     h5("This map shows major sites in the American and Russian nuclear weapons complexes, including military sites with deployed weapons as well as storage and production sites. Hover over a specific site for more information about it. Use the options below to select which sites are displayed on the map."),
            checkboxGroupInput("strana",
                               "Display",
                               choices = c("Russia", "United States"),
                               selected = c("Russia", "United States")), 
                               br(),
            checkboxGroupInput("type", 
                               "Display", 
                               choices = c("NSNW", "Strategic"), 
                               selected = c("NSNW", "Strategic")
            )
        ),
        
        # add a sidebar panel with multiple checkboxes that the viewer can
        # click to see multiple variables showed
        
        mainPanel(
            plotlyOutput("plot1")
            
            # display the plot (created in the server)
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot1 <- renderPlotly({
        
        g <- list(
            scope = 'world',
            projection = list(type = 'equirectangular'),
            showland = TRUE,
            showcountries = TRUE,
            showocean = TRUE,
            showcoastlines = TRUE,
            showsubunits =TRUE,
            coastlinecolor = toRGB("dimgray"),
            oceancolor = toRGB("lightsteelblue2"),
            landcolor = toRGB("gray95"),
            countrycolor = toRGB("gray85"),
            subunitcolor = toRGB("gray85"),
            countrywidth = 0.5,
            subunitwidth =0.5)
        
        m <- list(
            l=10,
            r=10,
            b=10,
            t=0,
            pad=4
        )
        
        
        location_data %>% 
            filter(side %in% c(input$strana)) %>%
            filter(str_detect(type, paste(c(input$type), collapse="|")) == TRUE) %>% 
        plot_geo(lat = ~lat, lon = ~long, autosize = F, width = 1200, height = 800, margin = m) %>% 
            add_markers(
            text = ~paste('</br> Name: ', base_location,
                          '</br> Delivery System: ', delivery_system,
                          '</br> Weapon: ', warhead), 
             marker = list(color = 'rgb(0,0,0)', line = list(color = 'rgb(0,0,0)')), symbol = ~status, size = I(14), hoverinfo = "text"
        ) %>% 
            layout(
                geo = g, 
                legend = list(x = 0.8, 
                              y = 0.2, 
                              bgcolor = 'rgb(255, 255, 255)', 
                              bordercolor = 'rgb(0,0,0)', 
                              borderwidth = 2), 
                paper_bgcolor='rgb(34,34,34)')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
