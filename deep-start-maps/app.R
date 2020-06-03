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
                titlePanel(""),

    # Application title
        column(2,
               imageOutput("logo", width = "100px", height="100px"),
               br(),
               br(),
               sidebarPanel(width=12, 
               h5("This map shows major sites in the American and Russian nuclear weapons complexes, including military sites with deployed weapons as well as storage and production sites. Hover over a specific site for more information about it. Use the options below to select which sites are displayed on the map."),
               br(), 
               checkboxGroupInput("strana",
                               "Country",
                               choices = c("Russia", "United States"),
                               selected = c("Russia", "United States")), 
                               br(),
            checkboxGroupInput("type", 
                                "Weapon type", 
                               choices = c("NSNW", "Strategic"), 
                               selected = c("NSNW", "Strategic")
            ))
        ),
        
        column(10,
               br(), 
            plotlyOutput("plot1", height="800px"),
        ), 
    br(), 
    column(12, 
           offset = 3, 
           h5("Sources:"),  
           h5("Hans M. Kristensen & Robert S. Norris (2017) Worldwide deployments of nuclear weapons, 
            2017, Bulletin of the Atomic Scientists, 73:5, 289-297,"), 
           h5("Hans M. Kristensen & Matt Korda (2020) Russian nuclear forces, 2020, 
            Bulletin of the Atomic Scientists, 76:2, 102-117,"),
           h5("The CSIS Missile Threat Website"))
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
                          '</br> Warhead: ', warhead, 
                          '</br> Warhead type: ', type), 
             marker = list(color = 'rgb(0,0,0)', line = list(color = 'rgb(0,0,0)')), symbol = ~status, size = I(14), hoverinfo = "text"
        ) %>% 
            layout(
                title = list(text ="Deep START exercise: U.S. and Russian Nuclear Weapons Deployment", 
                             y =0.9, x = 0.55),
                titlefont = list(
                    family = "Segoe UI",
                    size = 18,
                    color = 'rgb(255,255,255'), 
                geo = g, 
                legend = list(x = 0.8,
                              y = 0.2, 
                              bgcolor = 'rgb(255, 255, 255)', 
                              bordercolor = 'rgb(0,0,0)', 
                              borderwidth = 2), 
                paper_bgcolor='rgb(34,34,34)')
    })
    
    output$logo <- renderImage({
        
        # Use renderImage, assign it to output$image,
        # which corresponds with the imageOutput("Image") above.
        
        list(
            src = "ACONA.png",
            contentType = "image/png", 
            width = 96, 
            height = 72
        )
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
