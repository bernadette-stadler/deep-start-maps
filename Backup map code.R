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
library(sf)
library(rgeos)
library(ggplot2)
library(shinythemes)
library(stringr)
library(rnaturalearth)
library(rnaturalearthdata)
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
)) %>%
  clean_names()

# read in data, clean it up  

world <- ne_countries(returnclass = "sf")

# Save map as an r object. I'm doing it here instead of in the server in the hope that this
# speeds up the app.

location_data <- location_data %>%
  mutate(side = if_else(country == "Russia", "Russia", "United States"))

# mutate to specify side as U.S. or Russia. This converts any nuclear weapons stationed in NATO
# countries to "United States"

# Define UI for application that displays a map

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel(""),
  
  # set theme to darkly so that it matches the branding of ACONA website. No text goes in the title 
  # panel but spacing of the rest of the app is off if there is not title panle.I left empty quotes 
  # to keep spacing even. 
  
  column(
    3,
    
    # make left hand column. I used this instead of a sidebar for spacing reasons. 
    
    imageOutput("logo", width = "100px", height = "100px"),
    
    # add logo, use width and height arguements to ensure that image doesn't push other elements 
    # around page
    
    br(),
    br(),
    
    # two breaks for spacing 
    
    sidebarPanel(
      width = 12,
      
      # now add a sidebar for aesthetics (the dark grey box is nice!). Set width to 12 so it takes up 
      # the entire column 
      
      h5("DEFENSE INTELLIGENCE AGENCY"), 
      h5("***TOP SECRET***"), 
      br(), 
      h5("This interactive map shows major sites in the Russian nuclear weapons complexes, including 
         military sites with deployed weapons as well as storage and production sites. Hover over a 
         specific site for more information about it. Use the options below to select which sites are 
         displayed on the map. Use the icons in the upper right corner over the title to pan and zoom."),
      
      # explanatory text 
      
      
      # first interactive variable, manipulated by a checkbox that includes the United State or Russia 
      
      br(),
      
      # break for aesthetics 
      
      checkboxGroupInput("type",
                         "System type",
                         choices = c("NSNW", "Strategic", "ABM", "Strategic and NSNW"),
                         selected = c("NSNW", "Strategic", "ABM", "Strategic and NSNW")
      ), 
      
      br(), 
      
      # break for aesthetics 
      
      checkboxGroupInput("status",
                         "System status",
                         choices = c("Deployed", "Storage", "Production"),
                         selected = c("Deployed", "Storage", "Production")
      ), 
      h5("Created by the Negotiation Task Force at the Davis Center for Russian and Eurasian Studies at Harvard University for the Deep START simulation.")
      
      # second interactive variable, manipulated by a checkbox that includes NSNW and Strategic 
    )
  ),
  
  column(9, 
         
         # second column 
         
         br(), 
         br(), 
         br(), 
         br(), 
         br(), 
         br(), 
         
         # break to align top of map with sidebar
         
         plotlyOutput("plot1", height = "800px"),
         
         # map output, height specified in pixels to help with formatting beneath it
  ) 
  
  # end columns 
  
)

server <- function(input, output) {
  output$plot1 <- renderPlotly({
    
    loc_data <- location_data %>%
      filter(side == "Russia") %>%
      filter(status %in% c(input$status)) %>% 
      filter(type %in% c(input$type))
    
    # filter data based on inputs 
    
    if (nrow(loc_data) == 0) {
      
      # Because I used filter above, if nothing is checked, the data is not filtered at all 
      # and a FULL map is shown which doesn't make sense to the viewer. 
      
      loc_data %>%  
        filter(status == "abc") %>%
        
        # filters out everything because there is no status equal to abc 
        
        plot_ly(
          lat = ~lat,
          lon = ~long,
          height = 600,
          width = 1000,
          type = "scattermapbox", 
          split = ~type,
          text = ~ paste(
            "</br> Name: ", base_location,
            "</br> Delivery System: ", delivery_system,
            "</br> Warhead: ", warhead,
            "</br> Warhead type: ", type
          ),
          hoverinfo = "text", 
          marker = list(color = "type")) %>% 
        
        # set plotly specifications, including 
        layout(
          legend = list(
            x = 0.8,
            y = 0.2,
            bgcolor = "rgb(255, 255, 255)",
            bordercolor = "rgb(0,0,0)",
            borderwidth = 2
          ), 
          paper_bgcolor = "rgb(34,34,34)",
          mapbox= list(
            style = "white-bg",
            zoom = 1.5,
            center = list(lon = 80 ,lat= 50),
            layers = list(list(
              below = 'traces',
              sourcetype = "raster",
              source = list(
                "https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}")))))
      
    } else {
      
      loc_data %>%  
        plot_ly(
          lat = ~lat,
          lon = ~long,
          height = 600,
          width = 1000,
          type = "scattermapbox", 
          split = ~type,
          text = ~ paste(
            "</br> Name: ", base_location,
            "</br> Delivery System: ", delivery_system,
            "</br> Warhead: ", warhead,
            "</br> Warhead type: ", type
          ),
          hoverinfo = "text", 
          marker = list(color = "type")) %>% 
        layout(
          legend = list(
            x = 0.8,
            y = 0.2,
            bgcolor = "rgb(255, 255, 255)",
            bordercolor = "rgb(0,0,0)",
            borderwidth = 2
          ), 
          paper_bgcolor = "rgb(34,34,34)",
          mapbox= list(
            style = "white-bg",
            zoom = 1.5,
            center = list(lon = 80 ,lat= 50),
            layers = list(list(
              below = 'traces',
              sourcetype = "raster",
              source = list(
                "https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}")))))
      
    }
    
    
  })
  
  output$logo <- renderImage(
    {
      
      # Use renderImage, assign it to output$image,
      # which corresponds with the imageOutput("Image") above.
      
      list(
        src = "ACONA.png",
        contentType = "image/png",
        width = 96,
        height = 72
        
        # specify width and height for aesthetics 
      )
    },
    deleteFile = FALSE
    
    # make sure file doesn't get deleted after printing 
  )
}

# Run the application
shinyApp(ui = ui, server = server)