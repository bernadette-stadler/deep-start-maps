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

world <- ne_countries(scale = "medium", returnclass = "sf")

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
      
      h5("This map shows major sites in the American and Russian nuclear weapons complexes, including 
         military sites with deployed weapons as well as storage and production sites. Hover over a 
         specific site for more information about it. Use the options below to select which sites are 
         displayed on the map. Use the icons in the upper right corner over the title to pan and zoom."),
      
      # explanatory text 
      
      br(),
      
      # break for aesthetics 
      
      checkboxGroupInput("strana",
                         "Country",
                         choices = c("Russia", "United States"),
                         selected = c("Russia", "United States")
      ),
      
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
      )
      
      # second interactive variable, manipulated by a checkbox that includes NSNW and Strategic 
    )
  ),
  
  column(
    9,
    
    # second column 
    
    br(),
    
    # break two align top of map with sidebar
    
    plotlyOutput("plot1", height = "800px"),
    
    # map output, height specified in pixels to help with formatting beneath it
  ),
  
  # end first two columns 
  
  br(),
  
  # break for aesthetics 
  
  column(12,
         
         # bottom part of the page is one wide column taking up the full width 
         
         offset = 3,
         
         # offset to center text 
         
         h5("Sources:"),
         h5("Hans M. Kristensen & Robert S. Norris (2017) Worldwide deployments of nuclear weapons, 
            2017, Bulletin of the Atomic Scientists, 73:5, 289-297,"),
         h5("Hans M. Kristensen & Matt Korda (2020) Russian nuclear forces, 2020, 
            Bulletin of the Atomic Scientists, 76:2, 102-117,"),
         h5("The CSIS Missile Threat Website")
         
         # add text with proper indentations/new lines 
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
  
  output$plot1 <- renderPlotly({
    
    loc_dat <- location_data %>%
      filter(side %in% c(input$strana)) %>%
      filter(status %in% c(input$status)) %>% 
      filter(type %in% c(input$type))
    
    # modify locatiton data based on inputs, save it. A gich that I encountered is that if all 
    # boxes for a specific category are unchecked, an error was thrown because there was no data 
    # for the scatterplot layer of the map (since all rows from the data were removed). To fix 
    # this, I added the conditional statement below. If the number of rows in the data is zero, 
    # the app now prints a blank map. If the nubmer of rows in the location data is not zero, 
    # it prints a map with the relevant markers. 
    
    if (nrow(loc_dat) == 0) {
      
      plot <- ggplot()+ 
        
        # make a ggplot 
        
        geom_sf(data = world, color = "dimgray", fill= "gray91", size = 0.1)+
        
        # with a geom_sf layer that makes the map. Specify coutry color, line color and size 
        
        coord_sf(xlim = c(-130, 130), ylim = c(-90, 90))+
        
        # specifying expands the map to fill the space better 
        
        labs(y = "", x = "", type = "")+
        theme(panel.background = element_rect(fill = "lightsteelblue2"))+
        theme(panel.grid.major = element_line(color = "lightsteelblue2"))+
        theme(plot.margin = margin(1.5, 0, 0, 0, "cm"))
      
      
      ggplotly(plot, autosize = F, width = 1200, height = 750, tooltip = "text") %>% 
        layout(
          title = list(
            text = "Deep START exercise: U.S. and Russian Nuclear Weapons Deployment",
            y = 0.9, x = 0.55
          ),
          
          # add title, center it 
          
          titlefont= list(
            family = "Segoe UI",
            size = 18,
            color = "rgb(255,255,255)"
          ),
          
          # specify title font, size, and color 
          
          legend = list(
            x = 0.8,
            y = 0.2,
            bgcolor = "rgb(255, 255, 255)",
            bordercolor = "rgb(0,0,0)",
            borderwidth = 2
          ),
          
          # change legend location and color 
          
          paper_bgcolor = "rgb(34,34,34)"
          
          # color margins same as background in the shiny app
        )
      
    } else {
      plot <- ggplot()+ 
        geom_sf(data = world, color = "dimgray", fill= "gray91", size = 0.1)+
        coord_sf(xlim = c(-130, 130), ylim = c(-90, 90))+ 
        geom_point(data = loc_dat, aes(x =long, 
                                       y =lat, 
                                       color = type,
                                       shape = type,
                                       text = paste(
                                         "</br> Name: ", base_location,
                                         "</br> Delivery System: ", delivery_system,
                                         "</br> Warhead: ", warhead,
                                         "</br> Warhead type: ", type
                                       )))+
        scale_color_manual(values = c("NSNW" = "red", 
                                      "Strategic" = "dodgerblue", 
                                      "Strategic and NSNW" = "purple4", 
                                      "ABM" = "springgreen4"))+
        scale_shape_manual(values = c("NSNW" = 3, 
                                      "Strategic" = 15, 
                                      "Strategic and NSNW" = 16, 
                                      "ABM" = 17))+
        labs(y = "", x = "", type = "")+
        theme(panel.background = element_rect(fill = "lightsteelblue2"))+
        theme(panel.grid.major = element_line(color = "lightsteelblue2"))+
        theme(plot.margin = margin(1.5, 0, 0, 0, "cm"))
      
      ggplotly(plot, autosize = F, width = 1200, height = 750, tooltip = "text") %>% 
        layout(
          title = list(
            text = "Deep START exercise: U.S. and Russian Nuclear Weapons Deployment",
            y = 0.9, x = 0.55
          ),
          
          # add title, center it 
          
          titlefont= list(
            family = "Segoe UI",
            size = 18,
            color = "rgb(255,255,255)"
          ),
          
          # specify title font, size, and color 
          
          legend = list(
            x = 0.8,
            y = 0.2,
            bgcolor = "rgb(255, 255, 255)",
            bordercolor = "rgb(0,0,0)",
            borderwidth = 2
          ),
          
          # change legend location and color 
          
          paper_bgcolor = "rgb(34,34,34)"
          
          # color margins same as background in the shiny app
        )
    }
    
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)