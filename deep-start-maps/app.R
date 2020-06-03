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
)) %>%
  clean_names()

# read in data, clean it up

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
    2,
    
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
        "Weapon type",
        choices = c("NSNW", "Strategic"),
        selected = c("NSNW", "Strategic")
      )
      
      # second interactive variable, manipulated by a checkbox that includes NSNW and Strategic 
    )
  ),

  column(
    10,
    
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
  output$plot1 <- renderPlotly({
      
    g <- list(
      scope = "world",
      projection = list(type = "equirectangular"),
      showland = TRUE,
      showcountries = TRUE,
      showocean = TRUE,
      showcoastlines = TRUE,
      showsubunits = TRUE,
      coastlinecolor = toRGB("dimgray"),
      oceancolor = toRGB("lightsteelblue2"),
      landcolor = toRGB("gray95"),
      countrycolor = toRGB("gray85"),
      subunitcolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )
    
    # customize map parameters and save 

    m <- list(
      l = 10,
      r = 10,
      b = 10,
      t = 0,
      pad = 4
    )
    
    # customize margins and save 

    location_data %>%
      filter(side %in% c(input$strana)) %>%
      filter(str_detect(type, paste(c(input$type), collapse = "|")) == TRUE) %>%
    
    # pipe data into two filters that take the interactive variables from above. The second interactive 
    # line makes it so that if both NSNW and Strategic are checked, all three possible options ("NSNW",
    # "Strategic", and "Strategic and NSNW") are returned, instead of only "Strategic" and "NSNW".
        
      plot_geo(lat = ~lat, lon = ~long, autosize = F, width = 1200, height = 800, margin = m) %>%
    
    # make the map, specifying dimensions and margins 
        
      add_markers(
          
          # add markers 
          
        text = ~ paste(
          "</br> Name: ", base_location,
          "</br> Delivery System: ", delivery_system,
          "</br> Warhead: ", warhead,
          "</br> Warhead type: ", type
        ),
        
        # specify what should disply as hover text 
        
        marker = list(color = "rgb(0,0,0)", 
                      line = list(color = "rgb(0,0,0)")),
        symbol = ~status, 
        size = I(14), 
        hoverinfo = "text"
        
        # specify marker color, symbols, size, etc.
        
      ) %>%
      layout(
        title = list(
          text = "Deep START exercise: U.S. and Russian Nuclear Weapons Deployment",
          y = 0.9, x = 0.55
        ),
        
        # add title, center it 
        
        titlefont = list(
          family = "Segoe UI",
          size = 18,
          color = "rgb(255,255,255)"
        ),
        
        # specify title font, size, and color 
        
        geo = g,
        
        # set paramaters based on list created above 
        
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
