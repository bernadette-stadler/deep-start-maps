server <- function(input, output) {
  output$plot1 <- renderPlotly({
    
    g <- list(
      scope = "world",
      projection = list(type = "mercator"),
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
      filter(side == "Russia") %>%
      filter(status %in% c(input$status)) %>% 
      filter(str_detect(type, paste(c(input$type), collapse = "|")) == TRUE) %>%
      
      # pipe data into two filters that take the interactive variables from above. The second interactive 
      # line makes it so that if both NSNW and Strategic are checked, all three possible options ("NSNW",
      # "Strategic", and "Strategic and NSNW") are returned, instead of only "Strategic" and "NSNW".
      
      plot_geo(lat = ~lat, 
               lon = ~long, 
               autosize = F, 
               width = 1200, 
               height = 800, 
               margin = m, 
               split = ~type) %>%
      
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
        
        marker = list( 
          line = list(color = "rgb(0,0,0)")),
        symbol = ~type,
        color = ~status, 
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