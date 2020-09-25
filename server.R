# please uncomment and install the required libraries if not installed already
# install.packages("shiny")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("plotly")
# install.packages("dygraphs")

# import the required libraries
library(shiny)
library(sf)
library(leaflet)
library(RColorBrewer)
library(scales)
library(ggplot2)
library(dplyr)
library(plotly)
library(dygraphs)

# import and pre-process the required data for the respective visualisations
# the data has been pre-aggregated and stored in separate csv files as per the requirement
sf <- st_read("data/india_shapefiles/IND_adm1.shp")
sp <- as(sf, "Spatial")

tab_1 <- read.csv("data/tab_1.csv")
tab_1_type <- read.csv("data/tab_1_type.csv")
tab_1_diwali <- read.csv("data/tab_1_diwali.csv")

tab_2 <- read.csv("data/tab_2.csv")
tab_2_2 <- tab_2[tab_2$state == "Uttar Pradesh", ]

tab_4 <- read.csv("data/tab_4.csv")

tab_3 <- read.csv("data/tab_3.csv")


shinyServer(function(input, output, session) {

  # generate a choropleth map of India for the 4 pollutants and AQI
  output$tab_1_map <- renderLeaflet({
    
    output$tab_1_caption <- renderText({
      paste("Top 10 Polluted States for ", input$pollutant)
    })
    
    # process the data based on the input selected by the user
    tab_1 <- tab_1[tab_1$pollutant == input$pollutant, ]
    
    # create the spatial polygon dataframe for the map
    sp@data = data.frame(sp@data, tab_1[match(sp@data[,"ID_1"], tab_1[,"id"]),])
    
    # set up bins for each type of pollutant and AQI for better segregation and understanding
    if(input$pollutant == "AQI") {
      mybins <- c(0, 50, 100, 125, 150, 200, Inf)
    }
    else if(input$pollutant == "NO2"){
      mybins <- c(0, 10, 15, 20, 25, 35, Inf)
    }
    else if(input$pollutant == "SO2"){
      mybins <- c(0, 4, 8, 12, 16, Inf)
    }
    else if(input$pollutant == "SPM"){
      mybins <- c(0, 50, 100, 125, 150, 200, Inf)
    }
    else {
      mybins <- c(0, 40, 80, 100, 140, Inf)
    }
    
    # create a color palette for the choropleth map based on the data to be visualised
    mypalette <- colorBin( palette = "RdYlGn", domain = sp@data$value, na.color = "transparent", bins = mybins, reverse = TRUE)
    
    # set a label for the hover-on tooltips on the map
    mytext <- paste(
      "State: ", sp@data$state,"<br/>", 
      input$pollutant, ": ", round(sp@data$value, 2), 
      sep="") %>%
      lapply(htmltools::HTML)
    
    # visualise the map in shiny using leaflet using the appropriate conditions
    leaflet(sp) %>%
      addTiles() %>%
      addPolygons(fillColor = ~mypalette(value),
                  stroke=TRUE, 
                  fillOpacity = 1, 
                  color="black", 
                  weight = 1,
                  label = mytext,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "10px"), 
                    textsize = "18px", 
                    direction = "auto"),
                  highlight = highlightOptions(weight = 5, color = "black",
                                         bringToFront = TRUE),
                  layerId = ~id # extracting out the id of the state to be clicked
                  ) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addLegend( pal=mypalette, values=~value, opacity=0.9, title = input$pollutant, position = "bottomleft" ) %>%
      addControl(html = "<img src= 'map1.jpg'/>", position = "bottomright")
    
  })
  
  # set up click observe event on the map to add more interactivity
  observe({
    click = input$tab_1_map_shape_click
    sub = tab_3[tab_3$id == input$tab_1_map_shape_click$id, c("lat", "lon")] # check for the state which is clicked and subset the required data out
    lat = sub$lat
    lon = sub$lon
    
    if(is.null(click))
      return() # return nothing if the map is not clicked
    else
      leafletProxy("tab_1_map") %>%
      setView(lng = lon, lat = lat, zoom = 5.5) # set the view on the state which is clicked and zoom in
  })
  
  # set up click observe event on the map to add more interactivity
  observe({
    click = input$tab_1_map_shape_click
    
    # check for the state which is clicked and subset the required data out
    sub = tab_1_type[tab_1_type$id == input$tab_1_map_shape_click$id, c("state", "areaType", "value", "pollutant")]
    sub2 = tab_1_diwali[tab_1_diwali$id == input$tab_1_map_shape_click$id, c("state", "diwali", "value", "pollutant")]
    
    state = unique(sub$state)
    
    if(is.null(click))
      output$tab_1_caption3 <- renderUI({
        HTML(paste("<em>Hover & click on a state to know more!</em>")) # display the text if no state is clicked
      })
    else {
      
      # display a bar graph aggregated for a certain state, for types of areas, using ggplot + plotly
      output$tab_1_typeplot <- renderPlotly({
        
        output$tab_1_caption3 <- renderUI({
          HTML(paste(input$pollutant, "in ", state))
        })
        
        output$tab_1_type_caption <- renderUI({
          HTML(paste("Industrial areas have the worst air quality for majority states"))
        })
        
        tab_1_type <- sub[sub$pollutant == input$pollutant, ]
        
        # visualise the bar graph using ggplot + plotly
        p <- ggplot(tab_1_type, aes(x = areaType,
                                    y = value,
                                    fill = areaType,
                                    text = paste("Type of area:", areaType, "<br>Pollutant value:", round(value, 2)))) +
          geom_col() +
          theme(axis.text.x = element_text(size = 8)) +
          theme(axis.text.y = element_text(size = 8)) +
          xlab("Type of Area") +
          ylab("Pollutant Value")
        
        ggplotly(p, tooltip = "text")
        
      })
      
      # display a bar graph aggregated for a certain state, for Diwali festival, using ggplot + plotly
      output$tab_1_diwaliplot <- renderPlotly({
        
        output$tab_1_diwali_caption <- renderUI({
          paste("The air quality gets worse during Diwali for majority states")
        })
        
        tab_1_diwali <- sub2[sub2$pollutant == input$pollutant, ]
        
        # visualise the bar graph using ggplot + plotly
        p <- ggplot(tab_1_diwali, aes(x = value,
                                    y = diwali,
                                    fill = diwali,
                                    text = paste("Diwali:", diwali, "<br>Pollutant value:", round(value, 2)))) +
          geom_col() +
          theme(axis.text.x = element_text(size = 8)) +
          theme(axis.text.y = element_text(size = 8)) +
          xlab("Pollutant Value") +
          ylab("Diwali Festival") +
          scale_fill_manual("Diwali", values = c("yes" = "lightsalmon4", "no" = "orange"))
        
        ggplotly(p, tooltip = "text")
        
      })
      
    }
    })
  
  # generate a bar graph to show the ranks on the first tab
  output$tab_1_plot <- renderPlotly({
    
    # process the data as per the user selected input data
    tab_1 <- tab_1[tab_1$pollutant == input$pollutant, ]
    tab_1 <- arrange(tab_1, desc(value))
    tab_1 <- head(tab_1, 10)
    
    # visualise the the bar graph using ggplot + plotly
    p <- ggplot(tab_1, aes(value,
                           reorder(state, value),
                           text = paste("State:", state, "<br>Pollutant value:", round(value, 2)))) +
      geom_col(fill = "#006aa5") +
      theme(axis.text.x = element_text(size = 8)) +
      theme(axis.text.y = element_text(size = 8)) +
      xlab("Pollutant Value") +
      ylab("State")
    
    ggplotly(p, tooltip = "text")
  })
  
  # generate the comparative area charts on the second tab
  output$tab_2 <- renderPlotly({
    
    # process the data as per the user selected input data
    tab_2 <- tab_2[tab_2$pollutant == input$pollutant1, ]
    tab_2 <- tab_2[tab_2$state == input$state, ]
    
    tab_2_2 <- tab_2_2[tab_2_2$pollutant == input$pollutant1, ]

    tab_2$yearmonth <- as.Date(paste0("2018-", tab_2$month, "-1"))
    tab_2_2$yearmonth <- as.Date(paste0("2018-", tab_2_2$month, "-1"))

    # visualise the 1st area chart of the comparison using ggplot + plotly
    a <- ggplot(tab_2,
                aes(x=yearmonth,
                    y=value,
                    group = 1,
                    text = paste("Pollutant value:", round(value, 2)))) +
      geom_area( fill="#69b3a2", alpha=0.4) +
      geom_line(color="black") +
      geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
      xlab("Month") +
      ylab(input$pollutant) +
      scale_x_date(labels = date_format("%b"), breaks = tab_2$yearmonth)
    
    # set constant limits on Y axis for each of the selected pollutant
    if(tab_2$pollutant[1] == "AQI") {
      a <- a + scale_y_continuous(limits = c(0, 300))
    }
    else if(tab_2$pollutant[1] == "NO2") {
      a <- a + scale_y_continuous(limits = c(0, 50))
    }
    else if(tab_2$pollutant[1] == "SO2") {
      a <- a + scale_y_continuous(limits = c(0, 25))
    }
    else if(tab_2$pollutant[1] == "SPM") {
      a <- a + scale_y_continuous(limits = c(0, 300))
    }
    else if(tab_2$pollutant[1] == "RSPM") {
      a <- a + scale_y_continuous(limits = c(0, 200))
    }
    
    # visualise the 2nd area chart of the comparison using ggplot + plotly
    b <- ggplot(tab_2_2,
                aes(x=yearmonth,
                    y=value,
                    group = 1,
                    text = paste("Pollutant value:", round(value, 2)))) +
      geom_area( fill="#ff7d7d", alpha=0.4) +
      geom_line(color="black") +
      geom_point(shape=21, color="black", fill="#ff7d7d", size=4) +
      xlab("Month") +
      ylab(input$pollutant) +
      scale_x_date(labels = date_format("%b"), breaks = tab_2$yearmonth)
    
    # set constant limits on Y axis for each of the selected pollutant
    if(tab_2_2$pollutant[1] == "AQI") {
      b <- b + scale_y_continuous(limits = c(0, 300))
    }
    else if(tab_2_2$pollutant[1] == "NO2") {
      b <- b + scale_y_continuous(limits = c(0, 50))
    }
    else if(tab_2_2$pollutant[1] == "SO2") {
      b <- b + scale_y_continuous(limits = c(0, 25))
    }
    else if(tab_2_2$pollutant[1] == "SPM") {
      b <- b + scale_y_continuous(limits = c(0, 300))
    }
    else if(tab_2_2$pollutant[1] == "RSPM") {
      b <- b + scale_y_continuous(limits = c(0, 200))
    }
    
    a <- ggplotly(a, tooltip = "text")
    b <- ggplotly(b, tooltip = "text")
    
    # display the 2 area charts as a subplot for comparison
    subplot(b, a, margin = 0.04)
    
  })
  
  
  # generate text for display discussing about the plots
  output$tab_2_caption2 <- renderUI({
    
    HTML(paste("From the previous chapter, we learnt that Uttar Pradesh has the worst and the most hazardous Air Quality in India.",
               "As a matter of fact, Uttar Pradesh, once was also known to have the worst Air Quality in the world!",
               sep = "<br/>"))
    
  })
  
  output$tab_2_caption <- renderUI({
    HTML(paste(p("Uttar Pradesh", HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),
                 "VS", HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),
                 input$state)))
  })
  
  
  # generate instructions for the dygraph
  output$tab_4_caption <- renderUI({
    
    HTML(paste("<em>Hover on the graph to view its details on the top-right corner of the graph.<br>
               Use the seekbar under the graph to drill down in time to gain more insights.</em>"))
    
  })
  
  # generate the dygraph for time series of the pollutants for each state
  output$tab_4 <- renderDygraph({
    
    # process the data as per the user selected input data
    tab_4 <- tab_4[tab_2$state == input$state2, ]
    tab_4$date <-as.Date(tab_4$date,'%d-%m-%Y')
    
    # convert the data to an xts format for the plot
    don <- xts::as.xts(x = tab_4[[input$pollutant2]], order.by = tab_4$date)
    
    # visualise the dygraph for the time series of the pollutants using dygraph
    p <- dygraph(don) %>%
      dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 6, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)
    
    p

  })
  
  # generate the choropleth + symbol proportional map for the air quality with respect to the vehicle counts of states
  output$tab_3_map <- renderLeaflet({
    
    # create the spatial polygon dataframe for the map
    sp@data = data.frame(sp@data, tab_3[match(sp@data[,"ID_1"], tab_3[,"id"]),])
    
    # set up bins for the map for better segregation and understanding
    mybins <- c(0, 50, 100, 125, 150, 200, Inf)
    
    # create a color palette for the choropleth map based on the data to be visualised
    mypalette <- colorBin( palette="RdYlGn", domain=sp@data$value, na.color="transparent", bins = mybins, reverse = TRUE)
    
    # set a label for the hover-on tooltips on the map for shaped states
    mytext1 <- paste(
      "State: ", sp@data$state,"<br/>", 
      "AQI: ", round(sp@data$value, 2),"<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    # set a label for the hover-on tooltips on the map for markers
    mytext2 <- paste(
      "State: ", sp@data$state,"<br/>", 
      "AQI: ", round(sp@data$value, 2),"<br/>",
      "Vehicle Count: ", sp@data$vehicleCountPerState,
      sep="") %>%
      lapply(htmltools::HTML)
    
    # visualise the choropleth + symbol proportional map using leaflet
    leaflet(sp) %>%
      addTiles() %>%
      addPolygons(fillColor = ~mypalette(value),
                  stroke=TRUE, 
                  fillOpacity = 1, 
                  color="black", 
                  weight = 1,
                  label = mytext1,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "18px", 
                    direction = "auto"),
                  highlight = highlightOptions(weight = 5, color = "black",
                                               bringToFront = FALSE),
                  layerId = ~id # extracting out the id of the state to be clicked
      ) %>%
      # addCircles(lat = ~lat, lng = ~lon, radius = ~vehicleCountPerState * 10) %>%
      addMarkers(~lon, ~lat, icon = 
                   makeIcon(
                     iconUrl = "car_hd.png",
                     iconWidth = ~(vehicleCountPerState / 200) + 25,
                     iconHeight = ~(vehicleCountPerState / 300) + 10
                   ),
                 label = mytext2,
                 labelOptions = labelOptions( 
                   style = list("font-weight" = "normal", padding = "3px 8px"), 
                   textsize = "18px", 
                   direction = "auto"),
                 layerId = ~id # extracting out the id of the state to be clicked
                 ) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addLegend( pal=mypalette, values=~value, opacity=0.9, title = input$pollutant, position = "bottomleft" ) %>%
      addControl(html = "<img src= 'car.png'/><br/>Size of the car is<br/>proportional to the count<br/>of vehicles in the state", position = "bottomleft")

  })
  
  # set up click observe event on the map to add more interactivity (for the shapes)
  observe({
    click = input$tab_3_map_shape_click
    sub = tab_3[tab_3$id == input$tab_3_map_shape_click$id, c("lat", "lon")] # check for the state which is clicked and subset the required data out
    lat = sub$lat
    lon = sub$lon
    
    if(is.null(click))
      return() # return nothing if the map is not clicked
    else
      leafletProxy("tab_3_map") %>%
      setView(lng = lon, lat = lat, zoom = 5.5) # set the view on the state which is clicked and zoom in
  })
  
  # set up click observe event on the map to add more interactivity (for the shapes)
  observe({
    click = input$tab_3_map_shape_click
    sub = tab_3[tab_3$id == input$tab_3_map_shape_click$id, c("state", "value", "vehicleCountPerState")] # check for the state which is clicked and subset the required data out
    state = sub$state
    value = sub$value
    vehicleCount = sub$vehicleCountPerState
    
    if(is.null(click))
      output$tab_3_caption3 <- renderUI({
        HTML(paste("<em>Hover & click on a state or a car marker to know more!</em>")) # display text if no state is selected
      })
    else
      output$tab_3_caption3 <- renderUI({
        
        # display the reactive text whenever a state is selected showing respective details
        HTML(paste("State:", state, "<br/>",
                   "Air Quality Index:", round(value, 2), "<br/>",
                   "Vehicle Count:", vehicleCount))
        
      })
  })
  
  # set up click observe event on the map to add more interactivity (for the markers)
  observe({
    click = input$tab_3_map_marker_click
    sub = tab_3[tab_3$id == input$tab_3_map_marker_click$id, c("state", "value", "vehicleCountPerState", "lat", "lon")] # check for the state which is clicked and subset the required data out
    state = sub$state
    value = sub$value
    vehicleCount = sub$vehicleCountPerState
    
    if(is.null(click))
      output$tab_3_caption3 <- renderUI({
        HTML(paste("<em>Hover & click on a state or a car marker to know more!</em>")) # display text if no state is selected
      })
    else
      
      # display the reactive text whenever a state is selected showing respective details
      output$tab_3_caption3 <- renderUI({
        HTML(paste("State:", state, "<br/>",
                   "Air Quality Index:", round(value, 2), "<br/>",
                   "Vehicle Count:", vehicleCount))
        
      })
  })
  
  output$details <- renderUI({
    
    HTML(paste("Details:"))
    
  })
  
  # set up click observe event on the map to add more interactivity (for the markers)
  observe({
    click = input$tab_3_map_marker_click
    sub = tab_3[tab_3$id == input$tab_3_map_marker_click$id, c("lat", "lon")] # check for the state which is clicked and subset the required data out
    lat = sub$lat
    lon = sub$lon
    
    if(is.null(click))
      return() # return nothing if no state is clicked
    else
      leafletProxy("tab_3_map") %>%
      setView(lng = lon, lat = lat, zoom = 5.5) # set the view on the state which is clicked and zoom in
  })
  
  # set up the required text as conclusion from the visualisations for display on the tab
  output$conclusion <- renderUI({
    
    HTML(paste("Conclusion"))
    
  })
  
  output$tab_3_caption2 <- renderUI({
    
    HTML(paste("As the visualisations tell us, the Northern part of India is worsely affected by dangerous Air Quality.",
               "While some states show a direct correlation between the vehicle count and the Air Quality, some Southern states show good quality of air even though their vehicle count is high.",
               "Apart from the smoke emitted by vehicles, reasons like industrial emissions, Diwali festival and crop burning also affect the overall Air Quality of India.",
               sep = "<br/>"))
    
  })
  
  output$tab_3_caption <- renderUI({
    
    HTML(paste("States with:",
               "Worst Air Quality: Uttar Pradesh",
               "Best Air Quality: Sikkim",
               "Highest Vehicle Count: Maharashtra",
               "Lowest Vehicle Count: Sikkim",
               sep = "<br/>"))
    
  })

})