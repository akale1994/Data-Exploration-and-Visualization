# please uncomment and install the required libraries if not installed already
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("leaflet")
# install.packages("dplyr")
# install.packages("plotly")
# install.packages("shinycssloaders")
# install.packages("shinyWidgets")
# install.packages("dygraphs")

# import the required libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(dygraphs)

# import and pre-process the required data for designing the UI inputs for dropdowns
tab_2 <- read.csv("data/tab_2.csv")
state <- unique(tab_2$state)

# trigger the UI
shinyUI(fluidPage(
  
  # set the theme and background color for the application
  theme = shinytheme("flatly"),
  setBackgroundColor("ghostwhite"),
  
  # set up the UI with a navigation panel for separate tabs
  navbarPage(title = div(img(src = "logo.jpg", height = 45, width = 45), "Air Quality in India"),
             
             # define and set the zeroeth/home tab
             tabPanel("Home",
                      sidebarPanel(
                        br(),
                        h2("Introduction"),
                        br(),
                        p("Pollution, climate change, global warming and environmental crisis are the growing fears for everyone in today's world.
                          The growth of industries and the rise in the numbers of vehicles on road have major contributions towards it."),
                        br(),
                        p("India, the world's second most populous country, is highly endangered because of its rising air pollution.
                          Moreover, a news article also suggests that India's air quality is even more lethal than the leaders in global population, China."),
                        br(),
                        p("Here, a story of India's air pollution will be conveyed through various visualisations, explaining multiple aspects of its rise in the country.")
                      ),
                      mainPanel(
                        img(src='home3.jpg', align = "centre",height="100%", width="100%"),
                        h1("A Story through Visualisations", align = "center"),
                        h2("Abhilash Anil Kale", align = "center"),
                        HTML("<center>[PS - <em>The initial load might take upto 10 seconds for the maps. Thank you for being patient!</em>]</center>")
                      )
                      ),
             
             # define and set the first tab for chapter 1
             tabPanel("Chapter 1",
                      h1("A Tale of Pollutants throughout India"), 
                      
                      # set a sidebar panel with dropdown menu for pollutants and a bar graph
                      sidebarPanel(
                        h4("The Air Quality Index is a resultant of the 4 pollutants"),
                        selectInput("pollutant", "Choose a pollutant:", 
                                    c("Air Quality Index" = "AQI", 
                                      "Nitrogen Dioxide" = "NO2",
                                      "Sulphur Dioxide" = "SO2",
                                      "Suspended Particulate Matter" = "SPM",
                                      "Respirable Suspended Particulate Matter" = "RSPM")
                                    ),
                        h3("Factors Affecting the Pollutants"),
                        h4(htmlOutput("tab_1_caption3")),
                        p(htmlOutput("tab_1_type_caption")),
                        plotlyOutput("tab_1_typeplot"),
                        br(),
                        p(htmlOutput("tab_1_diwali_caption")),
                        plotlyOutput("tab_1_diwaliplot", height = 220)
                        ),
                      
                      # deploy the choropleth map and the rank graph on the main panel
                      mainPanel(leafletOutput("tab_1_map", height="800") %>% withSpinner(color="#3c4c6b"),
                                h3(textOutput("tab_1_caption")),
                                plotlyOutput("tab_1_plot"))
                      ),
             
             # define and set the second tab for chapter 2
             tabPanel("Chapter 2",
                      h1("A Comparative Analysis with the Worst Affected State through a Year"),
                      h4(htmlOutput("tab_2_caption2")),
                      br(),
                      
                      # set the dropdown menus for the selection of pollutant and state
                      fluidRow(column(6,
                        selectInput("pollutant1", "Choose a pollutant:", 
                                    c("Air Quality Index" = "AQI", 
                                      "Nitrogen Dioxide" = "NO2",
                                      "Sulphur Dioxide" = "SO2",
                                      "Suspended Particulate Matter" = "SPM",
                                      "Respirable Suspended Particulate Matter" = "RSPM")
                        )),
                        (column(6,
                        selectInput("state", "Choose a state:",
                                    state)
                      ))),
                      
                      # deploy the area graphs for comparison on the main panel
                      mainPanel(h2(htmlOutput("tab_2_caption")),
                                plotlyOutput("tab_2", height = 500, width = 1600) %>% withSpinner(color="#3c4c6b"))),
             
             # define and set the third tab for chapter 3
             tabPanel("Chapter 3",
                      h1("The Rise and Fall of Pollutants from 2003 through 2015"),
                      
                      # set the dropdown menus for the selection of pollutant and state
                      fluidRow(column(3,
                                      selectInput("pollutant2", "Choose a pollutant:", 
                                                  c("Air Quality Index" = "AQI", 
                                                    "Nitrogen Dioxide" = "NO2",
                                                    "Sulphur Dioxide" = "SO2",
                                                    "Suspended Particulate Matter" = "SPM",
                                                    "Respirable Suspended Particulate Matter" = "RSPM")
                                      )),
                               (column(3,
                                       selectInput("state2", "Choose a state:",
                                                   state)
                               ))),
                      h4(HTML("Looks like majority of the states have shown intent to improve its Air Quality since mid 2013, and comparatively, be more consistent.
                          <br/>This is due to the strict Government guidelines issued in India in 2013.")),
                      
                      # display a short help/guide to use the dygraph
                      p(htmlOutput("tab_4_caption", align = "right")),
                      
                      # deploy the dygraph plot on the main panel
                      mainPanel(dygraphOutput("tab_4", height = 520, width = 1600)  %>% withSpinner(color="#3c4c6b"))
                      ),
             
             # define and set the fourth tab for chapter 4
             tabPanel("Chapter 4",
                      h1("Do vehicles affect the Air Quality?"),
                      
                      # set a sidebar panel with reactive text and conclusion text for the visualisations
                      sidebarPanel(
                        HTML("The data used for this visualisation is of the year 2015.<br>Air Quality Index is considered for the air quality."),
                        h2(htmlOutput("details")),
                        h4(htmlOutput("tab_3_caption3")),
                        br(),
                        br(),
                        h2(htmlOutput("conclusion")),
                        p(htmlOutput("tab_3_caption2")),
                        br(),
                        p(htmlOutput("tab_3_caption"))
                      ),
                      
                      # deploy the choropleth + symbol proportional map on the main panel
                      mainPanel(leafletOutput("tab_3_map", height=800) %>% withSpinner(color="#3c4c6b"))
                      ),
             
             # display the sources of data used for the visualisations
             tabPanel("Data",
                      br(),
                      h2("Data retrieved from:"),
                      br(),
                      h4("1. Ambient Air Quality for India Dataset"),
                      tags$head(tags$style(HTML("a {color: blue}"))),
                      p(tags$a(href="https://data.gov.in/catalog/historical-daily-ambient-air-quality-data",
                               "historical daily ambient air quality data -- data.gov.in")),
                      br(),
                      h4("2. Dates for Diwali Dataset"),
                      p(tags$a(href="http://www.world-timedate.com/holidays/kali_puja_deepavali_date_list.php",
                               "kali puja deepavali date list -- world-timedate.com")),
                      br(),
                      h4("3. Registered Vehicles in India Dataset"),
                      p(tags$a(href="https://data.gov.in/resources/total-number-registered-motor-vehicles-india-during-1951-2013",
                               "total number of registered motor vehicles in India during 1951 to 2013 -- data.gov.in")),
                      br(),
                      h4("4. State-wise Registered Vehicles in India Dataset"),
                      p(tags$a(href="https://data.gov.in/catalog/state-wise-total-registered-motor-vehicles-india",
                               "state wise total registered motor vehicles in India -- data.gov.in")),
                      br(),
                      h4("5. Shapefiles - India"),
                      p(tags$a(href="http://www.diva-gis.org/datadown",
                               "shapefiles for India -- diva-gis.org")),
                      )
             )
  
))