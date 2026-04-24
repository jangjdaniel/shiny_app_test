library(shiny)
library(dplyr)
library(DT)
library(leaflet)
library(tigris)
library(sf)
library(here)

options(tigris_use_cache = TRUE)

# Load your data
travel_map <- readRDS(here("data", "trips.rds"))

# ---- UI ----
ui <- fluidPage(
  titlePanel("Travel Explorer"),
  
  tabsetPanel(
    
    # ---- TABLE TAB ----
    tabPanel("Data Explorer",
             br(),
             DTOutput("travel_table")
    ),
    
    # ---- MAP TAB ----
    tabPanel("Travel Map",
             br(),
             sliderInput(
               sliderInput(
                 "date_single",
                 "Select Month:",
                 min = min(travel_map$start_date),
                 max = max(travel_map$start_date),
                 value = min(travel_map$start_date),
                 timeFormat = "%Y-%m",
                 animate = TRUE
               ))
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- TABLE ----
  output$travel_table <- renderDT({
    datatable(
      travel_map,
      filter = "top",
      options = list(pageLength = 10)
    )
  })
  
  # ---- FILTERED DATA ----
  filtered_data <- reactive({
    req(input$date_range)
    
    travel_map |>
      filter(
        start_date >= input$date_range[1],
        start_date <= input$date_range[2]
      )
  })
  
  # ---- AGGREGATE VISITS ----
  visit_counts <- reactive({
    filtered_data() |>
      count(state, name = "visits")
  })
  
  # ---- MAP ----
  output$map <- renderLeaflet({
    
    # Load US states
    us <- states(cb = TRUE) |>
      shift_geometry()
    
    # Join data
    map_data <- us |>
      left_join(visit_counts(), by = c("NAME" = "state")) |>
      mutate(visits = ifelse(is.na(visits), 0, visits))
    
    pal <- colorNumeric("Blues", domain = map_data$visits)
    
    leaflet(map_data) |>
      addProviderTiles("CartoDB.Positron") |>
      addPolygons(
        fillColor = ~pal(visits),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste0(NAME, ": ", visits, " visits"),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal = pal,
        values = ~visits,
        title = "Trips",
        position = "bottomright"
      )
  })
}

# ---- RUN APP ----
shinyApp(ui, server)