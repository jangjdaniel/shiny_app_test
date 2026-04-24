library(shiny)
library(dplyr)
library(DT)
library(leaflet)
library(tigris)
library(sf)
library(lubridate)
library(here)

options(tigris_use_cache = TRUE)

# Load your data
travel_map <- readRDS(here("data", "trips.rds"))

# Just a sanity fix
travel_map <- travel_map |>
  dplyr::mutate(start_date = as.Date(start_date),
                end_date = as.Date(end_date)) |>
  dplyr::arrange(desc(start_date)) |>
  dplyr::select(-desc)


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
                 "date_single",
                 "Select Month:",
                 min = min(travel_map$start_date, na.rm = TRUE),
                 max = max(travel_map$start_date, na.rm = TRUE),
                 value = min(travel_map$start_date, na.rm = TRUE),
                 timeFormat = "%Y-%m",
                 animate = TRUE
               )
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
    req(input$date_single)
    
    travel_map |>
      dplyr::filter(
        lubridate::floor_date(start_date, "month") ==
          lubridate::floor_date(input$date_single, "month")
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
    us <- states(cb = TRUE)
    
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


#I also want to show histograms and counts for which means of transportation I chose, how many redeyes I took, city counts, etc.


# ---- RUN APP ----
shinyApp(ui, server)