# app.R
library(shiny)
library(tidyverse)
library(leaflet)
library(DT)
library(maps)  # for map.where()

ui <- fluidPage(
  titlePanel("Respondents Tracker"),
  
  # Main content
  fluidRow(
    column(width = 1),
    column(width = 10,
           
           # Map
           leafletOutput("map", height = 600),
           
           # Table headers + download buttons
           fluidRow(
             column(width = 6, h4("Outside U.S. Respondents Table")),
             column(width = 6, downloadButton("download_outside", "Download CSV"))
           ),
           fluidRow(
             column(width = 6, h4("State Mismatch Respondents Table")),
             column(width = 6, downloadButton("download_mismatch", "Download CSV"))
           ),
           
           # Column selector
           uiOutput("column_selector"),
           
           # Data table
           DTOutput("outside_table")
    ),
    column(width = 1)
  )
)

server <- function(input, output, session) {
  
  # ---------------- Load & clean data ----------------
  df <- read.csv("/Users/lrainose/Downloads/Pink Cloud 12.15.25.csv")
  
  df1 <- df[-c(1,2), ] %>%
    mutate(
      lat = as.numeric(LocationLatitude),
      long = as.numeric(LocationLongitude)
    ) %>%
    filter(!is.na(lat) & !is.na(long) & !is.na(Email2) & Email2 != "")
  
  
  # ---------------- US bounding boxes ----------------
  in_cont_us <- df1$long >= -125 & df1$long <= -66 & df1$lat >= 24 & df1$lat <= 50
  in_alaska <- df1$long >= -170 & df1$long <= -130 & df1$lat >= 50 & df1$lat <= 72
  in_hawaii <- df1$long >= -161 & df1$long <= -154 & df1$lat >= 18 & df1$lat <= 23
  
  inside_us <- df1[in_cont_us | in_alaska | in_hawaii, ]
  outside_us <- df1[!(in_cont_us | in_alaska | in_hawaii), ]
  
  # ---------------- State mismatch ----------------
  inside_us <- inside_us %>%
    mutate(
      state_inferred = maps::map.where("state", long, lat),
      state_inferred = gsub(":.*", "", state_inferred)
    )
  
  mismatch_states <- inside_us %>%
    filter(
      !is.na(state_reside),
      !is.na(state_inferred),
      tolower(state_reside) != tolower(state_inferred)
    )
  
  
  # ---------------- Map ----------------
  output$map <- renderLeaflet({
    leaflet(df1) %>%
      addTiles() %>%
      # Blue: inside US
      addCircleMarkers(
        data = inside_us,
        ~long, ~lat,
        label = ~Email1,
        radius = 5,
        color = "blue",
        fillOpacity = 0.7
      ) %>%
      # Red: outside US
      addCircleMarkers(
        data = outside_us,
        ~long, ~lat,
        label = ~Email1,
        radius = 5,
        color = "red",
        fillOpacity = 0.9
      ) %>%
      # Yellow: state mismatch
      addCircleMarkers(
        data = mismatch_states,
        ~long, ~lat,
        label = ~paste0(Email1, "\nEntered: ", state_reside, "\nInferred: ", state_inferred),
        radius = 5,
        color = "yellow",
        fillOpacity = 0.9
      )
  })
  
  # ---------------- Table & Download ----------------
  output$column_selector <- renderUI({
    selectInput(
      "columns",
      "Select columns to display (up to 5):",
      choices = names(outside_us),
      selected = c("Name._1","Name._2","Phone","Email1","RecordedDate"),
      multiple = TRUE,
      selectize = TRUE
    )
  })
  
  output$outside_table <- renderDT({
    req(input$columns)
    datatable(
      outside_us[, input$columns, drop = FALSE],
      options = list(pageLength = 10)
    )
  })
  
  output$download_outside <- downloadHandler(
    filename = function() {
      paste0("outside_us_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(outside_us[, input$columns, drop = FALSE], file, row.names = FALSE)
    }
  )
  # ---------------- Download handler for mismatches ----------------
  output$download_mismatch <- downloadHandler(
    filename = function() {
      paste0("state_mismatch_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(
        mismatch_states[, c("Name._1", "Name._2", "Email1", "Email2", "state_reside", "state_inferred", "lat", "long")],
        file,
        row.names = FALSE
      )
    }
  )
  
  
}

# ---------------- Run App ----------------
shinyApp(ui, server)
