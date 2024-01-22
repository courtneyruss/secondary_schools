library(httr)
library(jsonlite)
library(tidyverse)
library(readxl)
library(leaflet)
library(shiny)
library(shinythemes)


load_data <- function() {
  url <- "https://catalogue.data.govt.nz/api/3/action/datastore_search?resource_id=4b292323-9fcc-41f8-814b-3c7b19cf14b3&limit=32000"
  
  response <- httr::GET(url)
  content <- httr::content(response, "text")
  parsed <- jsonlite::fromJSON(content)
  
  school_records <- parsed$result$records
  
  schools <- school_records %>%
    filter(Status == "Open" & Org_Type %in% c("Secondary (Year 9-15)", "Secondary (Year 7-15)", "Secondary (Year 11-15)", "Composite")) %>%
    mutate(
      Authority = case_when(
        Authority %in% c("Private : Fully Registered", "Private: Provisionally Registered") ~ 'Private',
        Authority == "State : Integrated" ~ "State Integrated",
        TRUE ~ Authority
      ),
    ) %>%
    select(School_Id, School = Org_Name, Type = Org_Type, Authority, Latitude, Longitude, Total) %>%
    mutate(School_Id = as.numeric(School_Id), Roll = ifelse(is.na(Total), "Not Stated", Total))
  
  achievement <- readxl::read_excel('achievement.xlsx', sheet = 7) %>%
    rename(School_Id = "School: ID") %>%
    mutate(`School leavers attaining NCEA Level 2 or above` = ifelse(`Percent attaining NCEA level 2 or Above` == 'x', 'x', ifelse(is.na(`Percent attaining NCEA level 2 or Above`), NA, sprintf("%.1f%%", round(as.numeric(`Percent attaining NCEA level 2 or Above`) * 100, 1)))))
  
  join <- dplyr::left_join(schools, achievement, by = 'School_Id') %>%
    mutate(`School leavers attaining NCEA Level 2 or above` = if_else(is.na(`School leavers attaining NCEA Level 2 or above`), "Unavailable", `School leavers attaining NCEA Level 2 or above`))
  
  return(join)
}

find_closest_schools <- function(selected_school, all_schools) {
  distances <- sqrt((all_schools$Latitude - selected_school$Latitude)^2 + (all_schools$Longitude - selected_school$Longitude)^2)
  
  closest_schools <- all_schools[order(distances), ][-which(all_schools$School_Id == selected_school$School_Id)[1], ][1:5, ]
  
  return(closest_schools)
}

ui <- fluidPage(
  theme = shinytheme("yeti"),
  tags$head(
    tags$style(HTML("
      .title {
        font-size: 24px;
        color: #333;
        text-align: center;
        margin: 20px 0;
      }
      .sidebar {
        background-color: #f9f9f9;
        padding: 15px;
        border-radius: 10px;
        margin: 20px;
      }
      .filterSidebar {
        width: 100%;
        background-color: #ffffff;
        border: 2px solid #ccc;
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 20px;
      }
      .infoBox {
        padding: 20px;
        background-color: #ffffff;
        border: 2px solid #ccc;
        border-radius: 10px;
        margin-bottom: 20px;
      }
    "))
  ),
  titlePanel("NZ Secondary School Achievement"),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      div(
        class = "infoBox",
        div(
          id = "infoBox",
          HTML(paste0(
            "Data Source - School Information: ", tags$a(href = "https://catalogue.data.govt.nz/dataset/directory-of-educational-institutions/resource/4b292323-9fcc-41f8-814b-3c7b19cf14b3", "Ministry of Education via data.govt.nz"), "<br> <br>",
            "Data Source - School Achievement (2019 Data): ", tags$a(href = "https://fyi.org.nz/request/15222-data-on-performance-at-ncea-level-2#incoming-57480", "OIA Request by Mr ED Hirsche via fyi.org.nz"), "<br> <br>",
            "<div style='font-size: 14px;'>",
            "App Developer: Courtney Russ", "<br>",
            tags$a(href = "https://github.com/courtneyruss/secondary_schools", "GitHub"), " | ",
            tags$a(href = "https://www.termsfeed.com/live/fafca8ca-bb0f-4a8e-b4ca-cce543800872", "Disclaimer"), " | ",
            tags$a(href = "https://www.datascienceportfol.io/CourtneyRuss", "Contact"),
            "</div><br>",
            "<p xmlns:cc='http://creativecommons.org/ns#' xmlns:dct='http://purl.org/dc/terms/'>",
            "<span property='dct:title'>NZ Secondary School Explorer</span> by ",
            "<a rel='cc:attributionURL dct:creator' property='cc:attributionName' href='https://github.com/courtneyruss'>Courtney Russ</a> is marked with ",
            "<a href='http://creativecommons.org/publicdomain/zero/1.0?ref=chooser-v1' target='_blank' rel='license noopener noreferrer' style='display:inline-block;'>",
            "CC0 1.0<img style='height:22px!important;margin-left:3px;vertical-align:text-bottom;' src='https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1'>",
            "<img style='height:22px!important;margin-left:3px;vertical-align:text-bottom;' src='https://mirrors.creativecommons.org/presskit/icons/zero.svg?ref=chooser-v1'></a></p>"
          ))
        )
      )
    ),
    
    mainPanel(
      leafletOutput("map", width = "100%", height = "400px"),
      tabsetPanel(
        tabPanel("Selected School", tableOutput("selectedSchool")),
        tabPanel("Nearby Schools", tableOutput("closestSchools"))
      )
    )
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
        margin: 20px;
      }
      .container-fluid {
        border: 2px solid #ddd;
        border-radius: 10px;
        padding: 20px;
      }
    "))
  )
)

server <- function(input, output) {
  join <- load_data()
  
  output$map <- renderLeaflet({
    leaflet(data = join) %>%
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = ~School, group = "allSchools", clusterOptions = markerClusterOptions(maxClusterRadius = 45))
  })
  
  output$selectedSchool <- renderTable({
    click <- input$map_marker_click
    if (!is.null(click)) {
      selected_school <- join[join$Longitude == click$lng & join$Latitude == click$lat, ]
      selected_school_display <- selected_school[, c("School", "Type", "Authority", "Roll", "School leavers attaining NCEA Level 2 or above")]
      
      suppressed_col <- ifelse(selected_school$`School leavers attaining NCEA Level 2 or above` == 'x', '<a href="https://fyi.org.nz/request/15222/response/57480/attach/html/6/EDK%2012650%20School%20leavers%20with%20NCEA%20level%202%202015%202019.xlsx.html">Suppressed - See Caveats</a>', selected_school$`School leavers attaining NCEA Level 2 or above`)
      
      selected_school_display$`School leavers attaining NCEA Level 2 or above` <- suppressed_col
      
      return(selected_school_display)
    } else {
      return(NULL)
    }
  }, sanitize.text.function = identity)
  
  output$closestSchools <- renderTable({
    click <- input$map_marker_click
    if (!is.null(click)) {
      selected_school <- join[join$Longitude == click$lng & join$Latitude == click$lat, ]
      closest_schools <- find_closest_schools(selected_school, join)
      closest_schools <- closest_schools[!duplicated(closest_schools$School_Id), ]
      closest_schools_display <- closest_schools[, c("School", "Type", "Authority", "Roll", "School leavers attaining NCEA Level 2 or above")]
      
      suppressed_col <- ifelse(closest_schools$`School leavers attaining NCEA Level 2 or above` == 'x', '<a href="https://fyi.org.nz/request/15222/response/57480/attach/html/6/EDK%2012650%20School%20leavers%20with%20NCEA%20level%202%202015%202019.xlsx.html">Suppressed - See Caveats</a>', closest_schools$`School leavers attaining NCEA Level 2 or above`)
      
      closest_schools_display$`School leavers attaining NCEA Level 2 or above` <- suppressed_col
      
      return(closest_schools_display)
    } else {
      return(NULL)
    }
  }, sanitize.text.function = identity)
}

shinyApp(ui, server)
