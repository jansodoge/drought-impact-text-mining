library(shiny)
library(shinyWidgets)
library(leaflet)
library(htmltools)
library(sf)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)


dashboard_shiny_app <-
  function(impacts_locations_dataset_b,
           nuts_geo_data) {
    nuts1 <-  nuts_geo_data %>%
      filter(CNTR_CODE == "DE") %>%
      filter(LEVL_CODE == 1)
    nuts2 <- nuts_geo_data %>%
      filter(CNTR_CODE == "DE") %>%
      filter(LEVL_CODE == 2)
    nuts3 <- nuts_geo_data %>%
      filter(CNTR_CODE == "DE") %>%
      filter(LEVL_CODE == 3)
    
    
    impacts_locations_dataset_b <- impacts_locations_dataset_b %>%
      mutate(date = as.Date(paste0(year_date, "/", month_date, "/15"),
                            format = "%Y/%m/%d"))
    
    ui <- fluidPage(
      theme = shinytheme("superhero"),
      tags$head(tags$style(
        HTML(
          "
      mark {
        font-family: bold;
        color: red;
      }
      hr {border-top: 1px solid #000000;}
    "
        )
      )),
      navbarPage(
        "Drought Impact Monitor",
        id = "main",
        sidebarLayout(
          sidebarPanel(
            dateRangeInput(
              'dateRange',
              label = 'Filter drought impacts by time interval',
              start = as.Date('2000-01-01') ,
              end = as.Date('2021-01-01')
            ),
            radioButtons(
              "selected_nut",
              h3("Select NUTS-level"),
              choices = list(
                "NUTS-1" = 1,
                "NUTS-2" = 2,
                "NUTS-3" = 3
              ),
              selected = 3
            ),
            
            selectInput(
              "select_class",
              h3("Select impact class"),
              choices = append("all", unique(
                impacts_locations_dataset_b$type_of_class
              )),
              selected = 1
            ),
            shiny::h4("Click on the map to view regional-level data"),
            plotOutput("nut_specific_plot"),
            width = 5
          ),
          mainPanel(leafletOutput(
            "distPlot", height = 600, width = "75%"
          ),
          width = 7)
        )
      )
    )
    
    server <- function(input, session, output) {
      output$distPlot <- renderLeaflet({
        if (input$selected_nut == 1)
          joining_nuts_level <- nuts1
        if (input$selected_nut == 2)
          joining_nuts_level <- nuts2
        if (input$selected_nut == 3)
          joining_nuts_level <- nuts3
        
        
        if (input$select_class == "all") {
          map_df <- impacts_locations_dataset_b %>%
            dplyr::filter(
              as.Date(date) >= as.Date(input$dateRange[1]) &
                as.Date(date) <= as.Date(input$dateRange[2])
            ) %>%
            group_by(nuts_id) %>%
            count() %>%
            dplyr::right_join(joining_nuts_level, by = c("nuts_id" = "NUTS_ID")) %>%
            sf::st_as_sf() %>%
            mutate(n = ifelse(is.na(n), 0, n))
        } else{
          map_df <- impacts_locations_dataset_b %>%
            dplyr::filter(
              as.Date(date) >= as.Date(input$dateRange[1]) &
                as.Date(date) <= as.Date(input$dateRange[2])
            ) %>%
            dplyr::filter(type_of_class == input$select_class) %>%
            group_by(nuts_id) %>%
            count() %>%
            dplyr::right_join(joining_nuts_level, by = c("nuts_id" = "NUTS_ID")) %>%
            sf::st_as_sf() %>%
            mutate(n = ifelse(is.na(n), 0, n))
        }
        
        pal <- colorNumeric("YlOrRd", domain = map_df$n)
        leaflet() %>%
          setView(lng = 10.45,
                  lat = 51.16,
                  zoom = 5.5) %>%
          addProviderTiles(providers$Stamen.TonerLite) %>%
          addPolygons(
            data =  map_df,
            fillColor = ~ pal(n),
            weight = 1,
            smoothFactor = 0.5,
            opacity = 1.0,
            fillOpacity = 0.5,
            label = ~ NAME_LATN
          ) %>%
          addLegend(
            data = map_df,
            pal = pal,
            values = ~ n,
            opacity = 0.7,
            title = NULL,
            position = "bottomright"
          )
      })
      
      
      
      clicked_nut_lat <- reactive(input$distPlot_shape_click$lat[1])
      clicked_nut_lng <- reactive(input$distPlot_shape_click$lng[1])
      output$nut_specific_plot <- renderPlot({
        latitude <- clicked_nut_lat()
        longitude <- clicked_nut_lng()
        if (is.null(latitude))
          NULL
        if (!is.numeric(latitude))
          NULL
        
        
        clicked_sf_point <- sf::st_point(x = c(longitude, latitude))
        clicked_on_nut <-
          nuts3[sf::st_within(clicked_sf_point, nuts3)[[1]], ]$NUTS_ID
        clicked_on_nut_name_ltn <-
          nuts3[sf::st_within(clicked_sf_point, nuts3)[[1]], ]$NAME_LATN
        
        
        expand_grid(month = seq(1, 12),
                    year = seq(2000, 2020)) %>%
          dplyr::left_join(
            impacts_locations_dataset_b %>%
              dplyr::filter(nuts_id == clicked_on_nut),
            by = c("year" = "year_date",
                   "month" = "month_date")
          ) %>%
          mutate(MIS = ifelse(is.na(MIS), 0, MIS)) %>%
          drop_na(type_of_class) %>%
          ggplot(aes(x = year, y = MIS,  fill = type_of_class)) +
          geom_bar(stat = "identity") +
          labs(title = paste("Impacts observed in ", clicked_on_nut_name_ltn)) +
          theme(legend.position = "bottom") +
          scale_x_continuous(
            breaks = seq(2000, 2020, 2),
            labels = seq(2000, 2020, 2),
            limits = c(2000, 2020)
          )
      })
    }
    
    shinyApp(ui = ui, server = server)
  }
