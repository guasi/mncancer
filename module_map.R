
mod_map_ui <- function(id) {
  ns       <- NS(id)
  mdat     <- dat[dat$location != "Minnesota", ]
  controls <- lapply(names(map_control_vars), 
                     \(x) radioButtons(
                            inputId   = ns(x), 
                            label     = map_control_vars[[x]],
                            choices   = shift_to_top(sort(unique(mdat[[x]])),"All")))
  
  tagList(
    fluidRow(
      box(width = 8,
          leafletOutput(ns("leaflet_map"), height = 500)
      ),
      box(width = 4,
          controls
      )
    )
  )
}

mod_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {
      
      output$leaflet_map <- renderLeaflet({
        mdat   <- dat[dat$location != "Minnesota", ]
        latest <- last(sort(unique(mdat$year)))
        
        mdat <- mdat %>%
          filter(year == latest,
                 age  == "All Ages",
                 type == input$type,
                 sex  == input$sex) %>% 
          select(fips, rate)

        mn_map <- left_join(mn_map, mdat, by = c("FIPS" = "fips"))

        bins   <- unique(quantile(mdat$rate))
        pal    <- colorBin("Blues", domain = mn_map$rate, bins = bins)
        labels <-
          sprintf("<strong>%s</strong><br/>rate %g", mn_map$NAME, mn_map$rate) %>%
          lapply(htmltools::HTML)
        title  <- tags$span(strong("Minnesota", latest), 
                         br(), input$type, 
                         br(), input$sex, "sex,","All age groups") 

        leaflet(mn_map) %>%
          addProviderTiles("Esri.WorldGrayCanvas") %>%
          setView(-94, 46, zoom = 6) %>%
          addPolygons(
            weight = 1,
            opacity = 1,
            color = "white",
            dashArray = 3,
            fillColor = ~pal(rate),
            fillOpacity = 0.7,
            highlightOptions = highlightOptions(
              color = "#666",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            label = labels,
            labelOptions = labelOptions(
              style = list(padding = "3px 8px"),
              direction = "auto")) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values = ~rate,
                    title = "Rate of new cases <br>
                             per 100,000 persons",
                    na.label = "No data",
                    opacity = 1) %>% 
          addControl(title, 
                     position = "topright")
      })
    }
  )
}
