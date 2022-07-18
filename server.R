server <- function(input, output) {
  
  # UTILS ----------------------------------------------------
  shift_to_top <- function(x,i) {
    if (is.character(i)) {
      i <- which(grepl(i,x))  
    }
    
    if (length(i) > 0) {
      return(c(x[i], x[-i]))
    } else {
      return(x)
    }
  }
  
  # PLOTS ---------------------------------------------------
  r <- reactiveValues(
    top    = NULL,
    latest = NULL,
    prev   = NULL,
    change = NULL,
  )
  
  observeEvent(c(input$plot_geo,input$plot_group), {
    dat <- if (input$plot_geo == "state")
              dat[dat$location == "Minnesota", ]
            else
              dat[dat$location != "Minnesota", ]
    dat <- dat[dat$type != "All cancer types combined", ]

    removeUI(
      selector = "#plot_control_bundle"
    )
    
    controls <- lapply(names(plot_control_vars),
                      \(x) selectInput(
                        inputId   = paste0("plot_",x),
                        label     = plot_control_vars[[x]],
                        choices   = shift_to_top(sort(unique(dat[[x]])),"All"),
                        multiple  = ifelse(x == "location", FALSE, TRUE),
                        selectize = FALSE))

    insertUI(
     selector = "#plot_controls",
     ui       = div(id = "plot_control_bundle", controls)
    )
  })

  pdat <- reactive({
    # active dat
    mdat          <- if (input$plot_geo == "state")
                        dat[dat$location == "Minnesota", ]
                      else
                        dat[dat$location != "Minnesota", ]
    mdat          <- mdat[mdat$type != "All cancer types combined", ]
    vars          <- names(plot_control_vars)
    filtered_vars <- vars[sapply(vars, \(x) (!is.null(input[[paste0("plot_",x)]])))]
    
    for (var in filtered_vars) {
      mdat <- mdat[mdat[[var]] %in% input[[paste0("plot_",var)]], ]
    }
    
    # reactive values for dashboard value boxes
    yr       <- sort(unique(mdat$year))
    r$latest <- last(yr)
    r$prev   <- yr[length(yr)-1]
    
    temp <- mdat %>% 
      filter(age == "All Ages",
             sex == "All")
    
    r$top <- temp %>% 
      filter(year == r$latest) %>% 
      mutate(rank = rank(rate)) %>% 
      filter(rank == last(sort(rank)))
    
    top.prev <- temp %>% 
      filter(year == r$prev,
             type == r$top$type)
      
    r$change  = round(r$top$rate - top.prev$rate, 2)
    
    return(mdat)
  })
  
  output$vbox_count <- renderValueBox({
    req(r$top$count, input$plot_location)
    valueBox(
      value = r$top$count,
      subtitle = HTML("New cases of", r$top$type,
                      "<br> in",input$plot_location,"in",r$latest),
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$vbox_rate <- renderValueBox({
    req(r$top$rate, input$plot_location)
    valueBox(
      value    = r$top$rate,
      subtitle = HTML("New cases per 100,100 persons of", r$top$type,
                       "<br> in",input$plot_location,"in",r$latest),
      icon     = icon("users"),
      color    = "yellow"
    )
  })
  
  output$vbox_change <- renderValueBox({
    req(r$change, input$plot_location)
    valueBox(
      value    = r$change,
      subtitle = HTML("New cases rate change of", r$top$type,
                       "<br> in", input$plot_location,"from", r$prev, "to", r$latest),
      icon     =  icon(ifelse(r$change < 0, "arrow-down", "arrow-up")),
      color    = ifelse(r$change < 0, "green", "red")
    )
  })

  output$latest <- renderPlotly({
    req(input$plot_location, pdat())
    mdat    <- pdat()
    active  <- input$plot_location
    
    mdat <- mdat %>% 
      filter(year == r$latest,
             age  == "All Ages",
             sex  == "All") %>% 
      plot_ly(x = ~reorder(type,-rate), y = ~rate, type = "bar",  
              texttemplate = "%{y}") %>%
      layout(title    = paste0(active,", ",r$latest, ", rate of new cases \n all age groups, male and female"),
              xaxis   = list(title = ""),
              yaxis   = list(title = "Rate per 100,000 persons"),
              margin  = list(t = 60, r = 50),
              modebar = list(orientation = "v"),
              font    = list(size = 10))
  })
  
  output$historical <- renderPlotly({
    req(input$plot_location, pdat())
    mdat    <- pdat()
    active  <- input$plot_location
    if (active == "Minnesota") mdat <- mdat[!grepl("\\-",mdat$year), ]
    
    mdat %>% 
      filter(age == "All Ages",
             sex == "All") %>% 
      plot_ly(x = ~year, y = ~rate, color = ~type) %>%
      add_lines() %>%
      layout(title   = paste0(active,", rate of new cases \n all age groups, male and female"),
             xaxis   = list(title = ""),
             yaxis   = list(title = "Rate per 100,000 persons"),
             margin  = list(t = 60, r = 50),
             modebar = list(orientation = "v"),
             font    = list(size = 10))
  })
  
  output$sex <- renderPlotly({
    req(input$plot_location, pdat())
    mdat    <- pdat()
    active  <- input$plot_location
    
    mdat %>% 
      filter(year == r$latest,
             age  == "All Ages") %>% 
      plot_ly(x = ~reorder(type,-rate), y = ~rate, color = ~sex) %>%
      add_markers() %>% 
      layout(title   = paste0(active,", ",r$latest, ", rate of new cases \n all age groups"),
             xaxis   = list(title = ""),
             yaxis   = list(title = "Rate per 100,000 persons"),
             margin  = list(t = 60, r = 50),
             modebar = list(orientation = "v"),
             font    = list(size = 10))
  })

  output$age <- renderPlotly({
    req(input$plot_location, pdat())
    mdat    <- pdat()
    active  <- input$plot_location
    
    mdat %>% 
      filter(year == r$latest,
             sex == "All") %>% 
      plot_ly(x = ~reorder(type,-rate), y = ~rate, color = ~age) %>% 
      add_markers() %>% 
      layout(title   = paste0(active, ", ", r$latest, ", rate of new cases \n male and female"),
             xaxis   = list(title = ""),
             yaxis   = list(title = "Rate per 100,000 persons"),
             margin  = list(t = 60, r = 50),
             modebar = list(orientation = "v"),
             font    = list(size = 10))
  })
  
  
  # TABLE -------------------------------------------------------------
  observeEvent(input$geo, {
    dat <- if (input$geo == "state")
             dat[dat$location == "Minnesota", ]
           else 
             dat[dat$location != "Minnesota", ]
    
    removeUI(
      selector = "#control_bundle"
    )
    controls <- lapply(names(dat_control_vars), 
                       \(x) selectInput(
                         inputId   = x, 
                         label     = dat_control_vars[[x]],
                         choices   = shift_to_top(sort(unique(dat[[x]])),"All"),
                         multiple  = TRUE,
                         selectize = FALSE))
    
    insertUI(
      selector = "#controls",
      ui       = div(id = "control_bundle", controls)
    )
  })
  
  output$dt_table <- renderDT({
    mdat          <- if (input$geo == "state") 
                       dat[dat$location == "Minnesota", ]
                     else 
                       dat[dat$location != "Minnesota", ]
    vars          <- names(dat_control_vars)
    filtered_vars <- vars[sapply(vars, \(x) (!is.null(input[[x]])))]
    
    # filter
    for (var in filtered_vars) {
      mdat <- mdat[mdat[[var]] %in% input[[var]], ]
    }
    
    # variables to display
    mdat <- mdat[ , c(vars, "population", "count", "rate", "ci")]
    datatable(mdat,
              selection = "none",
              rownames  = FALSE,
              colnames  = c("Location",
                            "Type",
                            "Year",
                            "Age Group",
                            "Sex",
                            "Population",
                            "Number of New Cancers",
                            "Rate (per 100,000 people)",
                            "95% Confidence Interval"),
              options   = list(dom = "tp", scrollX = TRUE)) %>% 
      formatRound(columns = 6, digits = 0, mark = ",")
  })
  
  # MAP ----------------------------------------------------------
  map_controls <- function() {
    dat       <- dat[dat$location != "Minnesota", ]
    controls  <- lapply(names(map_control_vars), 
                       \(x) radioButtons(
                         inputId   = paste0("map_",x), 
                         label     = dat_control_vars[[x]],
                         choices   = shift_to_top(sort(unique(dat[[x]])),"All")))
    insertUI(
      selector = "#map_controls",
      ui       = div(id = "#map_control_bundle", controls)
    )
  }
  map_controls()
  
  output$leaflet_map <- renderLeaflet({
    mdat          <- dat[dat$location != "Minnesota", ]
    latest        <- last(sort(unique(mdat$year)))
    vars          <- names(map_control_vars)
    filtered_vars <- vars[sapply(vars, \(x) (!is.null(input[[paste0("map_",x)]])))]
    
    rates <- mdat %>% 
      filter(year == latest,
             age  == "All Ages",
             type == input$map_type,
             sex  == input$map_sex) %>% 
      select(fips, rate)
    
    mn_map <- left_join(mn_map, rates, by = c("FIPS" = "fips"))
    
    bins   <- quantile(rates$rate)
    pal    <- colorBin("Blues", domain = mn_map$rate, bins = bins)
    labels <- 
      sprintf("<strong>%s</strong><br/>rate %g", mn_map$NAME, mn_map$rate) %>% 
      lapply(htmltools::HTML)
    
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
                title = paste0(latest, " rate of new cases <br>
                               all age groups <br>
                               per 100,000 persons"),
                na.label = "No data",
                opacity = 1)
    
  })
}