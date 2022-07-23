
mod_plot_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      valueBoxOutput(ns("count")),
      valueBoxOutput(ns("rate")),
      valueBoxOutput(ns("change"))
    ),
    fluidRow(
      column(width = 8,
             tabBox(width = NULL,
                    tabPanel("Latest",    plotlyOutput(ns("latest"))),
                    tabPanel("Over Time", plotlyOutput(ns("historical"), height = 500)),
                    tabPanel("By Sex",    plotlyOutput(ns("sex"))),
                    tabPanel("By Age",    plotlyOutput(ns("age")))
             )
      ),
      column(width = 4,
             box(width = NULL,
                 mod_controls_ui("plt")
             ),
             box(width = NULL, 
                 title = "Notice", icon = icon("info"),
                 includeHTML("include/info.html"))
      )
    )
  )
}

mod_plot_server <- function(id, this_data) {
  moduleServer(id, function(input, output, session) {
    
    # VALUE BOXES --------------------------------------------------------
    rea <- reactive({
      req(this_data())
      mdat   <- this_data()
      yr     <- sort(unique(mdat$year))
      latest <- tail(yr,1)
      prev   <- yr[length(yr)-1]

      temp <- mdat %>%
        filter(age == "All Ages",
               sex == "All")

      if (nrow(temp) == 0) {
        top    <- data.frame(type  = unique(mdat$type),
                             count = NA,
                             rate  = NA)
        change <- 0
      } else {
        top <- temp %>%
          filter(year == latest) %>%
          mutate(rank = rank(rate)) %>%
          filter(rank == last(sort(rank)))
        
        top.prev <- temp %>%
          filter(year == prev,
                 type == top$type)
        
        change <- round(top$rate - top.prev$rate, 2)
      }
      
      vbox_count  = HTML("New cases<br>", top$type,
                         "<br> in", input$location, "in", latest,
                         "<br> all ages, female and male")
      vbox_rate   = HTML("New cases per 100,100 persons<br>", top$type,
                         "<br> in", input$location, "in", latest,
                         "<br> all ages, female and male")
      vbox_change = HTML("New cases rate change<br>", top$type,
                         "<br> in", input$location,"from", prev, "to", latest,
                         "<br> all ages, female and male")

      data.frame(latest   = latest,
                 v_count  = top$count,
                 v_rate   = top$rate,
                 v_change = change,
                 s_count  = vbox_count,
                 s_rate   = vbox_rate,
                 s_change = vbox_change)
    })
    
    output$count <- renderValueBox({
      valueBox(
        value    = rea()$v_count,
        subtitle = rea()$s_count,
        icon     = icon("users"),
        color    = "blue"
      )
    })
    output$rate <- renderValueBox({
      valueBox(
        value    = rea()$v_rate,
        subtitle = rea()$s_rate,
        icon     = icon("users"),
        color    = "yellow"
      )
    })
    output$change <- renderValueBox({
      valueBox(
        value    = rea()$v_change,
        subtitle = rea()$s_change,
        icon     = icon(ifelse(rea()$v_change < 0, "arrow-down", "arrow-up")),
        color    = ifelse(rea()$v_change < 0, "green", "red")
      )
    })
    
    # PLOTS --------------------------------------------------------------
    output$latest <- renderPlotly({
      req(input$location, this_data(), rea()$latest)
      ptitle <- paste0(input$location,", ",rea()$latest, ", rate of new cases \n all age groups, male and female")
      
      this_data() %>%
        filter(year == rea()$latest,
               age  == "All Ages",
               sex  == "All") %>%
        plot_ly(x = ~reorder(type,-rate), y = ~rate, type = "bar",
               texttemplate = "%{y}") %>%
        my_layout(title = ptitle)
    })
    
    output$historical <- renderPlotly({
      req(input$location, this_data())
      mdat <- this_data()
      if (input$location == "Minnesota") mdat <- mdat[!grepl("\\-", mdat$year), ]
      ptitle <- paste0(input$location,", rate of new cases \n all age groups, male and female")

      mdat %>%
        filter(age == "All Ages",
               sex == "All") %>%
        plot_ly(x = ~year, y = ~rate, color = ~type) %>%
        add_lines() %>%
        my_layout(title = ptitle)
    })
    
    output$sex <- renderPlotly({
      req(input$location, this_data())
      ptitle <- paste0(input$location,", ",rea()$latest, ", rate of new cases \n all age groups")
      
      this_data() %>%
        filter(year == rea()$latest,
               age  == "All Ages") %>%
        plot_ly(x = ~reorder(type,-rate), y = ~rate, color = ~sex) %>%
        add_markers() %>%
        my_layout(title = ptitle)
    })
    
    output$age <- renderPlotly({
      req(input$location, this_data())
      ptitle <- paste0(input$location,", ",rea()$latest, ", rate of new cases \n female and male")
      
      this_data() %>%
        filter(year == rea()$latest,
               sex  == "All") %>%
        plot_ly(x = ~reorder(type,-rate), y = ~rate, color = ~age) %>%
        add_markers() %>%
        my_layout(title = ptitle)
    })
  })
}
