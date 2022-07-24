
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
                    tabPanel("Over Time", plotlyOutput(ns("historical"))),
                    tabPanel("By Sex",    plotlyOutput(ns("sex"))),
                    tabPanel("By Age",    plotlyOutput(ns("age"))),
                    div(checkboxInput(ns("plotall"), "Plot all indicators"),
                        style = "padding: 0 0 0 25px", )
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
    
    r <- reactiveValues(
      latest = NULL,    # latest year
      n = 5,            # indicators to plot
      s = "Top five"    # string for plot title
    )
    
    # PLOTALL checkbox ---------------------------------------------------
    observeEvent(c(input$plotall, input$type), ignoreInit = T, {
      if (input$plotall) {
        updateSelectInput(session,
                          "type",
                          selected = character(0))
        r$n <- n_distinct(this_data()$type)
        r$s <- "All"
      } else if (length(input$type) == 0) {
        r$n <- 5
        r$s <- "Top five"
      } else {
        r$n <- n_distinct(this_data()$type)
        r$s <- "Selected"
      }
    })
    
    observeEvent(input$type, {
      freezeReactiveValue(input, "plotall")
      updateCheckboxInput(session,
                          "plotall",
                          value = F)
    })
    
    # VALUE BOXES --------------------------------------------------------
    vbox <- reactive({
      req(this_data())
      mdat     <- this_data()
      yr       <- sort(unique(mdat$year))
      r$latest <- latest <- tail(yr,1)
      prev     <- yr[length(yr)-1]

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

      data.frame(v_count  = top$count,
                 v_rate   = top$rate,
                 v_change = change,
                 s_count  = vbox_count,
                 s_rate   = vbox_rate,
                 s_change = vbox_change)
    })
    
    output$count <- renderValueBox({
      valueBox(
        value    = vbox()$v_count,
        subtitle = vbox()$s_count,
        icon     = icon("users"),
        color    = "blue"
      )
    })
    output$rate <- renderValueBox({
      valueBox(
        value    = vbox()$v_rate,
        subtitle = vbox()$s_rate,
        icon     = icon("users"),
        color    = "yellow"
      )
    })
    output$change <- renderValueBox({
      valueBox(
        value    = vbox()$v_change,
        subtitle = vbox()$s_change,
        icon     = icon(ifelse(vbox()$v_change < 0, "arrow-down", "arrow-up")),
        color    = ifelse(vbox()$v_change < 0, "green", "red")
      )
    })
    
    # PLOTS --------------------------------------------------------------
    output$latest <- renderPlotly({
      req(input$location, this_data(), r$latest)
      ptitle <- paste0(input$location,", ",r$latest, 
                       ", rate of new cases \n all age groups, male and female",
                       "\n", r$s, " indicators")
      
      this_data() %>%
        filter(year == r$latest,
               age  == "All Ages",
               sex  == "All") %>% 
        arrange(rate) %>% 
        tail(r$n) %>% 
        plot_ly(x = ~reorder(type, -rate), y = ~rate, type = "bar",
                texttemplate = "%{y}") %>%
        my_layout(title = ptitle)
    })
    
    output$historical <- renderPlotly({
      req(input$location, this_data())
      mdat <- this_data()
      if (input$location == "Minnesota") mdat <- mdat[!grepl("\\-", mdat$year), ]
      ptitle <- paste0(input$location,
                       ", rate of new cases \n all age groups, male and female",
                       "\n", r$s, " indicators")

      mdat %>%
        filter(age == "All Ages",
               sex == "All") %>% 
        arrange(year, rate) %>% 
        filter(type %in% tail(type, r$n)) %>% 
        plot_ly(x = ~year, y = ~rate, split = ~reorder(type,-rate)) %>%
        add_lines() %>%
        my_layout(title = ptitle)
    })
    
    output$sex <- renderPlotly({
      req(input$location, this_data())
      ptitle <- paste0(input$location,", ", r$latest, 
                       ", rate of new cases \n all age groups",
                       "\n", r$s, " indicators")
      
      this_data() %>%
        filter(year == r$latest,
               age  == "All Ages") %>%
        arrange(sex, desc(rate)) %>% 
        filter(type %in% head(type, r$n)) %>% 
        plot_ly(x = ~reorder(type, -rate), y = ~rate, split = ~sex) %>%
        add_markers() %>%
        my_layout(title = ptitle)
    })
    
    output$age <- renderPlotly({
      req(input$location, this_data())
      ptitle <- paste0(input$location,", ", r$latest, 
                       ", rate of new cases \n female and male",
                       "\n", r$s, " indicators")
      
      this_data() %>%
        filter(year == r$latest,
               sex  == "All") %>%
        arrange(age, rate) %>% 
        filter(type %in% tail(type, r$n)) %>% 
        plot_ly(x = ~reorder(type, -rate), y = ~rate, split = ~age) %>%
        add_markers() %>%
        my_layout(title = ptitle)
    })
  })
}
