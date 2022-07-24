mod_controls_ui <- function(id) {
  ns   <- NS(id)
  vars <- get(paste0(id,"_control_vars"))
  tagList(
    radioButtons(ns("geo"), 
                 "Geographic Area",
                 choices = c(`State`  = "state",
                             `County` = "county")),
    lapply(names(vars),
           \(x) selectInput(inputId   = ns(x),
                            label     = vars[[x]],
                            choices   = NULL,
                            multiple  = ifelse(x == "location" & id == "plt", FALSE, TRUE),
                            selectize = F))
  )
}

mod_controls_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    r <- reactiveValues(
      mdat = NULL,
      vars = NULL
    )
    
    # Update inputs upon region selected, filter data
    observeEvent(input$geo, {
      mdat <- if (input$geo == "state") dat[dat$location == "Minnesota", ]
              else                      dat[dat$location != "Minnesota", ]
      mdat <- if (id == "plt")          mdat[mdat$type != "All cancer types combined", ]
              else                      mdat
      
      vars <- names(get(paste0(id,"_control_vars")))
      for (var in vars) {
        freezeReactiveValue(input, var)
        updateSelectInput(session,
                          var,
                          choices = shift_to_top(sort(unique(mdat[[var]])),"All"))  
      }
      r$mdat = mdat
      r$vars = vars
    })
    
    # Filter data upon user input
    rdat <- reactive({
      mdat          <- r$mdat
      vars          <- r$vars
      filtered_vars <- vars[sapply(vars, \(x) (!is.null(input[[x]])))]
      
      for (var in filtered_vars) {
        mdat <- mdat[mdat[[var]] %in% input[[var]], ]
      }
      mdat
    })
    
    return(rdat)
  })
}