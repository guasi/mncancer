
mod_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(width = 8,
          DTOutput(ns("dt_table"))
      ),
      box(width = 4,
          mod_controls_ui("tbl")
      )
    )
  )
}

mod_table_server <- function(id, this_data) {
  moduleServer(id, function(input, output, session) {
    
    output$dt_table <- renderDT({
      req(input$geo, this_data())
      mdat <- this_data()
      
      # variables to display
      mdat <- mdat[ , c(names(tbl_control_vars), "population", "count", "rate", "ci")]
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
    }
  )
}
