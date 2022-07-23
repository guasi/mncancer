server <- function(input, output) {
  
  plt_dat <- reactive(mod_controls_server("plt"))
  mod_plot_server("plt", plt_dat())
  
  tbl_dat <- reactive(mod_controls_server("tbl"))
  mod_table_server("tbl", tbl_dat())
  
  mod_map_server("map")
}
