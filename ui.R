tab_plot <- tabItem(tabName = "plots",
                    fluidRow(
                      valueBoxOutput("vbox_count"),
                      valueBoxOutput("vbox_rate"),
                      valueBoxOutput("vbox_change"),
                    ),
                    fluidRow(
                      column(width = 8,
                             tabBox(width = NULL,
                               tabPanel("Latest",    plotlyOutput("latest")),
                               tabPanel("Over Time", plotlyOutput("historical", height = 500)),
                               tabPanel("By Sex",    plotlyOutput("sex")),
                               tabPanel("By Age",    plotlyOutput("age"))
                             )
                      ),
                      column(width = 4,
                             box(width = NULL,
                                 radioButtons("plot_geo","Geographic Area",
                                              choices = c(`State`  = "state",
                                                          `County` = "county")),
                                 div(id = "plot_controls")
                             ),
                             box(width = NULL, 
                                 title = "Notice", icon = icon("info"), #status = "warning", solidHeader = TRUE,
                                 includeHTML("include/info.html"))
                      )
                    )
)

tab_table <- tabItem(tabName = "tables", 
                     fluidRow(
                       box(width = 8,
                           DTOutput("dt_table")
                       ),
                       box(width = 4,
                           radioButtons("geo","Geographic Area", 
                                        choices = c(`State`  = "state",
                                                    `County` = "county")),
                           div(id = "controls")
                       )
                     )
)

tab_map <- tabItem(tabName = "maps",
                   fluidRow(
                     box(width = 8,
                         leafletOutput("leaflet_map", height = 500)
                     ),
                     box(width = 4,
                         div(id = "map_controls")
                     )
                   )
)

dashboardPage(
  dashboardHeader(title = "MN Cancer Rates"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Charts", tabName = "plots", icon = icon("chart-bar")),
      menuItem("Maps", tabName = "maps", icon = icon("map")),
      menuItem("Tables", tabName = "tables", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tab_plot,
      tab_map,
      tab_table
    )
  )
)
