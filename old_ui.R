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
      tabItem(tabName = "plots", mod_plot_ui("plt")),
      tabItem(tabName = "maps", mod_map_ui("map")),
      tabItem(tabName = "tables", mod_table_ui("tbl"))
    )
  )
)
