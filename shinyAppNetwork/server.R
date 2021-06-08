shinyServer(function(input, output, session) {
 
  observe_helpers()
  
  output$sidebarMenuUI <- renderMenu({
    sidebarMenu(id = "sidebarmenu",
      menuItem("Project", tabName = "chickenProject", icon = icon("book"), selected = TRUE),
      menuItem("Network", tabName = "chickenNetwork", icon = icon("connectdevelop")),
      menuItem("Time Series", tabName = "chickenTimeSeries", icon = icon("area-chart")),
      menuItem("Download", tabName = "chickenDownload", icon = icon("download"))
    )
  })
  
  # output$homeProjectInformation <- renderUI({
  #   fluidRow(
  #     column(12,
  #       box(width = 12, title = NULL)
  #     )
  #   )
  # })

  ##########################################
  ##########################################
  ##
  ## Render Modules
  ##
  ##########################################
  ##########################################
  chickenProjectModulesOutput <- callModule(chickenProjectTabItem, "chickenProject")
  chickenNetworkModulesOutput <- callModule(chickenNetworkTabItem, "chickenNetwork")
  chickenTimeSeriesModulesOutput <- callModule(chickenTimeSeriesTabItem, "chickenTimeSeries")
  chickenDownloadModulesOutput <- callModule(chickenDownloadTabItem, "chickenDownload")
})