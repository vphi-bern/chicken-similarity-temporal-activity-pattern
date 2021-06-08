dashboardPage(
  header = dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Main menu"),
      img(src = "chicken.svg")
    ), titleWidth = 170,
    userOutput("userVPHI"),
    controlbarIcon = shiny::icon("gears")
  ),
  sidebar = dashboardSidebar(width = 170, sidebarMenuOutput("sidebarMenuUI"), disable = F, collapsed = T),
  body = dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/shinyAppNetwork.css")
    ),
    tabItems(
      chickenProjectTabItemUI("chickenProject", title = "Project"),
      chickenNetworkTabItemUI("chickenNetwork", title = "Network"),
      chickenTimeSeriesTabItemUI("chickenTimeSeries", title = "Time Series"),
      chickenDownloadTabItemUI("chickenDownload", title = "Download")
    )
  ),
  footer = dashboardFooter(
    left = HTML("Developed by <a href='http://www.vphi.ch' target='_blank'>Veterinary Public Health Institute (VPHI) Team</a>, &#169; All rights reserved"),
    right = "Bern, 2021"
  )
)