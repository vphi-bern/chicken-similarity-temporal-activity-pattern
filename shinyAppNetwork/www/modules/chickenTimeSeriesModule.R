chickenTimeSeriesTabItemUI <- function(id, title) {
  ns <- NS(id)
  
  tabItem(id,
    fluidRow(
      box(width = 9, title = "Time series", collapsible = TRUE, closable = FALSE,
        withSpinner(uiOutput(ns("timeSeriesOutput"))),
        footer = uiOutput(ns("timeSeriesOutputFooter"))
      ),
      box(width = 3, title = "Controls", status = "primary",
        pickerInput(
          inputId = ns("timeSeriesPenNumber"),
          multiple = FALSE,
          label = "Select a pen",
          choices = pensNumberName,
          selected = 11
        ),
        pickerInput(
          inputId = ns("timeSeriesBirdIds"),
          multiple = FALSE,
          label = "Select a bird",
          choices = c(),
          options = list(
            'live-search' = TRUE
          )
        ),
        pickerInput(
          inputId = ns("timeSeriesBirdDays"),
          multiple = TRUE,
          label = "Select a day",
          choices = tsColnames,
          selected = 1:10,
          options = list(
            'live-search' = TRUE,
            # 'max-options' = 10,
            # 'max-options-text' = 'Maximun 10 days',
            'size' = 8,
            'actions-box' = TRUE,
            'selected-text-format' = "count > 3",
            'count-selected-text' = "{0} days on a total of {1}"
          )
        ),
        actionButton(inputId = ns("plotTimeSeries"), label = "Update graphs", width = "100%", class = "btn-success", style = "color: #FFF")
      ),
      # column(width = 3,
      #   uiOutput(ns("chickenTimeSeriesSummary"))  
      # ),
      uiOutput(ns("chickenTimeSeriesDetails"))
    )
  )
}

chickenTimeSeriesTabItem <- function(input, output, session) {
  ns <- session$ns

  dataTableHenData <- reactiveValues(
    movements = NULL,
    selectedMovementsDay = NULL,
    selectedMovementsDayRanged = NULL,
    footerTimeSeriesMessage = NULL,
    clicked = FALSE
  )
  
  observe({
    if(is.null(input$timeSeriesPenNumber)) return()
    penNumber <- input$timeSeriesPenNumber

    birdNodes <- networkmeasures_DTWcluster %>%
      filter(Pen == penNumber) %>%
      select(Bird)

    birdNodesName <- c(setNames(sort(birdNodes$Bird), sapply(sort(birdNodes$Bird), function(x) {sprintf("Bird %s", x)})))
    updatePickerInput(session = session, inputId = "timeSeriesBirdIds", choices = birdNodesName)
  })
  
  observe({
    if(is.null(dataTableHenData$selectedMovementsDayRanged)) {
      dataTableHenData$footerTimeSeriesMessage = ""
    } else {
      if(nrow(dataTableHenData$selectedMovementsDayRanged) == nrow(dataTableHenData$selectedMovementsDay)) {
        dataTableHenData$footerTimeSeriesMessage = "Select a time series period zooming the graph"
      } else {
        dataTableHenData$footerTimeSeriesMessage = sprintf("Selected time period from %s to %s", format(min(dataTableHenData$selectedMovementsDayRanged$date), "%H:%M:%S"), format(max(dataTableHenData$selectedMovementsDayRanged$date), "%H:%M:%S"))
      }
    }
  })
  
  observeEvent(input$timeSeriesBirdDays, {
    
    henData <- getHenDataFromCsV(c(1))
    
    if(nrow(henData) > 0) {
      dataTableHenData$movements <- henData
      dataTableHenData$selectedMovementsDay <- dataTableHenData$movements[,c('time', 'date', input$timeSeriesBirdDays)]
      dataTableHenData$selectedMovementsDayRanged <- dataTableHenData$selectedMovementsDay
      dataTableHenData$footerTimeSeriesMessage = "Select a time series period zooming the graph"
    } else {
      dataTableHenData$movements <- NULL
      dataTableHenData$selectedMovementsDay <- NULL
      dataTableHenData$selectedMovementsDayRanged = NULL
      dataTableHenData$footerTimeSeriesMessage = ""
    }
  }, once = TRUE)
  
  observeEvent(input$timeSeriesBirdIds, {
    if(dataTableHenData$clicked) {
      prepareHendData(input$timeSeriesBirdIds, input$timeSeriesBirdDays)
      
      # if(!is.null(dataTableHenData$movements)) dataTableHenData$movements <- NULL
      # if(!is.null(dataTableHenData$selectedMovementsDay)) dataTableHenData$selectedMovementsDay <- NULL
      # if(!is.null(dataTableHenData$selectedMovementsDayRanged)) dataTableHenData$selectedMovementsDayRanged = NULL
    } else {
      dataTableHenData$clicked <- TRUE
    }
  })
  
  observeEvent(input$plotTimeSeries, {
    prepareHendData(input$timeSeriesBirdIds, input$timeSeriesBirdDays)
  })
  
  # observeEvent(input$downloadFilteredData, {
  #   print(nrow(dataTableHenData$selectedMovementsDayRanged))
  # })
  
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      paste(sprintf('pen_%s_bird_%s', isolate(input$timeSeriesPenNumber), isolate(input$timeSeriesBirdIds)), "csv", sep = ".")
    },
    content = function(file) {
      outputData <- dataTableHenData$selectedMovementsDayRanged %>% 
        mutate(time = format(date, format = "%H:%M:%S")) %>% 
        select(-c(date)) %>% 
        rename_at(vars(2:length(colnames(.))), function(x) sprintf('day_%s', x))
      
      write.table(outputData, file = file, sep = ",", row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  observeEvent(input$graphRangeSelection, {
    if(is.null(input$graphRangeSelection)) return()
    
    if(!is.null(input$graphRangeSelection$min) && !is.null(input$graphRangeSelection$max)) {
      timeMin = input$graphRangeSelection$min
      timeMax = input$graphRangeSelection$max
      
      dataTableHenData$selectedMovementsDayRanged <- isolate(dataTableHenData$selectedMovementsDay) %>%
        filter(between(date, as_datetime(timeMin/1000), as_datetime(timeMax/1000)))
    } else {
      dataTableHenData$selectedMovementsDayRanged <- isolate(dataTableHenData$selectedMovementsDay)
    }
  })

  output$timeSeriesOutput <- renderUI({

    validate(
      need(!is.null(dataTableHenData$selectedMovementsDay), 'No data to show')
    )
    
    validate(
      need(length(colnames(dataTableHenData$selectedMovementsDay)) > 2, 'Please select at least one day')
    )

    shinyjs::html("timeSeriesOutput", loadingMessage)

    daysToChart <- colnames(dataTableHenData$selectedMovementsDay %>% select(-c('time', 'date')))

    yaxis <- create_yaxis(naxis = length(daysToChart), heights = rep(1, length(daysToChart)), allowDecimals = FALSE, min = 1, max = 5, startOnTick = F, tickInterval = 1)

    yaxis <- map2(yaxis, daysToChart, function(x, y) {
      x$title <- list(text = sprintf("Day %s", y))
      x
    })

    class(yaxis) <- "hc_yaxis_list"

    graphHeight <- "950px"
    if(length(daysToChart) %in% 0:3) graphHeight <- "400px"
    else if(length(daysToChart) %in% 4:6) graphHeight <- "700px"
    else if(length(daysToChart) %in% 7:10) graphHeight <- "900px"

    # , type = "stock"
    timeSeriesGraph <- highchart(height = graphHeight) %>%
      hc_chart(
        useUTC = TRUE,
        zoomType = "x",
        events = list(
          selection = JS(sprintf("function(event) {
            var data = {min: null, max: null, '.nonce': Math.random()};
            if(typeof(event.xAxis) != 'undefined') {
              //alert(Highcharts.dateFormat('%%Y %%m %%d %%H:%%M:%%S', event.xAxis[0].max));
              var data = {min: event.xAxis[0].min, max: event.xAxis[0].max, '.nonce': Math.random()};
            }
            Shiny.setInputValue('%s', data);
          }", ns('graphRangeSelection')))
        )
      ) %>%
      hc_tooltip(
        crosshairs = TRUE, shared = TRUE,
        dateTimeLabelFormats = list(
          millisecond = '%H:%M:%S', second = '%H:%M:%S', minute = '%H:%M:%S', hour = '%H:%M:%S',
          day = '%H:%M:%S', week = '%H:%M:%S', month = '%H:%M:%S', year = '%H:%M:%S'
        ),
        headerFormat = '<span style="font-size: 10px"><strong>Time {point.key}</strong></span><br/>',
        pointFormat = '<span style="color:{point.color}">\u25CF</span> Day {series.name}: <b> Level {point.y}</b><br/>'
      ) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_legend(enabled = TRUE, labelFormat = 'Day {name}') %>%
      hc_navigator(enabled = FALSE) %>%
      hc_rangeSelector(enabled = FALSE) %>%
      hc_xAxis(
        crosshair = T,
        type = 'datetime',
        dateTimeLabelFormats = list(
          millisecond = '%H:%M:%S', second = '%H:%M:%S', minute = '%H:%M:%S', hour = '%H:%M:%S',
          day = '%H:%M:%S', week = '%H:%M:%S', month = '%H:%M:%S', year = '%H:%M:%S'
        )
      ) %>%
      hc_yAxis(categories = 1:4) %>%
      hc_plotOptions(
        series = list(
          events = list(legendItemClick = 'function(){return false;}'),
          dataGrouping = list(approximation = "open"),
          lineWidth = 1
        ),
        line = list(step = "left")
      ) %>%
      hc_yAxis_multiples(yaxis)

    yaxisSerie <- 0
    
    for(dayToChart in daysToChart) {
      
      selectedDay <- dataTableHenData$selectedMovementsDay[c(dayToChart, 'date')] %>% 
        rename("level" := all_of(dayToChart))
      
      X <- rle(selectedDay$level)
      
      selectedDay <- selectedDay[cumsum(c(1, X$lengths[-length(X$lengths)])), ] %>% 
        add_row(level = last(selectedDay$level), date = ISOdate(2020, 1, 1, 17, 0, 0, tz = "UTC"))
      
      chickenDataPerDayTS <- xts(selectedDay[['level']], selectedDay[['date']], tzone = "UTC", descr = sprintf("Day %s", dayToChart))
      timeSeriesGraph <- timeSeriesGraph %>%
        hc_add_series(chickenDataPerDayTS, name = dayToChart, type = "line", yAxis = yaxisSerie, step = "left")
        # hc_add_series(dataTableHenData$selectedMovementsDay[[dayToChart]], name = gsub("_", " ", dayToChart), type = "line", yAxis = yaxisSerie, step = "left")
        # hc_add_series(henDayData, name = gsub("_", " ", dayToChart), type = "line", yAxis = yaxisSerie, step = "left",  hcaes(y = day, x = time))

      yaxisSerie <- yaxisSerie + 1
    }

    fluidRow(column(12, timeSeriesGraph))
  })
  
  output$timeSeriesOutputFooter <- renderUI({
    fluidRow(
      column(10, dataTableHenData$footerTimeSeriesMessage),
      column(2, align = 'right',
        downloadButton(
          ns("downloadFilteredData"), icon = icon('download'), label = '',
          class = "btn-sm", title = 'Click to download'
        )
      )
    )
  })
  
  output$chickenTimeSeriesSummary <- renderUI({
    fluidRow(
      # infoBox("Group", "A", icon = icon("th-large"), width = 12)
      # infoBox("Pen", "Pen", icon = icon("square-o")),
      infoBox("Movements", "-", icon = icon("random"), width = 12)
    )
  })
  
  output$chickenTimeSeriesDetails <- renderUI({
    req(!is.null(dataTableHenData$selectedMovementsDayRanged) && nrow(dataTableHenData$selectedMovementsDayRanged) > 0 && length(colnames(dataTableHenData$selectedMovementsDayRanged)) > 2)

    allMovementsGroupedLevel <- melt(data = dataTableHenData$selectedMovementsDayRanged %>% select(-c('time', 'date')), measure.vars = colnames(dataTableHenData$selectedMovementsDayRanged %>% select(-c('time', 'date')))) %>% 
      rename(Day = variable, Level = value) %>% 
      mutate(extra = paste(Day, Level, sep = "-"))
    
    allMovementsGroupedLevelUnique <- allMovementsGroupedLevel[c(allMovementsGroupedLevel$extra[-1] != allMovementsGroupedLevel$extra[-nrow(allMovementsGroupedLevel)], TRUE),] %>% 
      group_by(Day) %>% slice(-1) %>% ungroup()
    
    # allMovementsGroupedLevelUnique <- allMovementsGroupedLevelUnique
    
    req(nrow(allMovementsGroupedLevelUnique) > 0)
    
    allMovementsGroupedLevelFormatTable <- allMovementsGroupedLevelUnique %>% 
      mutate(Level = sprintf("Level %s", Level)) %>% 
      group_by(Day, Level) %>%
      summarise(Total = n()) %>%
      ungroup() %>%
      dcast(Day ~ Level, value.var = "Total", fill = 0) %>%
      mutate(Total = rowSums(select(., -1)))
    
    # daysToChart <- colnames(dataTableHenData$selectedMovementsDayRanged %>% select(-c("time", "date")))
    daysToChart <- as.character(allMovementsGroupedLevelFormatTable$Day)
    
    totalColumns <- length(colnames(allMovementsGroupedLevelFormatTable)) - 1
    temporalMovements <- allMovementsGroupedLevelFormatTable %>% 
      unite("graph", 2:all_of(totalColumns), sep = ",", remove = TRUE)
    
    allMovementsGroupedLevelFormatTable$graph <- temporalMovements$graph
    
    for(i in 1:nrow(allMovementsGroupedLevelFormatTable)) {
      allMovementsGroupedLevelFormatTable$graph[i] <- spk_chr(
        as.numeric(strsplit(allMovementsGroupedLevelFormatTable$graph[i], ",")[[1]]), type = "pie", width = 30, height = 30
      )
    }
    
    allEdges <- data.frame(Day = character(), From = character(), To = character())
    for(dayToChart in daysToChart) {
      fromValues = uniqueMovements(dataTableHenData$selectedMovementsDayRanged[[dayToChart]])
      if(length(fromValues) > 1) {
        uniqueValues <- data.frame(Day = dayToChart, From = fromValues)
        uniqueValues$To <- c(uniqueValues$From[2:length(uniqueValues$From)], 0)
        
        allEdges <- rbind(allEdges, uniqueValues)
      }
    }

    allEdges <- allEdges %>% 
      filter(To > 0) %>% 
      group_by(Day, From, To) %>% 
      summarise(Movements = n()) %>% 
      ungroup() %>% 
      mutate(
        title = sprintf("From: %s, to: %s<br />%s mov", From, To, Movements),
        color = unname(levelColorPaletteSparkline[From])
      ) %>% arrange(To)
    
    edgesInternalGraph <- allEdges %>%
      group_by(To) %>% summarise(total = sum(Movements)) %>%
      mutate(color = unname(levelColorPaletteSparkline[To]))
    
    uniqueNodes <- sort(unique(c(allEdges$From, allEdges$To)))
    allNodes <- data.frame(id = uniqueNodes, label = uniqueNodes, title = paste("Level", uniqueNodes), color = unname(levelColorPaletteSparkline[uniqueNodes]), shape = "ellipse")
    
    fluidRow(
      column(12,
        box(width = 12, "Movements", class = "chicken-format-table",
          format_table(allMovementsGroupedLevelFormatTable %>% arrange(Day) %>% mutate(Day = sprintf("Day %s", Day)), list(
            'Day' = formatter("span", style = "font-weight:bold"),
            'Level 1' = color_bar("#A4BAE8"),
            'Level 2' = color_bar("#ED9C88"),
            'Level 3' = color_bar("#FFCC7F"),
            'Level 4' = color_bar("#87CA8B"),
            'Level 5' = color_bar("#B2D47F"),
            'Total' = color_bar("#E0E0E0")
          ), align = c('l', rep('r', length(colnames(allMovementsGroupedLevelFormatTable)) - 2), 'c'), col.names = c("", colnames(allMovementsGroupedLevelFormatTable)[2:(length(colnames(allMovementsGroupedLevelFormatTable)) - 1)], "")) %>% htmltools::HTML() %>% div() %>% spk_add_deps()
        )
      ),
      column(12,
        # box(width = ifelse(totalDays > 30, 12, ifelse(totalDays > 20, 9, ifelse(totalDays > 7, 6, 3))), class = "birds-plot-height",
        box(width = 12, class = "birds-plot-height",
          hchart(allMovementsGroupedLevelUnique %>% group_by(Day, Level) %>% summarise(Total = n()) %>% ungroup() %>% mutate(Day_numeric = as.numeric(as.character(Day))), "column", hcaes(x = "Day_numeric", y = "Total", group = "Level")) %>%
            hc_xAxis(
              # type = "category",
              # ordinal = FALSE,
              title = list(text = ""),
              labels = list(format = "Day {value}")
            ) %>%
            hc_tooltip(
              shared = TRUE,
              headerFormat = '<span style="font-size: 10px"><strong>Day {point.key}</strong></span><br/>',
              pointFormat = '<span style="color:{point.color}">\u25CF</span> Level {series.name}: <b>{point.y} mov</b><br/>'
            ) %>%
            hc_chart(height = 300) %>%
            hc_legend(itemDistance = 10) %>%
            hc_yAxis(title = list(text = "Movements")) %>% 
            hc_colors(unname(levelColorPaletteSparkline[sort(unique(allMovementsGroupedLevelUnique %>% group_by(Day, Level) %>% summarise(Total = n()) %>% '$'('Level')))])) %>% 
            hc_plotOptions(
              column = list(pointPadding = 0, groupPadding = 0.1)
            ) 
        ),
        box(width = 3, class = "birds-plot-height",
          hcboxplotBirds(x = allMovementsGroupedLevelUnique %>% group_by(Day, Level) %>% summarise(Total = n()) %>% '$'('Total'), var = allMovementsGroupedLevelUnique %>% group_by(Day, Level) %>% summarise(Total = n()) %>% '$'('Level')) %>%
            hc_colors(unname(levelColorPaletteSparkline[sort(unique(allMovementsGroupedLevelUnique %>% group_by(Day, Level) %>% summarise(Total = n()) %>% '$'('Level')))])) %>%
            hc_plotOptions(boxplot = list(colorByPoint = TRUE)) %>%
            # hc_tooltip(
            #   formatter = JS('function(){var output = "";if(this.series.name == "Series 1") {output = "<span><strong>Level " + this.point.name + "</strong></span><br /><span>Maximum: " + this.point.high +"</span><br /><span>Upper quartile: " + this.point.q3 + "</span><br /><span>Median: " + this.point.median +"</span><br /><span>Lower quartile: " + this.point.q1 + "</span><br /><span>Minimum: " + this.point.low + "</span>"} else {output = "<span>" + this.y + "</span>"} return output;}')
            # ) %>%
            hc_legend(enabled = FALSE) %>%
            hc_xAxis(title = list(text = "Level")) %>%
            hc_chart(type = "column", height = 300) %>%
            hc_yAxis(min = 0, maxPadding = 0, startOnTick = TRUE, endOnTick = TRUE)
        ),
        box(width = 3,
          # footer = actionLink(inputId = "toAwesome2", label = "More examples", icon = icon("plus"), style = "color: #d9534f"),
          highchart(height = 300) %>%
            hc_colors(unname(levelColorPaletteSparkline[sort(unique(allMovementsGroupedLevelUnique %>% '$'('Level')))])) %>%
            hc_chart(marginTop = 0, marginRight = 0, marginLeft = 0) %>%
            hc_plotOptions(
              pie = list(
                allowPointSelect = FALSE, cursor = 'pointer',
                showInLegend = TRUE, dataLabels = list(enabled = FALSE)
              ),
              series = list(
                point = list(events = list(legendItemClick = 'function(){return false;}'))
              )
            ) %>%
            hc_legend(itemDistance = 10) %>%
            hc_tooltip(
              headerFormat = '<span style="font-size: 10px"><b>Level</b>: {point.key}</span><br/>',
              valueSuffix = ' mov'
            ) %>%
            hc_add_series(allMovementsGroupedLevelUnique %>% group_by(Level) %>% summarise(Total = n()), "pie", hcaes(y = "Total", name = "Level"), name = "Movements")
        ),
        box(width = 3, class = "birds-plot-height",
          highchart(height = 300) %>%
            hc_add_series(edgesInternalGraph %>% mutate(label = "Total", color = "#E0E0E0") %>% group_by(label, color) %>% summarise(total = sum(total)), hcaes(y = "total", name = "label", color = "color"),
              type = "pie", size = '30%', allowPointSelect = FALSE,
              cursor = 'pointer', showInLegend = FALSE,
              dataLabels = list(enabled = FALSE),
              tooltip = list(
                headerFormat = '<span style="font-size: 11px">Total</span><br/>'
              )
            ) %>%
            hc_add_series(edgesInternalGraph, hcaes(y = "total", name = "To", color = "color"),
              type = "pie", size = '60%', innerSize = "30%", allowPointSelect = FALSE,
              cursor = 'pointer', showInLegend = T,
              dataLabels = list(enabled = FALSE),
              tooltip = list(
                headerFormat = '<span style="font-size: 11px">To level: {point.key}</span><br/>'
              )
            ) %>%
            hc_add_series(allEdges %>% group_by(From, To, color) %>% summarise(Movements = sum(Movements)) %>% arrange(To), hcaes(y = "Movements", name = "From", color = "color"),
              type = "pie", size = '100%',
              innerSize = "60%", allowPointSelect = FALSE,
              cursor = 'pointer', showInLegend = FALSE,
              dataLabels = list(enabled = FALSE),
              tooltip = list(
                headerFormat = '<span style="font-size: 11px">From level: {point.key}</span><br/>'
              )
            ) %>%
            hc_tooltip(
              pointFormat = '<span style="color:{point.color}">\u25CF</span><b> {point.y} mov</b><br/>'
            ) %>%
            hc_plotOptions(
              pie = list(point = list(events = list(legendItemClick = JS('function(){return false;}'))))
            ) %>%
            hc_legend(itemDistance = 10)    
        ),
        box(width = 3, class = "birds-plot-height",
          visNetwork(allNodes, allEdges %>% rename(to = To, from = From, value = Movements) %>% group_by(from, to, color) %>% summarise(value = sum(value)) %>% ungroup() %>% mutate(title = sprintf("From: %s, to: %s<br />%s mov", from, to, value)), height = "280px", width = "100%") %>%
            visEdges(
              arrows = list(to = list(enabled = TRUE, scaleFactor = .01), scaling = list(min = 1, max = 3)), arrowStrikethrough = FALSE,
              smooth = list(enabled = TRUE)
            ) %>%
            visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = FALSE, tooltipDelay = 0) %>%
            visLayout(randomSeed = 25)
        ),
        box(width = 12, class = "custom-graph-group-output",
          map(daysToChart, function(dayToChart) {
            allEdgesDay <- allEdges %>% filter(Day == dayToChart)
            allNodeDay <- allNodes %>% filter(id %in% unique(c(allEdgesDay$From, allEdgesDay$To)))
            visNetwork(allNodeDay, allEdgesDay %>% rename(to = To, from = From, value = Movements), height = "280px", width = "100%", main = sprintf("<span class='birds-network-title'>Day %s</span>", dayToChart)) %>%
              visEdges(
                arrows = list(to = list(enabled = TRUE, scaleFactor = .01), scaling = list(min = 1, max = 3)), arrowStrikethrough = FALSE,
                smooth = list(enabled = TRUE)
              ) %>%
              visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = FALSE, tooltipDelay = 0) %>%
              visLayout(randomSeed = 25)
          }) %>% hw_grid(rowheight = 300, ncol = ifelse(length(daysToChart) < 4, length(daysToChart), 4)) %>% browsable()
        )
      )
    )
  })
  
  prepareHendData <- function(birdIds, birdDays) {
    henData <- getHenDataFromCsV(birdIds)
    dataTableHenData$movements <- henData
    
    if(length(birdIds) > 0) {
      if(nrow(henData) > 0) {
        dataTableHenData$selectedMovementsDay <- dataTableHenData$movements[,c('time', 'date', birdDays)]
        dataTableHenData$selectedMovementsDayRanged <- dataTableHenData$selectedMovementsDay
        dataTableHenData$footerTimeSeriesMessage = "Select a time series period zooming the graph"
      } else {
        dataTableHenData$movements <- NULL
        dataTableHenData$selectedMovementsDay <- NULL
        dataTableHenData$selectedMovementsDayRanged = NULL
        dataTableHenData$footerTimeSeriesMessage = ""
      }
    } else {
      dataTableHenData$movements <- NULL
      dataTableHenData$selectedMovementsDay <- NULL
      dataTableHenData$selectedMovementsDayRanged = NULL
      dataTableHenData$footerTimeSeriesMessage = ""
    }
  }
}