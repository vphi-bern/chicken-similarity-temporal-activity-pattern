chickenNetworkTabItemUI <- function(id, title) {
  ns <- NS(id)
  
  tabItem(id,
    fluidRow(
      box(width = 9, title = "Network",
        withSpinner(visNetworkOutput(ns("networkBird"), height = "650px", width = "100%")),
        footer = fluidRow(
          column(10, uiOutput(ns("networkLegendTitleItems"))),
          column(2, style = "text-align:right;",
            actionButton(ns("networkResetZoom"), label = NULL, NULL, icon = icon("arrows")),
            bsPopover(ns("networkResetZoom"), "Zoom", "Reset zoom graph", options = list(container = "body"))
          ),
          column(12, uiOutput(ns("networkIsolateNodes")))
        )
      ),
      box(width = 3, title = "Controls", status = "primary",
        pickerInput(
          inputId = ns("penNumberValue"),
          multiple = FALSE,
          label = "Select a pen",
          choices = pensNumberName,
          selected = 11
        ),
        pickerInput(
          inputId = ns("birdNetworkNodes"),
          multiple = TRUE,
          label = "Select a bird",
          choices = c(),
          # selected = birdNodesName,
          options = list(
            'live-search' = TRUE,
            'actions-box' = TRUE,
            'selected-text-format' = "count > 3",
            'count-selected-text' = "{0} birds choosed (on a total of {1})"
          )
        ),
        sliderTextInput(
          inputId = ns("birdEdgesRangeValue"),
          label = "Extent of association range value:",
          choices = getAllExtentOfAssociation(),
          selected = c(0.006000436, max(getAllExtentOfAssociation())),
          force_edges = T
        ) %>% helper(type = "inline", title = "Extent of association range value", content = c("Extent of association helper content"), size = "m", colour = "#428bca"),
        # numericRangeInput('birdBetweennessRangeValue', label = 'Choose betweenness range value:', value = c(min(nodesS$Betweenness), max(nodesS$Betweenness))),
        # numericRangeInput('birdCetralityRangeValue', label = 'Choose centrality range value:', value = c(min(nodesS$Centrality), max(nodesS$Centrality))),
        sliderTextInput(
          inputId = ns("birdBetweennessRangeValue"),
          label = "Choose betweenness range value:",
          choices = sort(unique(networkmeasures_DTWcluster$Betweenness)),
          selected = c(min(unique(networkmeasures_DTWcluster$Betweenness)), max(unique(networkmeasures_DTWcluster$Betweenness))),
          force_edges = T
        ) %>% helper(type = "inline", title = "Betweenness range value", content = c("Betweenness helper content"), size = "m", colour = "#428bca"),
        sliderTextInput(
          inputId = ns("birdCetralityRangeValue"),
          label = "Choose centrality range value:",
          choices = sort(unique(networkmeasures_DTWcluster$Centrality)),
          selected = c(min(unique(networkmeasures_DTWcluster$Centrality)), max(unique(networkmeasures_DTWcluster$Centrality))),
          force_edges = T
        ) %>% helper(type = "inline", title = "Centrality range value", content = c("Centrality helper content"), size = "m", colour = "#428bca"),
        prettyCheckbox(
          inputId = ns("networkHideIsolateNodes"),
          label = "Hide isolate nodes",
          value = F, status = "primary", shape = "curve"
        ),
        pickerInput(
          inputId = ns("networkLayout"), label = "Network layout",
          choices = networkLayoutsList, selected = "layout_in_circle",
          options = list(title = "Select a layout")
        ),
        pickerInput(
          inputId = ns("networkNodesColorBy"), label = "Nodes color by",
          choices = nodesColorByProperties, selected = "Cluster",
          options = list(title = "Select a propertie")
        ),
        prettyCheckbox(
          inputId = ns("groupNodesByIcons"),
          label = "Use different icons by group",
          value = FALSE, status = "primary", shape = "curve"
        ),
        conditionalPanel(
          condition = "input.groupNodesByIcons == true", ns = ns,
          pickerInput(
            inputId = ns("networkNodesIconBy"), label = "Nodes icon by",
            choices = nodesColorByProperties, selected = "Community_ID",
            options = list(title = "Select a propertie")
          )
        ),
        pickerInput(
          inputId = ns("networkReorderNodesBy"), label = "Order nodes by",
          choices = nodesReorderBy, selected = "Cluster",
          options = list(title = "Select a propertie")
        ),
        sliderTextInput(
          inputId = ns("networkNodesSize"),
          label = "Nodes size",
          choices = 1:100,
          selected = 50,
          force_edges = T
        ),
        prettyCheckbox(
          inputId = ns("networkSmoothEdges"),
          label = "Edges smooth curves",
          value = FALSE, status = "primary", shape = "curve"
        ),
        conditionalPanel(
          condition = "input.networkSmoothEdges == true", ns = ns,
          pickerInput(
            inputId = ns("networkEdgesSmoothType"), label = "Edges smooth type",
            choices = networkSmoothType, selected = "dynamic",
            options = list(title = "Select a propertie")
          )
        )
      ),
      uiOutput(ns("chickenNetworkSummary"))
    )
  )
}

chickenNetworkTabItem <- function(input, output, session) {
  ns <- session$ns

  dataTableFiltered <- reactiveValues(
    nodes = NULL,
    edges = NULL,
    isolateNodes = NULL,
    selectedPenDataFrame = NULL
  )
  
  observe({
    if(is.null(input$birdEdgesRangeValue) || is.null(input$networkHideIsolateNodes) || is.null(input$networkReorderNodesBy) || is.null(input$networkNodesColorBy) || is.null(input$birdNetworkNodes) || is.null(input$birdBetweennessRangeValue) || is.null(input$birdCetralityRangeValue) || is.null(input$networkNodesSize) || is.null(input$networkNodesIconBy)) return()
  
    varColumnNameToArrange <- rlang::sym(input$networkReorderNodesBy)
    varColumnNameNodesColorBy <- rlang::sym(input$networkNodesColorBy)
    
    penSelected <- getPenIDByChickenID(first(input$birdNetworkNodes))
    
    selectedPenDataFrame <- switch(as.character(penSelected), "11" = aimat11, "12" = aimat12, "13" = aimat13, "14" = aimat14)
    
    nodesS <- data.frame(id = as.integer(colnames(selectedPenDataFrame %>% select(-c("hens")))))
    nodesS <- merge(nodesS, networkmeasures_DTWcluster %>% select(Bird, Pen, Pen_ID, Betweenness, Centrality, Community, Community_ID, Status, Cluster), by.x = "id", by.y = "Bird", all.x = T) %>%
      mutate(
        title =  sprintf("<p style = 'margin-bottom:0;'><b>Bird:</b> %s<br><b>Pen:</b> %s<br><b>Community:</b> %s<br><b>Cluster:</b> %s<br><b>Betweenness:</b> %s<br><b>Centrality:</b> %s<br><b>Status:</b> %s</p>", id, Pen, Community, Cluster, Betweenness, Centrality, Status)
      ) %>% arrange(id)

    edgesS <- melt(data = selectedPenDataFrame, id.vars = c("hens")) %>%
      mutate(variable = as.integer(as.character(variable)), hens = as.integer(hens)) %>%
      rename(from = hens, to = variable) %>%
      filter(from > to, value > 0)

    nodessValue <- nodesS %>%
      filter(
        id %in% input$birdNetworkNodes,
        Betweenness >= input$birdBetweennessRangeValue[1] & Betweenness <= input$birdBetweennessRangeValue[2],
        Centrality >= input$birdCetralityRangeValue[1] & Centrality <= input$birdCetralityRangeValue[2]
      ) %>%
      arrange(!!varColumnNameToArrange)

    nodessValue$icon.color <- birdsClusterColorPalette[nodessValue[,input$networkNodesColorBy]]
    
    if(input$groupNodesByIcons == TRUE) {
      nodessValue$icon.code <- sapply(nodessValue[,input$networkNodesIconBy], function(x) fontAwesomeIconsBirds[[x]][["code"]])
      nodessValue$icon.name <- sapply(nodessValue[,input$networkNodesIconBy], function(x) fontAwesomeIconsBirds[[x]][["name"]])
    } else {
      nodessValue$icon.code <- sapply(nodessValue[,input$networkNodesIconBy], function(x) fontAwesomeIconsBirds[[2]][["code"]])
      nodessValue$icon.name <- sapply(nodessValue[,input$networkNodesIconBy], function(x) fontAwesomeIconsBirds[[2]][["name"]])
    }
    
    edgesSValue <- edgesS %>%
      filter(
        value >= input$birdEdgesRangeValue[1] & value <= input$birdEdgesRangeValue[2],
        from %in% nodessValue$id,
        to %in% nodessValue$id
      )

    isolateNodes <- nodessValue %>%
      filter(!id %in% unique(c(edgesSValue$from, edgesSValue$to)))

    if(input$networkHideIsolateNodes) {
      nodessValue <- nodessValue %>%
        filter(id %in% unique(c(edgesSValue$from, edgesSValue$to)))
    }

    colorFromToMergeNodes <- nodessValue %>%
      select(id, !!varColumnNameNodesColorBy) %>%
      rename("fromItem" := !!varColumnNameNodesColorBy) %>%
      mutate(toItem = fromItem)

    edgesSValue <- merge(edgesSValue, colorFromToMergeNodes %>% select(id, fromItem), by.x = "from", by.y = "id", all.x = T)
    edgesSValue <- merge(edgesSValue, colorFromToMergeNodes %>% select(id, toItem), by.x = "to", by.y = "id", all.x = T)
    edgesSValue <- edgesSValue %>% mutate(
      item = if_else(fromItem == toItem, as.integer(toItem), NA_integer_),
      color = if_else(is.na(item), "#808080", birdsClusterColorPalette[item]),
      title = sprintf("Extent of association between %s and %s: <b>%s</b>", from, to, value)
    )

    dataTableFiltered$nodes <- nodessValue
    dataTableFiltered$edges <- edgesSValue
    dataTableFiltered$isolateNodes <- isolateNodes
  })
  
  observe({
    if(is.null(input$penNumberValue)) return()
    penNumber <- input$penNumberValue
    
    birdNodes <- networkmeasures_DTWcluster %>%
      filter(Pen == penNumber) %>%
      select(Bird)
    
    birdNodesName <- c(setNames(sort(birdNodes$Bird), sapply(sort(birdNodes$Bird), function(x) {sprintf("Bird %s", x)})))
    updatePickerInput(session = session, inputId = "birdNetworkNodes", choices = birdNodesName, selected = birdNodesName)
  })
  
  output$networkBird <- renderVisNetwork({
    if(is.null(dataTableFiltered$nodes) || is.null(dataTableFiltered$edges)) return()

    validate(
      need(length(input$birdNetworkNodes) > 1, 'Check at least two birds!')
    )

    visNetwork(dataTableFiltered$nodes, dataTableFiltered$edges) %>%
      visIgraphLayout(layout = input$networkLayout, randomSeed = 123) %>%
      visNodes(shape = "icon", icon = list(face = "FontAwesome", size = input$networkNodesSize), physics = input$networkPhysicsNodes, shadow = list(enabled = T, size = 3, x = 2, y = 2)) %>%
      visEdges(physics = F, smooth = list(enabled = input$networkSmoothEdges, type = input$networkEdgesSmoothType)) %>%
      visOptions(
        selectedBy = list(
          variable = unname(nodesPropertiesGroupBy[input$networkNodesColorBy])
        ),
        highlightNearest = list(
          enabled = T, hover = F, degree = 1,
          hideColor = "rgba(200, 200, 200, 0.3)"
        ),
        nodesIdSelection = F
      ) %>%
      visPhysics(stabilization = FALSE) %>%
      addFontAwesome(name = "font-awesome-visNetwork")
  })
  
  output$networkLegendTitleItems <- renderUI({
      if(is.null(dataTableFiltered$nodes) || is.null(dataTableFiltered$edges)) return()

      req(nrow(dataTableFiltered$nodes) > 0)

      legendColorValues <- dataTableFiltered$nodes %>%
        group_by_at(vars(unname(nodesPropertiesGroupBy[input$networkNodesColorBy]), "icon.color")) %>%
        summarise() %>% as.data.frame()
      colnames(legendColorValues) <- c("item", "color")

      legendIconValues <- dataTableFiltered$nodes %>%
        group_by_at(vars(unname(nodesPropertiesGroupBy[input$networkNodesIconBy]), "icon.name")) %>%
        summarise() %>% as.data.frame()
      colnames(legendIconValues) <- c("item", "icon")
      
      fluidRowNodesIcon <- fluidRow()
      if(input$groupNodesByIcons == TRUE) {
        fluidRowNodesIcon <- lapply(1:nrow(legendIconValues), function(j) {
          tags$span(class = "label label-legend-item label-legend-icon-item", style = sprintf("background-color:%s!important;", "#808080"), icon(name = legendIconValues[j, "icon"]), sprintf("%s %s", unname(nodesColorByProperties2[input$networkNodesIconBy]), legendIconValues[j, "item"]))
        })
      }

      req(nrow(legendColorValues) > 0)

      fluidRow(
        column(12,
          tags$span(class = "label label-legend-title", "Legend:"),
          tags$br(),
          lapply(1:nrow(legendColorValues), function(i) {
            tags$span(class = "label label-legend-item", style = sprintf("background-color:%s!important;", legendColorValues[i, "color"]), sprintf("%s %s", unname(nodesColorByProperties2[input$networkNodesColorBy]), legendColorValues[i, "item"]))
          }),
          fluidRowNodesIcon
          # conditionalPanel(
          #   condition = "input.groupNodesByIcons == true", ns = ns,
          #   lapply(1:nrow(legendIconValues), function(j) {
          #     tags$span(class = "label label-legend-item label-legend-icon-item", style = sprintf("background-color:%s!important;", "#808080"), icon(name = legendIconValues[j, "icon"]), sprintf("%s %s", unname(nodesColorByProperties2[input$networkNodesIconBy]), legendIconValues[j, "item"]))
          #   })
          # )
          
        )
      )
    })

    output$networkIsolateNodes <- renderUI({
      if(is.null(dataTableFiltered$isolateNodes)) return()
      isolateNodes <- dataTableFiltered$isolateNodes
      req(nrow(isolateNodes) > 0)
      fluidRow(
        column(12,
          tags$span(class = "label label-nodes-title", sprintf("Isolate nodes (%s):", nrow(isolateNodes))),
          tags$br(),
          lapply(1:nrow(isolateNodes), function(i) {
            tags$span(class = "label label-nodes-item", style = sprintf("background-color:%s!important;", isolateNodes[i, "icon.color"]), title = sprintf("Bird %s", isolateNodes[i, "id"]), icon(name = isolateNodes[i, "icon.name"]), isolateNodes[i, "id"])
          })
        )
      )
    })

    observeEvent(input$networkResetZoom, {
      visNetworkProxy(ns("networkBird")) %>%
        visFit(animation = FALSE)
    }, ignoreInit = TRUE)
    
  #########################################
  #########################################
  #
  # 
  #
  #########################################
  #########################################
  output$chickenNetworkSummary <- renderUI({
    if(is.null(dataTableFiltered$nodes) || is.null(dataTableFiltered$edges)) return()
    
    req(nrow(dataTableFiltered$edges) > 1)
    req(nrow(dataTableFiltered$nodes) > 0)
    
    edgesFiltered <- dataTableFiltered$edges %>%
      mutate(var = "Extent of association")
    
    nodesFiltered <- dataTableFiltered$nodes 
    nodesFilteredClusterGrouped <- nodesFiltered %>%
      group_by(Cluster, Community) %>%
      summarise(Total = n()) %>%
      ungroup() %>%
      mutate(Cluster = sprintf("Cluster %s", Cluster), Community = sprintf("Community %s", Community)) %>%
      dcast(Cluster ~ Community, value.var = c("Total"), fill = 0)
    
    nodesFilteredClusterGrouped$Total <- rowSums(nodesFilteredClusterGrouped %>% select(-1))
    
    fluidRow(
      column(12,
        box(width = 8, title = NULL, class = "birds-plot-height",
          hchart(edgesFiltered$value, type = "histogram") %>%
            hc_chart(height = 300) %>%
            # hc_plotOptions(
            #   histogram = list(binsNumber = "sturges")
            # ) %>%
            hc_legend(enabled = FALSE) %>%
            hc_xAxis(title = list(text = "Extent of association")) %>%
            hc_yAxis(title = list(text = "Frequency"))
        ), 
        box(width = 4, title = NULL, class = "birds-plot-height",
          hcboxplotBirds(edgesFiltered$value, edgesFiltered$var, name = "Boxplot") %>% hc_chart(type = "column") %>%
            hc_chart(type = "column", height = 300) %>%
            hc_legend(itemDistance = 10) %>%
            hc_yAxis(maxPadding = 0, startOnTick = TRUE, endOnTick = TRUE)
        ),
        box(width = 12,
          format_table(nodesFilteredClusterGrouped, formatters = list(
            'Cluster' = formatter("span", style = "font-weight:bold"),
            area(col = 2:(length(colnames(nodesFilteredClusterGrouped)) - 1)) ~ color_bar("#E0E0E0")
            # 'Max' = color_bar("#E0E0E0"),
            # 'Median' = color_bar("#E0E0E0")
          ), align = c('l', rep('r', length(colnames(nodesFilteredClusterGrouped)) - 1)), col.names = c("", colnames(nodesFilteredClusterGrouped)[2:length(colnames(nodesFilteredClusterGrouped))])) %>% htmltools::HTML() %>% div() %>% spk_add_deps(),
          footer = fluidRow(column(12, helpText(icon("info-circle"), "Total of birds")))
        )
      )
    )
  })
}