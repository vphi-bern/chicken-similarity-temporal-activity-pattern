chickenDownloadTabItemUI <- function(id, title) {
  ns <- NS(id)
  
  tabItem(id,
    fluidRow(
      box(width = 8, title = "Raw data and code", collapsible = TRUE, closable = FALSE,
        h4('Data files'),
        fluidRow(
          column(6, 
            a(href="data/aimat11.csv", target="_blank", icon(name="file-text-o"), "Aimat 11 Matrix [CSV]")
          ),
          column(6,
            a(href="data/aimat12.csv", target="_blank", icon(name="file-text-o"), "Aimat 12 Matrix [CSV]")
          )
        ),
        fluidRow(
          column(6, 
            a(href="data/aimat13.csv", target="_blank", icon(name="file-text-o"), "Aimat 13 Matrix [CSV]")
          ),
          column(6,
            a(href="data/aimat14.csv", target="_blank", icon(name="file-text-o"), "Aimat 14 Matrix [CSV]")
          )
        ),
        fluidRow(
          column(6, 
            a(href="data/networkmeasures_DTWcluster.csv", target="_blank", icon(name="file-text-o"), "Network Measures DTW Cluster [CSV]")
          ),
          column(6, 
            a(href="data/download/allhentimestats.csv", target="_blank", icon(name="file-text-o"), "All Hens Time Stats [CSV]")
          )
        ),
        h4('Codes'),
        fluidRow(
          column(6,
            a(href="data/download/ChickenRanging_NB1.pdf", target="_blank", icon(name="file-pdf-o"), "Chicken Ranging NB1 [PDF]")
          ),
          column(6,
            a(href="data/download/ChickenRanging_NB2.pdf", target="_blank", icon(name="file-pdf-o"), "Chicken Ranging NB2 [PDF]")
          )
        ),
        fluidRow(
          column(6, 
            a(href="data/download/ChickenRanging_NB3.pdf", target="_blank", icon(name="file-pdf-o"), "Chicken Ranging NB3 [PDF]")
          ),
          column(6,
            a(href="data/download/ChickenNetwork_NB4.pdf", target="_blank", icon(name="file-pdf-o"), "Chicken Network NB4 [PDF]")
          )
        ),
        fluidRow(
          column(6, 
            a(href="data/download/ChickenRanging_NB5.pdf", target="_blank", icon(name="file-pdf-o"), "Chicken Ranging NB5 [PDF]")
          ),
          column(6,
            a(href="data/download/ChickenRanging_NB6.pdf", target="_blank", icon(name="file-pdf-o"), "Chicken Ranging NB6 [PDF]")
          )
        ),
        fluidRow(
          column(6, 
            a(href="data/download/Day-wiseDTWdistancematrix.pdf", target="_blank", icon(name="file-pdf-o"), "Day-wise DTW Distance Matrix [PDF]")
          ),
          column(6,
            a(href="data/download/GTestsClustersModules.pdf", target="_blank", icon(name="file-pdf-o"), "G Tests Clusters Modules [PDF]")
          )
        ),
        fluidRow(
          column(6, 
            a(href="data/download/RangingAnalysesbyClustersonPenlevel.pdf", target="_blank", icon(name="file-pdf-o"), "Ranging Analyses by Clusters on Pen level [PDF]")
          ),
          column(6,
            a(href="data/download/RangingDataWilcox.Test.pdf", target="_blank", icon(name="file-pdf-o"), "Ranging Data Wilcox Test [PDF]")
          )
        ),
        fluidRow(
          column(12, 
            a(href="data/download/TrendlineDTWdistanceperPen.pdf", target="_blank", icon(name="file-pdf-o"), "Trend line DTW distance per Pen [PDF]")
          )
        )
      ),
      box(width = 4, title = "Download time serie movements", status = "primary",
        pickerInput(
          inputId = ns("downloadPenNumber"),
          multiple = FALSE,
          label = "Select a pen",
          choices = pensNumberName,
          selected = 11
        ),
        pickerInput(
          inputId = ns("downloadBirdIds"),
          multiple = FALSE,
          label = "Select a bird",
          choices = c(),
          options = list(
            'live-search' = TRUE
          )
        ),
        downloadButton(
          ns("downloadData"), width = '100%',
          label = "Download data",
          class = "btn-success btn-block", style = "color: #FFF",
          icon = icon('download'), title = 'Click to download'
        )
      )
    )
  )
}

chickenDownloadTabItem <- function(input, output, session) {
  ns <- session$ns

  dataTableDownloadHenData <- reactiveValues(
    movements = NULL,
    selectedMovementsDay = NULL,
    clicked = FALSE
  )
  
  observe({
    if(is.null(input$downloadPenNumber)) return()
    penNumber <- input$downloadPenNumber

    birdNodes <- networkmeasures_DTWcluster %>%
      filter(Pen == penNumber) %>%
      select(Bird)

    birdNodesName <- c(setNames(sort(birdNodes$Bird), sapply(sort(birdNodes$Bird), function(x) {sprintf("Bird %s", x)})))
    updatePickerInput(session = session, inputId = "downloadBirdIds", choices = birdNodesName)
  })
  
  observeEvent(input$downloadBirdIds, {
    if(is.null(input$downloadBirdIds)) return()
    prepareHendData(input$downloadBirdIds)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(sprintf('pen_%s_bird_%s', isolate(input$downloadPenNumber), isolate(input$downloadBirdIds)), "csv", sep = ".")
    },
    content = function(file) {
      outputData <- dataTableDownloadHenData$selectedMovementsDay %>%
        mutate(time = format(date, format = "%H:%M:%S")) %>%
        select(-c(date)) %>%
        rename_at(vars(2:length(colnames(.))), function(x) sprintf('day_%s', x))
      
      write.table(outputData, file = file, sep = ",", row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  prepareHendData <- function(birdIds, birdDays = 1:72) {
    henData <- getHenDataFromCsV(birdIds)
    dataTableDownloadHenData$movements <- henData
    
    if(length(birdIds) > 0) {
      if(nrow(henData) > 0) {
        dataTableDownloadHenData$selectedMovementsDay <- dataTableDownloadHenData$movements[,c('time', 'date', birdDays)]
      } else {
        dataTableDownloadHenData$movements <- NULL
        dataTableDownloadHenData$selectedMovementsDay <- NULL
      }
    } else {
      dataTableDownloadHenData$movements <- NULL
      dataTableDownloadHenData$selectedMovementsDay <- NULL
    }
  }
}