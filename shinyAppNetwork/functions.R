add_arg_to_df_Birds <- highcharter:::add_arg_to_df

hcboxplotBirds <- function(x = NULL, var = NULL, var2 = NULL, outliers = TRUE, outliersSeriesName = "Outliers", outlierColor = "#516375", ...) {
  
  stopifnot(!is.null(x))
  
  if(is.null(var))
    var <- NA
  if(is.null(var2))
    var2 <- NA
  
  df <- tibble(x, g1 = var, g2 = var2)
  
  get_box_values <- function(x = rt(1000, df = 10)) { 
    boxplot.stats(x)$stats %>% 
      t() %>% 
      as_tibble() %>% 
      setNames(c("low", "q1", "median", "q3", "high"))
  }
  
  get_outliers_values <- function(x = rt(1000, df = 10)) {
    boxplot.stats(x)$out
  }
  
  series_box <- df %>%
    group_by(g1, g2) %>%  
    do(data = get_box_values(.$x)) %>% 
    unnest(cols = c(data)) %>% 
    group_by(g2) %>% 
    do(data = list_parse(rename(select(., -g2), name = g1))) %>% 
    # rename(name = g2) %>% 
    mutate(type = "boxplot", id = str_to_id(as.character(g2)))
  
  if(length(list(...)) > 0)
    series_box <- add_arg_to_df_Birds(series_box, ...)
  
  series_out <- df %>% 
    group_by(g1, g2) %>%  
    do(data = get_outliers_values(.$x)) %>% 
    unnest(cols = c(data)) %>% 
    group_by(g2) %>% 
    do(data = list_parse(select(., name = g1, y = data))) %>% 
    # rename(name = g2) %>% 
    mutate(type = "scatter", linkedTo = str_to_id(as.character(g2)), color = outlierColor)
  
  if(length(list(...)) > 0)
    series_out <- add_arg_to_df_Birds(series_out, ...)
  
  if(!has_name(list(...), "color")) {
    colors <- colorize(seq(1, nrow(series_box)))
    colors <- hex_to_rgba(colors, alpha = 0.75)  
  }
  
  if(!has_name(list(...), "name")) {
    series_box <- rename(series_box, "name" = "g2")
    series_out <- rename(series_out, "name" = "g2")
  }
  
  series_out <- series_out %>%
    mutate(name = outliersSeriesName)

  series_box <- series_box
  series_out <- series_out
  
  hc <- highchart() %>% 
    hc_chart(type = "bar") %>% 
    # hc_colors(colors) %>% 
    hc_xAxis(type = "category") %>% 
    hc_plotOptions(series = list(
      marker = list(
        symbol = "circle"
      )
    )) %>% 
    hc_tooltip(
      formatter = JS('function(){var output = "";if(this.series.type == "boxplot") {output = "<span><strong>Level " + this.point.name + "</strong></span><br /><span>Maximum: " + this.point.high +"</span><br /><span>Upper quartile: " + this.point.q3 + "</span><br /><span>Median: " + this.point.median +"</span><br /><span>Lower quartile: " + this.point.q1 + "</span><br /><span>Minimum: " + this.point.low + "</span>"} else {output = "<span>Outlier: " + this.y + "</span>"} return output;}')
    )
  
  hc <- hc_add_series_list(hc, list_parse(series_box))
  
  # if(is.na(var2) || is.na(var)) {
  #   hc <- hc %>% 
  #     hc_xAxis(categories = "") %>% 
  #     hc_plotOptions(series = list(showInLegend = FALSE))
  # }
  
  if(outliers)
    hc <- hc_add_series_list(hc, list_parse(series_out))
  
  hc
}

##############################################
# 
##############################################
getPenIDByChickenID <- function(chickenID) {
  return(networkmeasures_DTWcluster %>% filter(Bird == chickenID) %>% '$'('Pen') %>% first())
}

##############################################
# 
##############################################
getAllExtentOfAssociation <- function() {
  edges11 <- melt(data = aimat11, id.vars = c("hens")) %>%
    mutate(variable = as.integer(as.character(variable)), hens = as.integer(hens)) %>%
    rename(from = hens, to = variable) %>%
    filter(from > to, value > 0)
  
  edges12 <- melt(data = aimat12, id.vars = c("hens")) %>%
    mutate(variable = as.integer(as.character(variable)), hens = as.integer(hens)) %>%
    rename(from = hens, to = variable) %>%
    filter(from > to, value > 0)
  
  edges13 <- melt(data = aimat13, id.vars = c("hens")) %>%
    mutate(variable = as.integer(as.character(variable)), hens = as.integer(hens)) %>%
    rename(from = hens, to = variable) %>%
    filter(from > to, value > 0)
  
  edges14 <- melt(data = aimat14, id.vars = c("hens")) %>%
    mutate(variable = as.integer(as.character(variable)), hens = as.integer(hens)) %>%
    rename(from = hens, to = variable) %>%
    filter(from > to, value > 0)
  
  return(sort(unique(c(edges11$value, edges12$value, edges13$value, edges14$value))))
}

##############################################
# 
##############################################
getHenDataFromGitHUb <- function(idHen) {
  idHenUpdated <- as.integer(idHen)
  
  prefixHen <- ""
  
  if(idHenUpdated %in% 1:9) {
    prefixHen <- "X00"
  } else if(idHenUpdated %in% 10:99) {
    prefixHen <- "X0"
  } else {
    prefixHen <- "X"
  }
  
  henData <- data.frame()
  
  henDataURL <- sprintf("https://raw.githubusercontent.com/yandiru/appNetworkData/master/ts/%s%s.csv", prefixHen, idHenUpdated)
  
  henData <- data.frame()
  
  u <- try(henData <- read_csv(url(henDataURL), col_types = cols()), silent = T);
  if(!is.null(henData)) print(henData$root);
  
  colnames(henData) <- sapply(1:length(colnames(henData)), function(x) {sprintf("Day_%s", x)})
  
  henData
}

getHenDataFromCsV <- function(idHen) {
  idHenUpdated <- as.integer(idHen)

  prefixHen <- ""
  
  if(idHenUpdated %in% 1:9) {
    prefixHen <- "X00"
  } else if(idHenUpdated %in% 10:99) {
    prefixHen <- "X0"
  } else {
    prefixHen <- "X"
  }
  
  henData <- data.frame()
  
  henDataPath <- sprintf("www/data/ts/%s%s.csv", prefixHen, idHenUpdated)
  
  if(file.exists(henDataPath)) {
    henData <- read_csv(henDataPath, col_types = cols())
    # colnames(henData) <- sapply(1:length(colnames(henData)), function(x) {sprintf("Day_%s", x)})
    colnames(henData) <- 1:length(colnames(henData))
    henData <- henData %>% mutate(time = row_number())
    henData$date <- timeSequencePerDay()
  }
  
  henData
}

uniqueMovements <- function(movements) {
  outputMovements <- c()
  if(length(movements) > 0) {
    outputMovements <- rle(movements)$values
  }
  return(outputMovements)
}

timeSequencePerDay <- function(dayNumber) {
  timeSequence
}


