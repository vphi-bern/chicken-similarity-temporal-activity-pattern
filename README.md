# chicken-similarity-temporal-activity-pattern
R-Shiny app for research presented in the research paper "Similarity in temporal movement pattern in chickens increases with time and social association", DOI ""

### Abstract
Social organization is a key aspect of animal ecology that plays an important part in social evolution as well as in animal domestication. All group-living animals regularly engage in social interactions. While social interactions within a community are partly shaped by individual needs for acquiring certain resources, they simultaneously affect individual behaviour and activity patterns both in the social and the non-social domain. Here, we explored the relationship between social associations and individual activity patterns in the domestic chicken. Using a combination of social network analysis for quantifying social relationships and dynamic time warping for characterizing the movement patterns of chickens, we found that chickens were consistent in their individual variation in temporal activity and maintained stable social relationships in terms of preferred association partners. In addition to being consistent, social associations correlated with movement patterns and the correlation strengthened over the period of observation. These results demonstrate the importance of social relationships when considering the expression of individual behaviour. Notably, these differences emerge despite rather homogenous rearing conditions, environment, and low genetic diversity. Thus, while variation in behavioural phenotypes can be observed in isolated individuals, this study shows that the social environment within a group can shape and enhance variation in general movement patterns of individual animals.

### How to cite
Gomez Y* (2020) Similarity in temporal movement pattern in chickens increases with time and social association. doi: DOI

### About the app
An online version of this app you can see it [here](https://gomezya.shinyapps.io/shinyAppNetwork/).

This app was built using R and Shiny. All graphics were made with  [dplyr](https://cran.rstudio.com/web/packages/dplyr/index.html), [highcharter](https://cran.r-project.org/web/packages/highcharter/index.html) and [visNetwork](https://cran.rstudio.com/web/packages/visNetwork/index.html) packages.

### How to run it
```R
#### First make sure you have all the necessary packages installed
install.packages(c("shiny", "shinydashboard", "shinydashboardPlus", "highcharter", "xts", "lubridate", "shinycssloaders", "shinyWidgets", "htmltools", "tidyr", "rlang", "dplyr", "readr", "RCurl", "shinyjs", "formattable", "sparkline", "visNetwork", "RColorBrewer", "reshape2", "shinyhelper", "shinyBS", "purrr"))

library(shiny)

#### Easiest way is to use runGitHub
runGitHub("chicken-similarity-temporal-activity-pattern", username = "vphi-bern", subdir = "shinyAppNetwork")

#### Run a tar or zip file directly
runUrl("https://github.com/vphi-bern/chicken-similarity-temporal-activity-pattern/archive/master.zip")
```

Or you can clone the git repository, then use `runApp()`:
```R
# First clone the repository with git. If you have cloned it into
# ~/chicken-similarity-temporal-activity-pattern/shinyAppNetwork, first go to that directory, then use runApp().
setwd("~/chicken-similarity-temporal-activity-pattern/shinyAppNetwork")
runApp()
```

### [Veterinary Public Health Institute (VPHI)](http://www.vphi.ch), [Universit&#228;t Bern](https://www.unibe.ch/index_eng.html), &#169; All rights reserved
