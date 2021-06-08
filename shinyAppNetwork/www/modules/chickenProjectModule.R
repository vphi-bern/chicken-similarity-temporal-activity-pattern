chickenProjectTabItemUI <- function(id, title) {
  ns <- NS(id)
  
  tabItem(id,
    fluidRow(
      box(width = 12, headerBorder = TRUE, title = "Similarity in temporal movement pattern in chickens increases with time and social association",
        tags$h4("Abstract"),
        tags$p("Social organization is a key aspect of animal ecology that plays an important part in social evolution as well as in animal domestication. All group-living animals regularly engage in social interactions. While social interactions within a community are partly shaped by individual needs for acquiring certain resources, they simultaneously affect individual behaviour and activity patterns both in the social and the non-social domain. Here, we explored the relationship between social associations and individual activity patterns in the domestic chicken. Using a combination of social network analysis for quantifying social relationships and dynamic time warping for characterizing the movement patterns of chickens, we found that chickens were consistent in their individual variation in temporal activity and maintained stable social relationships in terms of preferred association partners. In addition to being consistent, social associations correlated with movement patterns and the correlation strengthened over the period of observation. These results demonstrate the importance of social relationships when considering the expression of individual behaviour. Notably, these differences emerge despite rather homogenous rearing conditions, environment, and low genetic diversity.  Thus, while variation in behavioural phenotypes can be observed in isolated individuals, this study shows that the social environment within a group can shape and enhance variation in general movement patterns of individual animals."),
        HTML("<h4>How to cite</h4>"),
        HTML("<p>Gomez Y* (2020) Similarity in temporal movement pattern in chickens increases with time and social association. doi: <a href='https://www.doi.org/' target='_blank'>DOI</a></p>"),
        tags$p("* Corresponding author"),
        tags$h4("About the app"),
        HTML("<p>This app was built using <a href='http://www.r-project.org/' target='_blank'>R</a> and <a href='http://shiny.rstudio.com/' target='_blank'>Shiny</a>. All graphics were made with <a href='https://cran.rstudio.com/web/packages/dplyr/index.html' target='_blank'>dplyr</a>, <a href='https://cran.rstudio.com/web/packages/highcharter/index.html' target='_blank'>highcharter</a> and <a href='https://cran.rstudio.com/web/packages/visNetwork/index.html' target='_blank'>visNetwork</a> packages.<br /><a href='http://www.vphi.ch' target='_blank'>Veterinary Public Health Institute (VPHI)</a>, Universit&#228;t Bern.</p>"),
        HTML("<p>All the source code of this application and the data used in this research are available in our <a href='https://github.com/vphi-bern/chicken-similarity-temporal-activity-pattern' target='_blank'>Github repository</a>.</p>")
      ),
      box(width = 12, title = "About the team",
        userList(
          lapply(contacts, function(user) {
            userListItem(
              image = paste("images/users/", user[['picture']], sep = ""),
              title = HTML(user[['name']]), 
              subtitle = HTML(user[['short_institution']])
            )
          })
        )
      )
    )
  )
}

chickenProjectTabItem <- function(input, output, session) {
  ns <- session$ns
}