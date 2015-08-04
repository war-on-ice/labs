## team server ui
## acthomas, 8-20-14

source("global.R")

shinyUI(
    fluidPage(
        h2('Bench Assists'),

        tags$head(tags$link(rel = "stylesheet", type = "text/css", id ='twentytwelve-fonts-css', href = "http://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700&#038;subset=latin,latin-ext", media="all")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "http://war-on-ice.com/css/capcheck.css")),

        h4('(Points for Players Who Have Left The Ice)'),
        dataTableOutput('mytable'),

        h2('Gordie Howe Hat Tricks'),
        h4('(Goal, Assist and Fighting Major in Same Game)'),
        dataTableOutput('gordie')
      
      )
    )
