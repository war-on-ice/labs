## team server ui
## acthomas, 8-20-14

source("global.R")

shinyUI(
    fluidPage(
        tags$head(tags$style(".container-fluid { font-size: 12px;}")),
        h2('Burtch dCorsi and dFenwick', align="center"),
        #helpText('Sortable Team Statistics'),

        #wellPanel(
        #    textInput("generalsearch", "Search", value="")
        #    ),
        checkboxInput("corsi", "Show Corsi", TRUE),
        dataTableOutput('mytable'),
        downloadButton("downloadData", "Download Game Table")
      )
    )
