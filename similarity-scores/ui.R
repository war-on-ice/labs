# User Interface

shinyUI(fluidPage(
        tags$head(tags$style(".container-fluid { font-size: 12px;}")),
        tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }" ),

    h2("Statistical Similarity Calculator via @MannyElk"),
    
    fluidRow(
        column(2, selectInput("sl1", "Season", choices = list("2005-06" = "20052006", "2006-07" = "20062007", 
                                                   "2007-08" = "20072008", "2008-09" = "20082009", 
                                                   "2009-10" = "20092010", "2010-11" = "20102011", 
                                                   "2011-12" = "20112012", "2012-13" = "20122013", 
                                                   "2013-14" = "20132014", "2014-15" = "20142015"), 
                              selected = "20142015")),
        column(3, textInput("t1", "Search", value = "Crosby")),
        column(3, uiOutput("sl2")), 
        column(2, numericInput("n1", "Comparables", value = 30, min = 1, max = 100)),
        column(2)##, column(2)
        
        ), 
    fluidRow(
        column(2),
        column(2,  sliderInput("s1", "G/60", min = 0, max = 1, value = 0.75, step = 0.25)),
        column(2, sliderInput("s2", "A/60", min = 0, max = 1, value = 0.75, step = 0.25)),
        column(2,  sliderInput("s3", "P/60", min = 0, max = 1, value = 1, step = 0.25)),
        column(2, sliderInput("s8", "iCF/60", min = 0, max = 1, value = 0.75, step = 0.25)),
        column(2)        
        ), 
    fluidRow(
        column(2),
        column(2, sliderInput("s5", "Rel CF%", min = 0, max = 1, value = 1, step = 0.25)),
        column(2, sliderInput("s6", "Rel SCF%", min = 0, max = 1, value = 0.5, step = 0.25)),
        column(2, sliderInput("s7", "Rel GF%", min = 0, max = 1, value = 0.5, step = 0.25)),
        column(2, sliderInput("s11", "TOI%", min = 0, max = 1, value = 0.5, step = 0.25))
        ,column(2, checkboxInput("more", "More options", value = F))
        ),
    
    conditionalPanel("input.more",
                     fluidRow(  
                         column(2, sliderInput("s12", "ZSO%", min = 0, max = 1, value = 0, step = 0.25)),
                         column(2, sliderInput("s13", "TOI% QoT", min = 0, max = 1, value = 0, step = 0.25)),
                         column(2, sliderInput("s14", "TOI% QoC", min = 0, max = 1, value = 0, step = 0.25)),
                         column(2, sliderInput("s15", "Corsi QoT", min = 0, max = 1, value = 0, step = 0.25)),
                         column(2, sliderInput("s10", "Age", min = 0, max = 1, value = 0, step = 0.25)),
                         column(2, sliderInput("s9", "Salary", min = 0, max = 1, value = 0, step = 0.25))
                         )
                     ),
    
    #h5("Summary"),
    #dataTableOutput("table2"),
    #h5("Comparables"),
    #dataTableOutput("table1")
        
        tabsetPanel(
            tabPanel("Comparables", dataTableOutput("table1")),
            tabPanel("Summary", dataTableOutput("table2"))
        #tabsetPanel(
        #    tabPanel("Comparables", ),
        #    tabPanel("Summary", ))
      
        )
    ))
