
##########################################################################################
# ui.r -- contains the definitions of objects that will be populated with R data.

shinyUI(
  fluidPage(
   #The header panel is the title for the whole page.
    h2("Optimal Time To Pull the Goalie (Trailing By One and Two)"),
    #we have one sidebar panel where you can put well panels.

    h3("Select Scoring Rates for Each Team:"),
    fluidRow (
        column (2),
        column (4,
                sliderInput("prob.es.trail", label="Goals Per 60 Minutes: Trailing Team at Even Strength",
                            min=1.5, value=2.5, max=4, step=0.1),
                sliderInput("prob.es.leads", label="Goals Per 60 Minutes: Leading Team at Even Strength",
                            min=1.5, value=2.5, max=4, step=0.1)),
        column (4,
                sliderInput("prob.en.trail", label="Goals Per 60 Minutes: Trailing Team with Goalie Pulled",
                            min=4, value=7.5, max=12, step=0.25),
                sliderInput("prob.en.leads", label="Goals Per 60 Minutes: Leading Team Facing 6 Skaters",
                            min=10, value=20, max=30, step=0.5)),
        column (2)

      ),
    
    helpText("Each scoring scenario is simulated 100,000 times to see whether the trailing team ties the game before the leading team scores an add-on goal. "),
    
    hr(),
    plotOutput(outputId = "mainbasegraph", height="800px", width="920px")

    )
  )




