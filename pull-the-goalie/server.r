
library(shinyRGL)
library(rgl)
source("global.r")


shinyServer(function(input, output, session){

  outcome.maker <- reactive(rbind(
    grand.test(c(input$prob.es.trail,input$prob.es.leads),
               c(input$prob.en.trail,input$prob.en.leads)),
    grand.test.2goals(c(input$prob.es.trail,input$prob.es.leads),
                      c(input$prob.en.trail,input$prob.en.leads))

    ))
  
  output$mainbasegraph <- renderPlot({

    thing.to.plot <- outcome.maker()
    x.vals <- as.numeric(colnames(thing.to.plot))
    
    maxpoint <- which.max(thing.to.plot[3,])
    max.time <- x.vals[maxpoint]

    maxpoint.2 <- which.max(thing.to.plot[6,])
    max.time.2 <- x.vals[maxpoint.2]
#max.val
    
    par(mfcol=c(2,1))

    plot (range(x.vals), range(c(thing.to.plot[c(1:2,4:5),])), ty="n",
          main=paste("Probability That Trailing Team Ties\nAt 60 Seconds:", thing.to.plot[1,12], " 90:", thing.to.plot[1,18], " 120:", thing.to.plot[1,24], " 180:", thing.to.plot[1,30]),
          ylab="Tie Probability", xlab="Time Remaining (Seconds)")
    lines (x.vals, thing.to.plot[1,], lwd=2)
    lines (x.vals, thing.to.plot[2,], col=2, lwd=2)
    abline (v=max.time, col=2, lwd=2)

    lines (x.vals, thing.to.plot[4,], col=4, lwd=2)
    lines (x.vals, thing.to.plot[5,], col=3, lwd=2)
    abline (v=max.time.2, col=3, lwd=2)

    legend ("bottomright", c("Pull The Goalie Now", "Keep The Goalie In"), col=1:2, lwd=c(2,2))
    
    plot (x.vals, thing.to.plot[3,], ty="l", main=paste("Biggest Increase In Tie Probability At", max.time, "Seconds"),
          ylab="Tie Probability Difference", xlab="Time Remaining (Seconds)")
    abline (v=max.time, col=2, lwd=2)
    abline (h=0, col=8)

    lines(x.vals, thing.to.plot[6,], lty=2, col=3)
    abline (v=max.time.2, col=3, lwd=2)
  
    
  })

  #output$notetaker <- renderPrint ({
  #  print(outcome.maker())
  #})
  
})

#reactive(print(pitcher.data()))
  
