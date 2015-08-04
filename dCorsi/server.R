## team server
## acthomas, 8-21-14

library(shiny)
#load("season1314.RData")

source("global.R")
#sess <- sessionInfo()
#writeLines(paste(sess), con="sessioninfo.txt")

shinyServer(function(input, output, session){
  #input=list(daterange=c("2013-10-01","2014-10-01"), minTOI=60, whichPosLoc="All")
    
    fulldata <- reactive ({
        #load("../common-data/woi-common.RData")
        load("burtch-table.RData")
        burtch.table[rev(order(burtch.table$dCorsiImpact)),]
    })
  

    mytable.prime <- reactive ({
        fulldata()
    })
  
    output$mytable = renderDataTable({
        output <- mytable.prime()

        output <- if (input$corsi) output[,c("Name","Gm","pos","teamyear",
                                             "CF60","ECF60","CA60","ECA60",
                                             "dCFImpact", "dCAImpact", "dCorsi60", "dCorsiImpact")] else
        output[,c("Name","Gm","pos","teamyear",
                  "FF60","EFF60","FA60","EFA60",
                  "dFFImpact", "dFAImpact", 
                  "dFenwick60", "dFenwickImpact")]
        output$Name <- gsub(" ",".",output$Name)
        output
    }       
        ,options=list(searching=TRUE, scrollX = "100%", scrollY = "100%",
             lengthChange = FALSE, pageLength=20#(, columns.type = c("html","numeric","html","numeric",
                                                 #     "numeric","numeric","numeric","numeric",
                                                 #     "numeric","numeric")
                                       )
        )
    autoInvalidate2 <- reactiveTimer(1*1000, session)
    output$downloadData <- downloadHandler (
        filename = paste0("war-on-ice-", {autoInvalidate2(); Sys.time()},".csv"),
        content = function (file) write.csv (fulldata(), file)
        )
  
  
})
