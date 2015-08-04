## team server
## acthomas, 8-21-14

#library(shiny)
#load("season1314.RData")

source("global.R")
load("../common-data/woi-common.RData")

shinyServer(function(input, output, session){
  #input=list(daterange=c("2013-10-01","2014-10-01"), minTOI=60, whichPosLoc="All")

    output$mytable = renderDataTable({

        load("bench-assists.RData")

        grand.sub$GameDate <- gamestest$date[match(paste0(grand.sub$season, grand.sub$gcode),
                                                   paste0(gamestest$season, gamestest$gcode))]
        grand.sub[["Game Date"]] <- woigame.url(grand.sub$GameDate,
                                                paste0(grand.sub$season, grand.sub$gcode))
        grand.sub[["Team"]] <- grand.sub$ev.team
        grand.sub[["Bench Assist"]] <- grand.sub$benchername
        grand.sub[["Goal Scorer"]] <- grand.sub$scorer
        grand.sub[["Goalie"]] <- grand.sub$goaliename
        grand.sub <- grand.sub[rev(order(grand.sub$season, grand.sub$gcode)),]
        
        grand.sub[,c("Game Date", "Bench Assist", "Team", "Goal Scorer", "Goalie")]
    }
        ,options=list(searching=TRUE, scrollX = "100%", scrollY = "100%",
             lengthChange = FALSE, pageLength=20)
        )

    output$gordie = renderDataTable({

        load("gaf.RData")
        gaf.out$GameDate <- gamestest$date[match(gaf.out$seasongcode,
                                           paste0(gamestest$season, gamestest$gcode))]
        gaf.out[["Player"]] <- gaf.out$id
        gaf.out[["Game Date"]] <- woigame.url(gaf.out$GameDate,
                                              gaf.out$seasongcode)
        gaf.out[,c("Game Date", "Player")]
    }
        ,options=list(searching=TRUE, scrollX = "100%", scrollY = "100%",
             lengthChange = FALSE, pageLength=20)
        )


                                        #    output$downloadData <- downloadHandler (
#        filename = paste0("war-on-ice-", {autoInvalidate2(); Sys.time()},".csv"),
#        content = function (file) write.csv (fulldata(), file)
#        )
  
  
})
