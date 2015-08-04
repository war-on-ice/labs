
load ("../common-data/woi-common.RData")

## ss <- "20132014"
grand.out <- NULL
for (ss in seasons) {

    print(ss)
    load (paste0("../source-data/nhlscrapr-",ss,".RData"))
    grand.data$subdistance <- NA
    goals <- subset (grand.data, etype=="GOAL")
    bench <- apply(goals, 1, function(gg) {
        players <- unlist(gg[c(paste0("a",1:6),paste0("h",1:6),"away.G","home.G")])
        c(gg[["ev.player.2"]], gg[["ev.player.3"]],
          !(gg[["ev.player.2"]] %in% players) && gg[["ev.player.2"]] != "   1",
          !(gg[["ev.player.3"]] %in% players) && gg[["ev.player.3"]] != "   1")
    })
    print(table(bench[3,]))
    print(table(bench[4,]))
    
    grand.out <- rbind(grand.out,
                       rbind(
                           data.frame(goals[bench[3,]=="TRUE",], bencher=bench[1,bench[3,]=="TRUE"]),
                           data.frame(goals[bench[4,]=="TRUE",], bencher=bench[2,bench[4,]=="TRUE"])))
}

grand.out$goalie <- 0
grand.out$goalie[grand.out$ev.team == grand.out$awayteam] <- grand.out$home.G[grand.out$ev.team == grand.out$awayteam]
grand.out$goalie[grand.out$ev.team == grand.out$hometeam] <- grand.out$away.G[grand.out$ev.team == grand.out$hometeam]
grand.out$scorer <- roster.unique$firstlast[as.numeric(as.character(grand.out$ev.player.1))]
grand.out$benchername <- roster.unique$firstlast[as.numeric(as.character(grand.out$bencher))]
grand.out$goaliename <- roster.unique$firstlast[as.numeric(as.character(grand.out$goalie))]


grand.sub <- grand.out[,c("season","gcode","period","seconds","ev.team","scorer","benchername","goaliename")]
save(grand.sub, file="bench-assists.RData")


###########################################################################

gaf.out <- NULL
#allfights <- NULL
#allgoals <- NULL
for (ss in seasons) {

    print(ss)
    load (paste0("../source-data/nhlscrapr-",ss,".RData"))

    fights <- grand.data[grand.data$etype=="PENL" & grepl("Fighting", grand.data$type),]
    #print(table(grand.data$type[grand.data$etype=="PENL"]))
    goals <- subset (grand.data, etype=="GOAL" & (as.character(ev.player.1) %in% as.character(fights$ev.player.1) |
                                     as.character(ev.player.2) %in% as.character(fights$ev.player.1) |
                                     as.character(ev.player.3) %in% as.character(fights$ev.player.1)) & seconds != 3900)
    
    subfights <- data.frame(seasongcode=paste0(fights$season, fights$gcode),
                            type=paste0(fights$type),
                            player=roster.unique$firstlast[as.numeric(as.character(fights$ev.player.1))],
                            stringsAsFactors=FALSE)
    
    subgoals <- data.frame(seasongcode=paste0(goals$season, goals$gcode),
                           goal=roster.unique$firstlast[as.numeric(as.character(goals$ev.player.1))],
                           assist1=roster.unique$firstlast[as.numeric(as.character(goals$ev.player.2))],
                           assist2=roster.unique$firstlast[as.numeric(as.character(goals$ev.player.3))],
                           stringsAsFactors=FALSE)
    
    for (kk in 1:nrow(subfights)) {
        
        goals2 <- subset(subgoals,
                         seasongcode == as.character(subfights$seasongcode[kk]) &
                         goal == subfights$player[kk])
        assists2 <- subset(subgoals,
                         seasongcode == as.character(subfights$seasongcode[kk]) &
                         (assist1 == subfights$player[kk] | assist2 == subfights$player[kk]))
        
        if (nrow(goals2)>0 & nrow(assists2) > 0) {
            gaf.out <- rbind(gaf.out, c(id=as.character(subfights$player[kk]),
                                        seasongcode=as.character(subfights$seasongcode[kk])))
        }
    }
    
#    allfights <- rbind(allfights, fights)
#    allgoals <- rbind(allgoals, goals)

}

gaf.out <- as.data.frame(gaf.out[rev(order(gaf.out[,2])),])
save(gaf.out, file="gaf.RData")
