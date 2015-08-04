# Server

# import libraries
library(repmis)

# import data
# woi.csv is a file downloaded from Dropbox. It contains all 5v5 skater data since 2005, split by season
woi <- source_DropboxData(file = "woi.csv", key = "h2yj5h9p9m78a6a", sep = ",", header = T)

# re-assign column names
colnames(woi) <- gsub("%", ".", colnames(woi))

# adjust salaries
Adj.Salary = round(ifelse(woi$season == "20052006", (woi$Salary/39.0)*69.0, ifelse(woi$season == "20062007", (woi$Salary/44.0)*69.0, ifelse(woi$season == "20072008", (woi$Salary/50.3)*69.0, ifelse(woi$season == "20082009", (woi$Salary/56.7)*69.0, ifelse(woi$season == "20092010", (woi$Salary/56.8)*69.0, ifelse(woi$season == "200102011", (woi$Salary/59.4)*69.0, ifelse(woi$season == "20112012", (woi$Salary/64.3)*69.0, ifelse(woi$season == "20122013", (woi$Salary/64.3)*69.0, ifelse(woi$season == "20132014", (woi$Salary/64.3)*69.0, woi$Salary))))))))), 3)

# create and append player-season codes
nn = paste(woi$Name, woi$season)
iCF60 = round((woi$iCF/woi$TOI)*60, 3)
woi <- cbind(nn, woi, iCF60, Adj.Salary)
# omit rows containing NA or NULL values
woi = na.omit(woi[woi$CorT. > 0 & woi$CorC. > 0, ])
# create regular skaters subsets by position. 40 gp is arbitrary.
reg.fwd = woi[woi$pos != "D" & woi$Gm > 40, ]
reg.def = woi[woi$pos == "D" & woi$Gm > 40, ]
options(digits = 4)

shinyServer(function(input, output) {
  # Player Search
    searchResult1 <- reactive({
        ru.sub <- levels(as.factor(woi$Name))
        pl.list <- ru.sub[nchar(as.character(ru.sub)) > 3]
        pl.list[grep(tolower(input$t1), tolower(pl.list))]
    })
    output$sl2 <- renderUI(selectInput("player1", "Select Player", searchResult1()))
                                        # Comparables
    player <- reactive (woi$nn[woi$Name == input$player1 & woi$season == input$sl1])
    
    similarity.F <- reactive(paste(format(100 * (1 - sqrt(input$s1*((reg.fwd$G60-woi$G60[woi$nn == player()])/(max(reg.fwd$G60)-min(reg.fwd$G60)))^2 +
                                                          input$s2*((reg.fwd$A60-woi$A60[woi$nn == player()])/(max(reg.fwd$A60)-min(reg.fwd$A60)))^2 +
                                                          input$s3*((reg.fwd$P60-woi$P60[woi$nn == player()])/(max(reg.fwd$P60)-min(reg.fwd$P60)))^2 +
                                                          input$s8*((reg.fwd$iCF60-woi$iCF60[woi$nn == player()])/(max(reg.fwd$iCF60)-min(reg.fwd$iCF60)))^2 +
                                                          input$s5*((reg.fwd$CF.Rel-woi$CF.Rel[woi$nn == player()])/(max(reg.fwd$CF.Rel)-min(reg.fwd$CF.Rel)))^2 +
                                                          input$s6*((reg.fwd$SCF.Rel-woi$SCF.Rel[woi$nn == player()])/(max(reg.fwd$SCF.Rel)-min(reg.fwd$SCF.Rel)))^2 +
                                                          input$s7*((reg.fwd$GF.Rel-woi$GF.Rel[woi$nn == player()])/(max(reg.fwd$GF.Rel)-min(reg.fwd$GF.Rel)))^2 +
                                                          input$s11*((reg.fwd$TOI.-woi$TOI.[woi$nn == player()])/(max(reg.fwd$TOI.)-min(reg.fwd$TOI.)))^2 +
                                                          input$s13*((reg.fwd$TOIT.-woi$TOIT.[woi$nn == player()])/(max(reg.fwd$TOIT.)-min(reg.fwd$TOIT.)))^2 +
                                                          input$s14*((reg.fwd$TOIC.-woi$TOIC.[woi$nn == player()])/(max(reg.fwd$TOIC.)-min(reg.fwd$TOIC.)))^2 +
                                                          input$s12*((reg.fwd$ZSO.-woi$ZSO.[woi$nn == player()])/(max(reg.fwd$ZSO.)-min(reg.fwd$ZSO.)))^2 +
                                                          input$s10*((reg.fwd$Age-woi$Age[woi$nn == player()])/(max(reg.fwd$Age)-min(reg.fwd$Age)))^2 +
                                                          input$s9*((reg.fwd$Adj.Salary-woi$Adj.Salary[woi$nn == player()])/(max(reg.fwd$Adj.Salary)-min(reg.fwd$Adj.Salary)))^2 +
                                                          input$s15*((reg.fwd$CorT.-woi$CorT.[woi$nn == player()])/(max(reg.fwd$CorT.)-min(reg.fwd$CorT.)))^2)/sqrt(input$s1+input$s2+input$s3+input$s8+input$s5+input$s6+input$s7+input$s11+input$s9+input$s10+input$s12+input$s13+input$s14+input$s15)), digits = 4), "%"))

    similarity.D <- reactive(paste(format(100 * (1 - sqrt(input$s1*((reg.def$G60-woi$G60[woi$nn == player()])/(max(reg.def$G60)-min(reg.def$G60)))^2 +
                                                          input$s2*((reg.def$A60-woi$A60[woi$nn == player()])/(max(reg.def$A60)-min(reg.def$A60)))^2 +
                                                          input$s3*((reg.def$P60-woi$P60[woi$nn == player()])/(max(reg.def$P60)-min(reg.def$P60)))^2 +
                                                          input$s8*((reg.def$iCF60-woi$iCF60[woi$nn == player()])/(max(reg.def$iCF60)-min(reg.def$iCF60)))^2 +
                                                          input$s5*((reg.def$CF.Rel-woi$CF.Rel[woi$nn == player()])/(max(reg.def$CF.Rel)-min(reg.def$CF.Rel)))^2 +
                                                          input$s6*((reg.def$SCF.Rel-woi$SCF.Rel[woi$nn == player()])/(max(reg.def$SCF.Rel)-min(reg.def$SCF.Rel)))^2 +
                                                          input$s7*((reg.def$GF.Rel-woi$GF.Rel[woi$nn == player()])/(max(reg.def$GF.Rel)-min(reg.def$GF.Rel)))^2 +
                                                          input$s11*((reg.def$TOI.-woi$TOI.[woi$nn == player()])/(max(reg.def$TOI.)-min(reg.def$TOI.)))^2 +
                                                          input$s13*((reg.def$TOIT.-woi$TOIT.[woi$nn == player()])/(max(reg.def$TOIT.)-min(reg.def$TOIT.)))^2 +
                                                          input$s14*((reg.def$TOIC.-woi$TOIC.[woi$nn == player()])/(max(reg.def$TOIC.)-min(reg.def$TOIC.)))^2 +
                                                          input$s12*((reg.def$ZSO.-woi$ZSO.[woi$nn == player()])/(max(reg.def$ZSO.)-min(reg.def$ZSO.)))^2 +
                                                          input$s10*((reg.def$Age-woi$Age[woi$nn == player()])/(max(reg.def$Age)-min(reg.def$Age)))^2 +
                                                          input$s9*((reg.def$Adj.Salary-woi$Adj.Salary[woi$nn == player()])/(max(reg.def$Adj.Salary)-min(reg.def$Adj.Salary)))^2 +
                                                          input$s15*((reg.def$CorT.-woi$CorT.[woi$nn == player()])/(max(reg.def$CorT.)-min(reg.def$CorT.)))^2)/sqrt(input$s1+input$s2+input$s3+input$s8+input$s5+input$s6+input$s7+input$s11+input$s9+input$s10+input$s12+input$s13+input$s14+input$s15)), digits = 4), "%"))
    
    
    table1.contents <- reactive({
        if (woi$pos[woi$nn == player()] != "D"){
            Similarity = similarity.F()
            newdata = data.frame(cbind(Similarity, reg.fwd$Name, reg.fwd$pos, reg.fwd$Team, reg.fwd$season, reg.fwd$Age, reg.fwd$Adj.Salary, reg.fwd$G60, reg.fwd$A60, reg.fwd$P60, reg.fwd$CF.Rel, reg.fwd$SCF.Rel, reg.fwd$GF.Rel, reg.fwd$ZSO., reg.fwd$TOI., reg.fwd$TOIT., reg.fwd$TOIC., reg.fwd$CorT., reg.fwd$CorC., reg.fwd$iCF60)[order(Similarity, decreasing = T),])
        } else {
            Similarity = similarity.D()
            newdata = data.frame(cbind(Similarity, reg.def$Name, reg.def$pos, reg.def$Team, reg.def$season, reg.def$Age, reg.def$Adj.Salary, reg.def$G60, reg.def$A60, reg.def$P60, reg.def$CF.Rel, reg.def$SCF.Rel, reg.def$GF.Rel, reg.def$ZSO., reg.def$TOI., reg.def$TOIT., reg.def$TOIC., reg.def$CorT., reg.def$CorC., reg.def$iCF60)[order(Similarity, decreasing = T),])
        }
        colnames(newdata) = c("Similarity", "Player", "Pos", "Team", "Season", "Age", "Adj. Salary", "G/60", "A/60", "P/60", "Rel CF%", "Rel SCF%", "Rel GF%", "ZSO%", "TOI%", "TOI% QoT", "TOI% QoC", "Corsi QoT", "Corsi QoC", "iCF/60")

        print(head(newdata))
        selfrow <- which(newdata$Player == woi$Name[woi$nn == player()][1] & newdata$Season == input$sl1)
        rbind(newdata[selfrow, ,drop=FALSE],
              newdata[newdata$Player != woi$Name[woi$nn == player()], ][1:input$n1, ])
    })
  
    output$table1 = renderDataTable({
        t1 <- table1.contents()
        t1##[1,] <- paste0 ("<b>",t1[1,],"<b>")
        
    }, options = list(searching = F, paging = F, scrollX = T, info=FALSE))

    table2.contents <- reactive ({
                                        # Summary
        if (woi$pos[woi$nn == player()] != "D"){
            Similarity = similarity.F()
            newdata = data.frame(cbind(Similarity, reg.fwd$Name, reg.fwd$pos, reg.fwd$Team, reg.fwd$season, reg.fwd$Age, reg.fwd$Adj.Salary, reg.fwd$G60, reg.fwd$A60, reg.fwd$P60, reg.fwd$CF.Rel, reg.fwd$SCF.Rel, reg.fwd$GF.Rel, reg.fwd$ZSO., reg.fwd$TOI., reg.fwd$TOIT., reg.fwd$TOIC., reg.fwd$CorT., reg.fwd$CorC., reg.fwd$iCF60)[order(Similarity, decreasing = T),])
        } else {
            Similarity = similarity.D()
            newdata = data.frame(cbind(Similarity, reg.def$Name, reg.def$pos, reg.def$Team, reg.def$season, reg.def$Age, reg.def$Adj.Salary, reg.def$G60, reg.def$A60, reg.def$P60, reg.def$CF.Rel, reg.def$SCF.Rel, reg.def$GF.Rel, reg.def$ZSO., reg.def$TOI., reg.def$TOIT., reg.def$TOIC., reg.def$CorT., reg.def$CorC., reg.def$iCF60)[order(Similarity, decreasing = T),])
        }
        
        plr = cbind(woi$Age, woi$Adj.Salary, woi$G60, woi$A60, woi$P60, woi$CF.Rel, woi$SCF.Rel, woi$GF.Rel, woi$ZSO., woi$TOI., woi$TOIT., woi$TOIC., woi$CorT., woi$CorC., woi$iCF60)[which(woi$nn == player()), ]
        comps = newdata[newdata[,1] != input$player1, ][1:input$n1, ]
        avg = c(mean(as.numeric(as.character(comps[,6]))),mean(as.numeric(as.character(comps[,7]))),mean(as.numeric(as.character(comps[,8]))),mean(as.numeric(as.character(comps[,9]))),mean(as.numeric(as.character(comps[,10]))),mean(as.numeric(as.character(comps[,11]))),mean(as.numeric(as.character(comps[,12]))),mean(as.numeric(as.character(comps[,13]))),mean(as.numeric(as.character(comps[,14]))),mean(as.numeric(as.character(comps[,15]))),mean(as.numeric(as.character(comps[,16]))),mean(as.numeric(as.character(comps[,17]))),mean(as.numeric(as.character(comps[,18]))),mean(as.numeric(as.character(comps[,19]))),mean(as.numeric(as.character(comps[,20]))))
        diff = as.vector(plr) - as.vector(avg)
        temp = data.frame(rbind(plr,avg,diff))
        names = c("Player", "Average", "Difference")
        table = cbind(names, temp)
        colnames(table) = c("","Age", "Adj. Salary", "G/60", "A/60", "P/60", "Rel CF%", "Rel SCF%", "Rel GF%", "OZ/DZSt%", "TOI%", "TOI% QoT", "TOI% QoC", "Corsi QoT", "Corsi QoC", "iCF/60")    
        table
    })
  
    output$table2 = renderDataTable({ table2.contents()}, options = list(searching = F, paging = F, scrollX = T, info=FALSE))
})
