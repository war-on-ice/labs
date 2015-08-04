

setwd ("/srv/shiny-server/dCorsi")
runcode <- round(runif(1)*100000)

.libPaths ("/home/ubuntu/R/x86_64-unknown-linux-gnu-library/3.1")

#library(dplyr)
#source ("../warbase/R/variables.R")
#source ("../woi-makedata.R")

library(warbase)
load ("../common-data/woi-common.RData")

compress.player.game.thisone <- function(df)  ## that is, from the individual game table.
    summarize(df,
              SH=sum(SHOT),    MS=sum(MISS),    G=sum(GOAL),
              HIT=sum(HIT),   "HIT-"=sum(HIT_TAKEN),   
              TK=sum(TAKE),             GV=sum(GIVE),
              PN=sum(PENL_TAKEN),      "PN-"=sum(PENL_DRAWN),
              BK=sum(BLOCKED_SHOT),       AB=sum(BLOCK),
              A=sum(ASSIST+ASSIST_2), A1=sum(ASSIST),     A2=sum(ASSIST_2),#  A1=A-A2,
              iSC=sum(iSC),
              
              FO_W=sum(FAC_WIN),        FO_L=sum(FAC_LOSE),
              
              CF=sum(CF), CA=sum(CA), CFoff=sum(CFoff), CAoff=sum(CAoff),
              FF=sum(FF), FA=sum(FA), FFoff=sum(FFoff), FAoff=sum(FAoff),
              SF=sum(SF), SA=sum(SA), SFoff=sum(SFoff), SAoff=sum(SAoff),
              GF=sum(GF), GA=sum(GA), GFoff=sum(GFoff), GAoff=sum(GAoff),
              SCF=sum(SCF), SCA=sum(SCA), SCFoff=sum(SCFoff), SCAoff=sum(SCAoff),
              
              ZSO=sum(ZSO),   ZSN=sum(ZSN), ZSD=sum(ZSD),
              ZSOoff=sum(ZSOoff),ZSNoff=sum(ZSNoff),ZSDoff=sum(ZSDoff),

              TOI.on=sum(TOI, na.rm=TRUE)/60, TOIoff=sum(TOIoff, na.rm=TRUE)/60,
              
              cTOI60=sum(cTOI60*TOI/60, na.rm=TRUE)/TOI.on*100,
              tTOI60=sum(tTOI60*TOI/60, na.rm=TRUE)/TOI.on*100,

              Gm=length(unique(paste(season, gcode)))
              )

player.connect <- src_sqlite("../common-data/waronice.sqlite")
sql.statement <- paste0 ("SELECT * FROM player7_1_0 WHERE season >= 20052006")   ##,period.set())
st.by.date <- tbl(player.connect, sql(sql.statement)) %>% collect %>% group_by(season, ID, Team) %>% compress.player.game.thisone %>% filter (Team != "") %>%
    mutate (Name=roster.unique$firstlast[match(ID, roster.unique$woi.id)],
            pos=roster.unique$pos[match(ID, roster.unique$woi.id)]) %>%
    filter (pos != "G")

pre.lm <- mutate (st.by.date, teamyear=paste0(Team, season),
                  TOI.G=TOI.on/Gm,
                  OZPct=ZSO/(ZSO + ZSN + ZSD),
                  NZPct=ZSN/(ZSO + ZSN + ZSD),
                  ##pos=pos,
                  CF60=CF/TOI.on*60,
                  CA60=CA/TOI.on*60,
                  FF60=FF/TOI.on*60,
                  FA60=FA/TOI.on*60)
pre.lm <- pre.lm[complete.cases(pre.lm),]
                     
r1 <- lm (CF60 ~ pos + TOI.G + teamyear + tTOI60 + OZPct + NZPct, weights=TOI.on, data=pre.lm)
#plot(r1$fitted.values + r1$residuals, r1$fitted.values); abline(a=0,b=1,col=2)
r2 <- lm (CA60 ~ pos + TOI.G + teamyear + tTOI60 + OZPct + NZPct, weights=TOI.on, data=pre.lm)
#plot(r2$fitted.values + r2$residuals, r2$fitted.values)
r3 <- lm (FF60 ~ pos + TOI.G + teamyear + tTOI60 + OZPct + NZPct, weights=TOI.on, data=pre.lm)
r4 <- lm (FA60 ~ pos + TOI.G + teamyear + tTOI60 + OZPct + NZPct, weights=TOI.on, data=pre.lm)

post.lm <- cbind (pre.lm,
                   ECF60 = c(r1$fitted.values),
                   ECA60 = c(r2$fitted.values),
                   EFF60 = c(r3$fitted.values),
                   EFA60 = c(r4$fitted.values))

post.lm <- mutate (post.lm,
                   dCorsi60=CF60-ECF60-(CA60-ECA60),
                   dFenwick60=FF60-EFF60-(FA60-EFA60),
                   dCFImpact=(CF60-ECF60)*TOI.on/60,
                   dCAImpact=(CA60-ECA60)*TOI.on/60,
                   dFFImpact=(FF60-EFF60)*TOI.on/60,
                   dFAImpact=(FA60-EFA60)*TOI.on/60,
                   
                   dCorsiImpact=dCorsi60*TOI.on/60,
                   dFenwickImpact=dFenwick60*TOI.on/60)


burtch.table <- post.lm[,c("Gm","Name","pos","teamyear","tTOI60","TOI.G","OZPct",
                           "NZPct","CF60","CA60","FF60","FA60","ECF60","ECA60","EFF60",
                           "EFA60","dCFImpact", "dCAImpact", "dFFImpact", "dFAImpact", "dCorsi60","dFenwick60","dCorsiImpact","dFenwickImpact")]
burtch.table[,-(1:4)] <- round(burtch.table[,-(1:4)], 2)
save(burtch.table, file="burtch-table.RData")


#info <- function(grand.data) {
#    t1 <- table(grand.data$etype[grand.data$seconds != 3900 &
#                                 grand.data$home.skaters > 1 &
#                                 grand.data$away.skaters > 1 &
#                                 substr(grand.data$gcode,1,1)==2])
#    data.frame(saves=t1[["SHOT"]], shots=t1[["SHOT"]] + t1[["GOAL"]], svpct=t1[["SHOT"]]/(t1[["SHOT"]] + t1[["GOAL"]]))
#}
#for (ss in seasons[6:11]) {
#    load (paste0("../source-data/nhlscrapr-",ss,".RData"))
#    print(info(grand.data))
#}

