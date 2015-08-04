

simple.test <- function () {#es.scoring=c(2.5, 2.5),   #goals per game
                        #en.scoring=c(7.5, 20),      #goals per game
                        #time.sequence=c(75, 180),
                        #sims=100000) {
  es.scoring=c(2.5, 2.5)   #goals per game
  en.scoring=c(7.5, 20)      #goals per game
  time.sequence=c(75, 105, 180)
  sims=100000
  
  es.trail.time <- rexp (sims, es.scoring[1]/3600)
  es.lead.time <- rexp (sims, es.scoring[2]/3600)
  #es.trail.time[es.trail.time > es.lead.time] <- 3600
  
  es.tie.prob.table <- sapply(time.sequence, function(tt) {c(mean(es.trail.time < tt & es.lead.time > es.trail.time),
                                                             mean(es.lead.time < tt & es.lead.time < es.trail.time))})
  es.tie.prob.table <- rbind(es.tie.prob.table, 1-colSums(es.tie.prob.table))
  rownames(es.tie.prob.table) <- paste0("ES", c("Tie","Down","Nothing"))
  
  en.trail.time <- rexp (sims, en.scoring[1]/3600)
  en.lead.time <- rexp (sims, en.scoring[2]/3600)
  en.trail.time[en.trail.time > en.lead.time] <- 3600
  
  en.tie.prob.table <- sapply(time.sequence, function(tt) {c(mean(en.trail.time < tt & en.lead.time > en.trail.time),
                                                             mean(en.lead.time < tt & en.lead.time < en.trail.time))})
  en.tie.prob.table <- rbind(en.tie.prob.table, 1-colSums(en.tie.prob.table))
  rownames(en.tie.prob.table) <- paste0("EN", c("Tie","Down","Nothing"))

  output <- rbind(en.tie.prob.table,
                  es.tie.prob.table)
  colnames(output) <- time.sequence
  p.tie.75 <- output[6,2]*output[1,1] + output[4,2]
  p.tie.180 <- output[1,3]
  
  
}


grand.test <- function (es.scoring=c(2.5, 2.5),   #goals per game
                        en.scoring=c(6, 15),      #goals per game
                        time.sequence=seq(5, 1200, by=5),
                        sims=100000) {
  #es.scoring=c(2.5, 2.5);  en.scoring=c(6, 15);  time.sequence=seq(5, 1200, by=5); sims=100000

  es.trail.time <- rexp (sims, es.scoring[1]/3600)
  es.lead.time <- rexp (sims, es.scoring[2]/3600)
  es.trail.time[es.trail.time > es.lead.time] <- 3600
  
  es.tie.prob.table <- sapply(time.sequence, function(tt) {mean(es.trail.time < tt)})
  
  en.trail.time <- rexp (sims, en.scoring[1]/3600)
  en.lead.time <- rexp (sims, en.scoring[2]/3600)
  en.trail.time[en.trail.time > en.lead.time] <- 3600
  
  en.tie.prob.table <- sapply(time.sequence, function(tt) mean(en.trail.time < tt))

  output <- rbind(en.tie.prob.table,
                  es.tie.prob.table,
                  en.tie.prob.table-es.tie.prob.table)
  colnames(output) <- time.sequence
  rownames(output) <- c("EN","ES","Diff")

  return(output)
}

grand.test.2goals <- function (es.scoring=c(2.5, 2.5),   #goals per game
                               en.scoring=c(6, 15),      #goals per game
                               time.sequence=seq(5, 1200, by=5),
                               sims=100000) {
  #es.scoring=c(2.5, 2.5);  en.scoring=c(6, 15);  time.sequence=seq(5, 1200, by=5); sims=100000

  es.trail.time <- rgamma (sims, 2, es.scoring[1]/3600)
  es.lead.time <- rexp (sims, es.scoring[2]/3600)
  es.trail.time[es.trail.time > es.lead.time] <- 3600
  
  es.tie.prob.table <- sapply(time.sequence, function(tt) {mean(es.trail.time < tt)})
  
  en.trail.time <- rgamma (sims, 2, en.scoring[1]/3600)
  en.lead.time <- rexp (sims, en.scoring[2]/3600)
  en.trail.time[en.trail.time > en.lead.time] <- 3600
  
  en.tie.prob.table <- sapply(time.sequence, function(tt) mean(en.trail.time < tt))

  output <- rbind(en.tie.prob.table,
                  es.tie.prob.table,
                  en.tie.prob.table-es.tie.prob.table)
  colnames(output) <- time.sequence
  rownames(output) <- c("EN","ES","Diff")

  return(output)
}
