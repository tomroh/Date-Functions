remove.weekends <- function(x,y) { 
    adjLOS <- wday(gen$Admit.Date.Time) +
        rep(0:6,length.out = 1+trunc(gen$GenLOS+hour(gen$Discharge.Date.Time)/24+minute(gen$Discharge.Date.Time)/60/24))
    adjLOS <- max(0,sum(adjLOS > 1 & adjLOS < 7) + hour(gen$Discharge.Date.Time)/24 + minute(gen$Discharge.Date.Time)/60/24)
    adjLOS
}
