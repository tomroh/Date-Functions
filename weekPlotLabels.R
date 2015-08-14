wd.list <- lapply(as.character(wday(1:7,label=TRUE)),function(x) c(x,"")) %>% unlist()
hour.intervals <- c(rep(seq(0,23,12),7),0)
labels <- paste(hour.intervals,wd.list,sep="\n")
labels
