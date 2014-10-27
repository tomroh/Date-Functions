#function for calculating number of specific weekdays (M,T,etc.) between dates
#dates must be in chron format (Note: For Excel Dates, set origin = c(month = 12, day = 30, year = 1899))

nweekdays <- Vectorize(function(a, b) ifelse(!is.na(a) & !is.na(b),sum(!wday(seq(a, b, "days")) %in% c(1, 7)),NA))
nweekends <- Vectorize(function(a, b) ifelse(!is.na(a) & !is.na(b),sum(wday(seq(a, b, 'days')) %in% c(1, 7)),NA))


nweekdays <- function(startdate,enddate,weekdayx)
{
  startwday <- as.numeric(weekdays(startdate))
  endwday <- as.numeric(weekdays(enddate))
  datediff <- (enddate-startdate)+1
  nweekdays <- floor(datediff/7)
  if (weekdayx >= startwday | weekdayx <= endwday) {
    nweekdays <- nweekdays + 1
  }
  nweekdays
}


#calculate all counts of all weekdays in a week between dates
allnweekdays <- function(startdate,enddate)
{
  startwday <- as.numeric(weekdays(startdate))
  endwday <- as.numeric(weekdays(enddate))
  datediff <- (enddate-startdate)+1
  WDs <- floor(datediff/7)
  WDCount <- c()
  for (weekdayx in 1:7)
  {
    if (weekdayx >= startwday | weekdayx <= endwday) {
      j <- WDs + 1
    }
    else {
      j <- WDs
    }
    WDCount <- c(WDCount,j)
  }
  WDCount
}

allnweekdays(arr[1,1],arr[nrow(arr),1])
