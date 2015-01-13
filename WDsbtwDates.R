#function for calculating number of specific weekdays (M,T,etc.) between dates
#dates must be in chron format (Note: For Excel Dates, set origin = c(month = 12, day = 30, year = 1899))

nweekdays <- Vectorize(function(a, b) ifelse(!is.na(a) & !is.na(b),sum(!wday(seq(a, b, "days")) %in% c(1, 7)),NA))
nweekends <- Vectorize(function(a, b) ifelse(!is.na(a) & !is.na(b),sum(wday(seq(a, b, 'days')) %in% c(1, 7)),NA))


#calculate number of weekdays between dates
nweekdays <- function(startdate,enddate,weekdayx) {
    require(lubridate)
    dates <- startdate + seq(0,enddate-startdate)
    sum(wday(dates) == weekdayx)
}

#calculate all counts of all weekdays in a week between dates
sapply(1:7,nweekdays,startdate = as.Date('2012-07-01'),
       enddate = as.Date('2014-06-30'))

