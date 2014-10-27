library(plyr)
library(fitdistrplus)
library(chron)
library(reshape)


rm(x)
rm(countm)
rm(countwd)
rm(counth)
rm(countwdh)

x <- read.table(file="Times/nonMHDischargearrtimes.csv", header = F,colClass = "character")

arrfunction(x)
?cast
arrfunction <- function(x) {


#split hour and minutes and bind into dataframe
timex <- strsplit(x[,2],":")
timex <- ldply(timex)

#change string date to date object
datex <- data.frame(as.Date(x[,1],"%m/%d/%Y"))
#strptime(x[,2], format="%H:%M"))

#create data frame with data and time descriptors
datetimesx <- data.frame(datex,years(datex[,1]),x[,2],months(datex[,1]),as.numeric(days(datex[,1])),weekdays(datex[,1]),as.numeric(timex[,1]),as.numeric(timex[,2]))
colnames(datetimesx) = c("Date","Time","Year","Month","Day","Weekday","Hour","Minute")

#function for calculating number of specific weekdays (M,T,etc.) between dates
nweekdays <- function(startdate,enddate,weekdayx)
{
startwday <- as.POSIXlt(startdate)$wday
endwday <- as.POSIXlt(enddate)$wday+1
datediff <- as.numeric(enddate-startdate)
nweekdays <- floor(datediff/7)
if (weekdayx >= startwday | weekdayx <= endwday) {
	nweekdays <- nweekdays + 1
	}
nweekdays
}

#calculate count for each weekday
ndays <- c()
for (i in 1:7) 
{
	ndays[i] <- nweekdays(datetimesx[1,1],datetimesx[nrow(datetimesx),1],i)
}
ndays

#plot arrivals by month
par(mfrow=c(2,2))
countm <- as.data.frame(table(datetimesx[,4]))
countm <- countm[order((factor(countm[,1],levels = month.name))),]
countm <- cbind(countm,countm[,2]/sum(countm[,2])*100)
colnames(countm) <- c("Month", "Freq","Percentage")
barplot(countm[,2],xlab="Month",ylab="Patient Arrivals",names.arg = substr(countm[,1],1,3),col="2")

#plot arrivals by weekday
wdlist <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
countwd <- as.data.frame(table(datetimesx[,6]))
countwd <- countwd[order((factor(countwd[,1],levels = wdlist))),]
countwd <- cbind(countwd,ndays,countwd[,2]/ndays,countwd[,2]/sum(countwd[,2])*100)
colnames(countwd) <- c("Weekday", "Freq","N Days","Mean","Percentage")
barplot(countwd[,4],xlab="Weekday",ylab="Mean Patient Arrivals per Day",names.arg = substr(countwd[,1],1,3),col="3")
countwd

#plot arrivals by hour of day
hxlabel <- c(0,' ',2,' ',4,' ',6,' ',8,' ',10,' ',12,' ',14,' ',16,' ',18,' ',20,' ',22,' ')
counth <- as.data.frame(table(datetimesx[,7]))
counth <- counth[order((factor(counth[,1],))),]
counth <- cbind(counth,counth[,2]/datediff,counth[,2]/sum(counth[,2])*100)
colnames(counth) <- c("Hour", "Freq","Mean","Percentage")
barplot(counth[,3],xlab="Hour of Day",ylab="Mean Patient Arrivals per Hour",names.arg = hxlabel,col="4")

#plot by hour of day with weekday series
countwdh <- datetimesx[,6:7]
countwdh <- as.data.frame(table(datetimesx[,6],datetimesx[,7]))
countwdh <- cast(countwdh,Var2 ~ Var1)
countwdh <- countwdh[,c("Var2",wdlist)]
meanwdh <- countwdh[2:8]/ndays
ts.plot(meanwdh,gpars = list(xlab="Hour",col = rainbow(10),lwd=4,axes=F))
legend("topleft", inset=.05, title="Weekdays",wdlist,fill = rainbow(10))
axis(1,0:23)
axis(2,0:ceiling(max(meanwdh)))

#print pdf
pdf('ArrivalBreakdown.pdf',15,10)
par(mfrow=c(2,2))
hxlabel <- c(0,' ',2,' ',4,' ',6,' ',8,' ',10,' ',12,' ',14,' ',16,' ',18,' ',20,' ',22,' ')
barplot(countm[,2],xlab="Month",ylab="Patient Arrivals",names.arg = substr(countm[,1],1,3),col="2")
barplot(countwd[,4],xlab="Weekday",ylab="Mean Patient Arrivals per Day",names.arg = substr(countwd[,1],1,3),col="3")
barplot(counth[,3],xlab="Hour of Day",ylab="Mean Patient Arrivals per Hour",names.arg = hxlabel,col="4")
ts.plot(meanwdh,gpars = list(xlab="Hour of Day",col = rainbow(10),lwd=2,axes=F))
legend("topleft", inset=.05,wdlist,fill = rainbow(10),cex = 1)
axis(1,0:23)
axis(2,0:ceiling(max(meanwdh)))
dev.off()

#print image
jpeg('ArrivalBreakdown.jpg',1200,900)
par(mfrow=c(2,2))
hxlabel <- c(0,' ',2,' ',4,' ',6,' ',8,' ',10,' ',12,' ',14,' ',16,' ',18,' ',20,' ',22,' ')
barplot(countm[,2],xlab="Month",ylab="Patient Arrivals",names.arg = substr(countm[,1],1,3),col="2")
barplot(countwd[,4],xlab="Weekday",ylab="Mean Patient Arrivals per Day",names.arg = substr(countwd[,1],1,3),col="3")
barplot(counth[,3],xlab="Hour of Day",ylab="Mean Patient Arrivals per Hour",names.arg = hxlabel,col="4")
ts.plot(meanwdh,gpars = list(xlab="Hour of Day",col = rainbow(10),lwd=2,axes=F))
legend("topleft", inset=.05,wdlist,fill = rainbow(10),cex = 1)
axis(1,0:23)
axis(2,0:ceiling(max(meanwdh)))
dev.off()


}
