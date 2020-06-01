#Import Dataset

MonData<-read.csv("activity.csv")

MonData$date<-as.POSIXct(MonData$date,format="%Y-%m-%d")

#What is mean total number of steps taken per day?

StepsPerDay<-sapply(split(MonData,MonData$date), function (x) colSums (x[1]))
StepsPerDayt<-as.data.frame(StepsPerDay)
Mydates<-substr(rownames(StepsPerDayt),1,10)
StepsPerDayt<-cbind(StepsPerDayt,Mydates)
rownames(StepsPerDayt)<-c()
StepsPerDayt<-as.data.frame(StepsPerDayt)
colnames(StepsPerDayt)<-c("TotalSteps","Date")
StepsPerDayt$Date<-as.POSIXct(StepsPerDayt$Date,format="%Y-%m-%d")
StepsPerDayNoNa<-StepsPerDayt[!is.na(StepsPerDayt$TotalSteps),]
StepsPerDayNoNa$TotalSteps<-as.numeric(as.character(StepsPerDayNoNa$TotalSteps))

#Plot Histogram
barplot(TotalSteps~Date,data=StepsPerDayNoNa,space=0, xlab="",ylab="Total Steps Per Day",las=2,cex.axis=0.7,cex=0.6,main="Histrogram showing steps per day")

#Smmary stats
summary(StepsPerDayNoNa$TotalSteps)


# What is the average daily activity pattern?
library(stringr)
StepsTimeSeries<-sapply(split(MonData,MonData$interval), function (x) colSums (x[1],na.rm=TRUE))
stepTimeSeriest<-as.data.frame(StepsTimeSeries)
MyMinutes<-as.numeric(substr(rownames(stepTimeSeriest),1,str_locate(rownames(stepTimeSeriest),"s")-2))
stepTimeSeriest<-cbind(stepTimeSeriest,MyMinutes)
rownames(stepTimeSeriest)<-c()
colnames(stepTimeSeriest)<-c("TotalSteps","Minutes")

# Plot time series plot

plot(stepTimeSeriest[,c(2,1)],type = "l",ylab= "Steps",main="Step Across the Day")

#Find largest number of step in 5 minute window

startMin<-stepTimeSeriest[stepTimeSeriest$TotalSteps==max(stepTimeSeriest$TotalSteps),2]
print(paste("Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:- ",startMin,"-",startMin+5,"minutes" ))
