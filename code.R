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
StepsTimeSeries<-sapply(split(MonData,MonData$interval), function (x) colMeans (x[1],na.rm=TRUE))
stepTimeSeriest<-as.data.frame(StepsTimeSeries)
MyMinutes<-as.numeric(substr(rownames(stepTimeSeriest),1,str_locate(rownames(stepTimeSeriest),"s")-2))
stepTimeSeriest<-cbind(stepTimeSeriest,MyMinutes)
rownames(stepTimeSeriest)<-c()
colnames(stepTimeSeriest)<-c("TotalSteps","Minutes")

# Plot time series plot

plot(stepTimeSeriest[,c(2,1)],type = "l",ylab= "Mean Steps",main="Mean Steps Across the Day")

#Find largest number of step in 5 minute window

startMin<-stepTimeSeriest[stepTimeSeriest$TotalSteps==max(stepTimeSeriest$TotalSteps),2]
print(paste("Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:- ",startMin,"-",startMin+5,"minutes" ))

# Imputing missing values

#Number of  missing values

permiss<-mean(is.na(MonData$steps))

print(paste("Calculate and report the total number of missing values in the dataset:-",permiss*nrow(MonData)))

#Replacing Na with average number of steps in time increment
library(stringr)
StepsTimeSeries<-sapply(split(MonData,MonData$interval), function (x) colMeans (x[1],na.rm=TRUE))
stepTimeSeriest<-as.data.frame(StepsTimeSeries)
MyMinutes<-as.numeric(substr(rownames(stepTimeSeriest),1,str_locate(rownames(stepTimeSeriest),"s")-2))
stepTimeSeriest<-cbind(stepTimeSeriest,MyMinutes)
rownames(stepTimeSeriest)<-c()
colnames(stepTimeSeriest)<-c("TotalSteps","Minutes")

MonDataNoNA<-MonData

             for (i in which(is.na(MonDataNoNA$steps))) {
                    MonDataNoNA$steps[i]<- stepTimeSeriest$TotalSteps[stepTimeSeriest$Minutes==MonDataNoNA$interval[i]]
             }

#What is mean total number of steps taken per day?

#First Including NA's
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

#Now With NA's Replaced with averages
        StepsPerDay<-sapply(split(MonDataNoNA,MonDataNoNA$date), function (x) colSums (x[1]))
        StepsPerDaytNoNa<-as.data.frame(StepsPerDay)
        Mydates<-substr(rownames(StepsPerDaytNoNa),1,10)
        StepsPerDaytNoNa<-cbind(StepsPerDaytNoNa,Mydates)
        rownames(StepsPerDaytNoNa)<-c()
        StepsPerDaytNoNa<-as.data.frame(StepsPerDaytNoNa)
        colnames(StepsPerDaytNoNa)<-c("TotalSteps","Date")
        StepsPerDaytNoNa$Date<-as.POSIXct(StepsPerDaytNoNa$Date,format="%Y-%m-%d")
        StepsPerDaytNoNa$TotalSteps<-as.numeric(as.character(StepsPerDaytNoNa$TotalSteps))

#Plot Histograms
par(mfrow=c(1,2))
barplot(TotalSteps~Date,data=StepsPerDayNoNa,space=0, xlab="",ylab="Total Steps Per Day",las=2,cex.axis=0.7,cex=0.6,main="Histrogram of steps per day - NA's Removed",cex.main=0.5)
barplot(TotalSteps~Date,data=StepsPerDaytNoNa,space=0, xlab="",ylab="Total Steps Per Day",las=2,cex.axis=0.7,cex=0.6,main="Histrogram of steps per day - NA's Replaced",cex.main=0.5)

#Summary stats
#With NA's Removed
print("Summary stats for data where NAs are removed")
summary(StepsPerDayNoNa$TotalSteps)
print("Interquartile Range for data where NAs are removed")
IQR(StepsPerDayNoNa$TotalSteps)

#With NA's Replaced
print("Summary stats for data where NAs are replaced with means")
summary(StepsPerDaytNoNa$TotalSteps)
print("Interquartile Range for data where NAs are replaced with means")
IQR(StepsPerDaytNoNa$TotalSteps)

## Are there differences in activity patterns between weekdays and weekends?

# Add column to identify Weekdays and Weekends

MonDataPlus<-MonDataNoNA

MonDataPlus$Day<-weekdays(MonDataNoNA$date,abbreviate = TRUE)
MonDataPlus$Weekend<-ifelse(MonDataPlus$Day=="Sat"|MonDataPlus$Day=="Sun",TRUE,FALSE)
MonDataPlus$Weekday<-ifelse(MonDataPlus$Day=="Sat"|MonDataPlus$Day=="Sun",FALSE,TRUE)
MonDataPlus$Day<-NULL

# Create Data sets with Weekdays only and  Weekend only

MonWE<-MonDataPlus[MonDataPlus$Weekend==TRUE,]
MonWD<-MonDataPlus[MonDataPlus$Weekend==FALSE,]


# What is the average daily activity pattern at Weekend?
library(stringr)

StepsTimeSeries<-sapply(split(MonWE,MonWE$interval), function (x) colMeans (x[1],na.rm=TRUE))
stepTimeSeriesWE<-as.data.frame(StepsTimeSeries)
MyMinutes<-as.numeric(substr(rownames(stepTimeSeriesWE),1,str_locate(rownames(stepTimeSeriesWE),"s")-2))
stepTimeSeriesWE<-cbind(stepTimeSeriesWE,MyMinutes)
rownames(stepTimeSeriesWE)<-c()
colnames(stepTimeSeriesWE)<-c("TotalSteps","Minutes")
stepTimeSeriesWE$Period<-rep("Weekend",nrow(stepTimeSeriesWE))


# What is the average daily activity pattern on Weekdays?
StepsTimeSeries<-sapply(split(MonWD,MonWD$interval), function (x) colMeans (x[1],na.rm=TRUE))
stepTimeSeriesWD<-as.data.frame(StepsTimeSeries)
MyMinutes<-as.numeric(substr(rownames(stepTimeSeriesWD),1,str_locate(rownames(stepTimeSeriesWD),"s")-2))
stepTimeSeriesWD<-cbind(stepTimeSeriesWD,MyMinutes)
rownames(stepTimeSeriesWD)<-c()
colnames(stepTimeSeriesWD)<-c("TotalSteps","Minutes")
stepTimeSeriesWD$Period<-rep("Weekday",nrow(stepTimeSeriesWD))

# Combine data
StepTime<-rbind(stepTimeSeriesWE,stepTimeSeriesWD)

# Plot timeseries plots
library(lattice)
xyplot(TotalSteps~Minutes|Period, data=StepTime,type = c("l"),layout = c(1, 2))



















